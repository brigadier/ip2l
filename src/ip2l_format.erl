-module(ip2l_format).

-include("../include/ip2l_format.hrl").
-include("ip2l.hrl").

%% API

-export([open/1, close/1, meta/1, lookup/3]).

meta(#ip2lfile{meta = Meta}) ->
	Meta.


open(FileName) ->
	case prim_file:open(FileName, [read, raw, binary]) of
		{ok, H} -> try_read_meta(H);
		Error -> Error
	end.

close(#ip2lfile{fhandle = Handle}) ->
	prim_file:close(Handle).


try_read_meta(Handle) ->
	case prim_file:read(Handle, 29) of
		{ok, <<DBType:8/unsigned-integer, DBColumn:8/unsigned-integer,
			   DBYear:8/unsigned-integer, DBMonth:8/unsigned-integer, DBDay:8/unsigned-integer,
			   IPv4DBCount:32/little-unsigned-integer, IPv4DBAddr:32/little-unsigned-integer,
			   IPv6DBCount:32/little-unsigned-integer, IPv6DBAddr:32/little-unsigned-integer,
			   IPv4IndexBaseAddr:32/little-unsigned-integer, IPv6IndexBaseAddr:32/little-unsigned-integer>>
		} ->

			IPv4columnsize = DBColumn bsl 2,
			IPv6columnsize = 16 + ((DBColumn - 1) bsl 2),

			{ok, #ip2lfile{
				fhandle = Handle,
				meta = #ip2lmeta{
					dbtype = DBType,
					dbcolumn = DBColumn,
					dbdate = {2000 + DBYear, DBMonth, DBDay},
					ipv4dbaddr = IPv4DBAddr,
					ipv4dbcount = IPv4DBCount,
					ipv6dbaddr = IPv6DBAddr,
					ipv6dbcount = IPv6DBCount,
					ipv4indexbaseaddr = IPv4IndexBaseAddr,
					ipv6indexbaseaddr = IPv6IndexBaseAddr,
					ipv4columnsize = IPv4columnsize,
					ipv6columnsize = IPv6columnsize
				}
			}};
		_ ->
			prim_file:close(Handle),
			{error, meta}
	end.


start_lo_hi(V, IntIP, _DBCount, IndexBaseAddr, FHandle) when IndexBaseAddr > 0 ->
	Shift = ifv4(V, 16, 112),

	Offset = ((IntIP bsr Shift) bsl 3) + IndexBaseAddr,
	{Low, High} = read2i(FHandle, Offset),
	{Low, High};

start_lo_hi(_V, _IntIP, DBCount, _IndexBaseAddr, _FHandle) ->
	{0, DBCount}.

lookup(v4, IntIPv4, #ip2lfile{fhandle = FHandle,
							  meta = #ip2lmeta{dbtype = DBType, ipv4dbaddr = DBAddr,
											   ipv4dbcount = DBCount, ipv4indexbaseaddr = IndexBaseAddr,
											   ipv4columnsize = ColumnSize}} = _IP2LFile) ->
	{Low, High} = start_lo_hi(v4, IntIPv4, DBCount, IndexBaseAddr, FHandle),
	bisect_ip(v4, IntIPv4, FHandle, DBType, DBAddr, Low, High, ColumnSize);



lookup(v6, IntIPv6, #ip2lfile{fhandle = FHandle,
							  meta = #ip2lmeta{dbtype = DBType, ipv6dbaddr = DBAddr,
											   ipv6dbcount = DBCount, ipv6indexbaseaddr = IndexBaseAddr,
											   ipv6columnsize = ColumnSize}} = _IP2LFile) ->
	{Low, High} = start_lo_hi(v6, IntIPv6, DBCount, IndexBaseAddr, FHandle),
	bisect_ip(v6, IntIPv6, FHandle, DBType, DBAddr, Low, High, ColumnSize).


bisect_ip(V, IntIP, FHandle, DBType, DBAddr, Low, High, ColumnSize) when Low =< High ->
	Mid = (Low + High) bsr 1,

	FromRowOffset = DBAddr + (Mid * ColumnSize),
	{IPFrom, IPTo} = read_ip_from_to(V, FromRowOffset, ColumnSize, FHandle),

	if
		IPFrom =< IntIP, IntIP < IPTo ->
			Offset = ifv4(V, FromRowOffset, FromRowOffset + 12),
			read_record(DBType, Offset, FHandle);
		IntIP < IPFrom ->
			bisect_ip(V, IntIP, FHandle, DBType, DBAddr, Low, Mid - 1, ColumnSize);
		true ->
			bisect_ip(V, IntIP, FHandle, DBType, DBAddr, Mid + 1, High, ColumnSize)

	end;
bisect_ip(_V, _IntIP, _FHandle, _DBType, _DBAddr, _Low, _High, _ColumnSize) ->
	not_found.



read_ip_from_to(v4, Offset, ColumnSize, FHandle) ->
	From = readi(FHandle, Offset),
	To = readi(FHandle, Offset + ColumnSize),
	{From, To};

read_ip_from_to(v6, Offset, ColumnSize, FHandle) ->
	From = readi128(FHandle, Offset),
	To = readi128(FHandle, Offset + ColumnSize),
	{From, To}.

read_record(DBType, Offset, FHandle) ->
	{CSHort, CLong} = maybe_country(FHandle, Offset, DBType),
	#ip2l{
		country_short = country_short(CSHort),
		country_long = CLong,
		region = maybe_string(FHandle, Offset, DBType, ?REGION_POSITION),
		city = maybe_string(FHandle, Offset, DBType, ?CITY_POSITION),
		isp = maybe_string(FHandle, Offset, DBType, ?ISP_POSITION),
		latitude = maybe_float(FHandle, Offset, DBType, ?LATITUDE_POSITION),
		longitude = maybe_float(FHandle, Offset, DBType, ?LONGITUDE_POSITION),
		domain = maybe_string(FHandle, Offset, DBType, ?DOMAIN_POSITION),
		zipcode = maybe_string(FHandle, Offset, DBType, ?ZIPCODE_POSITION),
		timezone = maybe_string(FHandle, Offset, DBType, ?TIMEZONE_POSITION),
		netspeed = netspeed(maybe_string(FHandle, Offset, DBType, ?NETSPEED_POSITION)),
		idd_code = maybe_string(FHandle, Offset, DBType, ?IDDCODE_POSITION),
		area_code = maybe_string(FHandle, Offset, DBType, ?AREACODE_POSITION),
		weather_code = maybe_string(FHandle, Offset, DBType, ?WEATHERSTATIONCODE_POSITION),
		weather_name = maybe_string(FHandle, Offset, DBType, ?WEATHERSTATIONNAME_POSITION),
		mcc = maybe_string(FHandle, Offset, DBType, ?MCC_POSITION),
		mnc = maybe_string(FHandle, Offset, DBType, ?MNC_POSITION),
		mobile_brand = maybe_string(FHandle, Offset, DBType, ?MOBILEBRAND_POSITION),
		elevation = maybe_string(FHandle, Offset, DBType, ?ELEVATION_POSITION),
		usage_type = usage_type(maybe_string(FHandle, Offset, DBType, ?USAGETYPE_POSITION))
	}.


ifv4(v4, Then, _) -> Then;
ifv4(v6, _, Else) -> Else.


maybe_country(FHandle, Offset, DBType) ->
	case element(DBType, ?COUNTRY_POSITION) of
		0 -> {undefined, undefined};
		EVal ->
			Pos = readi(FHandle, Offset + ((EVal - 1) bsl 2)),
			CShort = reads(FHandle, Pos),
			CLong = reads(FHandle, Pos + 3),
			{CShort, CLong}
	end.

maybe_float(FHandle, Offset, DBType, What) ->
	case element(DBType, What) of
		0 -> undefined;
		EVal ->
			readf(FHandle, Offset + ((EVal - 1) bsl 2))
	end.

maybe_string(FHandle, Offset, DBType, What) ->
	case element(DBType, What) of
		0 -> undefined;
		EVal ->

			reads(FHandle, readi(FHandle, Offset + ((EVal - 1) bsl 2)))
	end.


read2i(FHandle, Offset) ->
	{ok, _} = prim_file:position(FHandle, Offset - 1),
	{ok, <<I:32/little-unsigned-integer, I2:32/little-unsigned-integer>>} = prim_file:read(FHandle, 8),
	{I, I2}.


readi(FHandle, Offset) ->
	{ok, _} = prim_file:position(FHandle, Offset - 1),
	{ok, <<I:32/little-unsigned-integer>>} = prim_file:read(FHandle, 4),
	I.

readi128(FHandle, Offset) ->
	{ok, _} = prim_file:position(FHandle, Offset - 1),
	{ok, <<I:128/little-unsigned-integer>>} = prim_file:read(FHandle, 16),
	I.

readf(FHandle, Offset) ->
	{ok, _} = prim_file:position(FHandle, Offset - 1),
	{ok, <<F:32/little-unsigned-float>>} = prim_file:read(FHandle, 4),
	F.

reads(FHandle, Offset) ->
	{ok, _} = prim_file:position(FHandle, Offset),
	{ok, <<L:8/little-unsigned-integer>>} = prim_file:read(FHandle, 1),
	{ok, S} = prim_file:read(FHandle, L),
	?BINARY_COPY(S).


usage_type(<<"COM">>) -> commercial;
usage_type(<<"ORG">>) -> organization;
usage_type(<<"GOV">>) -> government;
usage_type(<<"MIL">>) -> military;
usage_type(<<"EDU">>) -> educational;
usage_type(<<"LIB">>) -> library;
usage_type(<<"CDN">>) -> cdn;
usage_type(<<"ISP">>) -> fixed_isp;
usage_type(<<"MOB">>) -> mobile_isp;
usage_type(<<"ISP/MOB">>) -> fixed_mobile_isp;
usage_type(<<"DCH">>) -> datacentre;
usage_type(<<"SES">>) -> spider;
usage_type(<<"RSV">>) -> reserved;
usage_type(_) -> undefined.



netspeed(<<"DIAL">>) -> dialup;
netspeed(<<"DSL">>) -> dsl;
netspeed(<<"COMP">>) -> company;
netspeed(<<"T1">>) -> t1;
netspeed(_) -> undefined.

country_short(<<"??">>) -> <<"XX">>;
country_short(V) -> V.