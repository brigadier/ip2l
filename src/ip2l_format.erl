-module(ip2l_format).

-include("../include/ip2l_format.hrl").
-include("ip2l.hrl").

%% API

-export([open/1, close/1, lookup/2, meta/1]).

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
	case prim_file:read(Handle, 21) of
		{ok, <<DBType:8/unsigned-integer, DBColumn:8/unsigned-integer,
			DBYear:8/unsigned-integer, DBMonth:8/unsigned-integer, DBDay:8/unsigned-integer,
			IPv4DBCount:32/little-unsigned-integer, IPv4DBAddr:32/little-unsigned-integer,
			IPv6DBCount:32/little-unsigned-integer, IPv6DBAddr:32/little-unsigned-integer>>} when DBType =< 24 ->
			{ok, #ip2lfile{
				fhandle = Handle,
				meta = #ip2lmeta{
					dbtype = DBType,
					dbcolumn = DBColumn,
					dbdate = {2000 + DBYear, DBMonth, DBDay},
					ipv4dbaddr = IPv4DBAddr,
					ipv4dbcount = IPv4DBCount,
					ipv6dbaddr = IPv6DBAddr,
					ipv6dbcount = IPv6DBCount
				}
			}};
		_ ->
			prim_file:close(Handle),
			{error, meta}
	end.


lookup({A, B, C, D} = _IPv4, IP2LFile) ->
	WIp = (A bsl 24) + (B bsl 16) + (C bsl 8) + D,
	lookup_ipv4(WIp, IP2LFile);
lookup(IPv4, IP2LFile) ->
	lookup_ipv4(IPv4, IP2LFile).

lookup_ipv4(WIp, #ip2lfile{meta = #ip2lmeta{ipv4dbcount = DBCount}} = IP2LFile) ->
	bisect_ipv4(WIp, 0, DBCount, IP2LFile).

bisect_ipv4(WIp, Low, High, #ip2lfile{meta = #ip2lmeta{dbcolumn = DBColumn, ipv4dbaddr = BaseAddr}} = IP2LFile) when Low =< High ->
	Mid = (Low + High) div 2,
	IPFrom = read_ipv4(BaseAddr + Mid * (DBColumn * 4), IP2LFile),
	IPTo = read_ipv4(BaseAddr + (Mid + 1) * (DBColumn * 4), IP2LFile),
	if
		IPFrom =< WIp, WIp =< IPTo -> read_record_ipv4(Mid, IP2LFile);
		WIp < IPFrom -> bisect_ipv4(WIp, Low, Mid - 1, IP2LFile);
		true -> bisect_ipv4(WIp, Mid + 1, High, IP2LFile)
	end.


read_ipv4(Offset, #ip2lfile{fhandle = FHandle} = _IP2LFile) ->
	readi(FHandle, Offset).

read_record_ipv4(Offset, #ip2lfile{fhandle = FHandle, meta = #ip2lmeta{dbcolumn = DBColumn, ipv4dbaddr = BaseAddr, dbtype = DBType}}) ->
	#ip2l{
		country_short = country_short(maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?COUNTRY_POSITION, 1)),
		country_long = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?COUNTRY_POSITION, 4),
		region = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?REGION_POSITION, 1),
		city = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?CITY_POSITION, 1),
		isp = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?ISP_POSITION, 1),
		latitude = maybe_float_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?LATITUDE_POSITION),
		longitude = maybe_float_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?LONGITUDE_POSITION),
		domain = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?DOMAIN_POSITION, 1),
		zipcode = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?ZIPCODE_POSITION, 1),
		timezone = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?TIMEZONE_POSITION, 1),
		netspeed = netspeed(maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?NETSPEED_POSITION, 1)),
		idd_code = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?IDDCODE_POSITION, 1),
		area_code = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?AREACODE_POSITION, 1),
		weather_code = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?WEATHERSTATIONCODE_POSITION, 1),
		weather_name = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?WEATHERSTATIONNAME_POSITION, 1),
		mcc = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?MCC_POSITION, 1),
		mnc = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?MNC_POSITION, 1),
		mobile_brand = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?MOBILEBRAND_POSITION, 1),
		elevation = maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?ELEVATION_POSITION, 1),
		usage_type = usage_type(maybe_string_v4(FHandle, Offset, DBColumn, BaseAddr, DBType, ?USAGETYPE_POSITION, 1))
	}.

calc_off_v4(BaseAddr, What, Offset, DBColumn, DBType) ->
	BaseAddr + Offset * (DBColumn * 4) + 4 * (element(DBType, What) - 1).


maybe_string_v4(FHandle, Mid, DBColumn, BaseAddr, DBType, What, Plus) ->
	EVal = element(DBType, What),
	if
		EVal =/= 0 -> reads(FHandle, readi(FHandle, calc_off_v4(BaseAddr, What, Mid, DBColumn, DBType)) + Plus);
		true -> undefined
	end.

maybe_float_v4(FHandle, Mid, DBColumn, BaseAddr, DBType, What) ->
	EVal = element(DBType, What),
	if
		EVal =/= 0 -> readf(FHandle, calc_off_v4(BaseAddr, What, Mid, DBColumn, DBType));
		true -> undefined
	end.

readi(FHandle, Offset) ->
	{ok, _} = prim_file:position(FHandle, Offset - 1),
	{ok, <<I:32/little-unsigned-integer>>} = prim_file:read(FHandle, 4),
	I.

readf(FHandle, Offset) ->
	{ok, _} = prim_file:position(FHandle, Offset - 1),
	{ok, <<F:32/little-unsigned-float>>} = prim_file:read(FHandle, 4),
	F.

reads(FHandle, Offset) ->
	{ok, _} = prim_file:position(FHandle, Offset - 1),
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