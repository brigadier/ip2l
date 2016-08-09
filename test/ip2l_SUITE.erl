-module(ip2l_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("ip2l.hrl").
-include("ip2l_format.hrl").
%% API
-compile(export_all).


groups() -> [{main, [], [
	testip2l,
	loadtest100k

]}].

all() -> [{group, main}].

testip2l(Config) ->
	{ok, #meta{dbtype = 1}} = ip2l:state(pool1),
	undefined = ip2l:state(pool3),
	#ip2l{country_short = <<"-">>, country_long = <<"-">>} = ip2l:lookup(pool1, {10, 10, 10, 10}),
	#ip2l{country_short = <<"AU">>, country_long = <<"Australia">>} = ip2l:lookup(pool1, {1, 10, 10, 10}),
	{error, closed} = ip2l:lookup(pool3, {1, 10, 10, 10}),

	DataDir = ?config(data_dir, Config),
	Dir3 = filename:join(DataDir, "dir3"),
	ok = file:make_link(filename:join(DataDir, "IP-COUNTRY-SAMPLE.BIN"), filename:join(Dir3, "IP-COUNTRY-SAMPLE.BIN")),
	ok = ip2l:reload_base(pool3),
	#ip2l{country_short = <<"AU">>, country_long = <<"Australia">>} = ip2l:lookup(pool1, {1, 10, 10, 10}),

	Dir2 = filename:join(DataDir, "dir2"),
	ok = ip2l:start_pool(pool2, [{size, 2}, {sup_flags, {one_for_all, 1, 5}}], Dir2),
	#ip2l{country_short = <<"AU">>, country_long = <<"Australia">>} = ip2l:lookup(pool2, {1, 10, 10, 10}),
	ip2l:stop_pool(pool1),
	case catch ip2l:lookup(pool1, {10, 10, 10, 10}) of
		{'EXIT', _} -> ok
	end,

	ip2l:reload_base(pool2, undefined),
	{error, closed} = ip2l:lookup(pool2, {10, 10, 10, 10}),

	ok.


%%looks like much slower than geoip2 :-(
loadtest100k(Config) ->
	ip2l:stop_pool(pool1),
	DataDir = ?config(data_dir, Config),
	Dir1 = filename:join(DataDir, "dir1"),
	ok = ip2l:start_pool(pool1, [{size, 32}, {sup_flags, {one_for_all, 1, 5}}], Dir1),
	erlang:process_flag(trap_exit, true),
	rand:seed(exs64),
	L1 = lists:seq(1, 1000),
	L2 = lists:seq(1, 100),
	?debugVal(calendar:local_time()),
	lists:foreach(
		fun(_) ->
			spawn_link(
				fun() ->
					lists:foreach(
						fun(_) ->
							IP = {rand:uniform(255), rand:uniform(255), rand:uniform(255), rand:uniform(255)},
							R = ip2l:lookup(pool1, IP),
							true = is_tuple(R) orelse R == not_found
						end,
						L2
					)
				end
			)
		end,
		L1
	),


	lists:foreach(
		fun(_) ->
			receive
				{'EXIT', _, normal} -> ok;
				X -> exit(X)
			end
		end,
		L1

	),
	?debugVal(calendar:local_time()),
	ok.


init_per_suite(Config) ->
	DataDir = ?config(data_dir, Config),
	Dir1 = filename:join(DataDir, "dir1"),
	Dir2 = filename:join(DataDir, "dir2"),
	Dir3 = filename:join(DataDir, "dir3"),
	os:cmd("rm -Rf " ++ filename:join(Dir1, "ip2l-current")),
	os:cmd("rm -Rf " ++ filename:join(Dir2, "ip2l-current")),
	os:cmd("rm -Rf " ++ filename:join(Dir3, "ip2l-current")),


	application:load(ip2l),
	application:set_env(
		ip2l,
		pools,
		[
			{
				pool1, [
				[
					{size, 2},
					{sup_flags, {one_for_all, 1, 5}}
				],
				[
					{path, Dir1}

				]]
			},
			{
				pool3, [
				[
					{size, 2},
					{sup_flags, {one_for_all, 1, 5}}
				],
				[
					{path, Dir3}
				]]
			}
		]
	),

	ip2l:start(),
	Config.

end_per_suite(_Config) ->
	application:stop(ip2l),
	application:stop(simplepool),
	ok.



init_per_group(_GroupName, Config) ->
	Config.


end_per_group(_GroupName, _Config) ->
	ok.


init_per_testcase(_TestCase, Config) ->
	Config.

end_per_testcase(_TestCase, _Config) ->
	ok.