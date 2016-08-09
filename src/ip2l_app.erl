-module(ip2l_app).
-behaviour(application).
-include("ip2l.hrl").
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	start_pools(),
	ip2l_sup:start_link().

stop(_State) ->
	ok.




start_pools() ->
	Pools = application:get_env(ip2l, pools, []),
	lists:foreach(
		fun({PoolName, [PoolOptions, Args]}) ->
			Size = proplists:get_value(size, PoolOptions, ?DEFAULT_WORKERS_NUM),
			SupFlags = proplists:get_value(sup_flags, PoolOptions, {one_for_all, 1, 5}),
			ok = simplepool:start_pool(
				PoolName,
				Size,
				ip2l_worker,
				Args,
				SupFlags,
				ip2l_controller
			)
		end,
		Pools
	).