-module(ip2l).
-include("ip2l.hrl").
-include("ip2l_format.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([
	lookup/2
	, start/0
%%	, open/1
	, start_pool/3
	, start_pool/4
	, stop_pool/1
	, reload_base/1, reload_base/2, state/1]).


-type deep_list() :: [char() | atom() | deep_list()].

-type name_all() :: string() | atom() | deep_list() | (RawFilename :: binary()).


start() ->
	true = ensure_started(ip2l).


-spec lookup(atom(), {integer(), integer(), integer(), integer()}) -> {ok, #ip2l{}} | {error, term()}.
lookup(Pool, IP) ->
	ip2l_worker:lookup(maybe_worker(Pool), IP).


-spec start_pool(atom()|{proc, atom()}, list()|map(), name_all()|undefined) -> {error, term()} | ok.
start_pool(Name, PoolArgs, Path) ->
	start_pool(local, Name, PoolArgs, Path).

-spec start_pool(global|local, atom()|{proc, atom()}, list()|map(), name_all()|undefined) -> {error, term()} | ok.
start_pool(Visibility, Name, PoolArgs, Path) ->
	Size = ip2l_util:option(size, PoolArgs, ?DEFAULT_WORKERS_NUM),
	SupFlags = ip2l_util:option(sup_flags, PoolArgs, {one_for_all, 1, 5}),
	simplepool:start_pool(Visibility, Name, Size, ip2l_worker, [{path, Path}], SupFlags, ip2l_controller).


-spec stop_pool(atom()) -> {error, term()} | ok.
stop_pool(PoolName) ->
	simplepool:stop_pool(PoolName).




-spec reload_base(atom()|{proc, atom()}) -> ok|undefined.
reload_base(PoolName) ->
	Controller = maybe_controller(PoolName),
	ip2l_controller:reload(Controller).

-spec reload_base(atom()|{proc, atom()}, name_all()|undefined) -> ok|undefined.
reload_base(PoolName, Path) ->
	Controller = maybe_controller(PoolName),
	ip2l_controller:reload(Controller, Path).

-spec state(atom()|{proc, atom()}) -> undefined | {ok, #ip2lmeta{}}.
state(PoolName) ->
	Controller = maybe_controller(PoolName),
	ip2l_controller:meta(Controller).


%%%===================================================================
%%% Internal functions
%%%===================================================================


ensure_deps_started(App) ->
	Deps = case application:get_key(App, applications) of
			   undefined -> [];
			   {_, V} -> V
		   end,
	lists:all(fun ensure_started/1, Deps).

ensure_started(App) ->
	application:load(App),
	ensure_deps_started(App)
		andalso case application:start(App) of
					ok ->
						true;
					{error, {already_started, App}} ->
						true;
					Else ->
						error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
						false
				end.


maybe_controller({proc, Controller}) -> Controller;
maybe_controller(Pool) -> simplepool:controller(Pool).

maybe_worker({proc, Worker}) -> Worker;
maybe_worker(Pool) -> simplepool:rand_worker(Pool).
