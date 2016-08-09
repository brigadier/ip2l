
-module(ip2l_worker).

-behaviour(gen_server).
-behaviour(gen_simplepool_worker).

-include_lib("eunit/include/eunit.hrl").
%% API
-export([simplepool_start_link/4, lookup/2]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {controller, file = undefined, handle = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

simplepool_start_link(Visibility, Name, Controller, Args) ->
	gen_server:start_link({Visibility, Name}, ?MODULE, [Controller | Args], []).


lookup(Worker, {_, _, _, _} = IP) ->
	gen_server:call(Worker, {lookup, IP}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Controller|_]) ->
	{ok, #state{controller = Controller}}.




handle_call({lookup, _IP}, _From, #state{handle = undefined} = State) ->
	{reply, {error, closed}, State};

handle_call({lookup, IP}, _From, #state{handle = Handle} = State) ->
	Result = ip2l_format:lookup(IP, Handle),
	{reply, Result, State};

handle_call({open, undefined}, _From, #state{handle = Handle} = State) ->
	maybeclose(Handle),
	{reply, ok, State#state{handle = undefined, file = undefined}};

handle_call({open, File}, _From, #state{handle = Handle} = State) ->
	maybeclose(Handle),
	{ok, Handle2} = ip2l_format:open(File),
	{reply, ok, State#state{file = File, handle = Handle2}};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.



handle_cast(_Request, State) ->
	{noreply, State}.



handle_info(_Info, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	ok.



code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
maybeclose(undefined) -> ok;
maybeclose(Handle) -> ip2l_format:close(Handle).