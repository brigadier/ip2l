-module(ip2l_controller).

-behaviour(gen_server).
-behaviour(gen_simplepool_worker).

-include_lib("eunit/include/eunit.hrl").
%% API
-export([simplepool_start_link/4, meta/1, reload/2, reload/1]).
-define(MARK, "ip2l_db_dont_delete-").

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {meta, dir, file, workers}).

%%%===================================================================
%%% API
%%%===================================================================

simplepool_start_link(Visibility, Name, Workers, Args) ->
	{_, Path} = lists:keyfind(path, 1, Args),
	gen_server:start_link({Visibility, Name}, ?MODULE, [Workers, Path | Args], []).

meta(Worker) ->
	gen_server:call(Worker, meta).


reload(Name) ->
	gen_server:call(Name, reload).

reload(Name, File) ->
	gen_server:call(Name, {reload, File}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Workers, Path | _]) ->
	case load(Path, Workers) of
		undefined ->
			{ok, #state{meta = undefined, dir = Path, file = undefined, workers = Workers}};
		{ok, {File, Meta}} ->
			{ok, #state{meta = Meta, dir = Path, file = File, workers = Workers}}
	end.


handle_call(reload, _From, #state{workers = Workers, dir = Path} = State) ->
	case load(Path, Workers) of
		undefined ->
			{reply, ok, State#state{meta = undefined, file = undefined, workers = Workers}};
		{ok, {File, Meta}} ->
			{reply, ok, State#state{meta = Meta, file = File, workers = Workers}}
	end;

handle_call({reload, Path}, _From, #state{workers = Workers} = State) ->
	case load(Path, Workers) of
		undefined ->
			{reply, ok, State#state{meta = undefined, dir = Path, file = undefined, workers = Workers}};
		{ok, {File, Meta}} ->
			{reply, ok, State#state{meta = Meta, dir = Path, file = File, workers = Workers}}
	end;


handle_call(meta, _From, #state{meta = undefined} = State) ->
	{reply, undefined, State};

handle_call(meta, _From, #state{meta = Meta} = State) ->
	{reply, {ok, Meta}, State};

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

load(Path, Workers) ->
	PatternUpload = filename:join(Path, "*.{bin,BIN}"),
	WorkDir = wpath(Path),
	PatternCurrent = filename:join(WorkDir, ?MARK ++ "*.bin"),
	FilesCurrent = lists:reverse(lists:sort(filelib:wildcard(PatternCurrent))),
	FilesUpload = lists:reverse(lists:sort(filelib:wildcard(PatternUpload))),

	ok = filelib:ensure_dir(WorkDir),

	Result = case try_load_file(FilesUpload) of
				 undefined ->
					 case try_load_file(FilesCurrent) of
						 {ok, _} = Ret -> Ret;
						 Else -> Else
					 end;
				 {ok, {File, Meta}} ->
					 HardlinkName = filename:join(WorkDir,
												  ?MARK ++ integer_to_list(erlang:system_time(seconds)) ++ ".bin"),
					 filelib:ensure_dir(HardlinkName),
					 ok = file:make_link(File, HardlinkName),
					 {ok, {HardlinkName, Meta}}
			 end,

	notify_workers(Workers, Result),
	cleanup(Path, Result),
	Result.


try_load_file([]) -> undefined;
try_load_file([File | Files]) ->
	case ip2l_format:open(File) of
		{ok, FRec} ->
			Meta = ip2l_format:meta(FRec),
			ip2l_format:close(FRec),
			{ok, {File, Meta}};
		_ ->
			try_load_file(Files)
	end.


notify_workers(Workers, {ok, {File, _Meta}}) ->
	notify_workers(Workers, File);

notify_workers(Workers, File) ->
	lists:foreach(
		fun(Worker) ->
			ok = gen_server:call(Worker, {open, File})
		end,
		Workers
	).


cleanup(Path, {ok, {File, _Meta}}) ->
	cleanup(Path, File);

cleanup(Path, DBFile) ->
	Pattern = filename:join(Path, "*.{bin,BIN}"),
	Pattern2 = filename:join(wpath(Path), "*.{bin,BIN}"),

	Files = filelib:wildcard(Pattern) ++ filelib:wildcard(Pattern2),
	lists:foreach(
		fun
			(F) when F == DBFile -> ok;
			(F) -> file:delete(F)
		end,
		Files
	),
	ok.

wpath(Path) ->
	filename:join(Path, "ip2l-current").