%%%-----------------------------------------------------------------------------
%%% @doc Custom Exometer reporter writing metrics into files.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_reporter).

-behaviour(exometer_report).

%% API
-export([register/1]).

%% Exometer report callbacks
-export(
   [
    exometer_init/1,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_report/5,
    exometer_call/3,
    exometer_cast/2,
    exometer_info/2,
    exometer_newentry/2,
    exometer_setopts/4,
    exometer_terminate/2
   ]).


-record(state, {files    :: dict:dict(),
                base_dir :: string()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

register(Options) ->
    exometer_report:add_reporter(?MODULE, Options).

%%%=============================================================================
%%% Exometer report callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_init(Options) ->
    RootDir = proplists:get_value(root_dir, Options),
    BaseDir = create_base_dir(RootDir),
    {ok, #state{files    = dict:new(),
                base_dir = BaseDir}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_subscribe(Metric, _DataPoint, _Interval, _Extra,
                   State = #state{files = Files, base_dir = BaseDir}) ->
    File = create_file([BaseDir | Metric]),
    NewFiles = dict:store(Metric, File, Files),
    {ok, State#state{files = NewFiles}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_unsubscribe(Metric, _DataPoint, _Extra,
                     State = #state{files = Files}) ->
    File = dict:fetch(Metric, Files),
    close_file(File),
    NewFiles = dict:erase(Metric, Files),
    {ok, State#state{files = NewFiles}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_report(Metric, _DataPoint, _Extra, Value,
                State = #state{files = Files})  ->
    File = dict:fetch(Metric, Files),
    write_metric(Metric, Value, File),
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_call(_Msg, _From, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_cast(_Msg, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_info(_Msg, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_newentry(_Entry, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_terminate(_Reason, #state{files = Files}) ->
    [close_file(File) || {_Metric, File} <- dict:to_list(Files)].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
create_base_dir(RootDir) ->
    Timestamp = mas_utils:to_string(mas_utils:timestamp()),
    BaseDir = filename:join([RootDir, Timestamp]),
    file:make_dir(BaseDir),
    BaseDir.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
create_file([Path, Component | Components]) ->
    NormalizedComponent = normalize_path_component(Component),
    NewPath = filename:join([Path, NormalizedComponent]),
    create_file([NewPath | Components]);
create_file([Path | []]) ->
    filelib:ensure_dir(Path),
    {ok, File} = file:open(Path, [append, delayed_write, raw]),
    File.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
normalize_path_component(Component) ->
    mas_utils:to_string(Component).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
close_file(File) ->
    file:close(File).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
write_metric(Metric, Value, File) ->
    Data = io_lib:fwrite("~p;~w~n", [Metric, Value]),
    file:write(File, Data).
