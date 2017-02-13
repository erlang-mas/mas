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

register(Args) ->
    exometer_report:add_reporter(?MODULE, Args).

%%%=============================================================================
%%% Exometer report callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
exometer_init(Args) ->
    RootDir = proplists:get_value(root_dir, Args),
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
    write_metric_entry(Metric, Value, File),
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
create_file(PathComponents) ->
    Path = filename:join(stringify_components(PathComponents)),
    filelib:ensure_dir(Path),
    {ok, File} = file:open(Path, [append, delayed_write, raw]),
    File.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
close_file(File) ->
    file:close(File).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
write_metric_entry(Metric, Value, File) ->
    Components = stringify_components(Metric ++ [Value]),
    Entry = string:join(Components, ";"),
    file:write(File, io_lib:fwrite("~s~n", [Entry])).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stringify_components(Components) ->
    [mas_utils:to_string(Component) || Component <- Components].
