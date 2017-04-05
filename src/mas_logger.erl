%%%-----------------------------------------------------------------------------
%%% @doc Wrapper for lager logging framework.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_logger).

%%% API
-export([start/0,
         debug/1, debug/2,
         info/1, info/2,
         warning/1, warning/2,
         error/1, error/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Configures and starts lager message sink.
%% @end
%%------------------------------------------------------------------------------
start() ->
    setup_lager(),
    lager:start().

%%------------------------------------------------------------------------------
%% @doc Logs message with debug severity.
%% @end
%%------------------------------------------------------------------------------
debug(Format) -> debug(Format, []).
debug(Format, Args) ->
    lager:log(debug, self(), Format, Args).

%%------------------------------------------------------------------------------
%% @doc Logs message with info severity.
%% @end
%%------------------------------------------------------------------------------
info(Format) -> info(Format, []).
info(Format, Args) ->
    lager:log(info, self(), Format, Args).

%%------------------------------------------------------------------------------
%% @doc Logs message with warning severity.
%% @end
%%------------------------------------------------------------------------------
warning(Format) -> warning(Format, []).
warning(Format, Args) ->
    lager:log(warning, self(), Format, Args).

%%------------------------------------------------------------------------------
%% @doc Logs message with error severity.
%% @end
%%------------------------------------------------------------------------------
error(Format) -> ?MODULE:error(Format, []).
error(Format, Args) ->
    lager:log(error, self(), Format, Args).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_lager() ->
    application:load(lager),
    LogsDir = filename:join(mas_config:get_env(logs_dir), node()),
    application:set_env(lager, log_root, LogsDir).
