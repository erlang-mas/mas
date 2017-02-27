%%%-----------------------------------------------------------------------------
%%% @doc Exometer setup and metric management.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_reporter).

%%% API
-export([setup/1,
         subscribe/2,
         unsubscribe/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%%-----------------------------------------------------------------------------
%%% @doc Setups Exometer filesystem reporter. Metric entries will be stored
%%%      inside provided logs directory.
%%% @end
%%%-----------------------------------------------------------------------------
setup(LogsDir) ->
    exometer_report:add_reporter(exometer_report_fs, [{base_dir, LogsDir}]).

%%%-----------------------------------------------------------------------------
%%% @doc Creates metric with given write interval and subscribes it to the
%%%      reporter.
%%% @end
%%%-----------------------------------------------------------------------------
subscribe(Metric, WriteInterval) ->
    exometer:new(Metric, counter),
    exometer_report:subscribe(exometer_report_fs, Metric, value, WriteInterval),
    Metric.

%%%-----------------------------------------------------------------------------
%%% @doc Removes all reporter subscriptions for given metric and deletes metric.
%%% @end
%%%-----------------------------------------------------------------------------
unsubscribe(Metric) ->
    exometer_report:unsubscribe_all(exometer_report_fs, Metric),
    exometer:delete(Metric).
