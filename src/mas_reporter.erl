%%%-----------------------------------------------------------------------------
%%% @doc Exometer setup and metric management.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_reporter).

%%% API
-export([setup/1,
         teardown/0,
         subscribe/2,
         subscribe/4,
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
%%% @doc Removes reporter and all its subscriptions.
%%% @end
%%%-----------------------------------------------------------------------------
teardown() ->
    exometer_report:remove_reporter(exometer_report_fs).

%%%-----------------------------------------------------------------------------
%%% @doc Creates metric with default type (counter) and data point (value).
%%%      Includes given write interval.
%%% @end
%%%-----------------------------------------------------------------------------
subscribe(Metric, WriteInterval) ->
    subscribe(Metric, counter, value, WriteInterval).

%%%-----------------------------------------------------------------------------
%%% @doc Creates metric with provided type, data point and write interval.
%%%      Subscribes metric to the reporter.
%%% @end
%%%-----------------------------------------------------------------------------
subscribe(Metric, Type, DataPoint, WriteInterval) ->
    exometer:new(Metric, Type),
    exometer_report:subscribe(exometer_report_fs, Metric, DataPoint,
                              WriteInterval),
    Metric.

%%%-----------------------------------------------------------------------------
%%% @doc Removes all reporter subscriptions for given metric, deletes metric.
%%% @end
%%%-----------------------------------------------------------------------------
unsubscribe(Metric) ->
    exometer_report:unsubscribe_all(exometer_report_fs, Metric),
    exometer:delete(Metric).
