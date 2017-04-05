%%%-----------------------------------------------------------------------------
%%% @doc Exometer setup and metric management.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_reporter).

%%% API
-export([setup/0,
         teardown/0,
         subscribe/1,
         subscribe/3,
         unsubscribe/1,
         update/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%%-----------------------------------------------------------------------------
%%% @doc Setups Exometer filesystem reporter. Metric entries will be stored
%%%      inside provided logs directory.
%%% @end
%%%-----------------------------------------------------------------------------
setup() ->
    LogsDir = mas_config:get_env(logs_dir),
    WriteInterval = mas_config:get_env(write_interval),
    BaseDir = filename:join([LogsDir, node()]),
    exometer_report:add_reporter(exometer_report_fs, [{base_dir, BaseDir}]),
    exometer_report:set_interval(exometer_report_fs, write_interval,
                                 WriteInterval).

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
subscribe(Metric) ->
    subscribe(Metric, counter, value).

%%%-----------------------------------------------------------------------------
%%% @doc Creates metric with provided type, data point and write interval.
%%%      Subscribes metric to the reporter.
%%% @end
%%%-----------------------------------------------------------------------------
subscribe(Metric, Type, DataPoint) ->
    exometer:new(Metric, Type),
    exometer_report:subscribe(exometer_report_fs, Metric, DataPoint,
                              write_interval),
    Metric.

%%%-----------------------------------------------------------------------------
%%% @doc Removes all reporter subscriptions for given metric, deletes metric.
%%% @end
%%%-----------------------------------------------------------------------------
unsubscribe(Metric) ->
    exometer_report:unsubscribe_all(exometer_report_fs, Metric),
    exometer:delete(Metric).

%%%-----------------------------------------------------------------------------
%%% @doc Updates metric with given value.
%%% @end
%%%-----------------------------------------------------------------------------
update(Metric, Value) ->
    exometer:update(Metric, Value).
