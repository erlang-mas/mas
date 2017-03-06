%%%-----------------------------------------------------------------------------
%%% @doc Metrics counter.
%% @end
%%%-----------------------------------------------------------------------------

-module(mas_counter).

%%% API
-export([new/1,
         new/2,
         update/2,
         reset/1,
         reset/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates new counter with initial values set to zero.
%% @end
%%------------------------------------------------------------------------------
new(Metrics) ->
    new(Metrics, 0).

%%------------------------------------------------------------------------------
%% @doc Creates new counter with provided metrics and applies initial value.
%% @end
%%------------------------------------------------------------------------------
new(Metrics, InitValue) ->
    dict:from_list([{Metric, InitValue} || Metric <- Metrics]).

%%------------------------------------------------------------------------------
%% @doc Updates counter by incrementing metric values with values provided in
%%      the data points.
%% @end
%%------------------------------------------------------------------------------
update(DataPoints, Counter) ->
    Fun = fun ({K, V}, DictAcc) -> dict:update_counter(K, V, DictAcc) end,
    lists:foldl(Fun, Counter, DataPoints).

%%------------------------------------------------------------------------------
%% @doc Resets all metric values to zero.
%% @end
%%------------------------------------------------------------------------------
reset(Counter) ->
    reset(Counter, 0).

%%------------------------------------------------------------------------------
%% @doc Resets all metric values to provided value.
%% @end
%%------------------------------------------------------------------------------
reset(Counter, Value) ->
    Fun = fun(K, DictAcc) -> dict:store(K, Value, DictAcc) end,
    lists:foldl(Fun, Counter, dict:fetch_keys(Counter)).
