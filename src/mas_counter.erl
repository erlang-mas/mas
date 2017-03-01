%%%-----------------------------------------------------------------------------
%%% @doc Metrics counter.
%% @end
%%%-----------------------------------------------------------------------------

-module(mas_counter).

%%% API
-export([new/1,
         new/2,
         update/2]).

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
