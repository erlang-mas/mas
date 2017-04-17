%%%-----------------------------------------------------------------------------
%%% @doc Common-use utility functions.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_utils).

%%% API
-export([group_by_key/1,
         shuffle/1,
         sample/1,
         index_of/2,
         timestamp/0,
         to_string/1,
         seed_random/0,
         partition/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Groupes listed tuples by the first element:
%%      [{K1, V1}, {K2, V2}, {K1, V3}, {K2, V4}] =>
%%      [{K1, [V1, V3]}, {K2, [V2, V4]}].
%% @end
%%------------------------------------------------------------------------------
group_by_key(L) ->
    Fold = fun({K, V}, D) -> dict:append(K, V, D) end,
    Dict = lists:foldl(Fold, dict:new(), L),
    dict:to_list(Dict).

%%------------------------------------------------------------------------------
%% @doc Associates each element in the list with a random number. The list is
%%      then sorted based on the generated number. Repeats the process log(n)
%%      times to ensure a fair shuffle.
%% @end
%%------------------------------------------------------------------------------
shuffle([]) -> [];
shuffle(List) ->
   randomize(round(math:log(length(List)) + 0.5), List).

%%------------------------------------------------------------------------------
%% @doc Picks random element from the list.
%% @end
%%------------------------------------------------------------------------------
sample([]) ->
    ok;
sample(L) ->
    Index = rand:uniform(length(L)),
    lists:nth(Index, L).

%%------------------------------------------------------------------------------
%% @doc Returns index of the element in the list.
%% @end
%%------------------------------------------------------------------------------
index_of(E, L) ->
    index_of(E, L, 1).

%%------------------------------------------------------------------------------
%% @doc Returns current OS system time in miliseconds.
%% @end
%%------------------------------------------------------------------------------
timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

%%------------------------------------------------------------------------------
%% @doc Converts term into string.
%% @end
%%------------------------------------------------------------------------------
to_string(X) when is_list(X) ->
    X;
to_string(X) ->
    io_lib:format("~p", [X]).

%%------------------------------------------------------------------------------
%% @doc Seeds random number generator for current process.
%% @end
%%------------------------------------------------------------------------------
seed_random() ->
    A1 = erlang:phash2([node()]),
    A2 = erlang:monotonic_time(),
    A3 = erlang:unique_integer(),
    rand:seed(exs1024, {A1, A2, A3}).

%%------------------------------------------------------------------------------
%% @doc Randomly splits list into given number of partitions.
%% @end
%%------------------------------------------------------------------------------
partition(L, 1) ->
    [L];

partition(L, 2) ->
    {A, B} = lists:partition(fun(_) -> rand:uniform(2) == 1 end, L),
    [A, B];

partition(L, N) ->
    {Chunk, T} = lists:partition(fun(_) -> rand:uniform(N) == 1 end, L),
    [Chunk | partition(T, N - 1)].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
randomize(List) ->
   D = lists:map(fun(A) ->
                    {rand:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)),
   D1.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
index_of(_, [], _) ->
    not_found;
index_of(E, [E | _], I) ->
    I;
index_of(E, [_ | T], I) ->
    index_of(E, T, I + 1).
