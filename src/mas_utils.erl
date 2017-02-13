%%%-----------------------------------------------------------------------------
%%% @doc Common-use utility functions.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_utils).

%% API
-export([group_by/1,
         shuffle/1,
         sample/1,
         index_of/2,
         timestamp/0,
         to_string/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Groupes listed tuples by the first element:
%% [{K1, V1}, {K2, V2}, {K1, V3}, {K2, V4}] =>
%% [{K1, [V1, V3]}, {K2, [V2, V4]}].
%% @end
%%------------------------------------------------------------------------------
group_by(L) ->
    Fold = fun({K, V}, D) -> dict:append(K, V, D) end,
    Dict = lists:foldl(Fold, dict:new(), L),
    dict:to_list(Dict).

%%------------------------------------------------------------------------------
%% @doc Randomly shuffles elements in the list.
%% @end
%%------------------------------------------------------------------------------
shuffle(L) ->
    Rand = [{rand:uniform(), N} || N <- L],
    [X || {_, X} <- lists:sort(Rand)].

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
to_string(T) ->
    io_lib:format("~p", [T]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
index_of(_, [], _) ->
    not_found;
index_of(E, [E | _], I) ->
    I;
index_of(E, [_ | T], I) ->
    index_of(E, T, I + 1).
