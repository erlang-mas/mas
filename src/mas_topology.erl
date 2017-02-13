%%%-----------------------------------------------------------------------------
%%% @doc Calculates destination of agent migration based on specified topology.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_topology).

%% API
-export([destination/3]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

destination(_Topology, _From, Nodes) when length(Nodes) < 2 ->
    no_destination;
destination(mesh, From, Nodes) ->
    Destinations = [Node || Node <- Nodes, Node =/= From],
    {ok, mas_utils:sample(Destinations)};
destination(ring, From, Nodes) ->
    N = length(Nodes),
    FromIdx = mas_utils:index_of(From, Nodes),
    NextIdx = case rand:uniform() < 0.5 of
                  true  -> FromIdx + 1;
                  false -> FromIdx - 1
              end,
    DestinationIdx = case NextIdx of
                         Idx when Idx > N -> 1;
                         Idx when Idx < 1 -> N;
                         Idx              -> Idx
                     end,
    {ok, lists:nth(DestinationIdx, Nodes)}.
