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
    {error, no_destination};
destination(mesh, From, Nodes) ->
    Destinations = [Node || Node <- Nodes, Node =/= From],
    {ok, mas_utils:sample(Destinations)}.
