%%%-----------------------------------------------------------------------------
%%% @doc Calculates destination of agent migration based on specified topology.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_topology).

%%% API
-export([destinations/3]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Calculates all valid destinations (pids or distributed nodes) from
%%      source node to target nodes based on given topology.
%% @end
%%------------------------------------------------------------------------------
destinations(_Topology, Nodes, _Source) when length(Nodes) < 2 ->
    no_destination;
destinations(Topology, Nodes, Source) ->
    case possible_destinations(Topology, Nodes, Source) of
        [] -> no_destination;
        Destinations -> {ok, Destinations}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
possible_destinations(_Topology, Nodes, none) -> Nodes;
possible_destinations(mesh, Nodes, Source) ->
    [Node || Node <- Nodes, Node =/= Source];
possible_destinations(ring, Nodes, Source) ->
    case mas_utils:index_of(Source, Nodes) of
        not_found -> [];
        SourceIdx ->
            N = length(Nodes),
            DestIdx = lists:usort([cycle_idx(SourceIdx - 1, N),
                                   cycle_idx(SourceIdx + 1, N)]),
            [lists:nth(Idx, Nodes) || Idx <- DestIdx, Idx =/= SourceIdx]
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cycle_idx(Idx, N) when Idx > N -> 1;
cycle_idx(Idx, N) when Idx < 1 -> N;
cycle_idx(Idx, _N) -> Idx.
