%%%-----------------------------------------------------------------------------
%%% @doc Handles node topologies based on directed graphs. Calculates neighbour
%%%      nodes accessible from given source.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_topology).

-export([new/2,
         nodes/1,
         nodes_from/2]).

-record(topology, {type     :: atom(),
                   graph    :: digraph:graph()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Builds topology of requested type from given list of nodes.
%% @end
%%------------------------------------------------------------------------------
new(Type, Nodes) ->
    Graph = build_graph(Type, lists:usort(Nodes)),
    #topology{type = Type, graph = Graph}.

%%------------------------------------------------------------------------------
%% @doc Returns all nodes belonging to topology.
%% @end
%%------------------------------------------------------------------------------
nodes(#topology{graph = Graph}) ->
    digraph:vertices(Graph).

%%------------------------------------------------------------------------------
%% @doc Returns all neighbour nodes accessible from given source node.
%% @end
%%------------------------------------------------------------------------------
nodes_from(Node, #topology{graph = Graph}) ->
    digraph:out_neighbours(Graph, Node).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
build_graph(Type, Nodes) ->
    G = digraph:new(),
    Indexes = lists:seq(1, length(Nodes)),
    lists:foreach(fun (Idx) ->
                      Neighbours = neighbours(Type, Idx, Indexes),
                      Source = lists:nth(Idx, Nodes),
                      digraph:add_vertex(G, Source),
                      Destinations = [lists:nth(I, Nodes) || I <- Neighbours],
                      connect(G, Source, Destinations)
                  end, Indexes),
    G.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
connect(G, Source, Destinations) ->
    [add_bidir_edge(G, Source, Destination) || Destination <- Destinations].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
neighbours(mesh, Idx, Indexes) ->
    Indexes -- [Idx];

neighbours(ring, _Idx, Indexes) when length(Indexes) =:= 1 -> [];
neighbours(ring, Idx, Indexes) when length(Indexes) =:= 2 ->
    Indexes -- [Idx];
neighbours(ring, Idx, Indexes) when Idx =:= 1 ->
    [length(Indexes), 2];
neighbours(ring, Idx, Indexes) when Idx =:= length(Indexes) ->
    [length(Indexes) - 1, 1];
neighbours(ring, Idx, _Indexes) ->
    [Idx - 1, Idx + 1].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_bidir_edge(G, V1, V2) ->
    {digraph:add_edge(G, V1, V2), digraph:add_edge(G, V2, V1)}.
