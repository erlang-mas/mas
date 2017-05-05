%%%-----------------------------------------------------------------------------
%%% @doc Handles node topologies based on directed graphs. Calculates neighbour
%%%      nodes accessible from given source.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_topology).

-export([new/1,
         new/2,
         add_node/2,
         add_nodes/2,
         remove_node/2,
         nodes/1,
         nodes_from/2]).

-record(topology, {type     :: atom(),
                   graph    :: digraph:graph(),
                   metadata :: any()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initiates topology.
%% @end
%%------------------------------------------------------------------------------
new(Type) ->
    new(Type, []).

%%------------------------------------------------------------------------------
%% @doc Initiates topology with graph built from given nodes.
%% @end
%%------------------------------------------------------------------------------
new(Type, Nodes) ->
    add_nodes(Nodes, #topology{type = Type, graph = digraph:new()}).

%%------------------------------------------------------------------------------
%% @doc Adds new node into topology graph.
%% @end
%%------------------------------------------------------------------------------
add_node(Node, Topology = #topology{type = Type}) ->
    do_add_node(Type, Node, Topology).

%%------------------------------------------------------------------------------
%% @doc Adds multiple nodes into topology graph.
%% @end
%%------------------------------------------------------------------------------
add_nodes(Nodes, Topology) ->
    lists:foldl(fun (Node, NewTopology) ->
                    add_node(Node, NewTopology)
                end, Topology, Nodes).

%%------------------------------------------------------------------------------
%% @doc Removes existing node from topology graph.
%% @end
%%------------------------------------------------------------------------------
remove_node(Node, Topology = #topology{type = Type}) ->
    do_remove_node(Type, Node, Topology).

%%------------------------------------------------------------------------------
%% @doc Returns all nodes belonging to topology.
%% @end
%%------------------------------------------------------------------------------
nodes(#topology{graph = G}) ->
    digraph:vertices(G).

%%------------------------------------------------------------------------------
%% @doc Returns all neighbour nodes accessible from given source node.
%% @end
%%------------------------------------------------------------------------------
nodes_from(Node, #topology{graph = G}) ->
    digraph:out_neighbours(G, Node).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_add_node(mesh, Node, Topology = #topology{graph = G}) ->
    Vertices = digraph:vertices(G),
    NewVertex = digraph:add_vertex(G, Node),
    [add_bidir_edge(G, NewVertex, V) || V <- Vertices],
    Topology.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_remove_node(mesh, Node, Topology = #topology{graph = G}) ->
    digraph:del_vertex(G, Node),
    Topology.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_bidir_edge(G, V1, V2) ->
    {digraph:add_edge(G, V1, V2), digraph:add_edge(G, V2, V1)}.
