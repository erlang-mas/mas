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
    Graph = digraph:new(),
    build_graph(Type, lists:usort(Nodes), Graph),
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
build_graph(_Type, [], _Graph) -> ok;
build_graph(mesh, Nodes, Graph) ->
    NodeConns = [{Node, Nodes -- [Node]} || Node <- Nodes],
    connect_nodes(NodeConns, Graph);
build_graph(grid, Nodes, Graph) ->
    NumNodes = length(Nodes),
    Dim = nearest_square_base(NumNodes),
    Grid = generate_grid(Dim),
    Dirs = [{0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}, {-1, -1}, {-1, 0},
            {-1, 1}],
    NodeCells = lists:sublist(Grid, NumNodes),
    CellConns = [cell_connections(Cell, NodeCells, Dirs) || Cell <- NodeCells],
    IndexConns = cell_to_index_conns(CellConns, Dim),
    NodeConns = index_to_node_conns(IndexConns, Nodes),
    connect_nodes(NodeConns, Graph).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_grid(Dim) ->
    generate_grid(Dim, Dim).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_grid(DimX, DimY) ->
    [{X, Y} || X <- lists:seq(0, DimX - 1), Y <- lists:seq(1, DimY)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cell_connections(Cell, Grid, Dirs) ->
    {Cell, cell_neighbours(Cell, Grid, Dirs)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cell_neighbours({X, Y}, Grid, Dirs) ->
    NeighCells = [{X + DirX, Y + DirY} || {DirX, DirY} <- Dirs],
    cells_inside_grid(NeighCells, Grid).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cells_inside_grid(Cells, Grid) ->
    [Cell || Cell <- Cells, lists:member(Cell, Grid)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cell_to_index_conns(CellConns, Dim) ->
    [cell_to_index_conn(CellConn, Dim) || CellConn <- CellConns].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cell_to_index_conn({Cell, NeighCells}, Dim) ->
    NeighIndexes = [cell_to_index(NeighCell, Dim) || NeighCell <- NeighCells],
    {cell_to_index(Cell, Dim), NeighIndexes}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
index_to_node_conns(IndexConns, Nodes) ->
    [index_to_node_conn(IndexConn, Nodes) || IndexConn <- IndexConns].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
index_to_node_conn({Idx, NeighIndexes}, Nodes) ->
    NeighNodes = [lists:nth(NeighIdx, Nodes) || NeighIdx <- NeighIndexes],
    {lists:nth(Idx, Nodes), NeighNodes}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
cell_to_index({X, Y}, Dim) ->
    X * Dim + Y.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
connect_nodes([], _G) -> ok;
connect_nodes([{SrcNode, DstNodes} | NodeConns], G) ->
    digraph:add_vertex(G, SrcNode),
    [add_bidir_edge(G, SrcNode, DstNode) || DstNode <- DstNodes],
    connect_nodes(NodeConns, G).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
nearest_square_base(N) ->
    nearest_square_base(N, 1).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
nearest_square_base(N, I) ->
    Square = I * I,
    case Square >= N of
        true -> I;
        false -> nearest_square_base(N, I + 1)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_bidir_edge(G, V1, V2) ->
    {digraph:add_edge(G, V1, V2), digraph:add_edge(G, V2, V1)}.
