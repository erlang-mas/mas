%%%-----------------------------------------------------------------------------
%%% @doc Maintains connection with neighour nodes and handles agent migrations
%%%      between them.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_world_broker).

-include("mas.hrl").

-behaviour(gen_server).

%%% API
-export([start_link/0,
         migrate_agents/2,
         broadcast_stop/0]).

%%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {topology        :: topology(),
                connected_nodes :: [node()]}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc Migrates agents to neighbour nodes. If there are no neighbour nodes,
%%%     sends agents back to source population.
%% @end
%%------------------------------------------------------------------------------
migrate_agents(Agents, Source) ->
    gen_server:cast(?SERVER, {migrate_agents, Agents, Source}).

broadcast_stop() ->
    gen_server:call(?SERVER, broadcast_stop).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    self() ! connect_nodes,
    net_kernel:monitor_nodes(true, [{node_type, hidden}, nodedown_reason]),
    mas_utils:seed_random(),
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agents, Agents, Source}, State) ->
    #state{connected_nodes = ConnectedNodes} = State,
    perform_migration(ConnectedNodes, Agents, Source),
    {noreply, State};
handle_cast(broadcast_stop, State) ->
    #state{connected_nodes = ConnectedNodes} = State,
    [mas_world:notify_stop(Node) || Node <- ConnectedNodes],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(connect_nodes, State) ->
    Topology = build_topology(),
    NeighHosts = mas_topology:nodes_from(localhost(), Topology),
    ConnectedNodes = net_adm:world_list(NeighHosts, verbose),
    log_connected_nodes(),
    {noreply, State#state{topology = Topology,
                          connected_nodes = ConnectedNodes}};
handle_info({nodeup, Node, _InfoList}, State) ->
    #state{connected_nodes = ConnectedNodes} = State,
    mas_logger:info("Node ~p connected", [Node]),
    NewConnectedNodes = lists:usort(ConnectedNodes ++ [Node]),
    {noreply, State#state{connected_nodes = NewConnectedNodes}};
handle_info({nodedown, Node, InfoList}, State) ->
    #state{connected_nodes = ConnectedNodes} = State,
    {nodedown_reason, Reason} = lists:keyfind(nodedown_reason, 1, InfoList),
    mas_logger:info("Node ~p disconnected with reason: ~p", [Node, Reason]),
    NewConnectedNodes = lists:usort(ConnectedNodes -- [Node]),
    {noreply, State#state{connected_nodes = NewConnectedNodes}};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
build_topology() ->
    Type = mas_config:get_env(nodes_topology),
    Hosts = load_hosts_from_file(),
    mas_logger:debug("Hosts: ~p", [Hosts]),
    mas_topology:new(Type, Hosts).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
load_hosts_from_file() ->
    case net_adm:host_file() of
        {error, _Reason} ->
            mas_logger:warning("Hosts file not found"),
            [];
        Hosts -> Hosts
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
localhost() ->
    {ok, Localhost} = inet:gethostname(),
    list_to_atom(Localhost).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_connected_nodes() ->
    mas_logger:debug("Connected nodes: ~p", [nodes(connected)]),
    mas_logger:debug("Connected hidden nodes: ~p", [nodes(hidden)]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
perform_migration([], Agents, Source = {_Node, Population}) ->
    mas_logger:warning("Unable to migrate agents from ~p, sending agents back",
                       [Source]),
    mas_migration:migrate_back(Population, Agents);
perform_migration(Nodes, Agents, Source) ->
    mas_migration:migrate_to_worlds(Nodes, Agents, Source).
