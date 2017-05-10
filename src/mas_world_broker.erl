%%%-----------------------------------------------------------------------------
%%% @doc Discovers universe of interconnected nodes and handles agent migrations
%%%      between them.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_world_broker).

-include("mas.hrl").

-behaviour(gen_server).

%%% API
-export([start_link/0,
         migrate_agents/2]).

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

migrate_agents(Agents, Source) ->
    gen_server:cast(?SERVER, {migrate_agents, Agents, Source}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    net_kernel:monitor_nodes(true),
    mas_utils:seed_random(),
    self() ! connect_nodes,
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agents, Agents, {Node, Population}}, State) ->
    #state{topology = Topology} = State,
    case mas_topology:nodes_from(Node, Topology) of
        [] ->
            mas_migration:send_back(Population, Agents);
        Destinations ->
            Destination = mas_utils:pick_random(Destinations),
            mas_migration:send_to_nodes([Destination], Agents)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(connect_nodes, State) ->
    Topology = build_topology(),
    {ok, Localhost} = inet:gethostname(),
    NeighbourHosts = mas_topology:nodes_from(list_to_atom(Localhost), Topology),
    ConnectedNodes = net_adm:world_list(NeighbourHosts, verbose),
    mas_logger:info("Connected nodes: ~p", [ConnectedNodes]),
    mas_logger:debug("Nodes (connected): ", [nodes(connected)]),
    mas_logger:debug("Nodes (hidden): ", [nodes(hidden)]),
    {noreply, State#state{topology = Topology,
                          connected_nodes = ConnectedNodes}};
handle_info({nodeup, Node}, State) ->
    #state{connected_nodes = ConnectedNodes} = State,
    mas_logger:info("Node ~p connected", [Node]),
    NewConnectedNodes = lists:usort(ConnectedNodes ++ [Node]),
    {noreply, State#state{connected_nodes = NewConnectedNodes}};
handle_info({nodedown, Node}, State) ->
    #state{connected_nodes = ConnectedNodes} = State,
    mas_logger:info("Node ~p disconnected", [Node]),
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
