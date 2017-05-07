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

-record(state, {topology    :: topology()}).

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
    Nodes = discover_nodes(),
    mas_logger:info("Connected nodes: ~p", [Nodes]),
    TopologyType = mas_config:get_env(nodes_topology),
    Topology = mas_topology:new(TopologyType, Nodes),
    {ok, #state{topology = rebuild_topology(Topology)}}.

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
handle_info({nodeup, Node}, State = #state{topology = Topology}) ->
    mas_logger:info("Node ~p connected", [Node]),
    NewTopology = mas_topology:add_node(Node, Topology),
    {noreply, State#state{topology = rebuild_topology(NewTopology)}};
handle_info({nodedown, Node}, State = #state{topology = Topology}) ->
    mas_logger:info("Node ~p disconnected", [Node]),
    NewTopology = mas_topology:remove_node(Node, Topology),
    {noreply, State#state{topology = rebuild_topology(NewTopology)}};
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
discover_nodes() ->
    case net_adm:host_file() of
        {error, _Reason} ->
            mas_logger:warning("Hosts file not found"),
            [];
        Hosts ->
            net_adm:world(Hosts)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
rebuild_topology(Topology) ->
    Nodes = mas_topology:nodes(Topology),
    ResetTopology = mas_topology:reset(Topology),
    mas_topology:add_nodes(lists:usort(Nodes), ResetTopology).
