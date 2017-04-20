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
         migrate_agents/1]).

%%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes       :: [node()],
                topology    :: topology()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

migrate_agents(Agents) ->
    gen_server:cast(?SERVER, {migrate_agents, Agents, {node(), self()}}).

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
    {ok, #state{nodes = Nodes,
                topology = mas_config:get_env(nodes_topology)}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agents, Agents, _Source = {Node, Population}}, State) ->
    #state{nodes = Nodes, topology = Topology} = State,
    case mas_topology:destinations(Topology, Nodes, Node) of
        {ok, Destinations} ->
            mas_migration:send_to_nodes(Destinations, Agents);
        no_destination ->
            mas_migration:send_back(Population, Agents)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({nodeup, Node}, State = #state{nodes = Nodes}) ->
    mas_logger:info("Node ~p connected", [Node]),
    NewNodes = lists:usort([Node | Nodes]),
    {noreply, State#state{nodes = NewNodes}};
handle_info({nodedown, Node}, State= #state{nodes = Nodes}) ->
    mas_logger:info("Node ~p disconnected", [Node]),
    NewNodes = lists:usort(Nodes -- [Node]),
    {noreply, State#state{nodes = NewNodes}};
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
