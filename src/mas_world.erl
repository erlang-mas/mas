%%%-----------------------------------------------------------------------------
%%% @doc Spawns multiple populations of agents. Handles agent migrations.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_world).

-include("mas.hrl").

-behaviour(gen_server).

%%% API
-export([start_link/0,
         migrate_agents/2,
         put_agents/2]).

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

put_agents(Node, Agents) ->
    gen_server:cast({?SERVER, Node}, {put_agents, Agents}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    mas_utils:seed_random(),
    self() ! spawn_populations,
    TopologyType = mas_config:get_env(topology),
    Topology = mas_topology:new(TopologyType),
    {ok, #state{topology = Topology}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agents, Agents, {_Node, Population}}, State) ->
    #state{topology = Topology} = State,
    case mas_topology:nodes_from(Population, Topology) of
        [] ->
            mas_migration:send_back(Population, Agents);
        Destinations ->
            mas_migration:send_to_populations(Destinations, Agents)
    end,
    {noreply, State};
handle_cast({put_agents, Agents}, State) ->
    #state{topology = Topology} = State,
    case mas_topology:nodes(Topology) of
        [] ->
            mas_logger:warning("No destination for migration target");
        Destinations ->
            mas_migration:send_to_populations(Destinations, Agents)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(spawn_populations, State = #state{topology = Topology}) ->
    Count = mas_config:get_env(population_count),
    Populations = spawn_populations(Count),
    NewTopology = mas_topology:add_nodes(Populations, Topology),
    {noreply, State#state{topology = NewTopology}};
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
spawn_populations(Count) ->
    [mas_population_sup:spawn_population() || _ <- lists:seq(1, Count)].
