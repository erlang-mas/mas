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
         put_agents/3]).

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

put_agents(Node, Agents, Source) ->
    gen_server:cast({?SERVER, Node}, {put_agents, Agents, Source}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    mas_utils:seed_random(),
    self() ! spawn_populations,
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agents, Agents, Source = {_Node, Population}}, State) ->
    #state{topology = Topology} = State,
    Destinations = mas_topology:nodes_from(Population, Topology),
    perform_migration(Destinations, Agents, Source),
    {noreply, State};
handle_cast({put_agents, Agents, Source}, State) ->
    #state{topology = Topology} = State,
    Destinations = mas_topology:nodes(Topology),
    perform_migration(Destinations, Agents, Source),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(spawn_populations, State) ->
    Count = mas_config:get_env(population_count),
    Populations = spawn_populations(Count),
    Topology = build_topology(Populations),
    {noreply, State#state{topology = Topology}};
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
build_topology(Nodes) ->
    Type = mas_config:get_env(topology),
    mas_topology:new(Type, Nodes).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
spawn_populations(Count) ->
    timer:sleep(round(timer:seconds(5))),
    [mas_population_sup:spawn_population() || _ <- lists:seq(1, Count)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
perform_migration([], Agents, Source = {_Node, Population}) ->
    mas_logger:warning("Unable to migrate agents from ~p, sending agents back",
                       [Source]),
    mas_migration:migrate_back(Population, Agents);
perform_migration(Populations, Agents, _Source) ->
    mas_migration:migrate_to_populations(Populations, Agents).
