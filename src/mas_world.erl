%%%-----------------------------------------------------------------------------
%%% @doc Spawns multiple populations of agents. Handles agent migrations.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_world).

-include("mas.hrl").

-behaviour(gen_server).

%%% API
-export([start_link/1,
         get_agents/0,
         migrate_agents/1,
         migrate_agents/2]).

%%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {population_count :: pos_integer(),
                populations = [] :: [pid()],
                topology         :: topology()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%%------------------------------------------------------------------------------
%% @doc Collects agents from all populations.
%% @end
%%------------------------------------------------------------------------------
get_agents() ->
    gen_server:call(?SERVER, get_agents).

%%------------------------------------------------------------------------------
%% @doc Migrates partitioned agent groups into target populations calculated
%%      based on configured topology.
%% @end
%%------------------------------------------------------------------------------
migrate_agents(Agents) ->
    gen_server:cast(?SERVER, {migrate_agents, Agents, self()}).

%%------------------------------------------------------------------------------
%% @doc Migrates agents group into world residing on distributed node.
%% @end
%%------------------------------------------------------------------------------
migrate_agents(Node, Agents) ->
    gen_server:cast({?SERVER, Node}, {migrate_agents, Agents, none}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(#config{population_count = Count, topology = Topology}) ->
    self() ! spawn_populations,
    {ok, #state{population_count = Count, topology = Topology}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(get_agents, _From, State = #state{populations = Populations}) ->
    Agents = collect_agents(Populations),
    {reply, {agents, Agents}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agents, Agents, Source}, State) ->
    #state{populations = Populations, topology = Topology} = State,
    case mas_topology:destinations(Topology, Populations, Source) of
        {ok, Destinations} ->
            mas_migration:send_to_populations(Destinations, Agents);
        no_destination ->
            mas_migration:send_back(Source, Agents)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(spawn_populations, State = #state{population_count = Count}) ->
    Populations = spawn_populations(Count),
    {noreply, State#state{populations = Populations}};
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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
collect_agents(Populations) ->
    Agents = [collect_population(Population) || Population <- Populations],
    lists:flatten(Agents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
collect_population(Population) ->
    {agents, Agents} = mas_population:get_agents(Population),
    Agents.
