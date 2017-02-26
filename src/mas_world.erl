%%%-----------------------------------------------------------------------------
%%% @doc Spawns multiple populations of agents. Handles agent migrations.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_world).

-include("mas.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1,
         migrate_agent/1,
         migrate_agents/1,
         get_agents/0]).

%% Server callbacks
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

get_agents() ->
    gen_server:call(?SERVER, get_agents).

migrate_agent(Agent) ->
    gen_server:cast(?SERVER, {migrate_agent, Agent, self()}).

migrate_agents(Agents) ->
    lists:foreach(fun migrate_agent/1, Agents).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(#config{population_count = Count, topology = Topology}) ->
    self() ! spawn_populations,
    {ok, #state{population_count = Count,
                topology = Topology}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(get_agents, _From, State) ->
    Agents = collect_agents(State),
    {reply, {agents, Agents}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agent, Agent, From}, State) ->
    do_migrate_agent(Agent, From, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(spawn_populations, State) ->
    Populations = spawn_populations(State#state.population_count),
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
do_migrate_agent(Agent, From, #state{populations = Populations,
                                     topology = Topology}) ->
    case mas_topology:destination(Topology, From, Populations) of
        {ok, Destination} ->
            mas_population:add_agent(Destination, Agent);
        no_destination ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
collect_agents(#state{populations = Populations}) ->
    Agents = [collect_population(Population) || Population <- Populations],
    lists:flatten(Agents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
collect_population(Population) ->
    {agents, Agents} = mas_population:get_agents(Population),
    Agents.
