%%%-----------------------------------------------------------------------------
%% @doc
%% Defines generic behaviour for MAS population.
%% @end
%%%-----------------------------------------------------------------------------

-module(mas_population).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_agent/2,
         get_agents/1]).

%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type agent()     :: any().
-type behaviour() :: atom().

-record(state, {agents                :: [agent()],
                module                :: module(),
                migration_probability :: float()}).

%%==============================================================================
%% Behaviour
%%==============================================================================

-callback initial_agent() -> agent().

-callback behaviours() -> [behaviour()].

-callback behaviour(agent()) -> behaviour().

-callback meeting({behaviour(), [agent()]}) -> [agent()].

%%==============================================================================
%% API functions
%%==============================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

get_agents(Pid) ->
    gen_server:call(Pid, get_agents).

add_agent(Pid, Agent) ->
    gen_server:cast(Pid, {add_agent, Agent}).

%%==============================================================================
%% Server callbacks
%%==============================================================================

init(_Args) ->
    Mod = mas_config:get_env(population),
    InitialAgents = generate_population(Mod),
    MP = mas_config:get_env(migration_probability),
    self() ! process_population,
    {ok, #state{module                = Mod,
                agents                = InitialAgents,
                migration_probability = MP}}.

handle_call(get_agents, _From, State) ->
    {reply, {agents, State#state.agents}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({add_agent, Agent}, State = #state{agents = Agents}) ->
    NewState = State#state{agents = [Agent | Agents]},
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(process_population, State) ->
    NewState = process_population(State),
    self() ! process_population,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

generate_population(Mod) ->
    PopulationSize = mas_config:get_env(population_size),
    [Mod:initial_agent() || _ <- lists:seq(1, PopulationSize)].

process_population(State) ->
    TaggedAgents = determine_behaviours(State),
    Arenas = form_arenas(TaggedAgents),
    ProcessedArenas = process_arenas(Arenas, State),
    NewAgents = normalize(ProcessedArenas),
    State#state{agents = NewAgents}.

determine_behaviours(State = #state{agents = Agents}) ->
    [{behaviour(Agent, State), Agent} || Agent <- Agents].

behaviour(Agent, #state{module = Mod, migration_probability = MP}) ->
    case rand:uniform() of
        R when R < MP  -> migration;
        R when R >= MP -> Mod:behaviour(Agent)
    end.

form_arenas(Agents) ->
    mas_utils:group_by(Agents).

process_arenas(Arenas, #state{module = Mod}) ->
    [apply_meetings(Mod, Arena) || Arena <- Arenas].

apply_meetings(_Mod, {migration, Agents}) ->
    [mas_world:migrate_agent(Agent) || Agent <- Agents], [];
apply_meetings(Mod, Arena) ->
    Mod:meeting(Arena).

normalize(Arenas) ->
    mas_utils:shuffle(lists:flatten(Arenas)).
