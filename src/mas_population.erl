%%%-----------------------------------------------------------------------------
%%% @doc Defines generic behaviour for MAS population.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_population).

-include("mas.hrl").

-behaviour(gen_server).

%%% API
-export([start_link/1,
         get_agents/1,
         add_agents/2]).

%%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {module          :: module(),
                agents          :: population(),
                mod_state       :: mod_state(),
                metrics_counter :: counter(),
                write_interval  :: integer()}).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback init(population(), sim_params()) ->
    {population(), mod_state()}.

-callback initial_agent(sim_params()) ->
    agent().

-callback behaviours() ->
    [behaviour()].

-callback behaviour(agent(), mod_state()) ->
    behaviour().

-callback apply_behaviour(behaviour(), population(), mod_state()) ->
    population().

-callback preprocess(population(), mod_state()) ->
    {population(), mod_state()}.

-callback postprocess(population(), mod_state()) ->
    {population(), mod_state()}.

-callback metrics(population(), mod_state()) ->
    [metric()].

-callback terminate(population(), mod_state()) ->
    any().

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(SP) ->
    gen_server:start_link(?MODULE, SP, []).

get_agents(Pid) ->
    gen_server:call(Pid, get_agents).

add_agents(Pid, Agents) ->
    gen_server:cast(Pid, {add_agents, Agents}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(SP) ->
    process_flag(trap_exit, true),
    mas_utils:seed_random(),
    State = init_state(SP),
    schedule_next_step(),
    schedule_metrics_update(State),
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(get_agents, _From, State = #state{agents = Agents}) ->
    {reply, {agents, Agents}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({add_agents, NewAgents}, State = #state{agents = Agents}) ->
    {noreply, State#state{agents = Agents ++ NewAgents}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(process, State) ->
    #state{module = Mod,
           agents = A,
           mod_state = MS,
           metrics_counter = Counter} = State,
    {A1, MS1} = preprocess(Mod, A, MS),
    TaggedAgents = determine_behaviours(Mod, A1, MS1),
    Arenas = form_arenas(TaggedAgents),
    ProcessedArenas = process_arenas(Mod, Arenas, MS1),
    A2 = extract_agents(ProcessedArenas),
    {A3, MS2} = postprocess(Mod, A2, MS1),
    BehaviourCounts = count_behaviours(Arenas),
    NewCounter = mas_counter:update(BehaviourCounts, Counter),
    schedule_next_step(),
    {noreply, State#state{agents = A3,
                          mod_state = MS2,
                          metrics_counter = NewCounter}};
handle_info(update_metrics, State) ->
    #state{module = Mod,
           agents = Agents,
           mod_state = ModState,
           metrics_counter = Counter} = State,
    ModMetrics = Mod:metrics(Agents, ModState),
    Metrics = [{agents_count, length(Agents)} |
               lists:append(ModMetrics, dict:to_list(Counter))],
    mas_logger:info("~p", [Metrics]),
    NewCounter = mas_counter:reset(Counter),
    schedule_metrics_update(State),
    {noreply, State#state{metrics_counter = NewCounter}};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, State) ->
    #state{module = Mod, agents = Agents, mod_state = ModState} = State,
    Mod:terminate(Agents, ModState).

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
init_state(SP) ->
    Mod = mas_config:get_env(population_mod),
    PopulationSize = mas_config:get_env(population_size),
    InitialAgents = generate_population(Mod, SP, PopulationSize),
    {Agents, ModState} = Mod:init(InitialAgents, SP),
    Behaviours = behaviours(Mod),
    MetricsCounter = mas_counter:new(Behaviours),
    WriteInterval = mas_config:get_env(write_interval),
    #state{module = Mod,
           agents = Agents,
           mod_state = ModState,
           metrics_counter = MetricsCounter,
           write_interval = WriteInterval}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_population(Mod, SP, Size) ->
    [Mod:initial_agent(SP) || _ <- lists:seq(1, Size)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
preprocess(Mod, Agents, ModState) ->
    Mod:preprocess(Agents, ModState).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
postprocess(Mod, Agents, ModState) ->
    Mod:postprocess(Agents, ModState).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviours(Mod) ->
    [migration | Mod:behaviours()].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
determine_behaviours(Mod, Agents, ModState) ->
    [{Mod:behaviour(Agent, ModState), Agent} || Agent <- Agents].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
form_arenas(TaggedAgents) ->
    mas_utils:group_by_key(TaggedAgents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_arenas(Mod, Arenas, ModState) ->
    [apply_behaviour(Mod, Arena, ModState) || Arena <- Arenas].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
apply_behaviour(_Mod, {migration, Agents}, _ModState) ->
    mas_world:migrate_agents(Agents),
    [];
apply_behaviour(Mod, {Behaviour, Agents}, ModState) ->
    Mod:apply_behaviour(Behaviour, Agents, ModState).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
extract_agents(Arenas) ->
    mas_utils:shuffle(lists:flatten(Arenas)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
count_behaviours(Arenas) ->
    [{Behaviour, length(Agents)} || {Behaviour, Agents} <- Arenas].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_next_step() ->
    self() ! process.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_metrics_update(#state{write_interval = WriteInterval}) ->
    erlang:send_after(WriteInterval, self(), update_metrics).
