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

-record(state, {module                :: module(),
                mod_state             :: mod_state(),
                agents                :: population(),
                step                  :: integer(),
                measurement           :: integer(),
                metrics               :: counter(),
                measurement_interval  :: integer()}).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback init_agent(sim_params()) ->
    agent().

-callback init(population(), sim_params()) ->
    {population(), mod_state()}.

-callback step(population(), mod_state()) ->
    {population(), mod_state()} |
    {population(), population(), mod_state()}.

-callback measure(population(), mod_state()) ->
    {[metric_entry()], mod_state()}.

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
    schedule_measurement(State#state.measurement_interval),
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
handle_cast({add_agents, NewAgents}, State) ->
    #state{agents = Agents, metrics = Metrics} = State,
    NewMetrics = update_metric(received_agents, length(NewAgents), Metrics),
    {noreply, State#state{agents = Agents ++ NewAgents,
                          metrics = NewMetrics}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(make_step, State) ->
    #state{module = Mod,
           mod_state = ModState,
           agents = Agents,
           step = Step,
           metrics = Metrics} = State,
    {NewAgents, Emigrants, NewModState} = Mod:step(Agents, ModState),
    migrate_agents(Emigrants),
    NewMetrics = update_metric(migrations, length(Emigrants), Metrics),
    schedule_next_step(),
    {noreply, State#state{mod_state = NewModState,
                          agents = NewAgents,
                          step = Step + 1,
                          metrics = NewMetrics}};
handle_info(measure, State) ->
    #state{module = Mod,
           mod_state = ModState,
           agents = Agents,
           measurement = Measurement,
           metrics = Metrics,
           measurement_interval = MeasurementInterval} = State,
    {ModMetrics, NewModState} = Mod:measure(Agents, ModState),
    M1 = update_metric(agents_count, length(Agents), Metrics),
    M2 = mas_counter:reset(M1),
    report_metrics(Measurement, ModMetrics ++ dict:to_list(M1)),
    schedule_measurement(MeasurementInterval),
    {noreply, State#state{mod_state = NewModState,
                          measurement = Measurement + 1,
                          metrics = M2}};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, State) ->
    #state{module = Mod, mod_state = ModState, agents = Agents} = State,
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
    MeasurementInterval = mas_config:get_env(measurement_interval),
    InitialAgents = generate_population(Mod, SP),
    {Agents, ModState} = Mod:init(InitialAgents, SP),
    Metrics = mas_counter:new([migrations, agents_count, received_agents]),
    #state{module = Mod,
           mod_state = ModState,
           agents = Agents,
           step = 1,
           measurement = 1,
           metrics = Metrics,
           measurement_interval = MeasurementInterval}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_population(Mod, SP) ->
    Size = mas_config:get_env(population_size),
    [Mod:init_agent(SP) || _ <- lists:seq(1, Size)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
migrate_agents(Emigrants) ->
    mas_migration_disp:migrate_agents(Emigrants).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_next_step() ->
    self() ! make_step.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_measurement(MeasurementInterval) ->
    erlang:send_after(MeasurementInterval, self(), measure).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_metric(Name, Value, Metrics) ->
    mas_counter:update([{Name, Value}], Metrics).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
report_metrics(Measurement, Metrics) ->
    mas_logger:info("~p", [[{measurement, Measurement} | Metrics]]).
