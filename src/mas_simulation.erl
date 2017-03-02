%%%-----------------------------------------------------------------------------
%%% @doc Simulation coordinator. Starts simulation with provided parameters,
%%%      automaticaly stops simulation and retrieves results after specified
%%%      amount of time, sends results back to simulation initiator.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_simulation).

-include("mas.hrl").

-behaviour(gen_fsm).

%%% API
-export([start_link/1,
         start_simulation/2,
         stop_simulation/0]).

%%% FSM callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%%% FSM states
-export([idle/3,
         processing/3]).

-define(SERVER, ?MODULE).

-record(state, {module     :: module(),
                simulation :: simulation(),
                config     :: config()}).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback simulation_setup(sim_params()) -> any().

-callback simulation_teardown(sim_params()) -> any().

-callback simulation_result(sim_params(), [agent()]) -> any().

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(Config) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Config, []).

%%------------------------------------------------------------------------------
%% @doc Starts simulation with provided simulation parameters. Simulation
%%      terminates automaticaly if given time constraint is a positive integer.
%%      Otherwise simulation runs infinitely. MAS engine is able to process
%%      only one simulation at once.
%% @end
%%------------------------------------------------------------------------------
start_simulation(SP, Time) ->
    gen_fsm:sync_send_event(?SERVER, {start_simulation, SP, Time}).

%%------------------------------------------------------------------------------
%% @doc Terminates simulation.
%% @end
%%------------------------------------------------------------------------------
stop_simulation() ->
    gen_fsm:sync_send_event(?SERVER, stop_simulation).

%%%=============================================================================
%%% FSM callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Config = #config{simulation_mod = Mod}) ->
    process_flag(trap_exit, true),
    setup_reporter(Config),
    {ok, idle, #state{module = Mod, config = Config}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
idle({start_simulation, SP, Time}, {Pid, _Ref}, State) ->
    {reply, ok, processing, simulation_start(SP, Time, Pid, State)};
idle(_Event, _From, State) ->
    {reply, ignored, idle, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
processing(stop_simulation, _From, State) ->
    {reply, ok, idle, simulation_stop(State)};
processing(_Event, _From, State) ->
    {reply, ignored, processing, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(timeup, processing, State) ->
    {next_state, idle, simulation_stop(State)};
handle_info(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    mas_reporter:teardown().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {next_state, StateName, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_reporter(#config{logs_dir = LogsDir, write_interval = WriteInterval}) ->
    mas_reporter:setup(LogsDir, WriteInterval).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_start(SP, Time, Subscriber, State) ->
    #state{module = Mod, config = Config} = State,
    simulation_setup(Mod, SP),
    mas_sup:start_simulation(SP, Config),
    schedule_timer(Time),
    Simulation = simulation_record(SP, Time, Subscriber),
    State#state{simulation = Simulation}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_stop(State = #state{module = Mod, simulation = Simulation}) ->
    #simulation{params = SP, subscriber = Subscriber} = Simulation,
    {agents, Agents} = mas_world:get_agents(),
    mas_sup:stop_simulation(),
    simulation_teardown(Mod, SP),
    Result = simulation_result(Mod, SP, Agents),
    report_result(Result, Subscriber),
    State#state{simulation = none}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_timer(Time) when Time > 0 ->
    erlang:send_after(Time, ?SERVER, timeup);
schedule_timer(_Time) -> ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_record(SP, Time, Subscriber) ->
    #simulation{params = SP, time = Time, subscriber = Subscriber}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
report_result(Result, Subscriber) ->
    Subscriber ! {result, Result}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_setup(Mod, SP) ->
    Mod:simulation_setup(SP).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_teardown(Mod, SP) ->
    Mod:simulation_teardown(SP).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_result(Mod, SP, Agents) ->
    Mod:simulation_result(SP, Agents).
