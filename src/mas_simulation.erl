%%%-----------------------------------------------------------------------------
%%% @doc Simulation coordinator. Starts simulation, retrieves results after
%%%      specified amount of time and stops simulation.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_simulation).

-include("mas.hrl").

-behaviour(gen_fsm).

%%% API
-export([start_link/1,
         start_simulation/2]).

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

-record(simulation, {params     :: sim_params(),
                     time       :: pos_integer(),
                     subscriber :: pid()}).

-type simulation() :: #simulation{}.

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

start_simulation(SP, Time) ->
    gen_fsm:sync_send_event(?SERVER, {start_simulation, SP, Time}).

%%%=============================================================================
%%% FSM callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Config = #config{simulation_mod = Mod, logs_dir = LogsDir}) ->
    mas_reporter:setup(LogsDir),
    {ok, idle, #state{module = Mod, config = Config}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
idle({start_simulation, SP, Time}, {Pid, _Ref}, State) ->
    #state{module = Mod, config = Config} = State,
    simulation_setup(Mod, SP),
    mas_sup:start_simulation(SP, Config),
    schedule_timer(Time),
    Simulation = simulation_record(SP, Time, Pid),
    {reply, ok, processing, State#state{simulation = Simulation}};
idle(_Event, _From, State) ->
    {reply, ignored, idle, State}.

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
    #state{module = Mod, simulation = Simulation} = State,
    #simulation{params = SP, subscriber = Subscriber} = Simulation,
    {agents, Agents} = mas_world:get_agents(),
    mas_sup:stop_simulation(),
    simulation_teardown(Mod, SP),
    Result = simulation_result(Mod, SP, Agents),
    report_result(Result, Subscriber),
    {next_state, idle, State};
handle_info(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

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
