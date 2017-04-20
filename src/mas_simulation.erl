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
-export([start_link/0,
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

-record(state, {simulation :: simulation()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc Starts simulation with provided simulation parameters. Simulation
%%      stops automaticaly if given time constraint is a positive integer.
%%      Otherwise simulation runs until stopped manualy.
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
init(_Args) ->
    process_flag(trap_exit, true),
    mas_reporter:setup(),
    {ok, idle, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
idle({start_simulation, SP, Time}, From, State) ->
    mas_sup:start_simulation(SP),
    schedule_timer(Time),
    Simulation = simulation_record(SP, Time, From),
    {reply, ok, processing, State#state{simulation = Simulation}};
idle(_Event, _From, State) ->
    {reply, ignored, idle, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
processing(stop_simulation, _From, State) ->
    do_stop_simulation(State),
    {reply, ok, idle, State#state{simulation = none}};
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
    do_stop_simulation(State),
    {next_state, idle, State#state{simulation = none}};
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
do_stop_simulation(#state{simulation = Simulation}) ->
    #simulation{result_sink = ResultSink} = Simulation,
    mas_sup:stop_simulation(),
    gen_fsm:reply(ResultSink, stopped).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_timer(Time) when Time > 0 ->
    erlang:send_after(Time, ?SERVER, timeup);
schedule_timer(_Time) -> ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
simulation_record(SP, Time, ResultSink) ->
    #simulation{sim_params = SP, time = Time, result_sink = ResultSink}.
