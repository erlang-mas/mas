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
    {ok, idle, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
idle({start_simulation, SP, Time}, From, State) ->
    mas_logger:info("Starting simultion with parameters: ~p "
                    "and time constraint: ~p", [SP, Time]),
    mas_sup:start_simulation(SP),
    mas_logger:info("Simulation started"),
    schedule_timer(Time),
    Simulation = simulation_record(SP, Time, From),
    {reply, ok, processing, State#state{simulation = Simulation}};
idle(_Event, _From, State) ->
    {reply, ignored, idle, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
processing(stop_simulation, From, State = #state{simulation = Simulation}) ->
    gen_fsm:reply(From, ok),
    do_stop_simulation(Simulation),
    Duration = calculate_duration(Simulation),
    mas_logger:info("Simulation stopped after ~.2f sec.", [Duration]),
    {next_state, idle, State#state{simulation = none}};
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
handle_info(timeup, processing, State = #state{simulation = Simulation}) ->
    do_stop_simulation(Simulation),
    {next_state, idle, State#state{simulation = none}};
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
do_stop_simulation(#simulation{result_sink = ResultSink}) ->
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
    #simulation{sim_params = SP,
                time = Time,
                result_sink = ResultSink,
                start_time = mas_utils:timestamp()}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
calculate_duration(#simulation{start_time = StartTime}) ->
    (mas_utils:timestamp() - StartTime) / 1000.
