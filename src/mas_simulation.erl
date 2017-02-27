%%%-----------------------------------------------------------------------------
%%% @doc Simulation coordinator. Starts simulation, retrieves results after
%%%      specified amount of time, stops simulation.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_simulation).

-include("mas.hrl").

-behaviour(gen_server).

%%% API
-export([start_link/1,
         start_simulation/2]).

%%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

start_simulation(SP, Time) ->
    gen_server:call(?SERVER, {start_simulation, SP, Time}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Config = #config{simulation_mod = Mod, logs_dir = LogsDir}) ->
    mas_reporter:setup(LogsDir),
    {ok, #state{module = Mod, config = Config}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({start_simulation, SP, Time}, {Pid, _Ref}, State) ->
    #state{module = Mod, config = Config} = State,
    simulation_setup(Mod, SP),
    mas_sup:start_simulation(SP, Config),
    schedule_timer(Time),
    Simulation = simulation_record(SP, Time, Pid),
    {reply, started, State#state{simulation = Simulation}};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(timeup, State = #state{module = Mod, simulation = Simulation}) ->
    #simulation{params = SP, subscriber = Subscriber} = Simulation,
    {agents, Agents} = mas_world:get_agents(),
    mas_sup:stop_simulation(),
    simulation_teardown(Mod, SP),
    Result = simulation_result(Mod, SP, Agents),
    report_result(Result, Subscriber),
    {noreply, State};
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
