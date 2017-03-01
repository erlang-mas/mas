%%%-----------------------------------------------------------------------------
%%% @doc MAS API.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas).

%%% API
-export([start/0,
         start_simulation/2,
         stop_simulation/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts MAS engine.
%% @end
%%------------------------------------------------------------------------------
start() ->
    application:ensure_all_started(mas).

%%------------------------------------------------------------------------------
%% @doc Starts simulation with provided simulation parameters. Simulation
%%      terminates automaticaly if given time constraint is a positive integer.
%%      Otherwise simulation runs infinitely.
%% @end
%%------------------------------------------------------------------------------
start_simulation(SP, Time) ->
    mas_simulation:start_simulation(SP, Time).

%%------------------------------------------------------------------------------
%% @doc Terminates simulation.
%% @end
%%------------------------------------------------------------------------------
stop_simulation() ->
    mas_simulation:stop_simulation().
