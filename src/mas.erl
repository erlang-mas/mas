%%%-----------------------------------------------------------------------------
%%% @doc MAS API.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas).

%%% API
-export([start/0,
         start_simulation/2,
         get_results/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start() ->
    application:ensure_all_started(mas).

start_simulation(SP, Time) ->
    mas_simulation:start_simulation(SP, Time).

get_results() ->
    mas_world:get_agents().
