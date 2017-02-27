%%%-----------------------------------------------------------------------------
%%% @doc Simulation supervisor. Spawns world, populations and internode broker.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_simulation_sup).

%%% API
-export([start_link/2]).

%%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(SP, Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {SP, Config}).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init({SP, Config}) ->
    {ok, {{one_for_all, 0, 1},
     [
      {mas_population_sup,
       {mas_population_sup, start_link, [SP, Config]},
       temporary, infinity, supervisor, [mas_population_sup]
      },

      {mas_world,
       {mas_world, start_link, [Config]},
       temporary, 1000, worker, [mas_world]
      },

      {mas_broker,
       {mas_broker, start_link, [Config]},
       temporary, 1000, worker, [mas_broker]
      }
     ]
    }}.
