%%%-----------------------------------------------------------------------------
%%% @doc Simulation supervisor. Spawns world, populations and internode broker.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_simulation_sup).

%%% API
-export([start_link/1]).

%%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(SP) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, SP).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(SP) ->
    {ok, {{one_for_all, 0, 1},
     [
      {mas_population_sup,
       {mas_population_sup, start_link, [SP]},
       temporary, 10000, supervisor, [mas_population_sup]
      },

      {mas_migration_disp,
       {mas_migration_disp, start_link, []},
       permanent, 1000, worker, [mas_migration_disp]
      },

      {mas_world,
       {mas_world, start_link, []},
       temporary, 1000, worker, [mas_world]
      },

      {mas_world_broker,
       {mas_world_broker, start_link, []},
       temporary, 1000, worker, [mas_world_broker]
      }
     ]
    }}.
