%%%-----------------------------------------------------------------------------
%%% @doc MAS top level supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_sup).

-include("mas.hrl").

-behaviour(supervisor).

%%% API
-export([start_link/0,
         start_simulation/2,
         stop_simulation/0]).

%%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_simulation(SP, Config) ->
    ChildSpec = {mas_simulation_sup,
                 {mas_simulation_sup, start_link, [SP, Config]},
                 temporary, 5000, supervisor, [mas_simulation_sup]
                },
    supervisor:start_child(mas_sup, ChildSpec).

stop_simulation() ->
    supervisor:terminate_child(mas_sup, mas_simulation_sup).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    Config = mas_config:fetch_all(),
    mas_reporter:setup(Config#config.logs_dir),
    {ok, {{one_for_all, 0, 1},
     [
      {mas_simulation,
       {mas_simulation, start_link, [Config]},
       temporary, 1000, worker, [mas_simulation]
      }
     ]
    }}.
