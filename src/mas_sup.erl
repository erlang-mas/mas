%%%-----------------------------------------------------------------------------
%%% @doc MAS top level supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_sup).

-behaviour(supervisor).

%%% API
-export([start_link/0,
         start_simulation/1,
         stop_simulation/0]).

%%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @doc Starts simulation supervisor.
%% @end
%%------------------------------------------------------------------------------
start_simulation(SP) ->
    ChildSpec = {mas_simulation_sup,
                 {mas_simulation_sup, start_link, [SP]},
                 temporary, 5000, supervisor, [mas_simulation_sup]
                },
    supervisor:start_child(mas_sup, ChildSpec).

%%------------------------------------------------------------------------------
%% @doc Terminates simulation supervisor.
%% @end
%%------------------------------------------------------------------------------
stop_simulation() ->
    supervisor:terminate_child(mas_sup, mas_simulation_sup).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    {ok, {{one_for_all, 1, 60},
     [
      {mas_simulation,
       {mas_simulation, start_link, []},
       permanent, 1000, worker, [mas_simulation]
      }
     ]
    }}.
