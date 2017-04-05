%%%-----------------------------------------------------------------------------
%%% @doc Supervises agent populations.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_population_sup).

-behaviour(supervisor).

%%% API
-export([start_link/1,
         spawn_population/0]).

%%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(SP) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, SP).

%%------------------------------------------------------------------------------
%% @doc Spawns single agents population, returns it pid.
%% @end
%%------------------------------------------------------------------------------
spawn_population() ->
    {ok, Pid} = supervisor:start_child(?SERVER, []),
    Pid.

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(SP) ->
    {ok, {{simple_one_for_one, 0, 1},
     [
      {mas_population,
       {mas_population, start_link, [SP]},
       temporary, 1000, worker, [mas_population]
      }
     ]
    }}.
