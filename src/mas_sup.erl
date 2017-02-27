%%%-----------------------------------------------------------------------------
%%% @doc MAS top level supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_sup).

-include("mas.hrl").

-behaviour(supervisor).

%%% API
-export([start_link/0]).

%%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
      {mas_population_sup,
       {mas_population_sup, start_link, [Config]},
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
