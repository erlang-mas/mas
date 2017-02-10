%%%-----------------------------------------------------------------------------
%%% @doc Main application module. Spawns agents world and populations
%%% supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([get_results/0]).

%% Application callbacks
-export([start/2,
         stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

get_results() ->
    mas_world:get_agents().

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) ->
    ok.

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 0,
                 period    => 1},
    ChildSpecs = [
        child_spec(mas_population_sup, supervisor),
        child_spec(mas_world, worker)
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
child_spec(M, Type) ->
    #{id       => M,
      start    => {M, start_link, []},
      restart  => temporary,
      shutdown => 1000,
      type     => Type,
      modules  => [M]}.
