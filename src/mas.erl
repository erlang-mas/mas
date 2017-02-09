%%%-----------------------------------------------------------------------------
%% @doc
%% Main application module. Spawns agents world controller and populations
%% supervisor.
%% @end
%%%-----------------------------------------------------------------------------

-module(mas).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%==============================================================================
%% Application callbacks
%%==============================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    ok.

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

init(_Args) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 0,
                 period    => 1},
    ChildSpecs = [
        child_spec(mas_population_sup, supervisor),
        child_spec(mas_world, worker)
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%==============================================================================
%% Internal functions
%%==============================================================================

child_spec(M, Type) ->
    #{id       => M,
      start    => {M, start_link, []},
      restart  => temporary,
      shutdown => 1000,
      type     => Type,
      modules  => [M]}.
