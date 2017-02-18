%%%-----------------------------------------------------------------------------
%%% @doc Supervises multiple agent populations.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_population_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
         spawn_population/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

spawn_population() ->
    {ok, Pid} = supervisor:start_child(?SERVER, []),
    Pid.

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Config) ->
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 0,
                 period    => 1},
    ChildSpecs = [#{id       => mas_population,
                    start    => {mas_population, start_link, [Config]},
                    restart  => temporary,
                    shutdown => 1000,
                    type     => worker,
                    modules  => [mas_population]}],
    {ok, {SupFlags, ChildSpecs}}.
