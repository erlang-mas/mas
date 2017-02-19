%%%-----------------------------------------------------------------------------
%%% @doc Main application module. Spawns agents world and populations
%%% supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas).

-include("mas.hrl").

-behaviour(application).
-behaviour(supervisor).

%% API
-export([start/0, get_results/0]).

%% Application callbacks
-export([start/2,
         stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start() ->
    application:ensure_all_started(exometer_core),
    application:start(mas).

get_results() ->
    mas_world:get_agents().

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Config = mas_config:fetch_all(),
    setup_exometer(Config),
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

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
init(Config) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 0,
                 period    => 1},
    ChildSpecs = [
        child_spec(mas_population_sup, supervisor, Config),
        child_spec(mas_world,          worker,     Config),
        child_spec(mas_broker,         worker,     Config)
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_exometer(#config{logs_dir = LogsDir}) ->
    exometer_report:add_reporter(exometer_report_fs, [{base_dir, LogsDir}]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
child_spec(Mod, Type, Args) ->
    #{id       => Mod,
      start    => {Mod, start_link, [Args]},
      restart  => temporary,
      shutdown => 1000,
      type     => Type,
      modules  => [Mod]}.
