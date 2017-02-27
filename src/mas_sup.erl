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
child_spec(Mod, Type, Args) ->
    #{id       => Mod,
      start    => {Mod, start_link, [Args]},
      restart  => temporary,
      shutdown => 1000,
      type     => Type,
      modules  => [Mod]}.
