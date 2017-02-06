%%%-------------------------------------------------------------------
%% @doc MAS supervised application.
%% @end
%%%-------------------------------------------------------------------

-module(mas).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    ChildSpecs = [child_spec(mas_world)],
    {ok, {{one_for_all, 0, 1}, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
child_spec(M) ->
    {M, {M, start_link, []}, permanent, brutal_kill, worker, [M]}.
