%%%-------------------------------------------------------------------
%% @doc Spawns multiple MAS populations and handles agent migrations
%% between them.
%% @end
%%%-------------------------------------------------------------------

-module(mas_world).

-behaviour(gen_server).

%% API
-export([start_link/0, migrate_agent/1]).

%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {populations :: [pid()]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

migrate_agent(Agent) ->
    gen_server:cast(?SERVER, {migrate, Agent, self()}).

%%====================================================================
%% Server callbacks
%%====================================================================

init([]) ->
    {ok, #state{populations=spawn_populations()}}.

handle_call(_Request, _From, State) ->
    {reply, undef, State}.

handle_cast({migrate, Agent, From}, State) ->
    Populations = [P || P <- State#state.populations, P =/= From],
    Destination = mas_utils:sample(Populations),
    mas_population:add_agent(Destination, Agent),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

spawn_populations() ->
    Islands = mas_config:get_env(islands),
    [mas_population:start_link() || _ <- lists:seq(1, Islands)].
