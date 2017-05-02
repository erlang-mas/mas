%%%-----------------------------------------------------------------------------
%%% @doc Dispatches agents to migration brokers.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_migration_disp).

-behaviour(gen_server).

%%% API
-export([start_link/0,
         migrate_agents/1]).

%%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {node_migration_probability  :: float()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

migrate_agents(Agents) ->
    Source = {node(), self()},
    gen_server:cast(?SERVER, {migrate_agents, Agents, Source}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    NMP = mas_config:get_env(node_migration_probability),
    {ok, #state{node_migration_probability = NMP}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agents, Agents, Source}, State) ->
    #state{node_migration_probability = NMP} = State,
    case rand:uniform() < NMP of
        true ->
            mas_world_broker:migrate_agents(Agents, Source);
        false ->
            mas_world:migrate_agents(Agents, Source)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
