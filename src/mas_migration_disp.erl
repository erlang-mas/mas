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

-record(state, {node_migration_probability  :: float(),
                write_interval              :: integer()}).

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
    WriteInterval = mas_config:get_env(write_interval),
    schedule_metrics_update(WriteInterval),
    {ok, #state{node_migration_probability = NMP,
                write_interval = WriteInterval}}.

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
handle_info(update_metrics, State) ->
    #state{write_interval = WriteInterval} = State,
    {_, QueueLen} = erlang:process_info(self(), message_queue_len),
    mas_logger:info("~p", [{migration_disp_queue_len, QueueLen}]),
    schedule_metrics_update(WriteInterval),
    {noreply, State};
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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_metrics_update(WriteInterval) ->
    erlang:send_after(WriteInterval, ?SERVER, update_metrics).
