%%%-----------------------------------------------------------------------------
%%% @doc Spawns multiple populations of agents. Handles agent migrations.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_world).

-include("mas.hrl").

-behaviour(gen_server).

%%% API
-export([start_link/0,
         migrate_agents/2,
         put_agents/2]).

%%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {populations     :: [pid()],
                topology        :: topology(),
                write_interval  :: integer()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

migrate_agents(Agents, {_Node, Population}) ->
    gen_server:cast(?SERVER, {migrate_agents, Agents, Population}).

put_agents(Node, Agents) ->
    gen_server:cast({?SERVER, Node}, {migrate_agents, Agents, none}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    mas_utils:seed_random(),
    self() ! spawn_populations,
    WriteInterval = mas_config:get_env(write_interval),
    schedule_metrics_update(WriteInterval),
    {ok, #state{topology = mas_config:get_env(topology),
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
    #state{populations = Populations, topology = Topology} = State,
    case mas_topology:destinations(Topology, Populations, Source) of
        {ok, Destinations} ->
            mas_migration:send_to_populations(Destinations, Agents);
        no_destination ->
            mas_migration:send_back(Source, Agents)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(spawn_populations, State) ->
    Count = mas_config:get_env(population_count),
    {noreply, State#state{populations = spawn_populations(Count)}};
handle_info(update_metrics, State) ->
    #state{write_interval = WriteInterval} = State,
    {_, QueueLen} = erlang:process_info(self(), message_queue_len),
    mas_logger:info("~p", [{world_queue_len, QueueLen}]),
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
spawn_populations(Count) ->
    [mas_population_sup:spawn_population() || _ <- lists:seq(1, Count)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_metrics_update(WriteInterval) ->
    erlang:send_after(WriteInterval, ?SERVER, update_metrics).
