%%%-----------------------------------------------------------------------------
%%% @doc Spawns multiple populations of agents. Handles agent migrations.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_world).

-behaviour(gen_server).

%% API
-export([start_link/0,
         migrate_agent/1,
         get_agents/0]).

%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {populations       :: [pid()],
                populations_count :: integer(),
                topology          :: atom()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_agents() ->
    gen_server:call(?SERVER, get_agents).

migrate_agent(Agent) ->
    gen_server:cast(?SERVER, {migrate_agent, Agent, self()}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    self() ! spawn_populations,
    {ok, #state{populations       = [],
                populations_count = mas_config:get_env(populations_count),
                topology          = mas_config:get_env(topology)}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(get_agents, _From, State) ->
    Results = gather_agents(State),
    {reply, {results, Results}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agent, Agent, From}, State) ->
    Destination = calculate_destination(From, State),
    mas_population:add_agent(Destination, Agent),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(spawn_populations, State) ->
    Populations = spawn_populations(State),
    {noreply, State#state{populations = Populations}};
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
spawn_populations(#state{populations_count = Count}) ->
    [mas_population_sup:spawn_population() || _ <- lists:seq(1, Count)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
calculate_destination(From, #state{populations = Populations,
                                   topology    = Topology}) ->
    mas_topology:calculate_destination(Topology, From, Populations).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
gather_agents(#state{populations = Populations}) ->
    Results = [gather_population(Population) || Population <- Populations],
    lists:flatten(Results).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
gather_population(Population) ->
    {agents, Agents} = mas_population:get_agents(Population),
    Agents.
