%%%-----------------------------------------------------------------------------
%%% @doc Defines generic behaviour for MAS population.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_population).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_agent/2,
         get_agents/1]).

%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type agent()             :: any().
-type behaviour()         :: atom().
-type metric()            :: [any()].
-type metrics_counter()   :: dict:dict(term(), integer()).

-record(config, {migration_probability       :: float(),
                 world_migration_probability :: float(),
                 write_interval              :: integer()}).

-type config() :: #config{}.

-record(state, {module             :: module(),
                agents             :: [agent()],
                config             :: config(),
                metrics            :: [metric()],
                behaviours_counter :: metrics_counter()}).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback initial_agent() -> agent().

-callback behaviours() -> [behaviour()].

-callback behaviour(agent()) -> behaviour().

-callback meeting({behaviour(), [agent()]}) -> [agent()].

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

get_agents(Pid) ->
    gen_server:call(Pid, get_agents).

add_agent(Pid, Agent) ->
    gen_server:cast(Pid, {add_agent, Agent}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    process_flag(trap_exit, true),
    State = init_state(),
    schedule_metrics_update(State#state.config),
    self() ! process_population,
    {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(get_agents, _From, State) ->
    {reply, {agents, State#state.agents}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({add_agent, Agent}, State = #state{agents = Agents}) ->
    NewState = State#state{agents = [Agent | Agents]},
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(process_population, State) ->
    NewState = process_population(State),
    self() ! process_population,
    {noreply, NewState};
handle_info(update_metrics, State = #state{config = Config}) ->
    update_metrics(State),
    schedule_metrics_update(Config),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, #state{metrics = Metrics}) ->
    lists:foreach(fun unsubscribe_metric/1, Metrics).

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
init_state() ->
    Mod = mas_config:get_env(population),
    Config = fetch_config(),
    Behaviours = behaviours(Mod),
    #state{
        module             = Mod,
        agents             = generate_population(Mod),
        config             = Config,
        metrics            = setup_metrics(Behaviours, Config),
        behaviours_counter = mas_counter:new(Behaviours)
    }.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
fetch_config() ->
    MP  = mas_config:get_env(migration_probability),
    WMP = mas_config:get_env(world_migration_probability),
    #config{
        migration_probability       = MP,
        world_migration_probability = WMP,
        write_interval              = mas_config:get_env(write_interval)
    }.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_population(Mod) ->
    Size = mas_config:get_env(population_size),
    [Mod:initial_agent() || _ <- lists:seq(1, Size)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_population(State = #state{behaviours_counter = Counter}) ->
    TaggedAgents = tag_agents(State),

    Arenas = form_arenas(TaggedAgents),
    ProcessedArenas = process_arenas(Arenas, State),

    NewAgents = normalize(ProcessedArenas),

    BehaviourCounts = count_behaviours(Arenas),
    NewCounter = mas_counter:update(BehaviourCounts, Counter),

    State#state{agents = NewAgents, behaviours_counter = NewCounter}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
tag_agents(State = #state{agents = Agents}) ->
    [{behaviour(Agent, State), Agent} || Agent <- Agents].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviour(Agent, #state{module = Mod, config = Config}) ->
    MP  = Config#config.migration_probability,
    WMP = Config#config.world_migration_probability,
    case rand:uniform() of
        R when R < MP       -> migration;
        R when R < MP + WMP -> world_migration;
        _                   -> Mod:behaviour(Agent)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviours(Mod) ->
    [migration, world_migration] ++ Mod:behaviours().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
form_arenas(Agents) ->
    mas_utils:group_by(Agents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_arenas(Arenas, #state{module = Mod}) ->
    [apply_meetings(Arena, Mod) || Arena <- Arenas].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
apply_meetings({migration, Agents}, _Mod) ->
    mas_world:migrate_agents(Agents), [];
apply_meetings({world_migration, Agents}, _Mod) ->
    mas_broker:migrate_agents(Agents), [];
apply_meetings(Arena, Mod) ->
    Mod:meeting(Arena).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
normalize(Arenas) ->
    mas_utils:shuffle(lists:flatten(Arenas)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
count_behaviours(Arenas) ->
    [{Behaviour, length(Agents)} || {Behaviour, Agents} <- Arenas].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_metrics(Behaviours, #config{write_interval = WriteInterval}) ->
    [subscribe_metric(Behaviour, WriteInterval) || Behaviour <- Behaviours].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
subscribe_metric(Name, WriteInterval) ->
    Metric = [node(), self(), Name],
    exometer:new(Metric, counter),
    exometer_report:subscribe(mas_reporter, Metric, value, WriteInterval),
    Metric.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unsubscribe_metric(Metric) ->
    exometer_report:unsubscribe_all(mas_reporter, Metric),
    exometer:delete(Metric).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_metrics_update(#config{write_interval = WriteInterval}) ->
    erlang:send_after(WriteInterval, self(), update_metrics).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_metrics(#state{metrics = Metrics, behaviours_counter = Counter}) ->
    lists:foreach(fun(Metric = [_Node, _Pid, Behaviour]) ->
                      exometer:update(Metric, dict:fetch(Behaviour, Counter))
                  end, Metrics).
