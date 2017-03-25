%%%-----------------------------------------------------------------------------
%%% @doc Defines generic behaviour for MAS population.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_population).

-include("mas.hrl").

-behaviour(gen_server).

%%% API
-export([start_link/2,
         add_agents/2,
         get_agents/1]).

%%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {module             :: module(),
                agents             :: [agent()],
                sim_params         :: sim_params(),
                behaviours_counter :: counter(),
                config             :: mas:config()}).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

-callback initial_agent(sim_params()) -> agent().

-callback behaviours() -> [behaviour()].

-callback behaviour(agent(), sim_params()) -> behaviour().

-callback meeting({behaviour(), [agent()]}, sim_params()) -> [agent()].

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(SP, Config) ->
    gen_server:start_link(?MODULE, {SP, Config}, []).

get_agents(Pid) ->
    gen_server:call(Pid, get_agents).

add_agents(Pid, Agents) ->
    gen_server:cast(Pid, {add_agents, Agents}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init({SP, Config}) ->
    process_flag(trap_exit, true),
    mas_utils:seed_random(),
    State = init_state(SP, Config),
    schedule_metrics_update(Config),
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
handle_cast({add_agents, NewAgents}, State = #state{agents = Agents}) ->
    {noreply, State#state{agents = Agents ++ NewAgents}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(process_population, State) ->
    self() ! process_population,
    {noreply, process_population(State)};
handle_info(update_metrics, State = #state{config = Config}) ->
    schedule_metrics_update(Config),
    {noreply, update_metrics(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, #state{behaviours_counter = Counter}) ->
    Behaviours = dict:fetch_keys(Counter),
    unsubscribe_metrics(Behaviours).

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
init_state(SP, Config = #config{population_mod = Mod}) ->
    Agents = generate_population(Mod, SP, Config),
    Behaviours = behaviours(Mod),
    BehavioursCounter = mas_counter:new(Behaviours),
    subscribe_metrics(Behaviours),
    #state{
        module = Mod,
        agents = Agents,
        sim_params = SP,
        behaviours_counter = BehavioursCounter,
        config = Config
    }.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_population(Mod, SP, #config{population_size = Size}) ->
    [Mod:initial_agent(SP) || _ <- lists:seq(1, Size)].

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
behaviour(Agent, #state{module = Mod, sim_params = SP, config = Config}) ->
    #config{migration_probability = MP,
            node_migration_probability = NMP} = Config,
    case rand:uniform() of
        R when R < MP       -> migration;
        R when R < MP + NMP -> node_migration;
        _                   -> Mod:behaviour(Agent, SP)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviours(Mod) ->
    [migration, node_migration] ++ Mod:behaviours().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
form_arenas(Agents) ->
    mas_utils:group_by(Agents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
process_arenas(Arenas, State) ->
    [apply_meetings(Arena, State) || Arena <- Arenas].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
apply_meetings({migration, Agents}, _State) ->
    mas_world:migrate_agents(Agents), [];
apply_meetings({node_migration, Agents}, _State) ->
    mas_broker:migrate_agents(Agents), [];
apply_meetings(Arena, #state{module = Mod, sim_params = SP}) ->
    Mod:meeting(Arena, SP).

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
subscribe_metrics(Behaviours) ->
    mas_reporter:subscribe(metric(agents_count), gauge, value),
    [mas_reporter:subscribe(metric(Behaviour)) || Behaviour <- Behaviours].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unsubscribe_metrics(Behaviours) ->
    mas_reporter:unsubscribe(metric(agents_count)),
    [mas_reporter:unsubscribe(metric(Behaviour)) || Behaviour <- Behaviours].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
schedule_metrics_update(#config{write_interval = WriteInterval}) ->
    erlang:send_after(WriteInterval, self(), update_metrics).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_metrics(State) ->
    #state{agents = Agents, behaviours_counter = Counter} = State,
    mas_reporter:update(metric(agents_count), length(Agents)),
    Behaviours = dict:fetch_keys(Counter),
    lists:foreach(fun(Behaviour) ->
                      mas_reporter:update(metric(Behaviour),
                                          dict:fetch(Behaviour, Counter))
                  end, Behaviours),
    State#state{behaviours_counter = mas_counter:reset(Counter)}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
metric(Name) -> [self(), Name].
