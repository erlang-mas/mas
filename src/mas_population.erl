%%%-------------------------------------------------------------------
%% @doc Defines generic behaviour for MAS population.
%% @end
%%%-------------------------------------------------------------------

-module(mas_population).

%% API
-export([start_link/0, init/0, add_agent/2]).

-type agent()     :: any().
-type behaviour() :: atom().

-record(state, {agents :: [agent()],
                module :: module()}).

%%%===================================================================
%%% Behaviour
%%%===================================================================

-callback initial_agent() -> agent().

-callback behaviours() -> [behaviour()].

-callback behaviour_function(agent()) -> behaviour().

-callback meeting_function({behaviour(), [agent()]}) -> [agent()].

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    spawn_link(?MODULE, init, []).

add_agent(Population, Agent) ->
    Population ! {agent, Agent}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init() ->
    Mod = mas_config:get_env(population),
    InitialAgents = generate_population(Mod),
    loop(#state{agents=InitialAgents,
                module=Mod}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
generate_population(Mod) ->
    PopulationSize = mas_config:get_env(population_size),
    [Mod:initial_agent() || _ <- lists:seq(1, PopulationSize)].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
loop(State) ->
    receive
        Msg -> loop(handle_msg(Msg, State))
    after 0 ->
        loop(process_population(State))
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_msg({agent, NewAgent}, State = #state{agents=Agents}) ->
    State#state{agents=[NewAgent | Agents]}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
process_population(State = #state{agents=Agents, module=Mod}) ->
    TaggedAgents    = determine_behaviours(Mod, Agents),
    Arenas          = form_arenas(TaggedAgents),
    ProcessedArenas = process_arenas(Mod, Arenas),
    NewAgents       = normalize(ProcessedArenas),
    State#state{agents=NewAgents}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
determine_behaviours(Mod, Agents) ->
    [{behaviour(Mod, Agent), Agent} || Agent <- Agents].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
behaviour(Mod, Agent) ->
    MP = mas_config:get_env(migration_probability),
    case rand:uniform() of
        R when R < MP  -> migration;
        R when R >= MP -> Mod:behaviour_function(Agent)
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
form_arenas(Agents) ->
    mas_utils:group_by(Agents).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
process_arenas(Mod, Arenas) ->
    [apply_meetings(Mod, Arena) || Arena <- Arenas].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
apply_meetings(_Mod, {migration, Agents}) ->
    [mas_world:migrate_agent(Agent) || Agent <- Agents], [];
apply_meetings(Mod, Arena) ->
    Mod:meeting_function(Arena).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
normalize(Arenas) ->
    mas_utils:shuffle(lists:flatten(Arenas)).
