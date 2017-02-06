%%%-------------------------------------------------------------------
%% @doc Defines generic behaviour for MAS population.
%% @end
%%%-------------------------------------------------------------------

-module(mas_population).

%% API
-export([start/0, start_link/0, init/0, add_agent/2]).

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

start() ->
    spawn(?MODULE, init, []).

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
    lists:duplicate(PopulationSize, Mod:initial_agent()).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
loop(State = #state{agents=Agents, module=Mod}) ->
    receive
        {agent, NewAgent} ->
            loop(State#state{agents=[NewAgent | Agents]})
    after 0 ->
        TaggedAgents = determine_behaviours(Mod, Agents),

        ArenasBefore = form_arenas(TaggedAgents),
        ArenasAfter  = apply_meetings(Mod, ArenasBefore),

        NewAgents = normalize(ArenasAfter),
        loop(State#state{agents=NewAgents})
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
determine_behaviours(Mod, Agents) ->
    [{behaviour_function(Mod, Agent), Agent} || Agent <- Agents].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
behaviour_function(Mod, Agent) ->
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
apply_meetings(Mod, Arenas) ->
    [meeting_function(Mod, Arena) || Arena <- Arenas].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
meeting_function(_Mod, {migration, Agents}) ->
    [mas_world:migrate_agent(Agent) || Agent <- Agents],
    [];
meeting_function(Mod, Arena) ->
    Mod:meeting_function(Arena).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
normalize(Arenas) ->
    mas_utils:shuffle(lists:flatten(Arenas)).
