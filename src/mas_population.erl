%%%-------------------------------------------------------------------
%% @doc Defines generic behaviour for MAS population.
%% @end
%%%-------------------------------------------------------------------

-module(mas_population).

%% API
-export([start/0, start_link/0, init/0]).

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
    Tagged = determine_behaviours(Mod, Agents),

    ArenasBefore = form_arenas(Tagged),
    ArenasAfter  = apply_meetings(Mod, ArenasBefore),

    NewAgents = normalize(ArenasAfter),
    loop(State#state{agents=NewAgents}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
determine_behaviours(Mod, Agents) ->
    [{Mod:behaviour_function(Agent), Agent} || Agent <- Agents].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
form_arenas(Agents) ->
    mas_utils:group_by(Agents).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
apply_meetings(Mod, Arenas) ->
    [Mod:meeting_function(Arena) || Arena <- Arenas].

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
normalize(Arenas) ->
    mas_utils:shuffle(lists:flatten(Arenas)).
