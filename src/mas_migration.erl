%%%-----------------------------------------------------------------------------
%%% @doc Agent migrations API.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_migration).

%%% API
-export([send_to_nodes/2,
         send_to_populations/2,
         send_back/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Partitions provided agents list into number of chunks equal to number
%%      of possible destinations. Sends each chunk to different node.
%% @end
%%------------------------------------------------------------------------------
send_to_nodes(Nodes, Agents) ->
    lists:foreach(fun send_to_node/1, group(Nodes, Agents)).

%%------------------------------------------------------------------------------
%% @doc Partitions provided agents list into number of chunks equal to number
%%      of possible destinations. Sends each chunk to different population.
%% @end
%%------------------------------------------------------------------------------
send_to_populations(Populations, Agents) ->
    lists:foreach(fun send_to_population/1, group(Populations, Agents)).

%%------------------------------------------------------------------------------
%% @doc Sends agents back to population that initiated the migration.
%% @end
%%------------------------------------------------------------------------------
send_back(none, _Agents) -> ok;
send_back(Source, Agents) -> send_to_population({Source, Agents}).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
group(Destinations, Agents) ->
    AgentGroups = mas_utils:partition(Agents, length(Destinations)),
    lists:zip(mas_utils:shuffle(Destinations), AgentGroups).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_to_node({_Node, []}) -> ok;
send_to_node({Node, Agents}) ->
    mas_world:put_agents(Node, Agents).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_to_population({_Population, []}) -> ok;
send_to_population({Population, Agents}) ->
    mas_population:add_agents(Population, Agents).
