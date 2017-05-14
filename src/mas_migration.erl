%%%-----------------------------------------------------------------------------
%%% @doc Agent migrations API.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_migration).

%%% API
-export([migrate_agents/1,
         migrate_to_populations/2,
         migrate_to_worlds/3,
         migrate_back/2]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Called by population. Initiates migration process by sending group of
%%      agents to migration dispatcher. Migration dispatcher based on configured
%%      probability delegates agents to either population broker or world
%%      broker. Brokers conduct actual migration of agents.
%% @end
%%------------------------------------------------------------------------------
migrate_agents(Agents) ->
    Source = {node(), self()},
    mas_migration_disp:migrate_agents(Agents, Source).

%%------------------------------------------------------------------------------
%% @doc Called by migration broker. Conducts actual migration of agents between
%%      populations. Partitions provided agents list into number of chunks equal
%%      to number of possible destinations. Sends each chunk to different
%%      population.
%% @end
%%------------------------------------------------------------------------------
migrate_to_populations(Populations, Agents) ->
    Migrations = prepare_migrations(Populations, Agents),
    [migrate_to_population(Migration) || Migration <- Migrations].

%%------------------------------------------------------------------------------
%% @doc Called by migration broker. Conducts actual migration of agents between
%%      worlds residing on different nodes. Partitions provided agents list into
%%      number of chunks equal to number of possible destinations. Sends each
%%      chunk to different node.
%% @end
%%------------------------------------------------------------------------------
migrate_to_worlds(Nodes, Agents, Source) ->
    Migrations = prepare_migrations(Nodes, Agents),
    [migrate_to_world(Source, Migration) || Migration <- Migrations].

%%------------------------------------------------------------------------------
%% @doc Migrates agents back to population that initiated migration process.
%% @end
%%------------------------------------------------------------------------------
migrate_back(Population, Agents) ->
    Migration = {Population, Agents},
    migrate_to_population(Migration).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
prepare_migrations(Destinations, Agents) ->
    AgentGroups = mas_utils:partition(Agents, length(Destinations)),
    lists:zip(mas_utils:shuffle(Destinations), AgentGroups).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
migrate_to_world(_Source, {_Node, []}) -> ok;
migrate_to_world(Source, {Node, Agents}) ->
    mas_world:put_agents(Node, Agents, Source).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
migrate_to_population({_Population, []}) -> ok;
migrate_to_population({Population, Agents}) ->
    mas_population:add_agents(Population, Agents).
