%%%-----------------------------------------------------------------------------
%%% @doc Configuration utilities.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mas_config).

-include("mas.hrl").

%%% API
-export([get_env/1,
         get_env/2,
         fetch_all/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Retrieves value from application environment, crashes if value is not
%% set.
%% @end
%%------------------------------------------------------------------------------
get_env(Key) ->
    element(2, {ok, _} = application:get_env(mas, Key)).

%%------------------------------------------------------------------------------
%% @doc Retrieves value from application environment, provides default value
%% if not set.
%% @end
%%------------------------------------------------------------------------------
get_env(Key, Default) ->
    case application:get_env(mas, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.

%%------------------------------------------------------------------------------
%% @doc Builds complete configuration record.
%% @end
%%------------------------------------------------------------------------------
fetch_all() ->
    #config{
        population_behaviour        = get_env(population_behaviour),
        simulation_mod              = get_env(simulation_mod),
        population_count            = get_env(population_count),
        population_size             = get_env(population_size),
        topology                    = get_env(topology),
        nodes_topology              = get_env(nodes_topology),
        migration_probability       = get_env(migration_probability),
        node_migration_probability  = get_env(node_migration_probability),
        write_interval              = get_env(write_interval),
        logs_dir                    = get_env(logs_dir)
    }.
