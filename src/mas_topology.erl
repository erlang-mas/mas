%%%-----------------------------------------------------------------------------
%% @doc
%% Calculates destination of agent migration based on given topology.
%% @end
%%%-----------------------------------------------------------------------------

-module(mas_topology).

%% API
-export([calculate_destination/3]).

%%==============================================================================
%%% API functions
%%==============================================================================

calculate_destination(mesh, From, Nodes) ->
    Destinations = [Node || Node <- Nodes, Node =/= From],
    mas_utils:sample(Destinations).
