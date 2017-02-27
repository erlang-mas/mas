-module(mas_test_simulation).

-behaviour(mas_simulation).

-export([simulation_setup/1,
         simulation_teardown/1,
         simulation_result/2]).

simulation_setup(_SP) ->
    io:format("Simulation setup~n", []).

simulation_teardown(_SP) ->
    io:format("Simulation teardown~n", []).

simulation_result(_SP, Agents) ->
    io:format("Simulation result~n", []),
    Agents.
