-module(mas_test_simulation).

-behaviour(mas_simulation).

-export([simulation_setup/1,
         simulation_teardown/1,
         simulation_result/2]).

simulation_setup(_SP) -> ok.

simulation_teardown(_SP) -> ok.

simulation_result(_SP, Agents) ->
    {Agents, length(Agents)}.
