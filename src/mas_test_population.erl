-module(mas_test_population).

-behaviour(mas_population).

-export([initial_agent/0,
         behaviour_function/1,
         behaviours/0,
         meeting_function/1]).

-record(agent, {energy=100}).

initial_agent() -> #agent{}.

behaviour_function(Agent) -> fight.

behaviours() -> [fight, reproduce].

meeting_function({fight, Agents}) ->
    Agents;
meeting_function({reproduce, Agents}) ->
    Agents.
