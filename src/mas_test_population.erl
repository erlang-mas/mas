-module(mas_test_population).

-behaviour(mas_population).

-export([initial_agent/0,
         behaviour_function/1,
         behaviours/0,
         meeting_function/1]).

-record(agent, {id     :: integer(),
                energy :: integer()}).

initial_agent() -> #agent{id=rand:uniform(100), energy=100}.

behaviour_function(Agent) -> fight.

behaviours() -> [fight, reproduce].

meeting_function({fight, Agents}) ->
    Agents;
meeting_function({reproduce, Agents}) ->
    Agents.
