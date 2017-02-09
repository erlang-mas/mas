-module(mas_test_population).

-behaviour(mas_population).

-export([initial_agent/0,
         behaviours/0,
         behaviour/1,
         meeting/1]).

-record(agent, {id     :: integer(),
                energy :: integer()}).

initial_agent() -> #agent{id=rand:uniform(100), energy=100}.

behaviours() -> [fight, reproduce].

behaviour(Agent) -> fight.

meeting({fight, Agents}) ->
    Agents;
meeting({reproduce, Agents}) ->
    Agents.
