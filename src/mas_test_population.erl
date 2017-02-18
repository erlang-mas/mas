-module(mas_test_population).

-behaviour(mas_population).

-export([sim_params/0,
         initial_agent/1,
         behaviours/0,
         behaviour/2,
         meeting/2]).

-record(agent, {id     :: integer(),
                energy :: integer()}).

sim_params() ->
    ok.

initial_agent(_SimParams) -> #agent{id=rand:uniform(100), energy=100}.

behaviours() -> [fight, reproduce].

behaviour(Agent, _SimParams) ->
    case rand:uniform() of
        R when R < 0.3  -> fight;
        R when R >= 0.3 -> reproduce
    end.

meeting({fight, Agents}, _SimParams) ->
    Agents;
meeting({reproduce, Agents}, _SimParams) ->
    Agents.
