 -record(simulation, {params     :: sim_params(),
                      time       :: pos_integer(),
                      subscriber :: pid()}).

-type simulation() :: #simulation{}.
-type sim_params() :: any().
-type agent()      :: any().
-type behaviour()  :: atom().
-type topology()   :: atom().
-type counter()    :: dict:dict(term(), integer()).
-type metric()     :: list().
