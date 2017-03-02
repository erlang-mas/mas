-record(config, {population_mod             :: module(),
                 simulation_mod             :: module(),
                 population_count           :: integer(),
                 population_size            :: integer(),
                 topology                   :: atom(),
                 nodes_topology             :: atom(),
                 migration_probability      :: float(),
                 node_migration_probability :: float(),
                 write_interval             :: integer(),
                 logs_dir                   :: string()}).

 -record(simulation, {params     :: sim_params(),
                      time       :: pos_integer(),
                      subscriber :: pid()}).

-type config()     :: #config{}.
-type simulation() :: #simulation{}.
-type sim_params() :: any().
-type agent()      :: any().
-type behaviour()  :: atom().
-type topology()   :: atom().
