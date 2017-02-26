-record(config, {population_behaviour       :: module(),
                 population_count           :: integer(),
                 population_size            :: integer(),
                 topology                   :: atom(),
                 nodes_topology             :: atom(),
                 migration_probability      :: float(),
                 node_migration_probability :: float(),
                 write_interval             :: integer(),
                 logs_dir                   :: string()}).

-type config()   :: #config{}.
-type topology() :: atom().
