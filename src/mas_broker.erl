%%%-----------------------------------------------------------------------------
%%% @doc Monitores nodes. Handles agent migrations between nodes.
%% @end
%%%-----------------------------------------------------------------------------

-module(mas_broker).

-include("mas.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1,
         migrate_agents/1]).

%% Server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes    = []   :: [node()],
                config          :: mas:config()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

migrate_agents(Agents) ->
    gen_server:cast(?SERVER, {migrate_agents, Agents, node()}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Config) ->
    net_kernel:monitor_nodes(true),
    {ok, #state{nodes = discover_nodes(), config = Config}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({migrate_agents, Agents, From}, State) ->
    do_migrate_agents(Agents, From, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({nodeup, Node}, State) ->
    {noreply, add_node(Node, State)};
handle_info({nodedown, Node}, State) ->
    {noreply, remove_node(Node, State)};
handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
discover_nodes() ->
    net_adm:world().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_migrate_agents(Agents, From, #state{nodes = Nodes, config = Config}) ->
    case mas_topology:destination(Config#config.topology, From, Nodes) of
        {ok, Destination} ->
            spawn(Destination, mas_world, migrate_agents, [Agents]);
        no_destination ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_node(Node, State = #state{nodes = Nodes}) ->
    State#state{nodes = lists:usort([Node | Nodes])}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
remove_node(Node, State = #state{nodes = Nodes}) ->
    State#state{nodes = lists:usort(Nodes -- [Node])}.
