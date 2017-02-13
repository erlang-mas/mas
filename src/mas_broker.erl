%%%-----------------------------------------------------------------------------
%%% @doc Monitores nodes. Handles agent migrations between nodes.
%% @end
%%%-----------------------------------------------------------------------------

-module(mas_broker).

-behaviour(gen_server).

%% API
-export([start_link/0,
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
                topology = mesh :: atom()}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

migrate_agents(Agents) ->
    gen_server:cast(?SERVER, {migrate_agents, Agents, node()}).

%%%=============================================================================
%%% Server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_Args) ->
    net_kernel:monitor_nodes(true),
    {ok, #state{nodes    = discover_nodes(),
                topology = mas_config:get_env(nodes_topology)}}.

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
do_migrate_agents(Agents, From, #state{nodes = Nodes, topology = Topology}) ->
    case mas_topology:destination(Topology, From, Nodes) of
        {ok, Destination} ->
            spawn(Destination, mas_world, migrate_agents, [Agents]);
        {error, no_destination} ->
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
