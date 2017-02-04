%%%-------------------------------------------------------------------
%% @doc Configuration utilities.
%% @end
%%%-------------------------------------------------------------------

-module(mas_config).

%% API
-export([get_env/1, get_env/2]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Retrieves a value from the application environment, crashes
%% if value is not set.
%% @end
%%--------------------------------------------------------------------
get_env(Key) ->
    element(2, {ok, _} = application:get_env(mas, Key)).

%%--------------------------------------------------------------------
%% @doc Retrieves a value from the application environment, providing
%% the default value if not set.
%% @end
%%--------------------------------------------------------------------
get_env(Key, Default) ->
    case application:get_env(mas, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.
