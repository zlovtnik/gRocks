%%%-------------------------------------------------------------------
%% @doc gRocks public API
%% @end
%%%-------------------------------------------------------------------

-module(gRocks_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:set_primary_config(level, info),
    logger:info("Starting Cowboy server..."),
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/process">>, handlers, []}
        ]}
    ]),
    case cowboy:start_clear(my_http, [{port, 8080}], #{env => #{dispatch => Dispatch}}) of
        {ok, Pid} ->
            logger:info("Cowboy started successfully with PID: ~p", [Pid]);
        {error, Reason} ->
            logger:error("Failed to start Cowboy: ~p", [Reason])
    end,
    gRocks_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
