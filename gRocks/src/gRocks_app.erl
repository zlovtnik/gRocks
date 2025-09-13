%%%-------------------------------------------------------------------
%% @doc gRocks public API
%% @end
%%%-------------------------------------------------------------------

-module(gRocks_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/process", handlers, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    gRocks_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
