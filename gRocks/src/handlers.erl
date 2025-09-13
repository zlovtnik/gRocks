-module(handlers).
-behaviour(cowboy_handler).
-export([init/2, terminate/3]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Result = utils:bind(utils:parse_data(Body), fun(Nums) ->
                utils:bind(utils:validate_data(Nums), fun(Valid) ->
                    utils:compute_average(Valid)
                end)
            end),
            case Result of
                {ok, Avg} ->
                    Json = jsx:encode(#{<<"average">> => Avg}),
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req2),
                    {ok, Req3, State};
                {error, Reason} ->
                    Json = jsx:encode(#{<<"error">> => atom_to_binary(Reason, utf8)}),
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Json, Req2),
                    {ok, Req3, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(405, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.