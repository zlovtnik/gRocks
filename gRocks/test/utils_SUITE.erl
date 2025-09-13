-module(utils_SUITE).
-include_lib("eunit/include/eunit.hrl").

parse_data_test() ->
    ?assertEqual({ok, [1.0, 2.0]}, utils:parse_data(<<"[1,2]">>)),
    ?assertEqual({error, not_a_list}, utils:parse_data(<<"{}">>)),
    ?assertEqual({error, parse_error}, utils:parse_data(<<"invalid">>)).

validate_data_test() ->
    ?assertEqual({ok, [1.0, 2.0]}, utils:validate_data([1.0, 2.0])),
    ?assertEqual({error, invalid_numbers}, utils:validate_data([1.0, -1.0])).

compute_average_test() ->
    ?assertEqual({ok, 2.0}, utils:compute_average([1.0, 2.0, 3.0])),
    ?assertEqual({error, empty_list}, utils:compute_average([])).

bind_test() ->
    ?assertEqual({ok, 2}, utils:bind({ok, 1}, fun(X) -> {ok, X + 1} end)),
    ?assertEqual({error, reason}, utils:bind({error, reason}, fun(_) -> {ok, 1} end)).