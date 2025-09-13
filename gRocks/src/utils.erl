-module(utils).
-export([parse_data/1, validate_data/1, compute_average/1, bind/2]).

%% Parse JSON binary to list of floats
-spec parse_data(binary()) -> {ok, list(float())} | {error, term()}.
parse_data(Json) ->
    try jsx:decode(Json) of
        List when is_list(List) ->
            case lists:all(fun is_number/1, List) of
                true -> {ok, [float(X) || X <- List]};
                false -> {error, not_all_numbers}
            end;
        _ -> {error, not_a_list}
    catch
        _:_ -> {error, parse_error}
    end.

%% Validate that all numbers are non-negative floats
-spec validate_data(list(float())) -> {ok, list(float())} | {error, term()}.
validate_data(Nums) ->
    Valid = lists:filter(fun(X) -> is_float(X) andalso X >= 0 end, Nums),
    case length(Valid) == length(Nums) of
        true -> {ok, Valid};
        false -> {error, invalid_numbers}
    end.

%% Compute average of list of floats
-spec compute_average(list(float())) -> {ok, float()} | {error, term()}.
compute_average([]) -> {error, empty_list};
compute_average(Nums) ->
    Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0.0, Nums),
    {ok, Sum / length(Nums)}.

%% Bind function for monad-like chaining
-spec bind({ok, A} | {error, E}, fun((A) -> {ok, B} | {error, E})) -> {ok, B} | {error, E}.
bind({ok, Value}, Fun) -> Fun(Value);
bind({error, Reason}, _Fun) -> {error, Reason}.