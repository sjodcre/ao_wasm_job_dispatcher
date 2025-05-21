-module(job_worker).
-export([simulate/1]).

simulate(CodeStr) ->
    timer:sleep(500),
    simulate_eval(CodeStr).

simulate_eval(CodeStr) when is_binary(CodeStr) ->
    case binary:match(CodeStr, <<"add(">>) of
        {0, _} ->
            parse_add_args(CodeStr);
        nomatch ->
            error(unsupported_code)
    end.

parse_add_args(<<"add(", Rest/binary>>) ->
    case binary:split(Rest, <<")">>) of
        [ArgsBin, _] ->
            case binary:split(ArgsBin, <<",">>, [global]) of
                [A, B] ->
                    A1 = binary_to_integer(trim_bin(A)),
                    B1 = binary_to_integer(trim_bin(B)),
                    A1 + B1;
                _ -> error(bad_args)
            end;
        _ -> error(bad_syntax)
    end.

trim_bin(Bin) when is_binary(Bin) ->
    list_to_binary(string:trim(binary_to_list(Bin))).