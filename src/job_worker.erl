-module(job_worker).
-export([simulate/1, run_wasm_file/1]).

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

% run_wasm_file(FileName) ->
%     Port = open_port({spawn, "iwasm " ++ FileName}, [stream, exit_status]),
%     collect_output(Port, "").

run_wasm_file(FileName) ->
    io:format("Running WASM: ~s~n", [FileName]),
    % Port = open_port({spawn, "iwasm " ++ FileName}, [binary, stream, exit_status]),
    Port = open_port({spawn, "sh -c 'iwasm " ++ FileName ++ " 2>&1'"}, [binary, stream, exit_status]),
    collect_output(Port, []).

collect_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            io:format("Received chunk: ~p~n", [Data]),
            collect_output(Port, [Data | Acc]);
        {Port, {exit_status, 0}} ->
            Output = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
            Trimmed = string:trim(Output),
            io:format("Final Output: ~p~n", [Trimmed]),
            Trimmed;
        {Port, {exit_status, Code}} ->
            io:format("Non-zero exit: ~p~n", [Code]),
            {error, {exit_status, Code, iolist_to_binary(lists:reverse(Acc))}}
    after 3000 ->
        io:format("Timeout reached~n", []),
        {error, timeout}
    end.

% collect_output(Port, Acc) ->
%     receive
%         {Port, {data, Data}} ->
%             collect_output(Port, Acc ++ Data);
%         {Port, {exit_status, 0}} ->
%             string:trim(Acc);
%         {Port, {exit_status, Code}} ->
%             {error, {exit_status, Code, Acc}}
%     after 3000 ->
%         {error, timeout}
%     end.