-module(dispatcher).
-export([main/1]).

main(Args) ->
    ok = job_registry:start_link(),  
    case Args of
        % [Device, <<"run">>, <<"--code">>, CodeStr] ->
        %     % Simulate job dispatching
        %     {ok, JobId} = wasm64_device:run(Device, CodeStr),
        %     io:format("~p~n", [{ok, JobId}]);

        % [Device, <<"status">>, JobIdStr] ->
        %     {ok, Status} = wasm64_device:status(Device, JobIdStr),
        %     io:format("~p~n", [{ok, Status}]);

        [DeviceStr, "run", "--code", CodeStrStr] ->
            Device = list_to_binary(DeviceStr),
            CodeStr = list_to_binary(CodeStrStr),
            {ok, JobId} = wasm64_device:run(Device, CodeStr),
            io:format("~p~n", [{ok, JobId}]);

        [DeviceStr, "status", JobIdStr] ->
            Device = list_to_binary(DeviceStr),
            JobId = list_to_binary(JobIdStr),
            {ok, Status} = wasm64_device:status(Device, JobId),
            io:format("~p~n", [{ok, Status}]);
        _ ->
            usage()
    end.

usage() ->
    io:format("Usage:\n", []),
    io:format("  job_dispatcher wasm64@1.0 run --code \"add(2, 3)\"\n", []),
    io:format("  job_dispatcher wasm64@1.0 status <job_id>\n", []),
    halt(1).
