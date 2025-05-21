-module(dispatcher).
-export([main/1]).

main(Args) ->
    ok = job_registry:start_link(),  
    case Args of
        [Device, <<"run">>, <<"--code">>, CodeStr] ->
            % Simulate job dispatching
            {ok, JobId} = wasm64_device:run(Device, CodeStr),
            io:format("~p~n", [{ok, JobId}]);

        [Device, <<"status">>, JobIdStr] ->
            {ok, Status} = wasm64_device:status(Device, JobIdStr),
            io:format("~p~n", [{ok, Status}]);

        _ ->
            usage()
    end.

usage() ->
    io:format("Usage:\n", []),
    io:format("  job_dispatcher wasm64@1.0 run --code \"add(2, 3)\"\n", []),
    io:format("  job_dispatcher wasm64@1.0 status <job_id>\n", []),
    halt(1).
