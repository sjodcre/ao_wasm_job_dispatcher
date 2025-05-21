-module(wasm_job_mock_tests).
-include_lib("eunit/include/eunit.hrl").

% end_to_end_run_status_test_() ->
%     %% Setup: ensure registry is ready
%     ok = job_registry:start_link(),

%     %% Step 1: Run a WASM job
%     {ok, #{job_id := JobId}} = wasm64_device:run(<<"wasm64@1.0">>, <<"add(2,3)">>),

%     %% Step 2: Wait for worker to simulate job
%     timer:sleep(600),

%     %% Step 3: Query status of the job
%     {ok, #{status := {completed, Result}}} = wasm64_device:status(<<"wasm64@1.0">>, JobId),

%     %% Step 4: Assert it ran correctly
%     ?_assertEqual(5, Result).

wait_for_result(JobId, Tries) when Tries > 0 ->
    case wasm64_device:status(<<"wasm64@1.0">>, JobId) of
        {ok, #{status := {completed, Result}}} -> Result;
        _ ->
            timer:sleep(100),
            wait_for_result(JobId, Tries - 1)
    end.

end_to_end_run_status_test_() ->
    ok = job_registry:start_link(),
    {ok, #{job_id := JobId}} = wasm64_device:run(<<"wasm64@1.0">>, <<"add(2,3)">>),
    Result = wait_for_result(JobId, 10),
    ?_assertEqual(5, Result).
