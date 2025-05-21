-module(wasm64_device).
-export([run/2, status/2]).

run(Device, CodeStr) ->
    JobId = job_registry:generate_job_id(),
    job_registry:register_job(JobId, running),
    spawn(fun() ->
        Result = job_worker:simulate(CodeStr),
        job_registry:update_job(JobId, {completed, Result})
    end),
    {ok, #{job_id => JobId}}.

status(_Device, JobId) ->
    case job_registry:lookup(JobId) of
        {ok, Status} -> {ok, #{status => Status}};
        error -> {error, not_found}
    end.
