-module(job_registry).
-export([start_link/0, generate_job_id/0, register_job/2, update_job/2, lookup/1]).

-define(TABLE, job_registry_table).

start_link() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set]),
            ok;
        _ ->
            ok
    end.

generate_job_id() ->
    {Mega, Sec, Micro} = os:timestamp(),
    list_to_binary(io_lib:format("job_~p_~p_~p", [Mega, Sec, Micro])).

register_job(JobId, State) ->
    ets:insert(?TABLE, {JobId, State}),
    ok.

update_job(JobId, NewState) ->
    ets:insert(?TABLE, {JobId, NewState}),
    ok.

lookup(JobId) ->
    case ets:lookup(?TABLE, JobId) of
        [{_, Status}] -> {ok, Status};
        [] -> error
    end.
