-module(wasm_dispatcher).
-behaviour(gen_server).

%% API
-export([start_link/0, execute/2, get_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    jobs = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

execute(ModuleBinary, FunctionName) ->
    gen_server:call(?MODULE, {execute, ModuleBinary, FunctionName}).

get_status(JobId) ->
    gen_server:call(?MODULE, {get_status, JobId}).

init([]) ->
    {ok, #state{}}.

handle_call({execute, ModuleBinary, FunctionName}, _From, State) ->
    JobId = make_ref(),
    %% Spawn a new process to execute the WASM module
    Pid = spawn(fun() ->
        Result = wasm_executor:run(ModuleBinary, FunctionName),
        %% Notify dispatcher of completion
        ?MODULE ! {job_complete, JobId, Result}
    end),
    NewJobs = State#state.jobs#{JobId => {Pid, running}},
    {reply, {ok, JobId}, State#state{jobs = NewJobs}};

handle_call({get_status, JobId}, _From, State) ->
    case maps:get(JobId, State#state.jobs, undefined) of
        {_, Status} ->
            {reply, {ok, Status}, State};
        undefined ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({job_complete, JobId, Result}, State) ->
    %% Update job status
    NewJobs = maps:update(JobId, fun({Pid, _}) -> {Pid, {completed, Result}} end, State#state.jobs),
    {noreply, State#state{jobs = NewJobs}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
