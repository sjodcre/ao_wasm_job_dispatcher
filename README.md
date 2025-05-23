# wasm_job_mock

🧪 Simulated HyperBEAM-style WASM Job Dispatcher in Erlang

This project mocks the behavior of AO's `~wasm64@1.0` devices and HyperBEAM by simulating job dispatch, processing, and result retrieval using Erlang processes and ETS.

---

## 🚀 Features

- Simulated WASM job execution (e.g., `add(2,3)`)
- Concurrent job processing via `spawn`
- Job state tracking using ETS
- CLI interface for running and checking jobs
- EUnit-tested end-to-end

---

## 🧱 Project Structure

```
wasm_job_mock/
├── src/
│   ├── dispatcher.erl         # CLI entrypoint (escript main)
│   ├── wasm64_device.erl      # Device logic (runs and tracks jobs)
│   ├── job_registry.erl       # In-memory ETS job registry
│   └── job_worker.erl         # Simulates WASM-like string-based job eval
├── test/
│   └── wasm_job_mock_tests.erl # Full test coverage
├── rebar.config                # Uses escript_main_module = dispatcher
├── dispatcher.app.src          # Application config
└── README.md
```

---

## 🧑‍💻 CLI Usage

### Run a Job
```bash
./_build/default/bin/job_dispatcher wasm64@1.0 run --code "add(2,3)"
```

Example output:
```erlang
{ok, #{job_id => <<"job_1748_18030_707690">>, result => 5}}
```

Or (if using async version):
```erlang
{ok, #{job_id => <<"job_1748_18030_707690">>}}
```

### Check Job Status
```bash
./_build/default/bin/job_dispatcher wasm64@1.0 status job_1748_18030_707690
```

Example output:
```erlang
{ok, #{status => {completed, 5}}}
```

---

## 🔁 Flow Summary

1. CLI command invokes `dispatcher:main/1`
2. CLI args (`string`) are converted to binaries
3. Starts job registry ETS table
4. Dispatches job via `wasm64_device:run/2`
5. Generates `job_id` and inserts into ETS with state `running`
6. Spawns a process to call `job_worker:simulate/1`
7. Evaluates `"add(2,3)"` and computes result `5`
8. Updates ETS state to `{completed, 5}`
9. User may query job status via CLI
10. Job result retrieved from ETS by `job_id`

---

## 🧪 Running Tests

To run the full test suite:
```bash
rebar3 eunit
```

Test file:
- `test/wasm_job_mock_tests.erl`

---

## 📚 Concepts Demonstrated

| Concept                 | HyperBEAM Equivalent               |
|-------------------------|------------------------------------|
| `spawn`, `receive`      | Actor model job execution          |
| `ETS` state table       | AO device state (`state.json`)     |
| CLI → device interface  | AO job routing                     |
| Mocked WASM eval        | Placeholder for real WASM runner   |

---

## ✅ Requirements

- Erlang/OTP 22+
- `rebar3` installed
