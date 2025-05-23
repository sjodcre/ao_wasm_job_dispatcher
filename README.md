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
│   ├── dispatcher.erl         # CLI interface
│   ├── wasm64_device.erl      # Device interface for jobs
│   ├── job_registry.erl       # In-memory job status tracker
│   └── job_worker.erl         # Simulates WASM execution
├── test/
│   └── wasm_job_mock_tests.erl # End-to-end system test
├── rebar.config
├── wasm_job_mock.app.src
└── README.md
```

---

## 🧑‍💻 CLI Usage

### Run a Job
```bash
./job_dispatcher wasm64@1.0 run --code "add(2,3)"
```

Example output:
```erlang
{ok, #{job_id => <<"job_123_456_789">>}}
```

### Check Job Status
```bash
./job_dispatcher wasm64@1.0 status <<"job_123_456_789">>
```

Example output:
```erlang
{ok, #{status => {completed, 5}}}
```

---

## 🔁 Flow Summary

1. **CLI user** runs a command like `./job_dispatcher wasm64@1.0 run --code "add(2,3)"`
2. `dispatcher:main/1` starts the job registry
3. Calls `wasm64_device:run/2`
4. Generates a `job_id` and registers job with state `[running]`
5. Spawns a new Erlang process
6. `job_worker:simulate/1` executes `"add(2,3)"` and returns `5`
7. Updates job state to `[completed]`
8. CLI later runs `status <job_id>`
9. `wasm64_device:status/2` queries `job_registry`
10. Returns `{completed, 5}` to user

---

## 🧪 Running Tests

To run the full end-to-end system test:
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