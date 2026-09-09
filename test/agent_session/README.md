# Agent session tests

The suite is organized by behavior. `fixtures.ml` contains shared actor, identity,
native-tool and moderator construction helpers. Helpers used by only one area
stay beside those tests. Test modules depend on fixtures, not on other test modules.

| Area | Modules |
| --- | --- |
| Actor operation, workspaces, queues and quotas | `actor_tests.ml`, `infrastructure_tests.ml` |
| Durable state and publication | `state_tests.ml`, `invocation_tests.ml`, `persistence_tests.ml` |
| Jobs and follow-up scheduling | `jobs_tests.ml`, `follow_up_tests.ml` |
| Moderator ownership and lifecycle | `moderator_handoff_tests.ml`, `moderator_lifecycle_tests.ml` |
| Captured runtime construction and native/script integration | `runtime_builder_tests.ml` |
| Moderator queue delivery and observations | `event_delivery_tests.ml`, `observations_tests.ml` |
| Native calls and stream routing | `moderator_native_calls_tests.ml`, `moderator_routing_tests.ml` |
| Native result contracts and publication | `native_result_tests.ml` |
| Borrowed native execution and lifetime | `borrowed_execution_tests.ml` |
| One-off source and capability preparation | `one_off_preparation_tests.ml` |
| Persisted one-off execution and scoped file tools | `one_off_execution_tests.ml` |
| Submitted `run_chatml` requests and inherited policy | `run_chatml_tests.ml` |
| Registered one-off outcomes and runtime-request ownership | `run_chatml_registration_tests.ml`, `runtime_request_tests.ml` |
| Standalone ChatML handlers | `standalone_tests.ml` |
| Permission and compaction workflows | `permissions_tests.ml`, `compaction_tests.ml` |

Run the complete suite using the existing entrypoint:

```sh
dune build @test/runtest-agent_session_test
```

`dune runtest` also includes every module. Dune's inline-test runner partitions the
library by source file, so each module can run independently of the other tests.
Add new tests to the relevant module; add a new module to this directory's `dune`
file when a distinct area needs one. Preserve high-signal integration and expect
coverage rather than duplicating fixtures or testing trivial type guarantees.
