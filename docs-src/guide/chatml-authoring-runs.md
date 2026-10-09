# Scoped run decisions

The `Run` module belongs to the `moderator_v1` and
`delegated_moderator_v1` compiler surfaces. Its operations are transactional tasks:

| Operation | Input | Result |
| --- | --- | --- |
| `Run.continue()` | No arguments | `task unit` |
| `Run.wait(wake)` | JSON describing one exact authorized wake | `task unit` |
| `Run.finish(decision)` | JSON describing a finish decision | `task unit` |

These operations require an actor-owned callback scope. Merely compiling a script
against this surface or observing a run ID grants no authority. A runtime with no
installed run callback scope rejects them. A checked scope binds the current
principal, source installation, session generation, run revision and actual
moderator execution.

Each call stages a private native receipt. The script receives unit, rather than
the receipt or permission to schedule work. `Task.catch` rollback discards only the
owning callback's staged decisions. A discarded decision cannot later commit.
Identical surviving decisions coalesce; incompatible decisions reject preparation.
Unrelated effects retain their order. Preparation seals the selected decision;
failed or repeated preparation invalidates it. Rollback after preparation and
callback release also invalidate the owning selection. An unknown rollback receipt
cannot invalidate another callback's prepared decision.

`Run.continue` expresses one continuation decision. It does not run an inference,
restart a session or create a second executor. Host admission consumes an accepted
continuation through the session's existing scheduling boundary and limits.

`Run.wait` names a finite actual occurrence. Its JSON contains `run_id`, the
captured `source`, and `occurrence`. The occurrence is an exact job attempt,
delivered timer count with its creator and optional subscription epoch, or exact
subscription delivery ID with epoch and creator. A bare schedule or subscription
ID does not identify a wake. Replacing a source, advancing an attempt or epoch, or
using another callback's source cannot make an old wake current authority.

`Run.finish` takes a JSON run action with `kind` equal to `finish`, a `terminal`
decision, and `relinquish`, a bounded array of exact owned work occurrences. The
terminal kinds are `completed`, `failed`, `cancelled`, `limited` and `interrupted`.
Completed may carry an authorized bounded result reference. A failed job result
cannot represent a successful completion. Failure, cancellation, interruption and
uncertain external delivery evidence must remain truthful and retained.

A finish decision is distinct from a terminal receipt. The host requires owned
work to be settled or explicitly transferred under its checked relinquishment
policy. In particular, a `Turn_end` callback cannot manufacture completion while
its actual root operation is still being joined. Pending finish disposition and
subsequent actual completion remain separate durable facts. Finishing a run does
not stop the session or terminate unrelated jobs. Existing explicit session end
requests retain their session semantics; incompatible continuation and finish
requests must be resolved at the owning checkpoint boundary.

Native staging and rollback invariants are exercised by
`test/agent_session/run_action_scope_tests.ml`; the bounded domains and immutable
outcome evidence are exercised by `test/run_lifecycle_protocol_test.ml`. The native
adapters are `lib/chat_response/run_operations.ml` and
`lib/agent_session/run_action_service.ml`. These references describe the staging
contract; actor admission and completion require the corresponding host-owned
integration and its real callback tests.
