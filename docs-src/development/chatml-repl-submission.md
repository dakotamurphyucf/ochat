# Bounded ChatML REPL submission parsing

[`Chatml_parse.Submission`](../../lib/chatml/chatml_parse.mli) admits one immutable
captured source as a whole unit before any REPL typecheck or evaluation. The
caller supplies positive source-byte, token, parser/lexer work and identifier
site limits, plus a cancellation checkpoint. It returns one of:

- `Complete` with the existing program AST and source-bound identifier sites;
- `Need_more` at the exact captured physical EOF for an accepted unfinished
  parser prefix, nested comment or string;
- `Rejected` with a source-owned syntax diagnostic or a typed resource limit.

The lexer reports unfinished comments and strings as typed internal states.
For strings, the incremental parser makes a bounded, private string-token
category probe from the current real parser checkpoint. It stops at the first
shift or rejection, never resumes that hypothetical shift, and discards the
probe. This checks whether a string could occur here before reporting
`Need_more`; an unfinished
string after an already invalid prefix is syntax. An incomplete comment is
trivia after the preceding token has been accepted. The parser uses the
production grammar without appending closing tokens or publishing a partial
AST. A cut inside a multi-character token can be terminal syntax even when
adding bytes would have made another token: for example, a trailing `-` cut
from a later `->` is presently a minus token.

The existing `parse_program`, `parse_captured` and interface entrypoints keep
their batch behavior. `Submission` does not typecheck, execute, run dependency
initializers or mutate a REPL session. Its `Need_more` result does not commit a
typing namespace. The [developer runtime design](../design/chatml-developer-runtime.md)
defines the later session revision and sequential statement commit policy.

## Hosted process-local foundation

The optional `Daemon.Developer.Repl` service uses the daemon's installed
Developer source owner. A create request selects an authentic `One_off_v1`,
`Tool_v1` or `Developer_tool_v1` capture, retains its original authenticated
actor, and binds one typing owner and evaluator to that captured contract.
`Developer_tool_v1` additionally requires the configured Developer compiler
contract and current provider/native/child-work authority for operations such
as `Model.spawn`; selecting the target does not grant that authority. The
standalone `chatml-repl` frontend intentionally selects `One_off_v1`. A current
reconnected actor must have the same principal ID and authentication kind;
both actors and the selected source are checked again after waits. This source
selection admits no native or Task effect by itself. The selected Standard
surfaces have no `print` operation, so the service does not install a process
wide output sink.

Concurrent authenticated reads repeat the same source and actor validation
without replacing an active statement's admission proof. At its effect entry,
the statement still checks the current source owner and exact selected
contract. An accepted instance separately retains its original source selection.
Editing, replacing, refreshing or closing the authoring context does not
reinterpret or revoke that capture. A stale editor/candidate proof still
rejects; current physical root, source policy, upload pin/deadline and original
and current actor checks remain mandatory for the retained selection.

Each submission carries an opaque instance, epoch, expected revision and
idempotency key. The parser returns bounded `Need_more` or `Rejected` detail
without running a statement. A complete submission commits successful
statements in order. If a later statement fails, the result is `Stopped` with
the count of completed statements and any admitted bounded receipts; earlier
successes remain committed. `Output_limited` can omit receipt detail without
turning a finished or stopped operation into the other. A Task expression may
return a scoped inert handle, but submitting it never drives the Task.

The bounded per-instance key ledger retains a result or an expired-result
tombstone. Same-key retries with the original epoch, revision and payload
never re-execute; a changed request conflicts. A final key slot is reserved
for authenticated close after fresh work keys are exhausted. Cancel signals
the exact active callback and joins its finalization; reset joins work,
invalidates the epoch's Task handles and retains old-key replay evidence.
Reset rebuilds lexical/runtime state from the same original capture; selecting
changed project bytes or another environment requires a new Create.
Close joins work and releases the evaluator. Instances and handles expire on
daemon restart. The daemon joins its REPL callbacks before closing their
shared source owner.
