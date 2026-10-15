# Developer capture and deterministic fixture tests (OCH-265)

`Chatml_developer_capture` freezes an already admitted project build or prepared
single captured source, its authentic standalone target, entrypoint, exact JSON
input, source provenance and optional fixture suite. Its graph fingerprint comes
from the prepared inventory, which binds the selected contract, environment,
source IDs and digests. The capture fingerprint also binds the provenance label,
entrypoint, input and fixture digest. `single_captured` retains the original
source identity and bytes under an implicit environment; the old `single_file`
constructor keeps its fixed synthetic ID through the same preparation algorithm.
An embedded script origin binds the current document coordinates, genuine parsed
input and declaration range, and exact selected script source as bounded
comparison data. Capture admission requires that script to be the prepared
entry, not merely another source in the graph. The origin does not grant access
to an editor buffer or native source.
Local saved revisions, editor versions and upload receipt IDs are comparison
data, not authorizations. The host must obtain the build or source and provenance
through its current actor/source or bundle admission; a capture never saves a
file or reads a path. This core runs One_off_v1. Tool_v1
needs a host-owned two-argument tool context and outcome contract, so it rejects
explicitly until that separate admitted developer fixture path is implemented.
Moderator/session targets also reject before execution.

Every fixture step names one `Tool.call`, has bounded compiled input and output
schemas, an exact expected JSON input, and a bounded response. Every step is
required in order. An empty suite is valid for a pure test, but any `Tool.call`
then fails. Input/schema/result trees have ceilings of 1 MiB, 128 levels and
100,000 nodes each; fixture suites have ceilings of 4,096 steps and 16 MiB before
a digest is computed. Editor provenance has a 4,096-version ceiling. Callback
exceptions, including `Exit`, propagate unchanged from admission.
Fixture input matching uses the same finite floating-point number representation
as ChatML `Json.t`, so spellings such as `1` and `1.0` match after import/export.
Both the authored and converted input/response must satisfy their schemas;
precision changes that violate a declared enum reject before capture.
Out-of-range or nonfinite numbers and numeric lexemes above 4,096 bytes reject
before capture. JSON object order and all other JSON structure still match exactly.
Fixtures install no live tool, model, process, job or scheduling capability.

`Chatml_developer_test.Check.perform` compiles the captured graph in the
existing joined compiler worker. It neither runs initializers nor calls a
fixture. A successful check privately retains its frozen linked artifact and
the exact capture object. A rejected check exposes only a bounded diagnostic;
a source location appears only after it is verified against the captured
inventory. The test runner accepts only that same capture object with a fixture
suite. Host-issued check and run IDs are separate from conversation Run and Job
IDs; the later host service owns uniqueness, authorization and idempotency.

The test runner uses one bounded `Chatml_execution` scope for input import,
linked initialization, fixture calls and result export. `Tool.call` must match
the next fixture. Pure language evaluation, Task composition and bounded local
`Log.*` events are allowed; the operation registry excludes other host effects.
The existing execution control observes every attempted effect before dispatch, so a script
cannot turn an unexpected fixture or unsupported effect into a successful test
by catching its task error. A successful task must also consume all required
steps. Each run creates a fresh linked instance; repeating the same capture is
a new execution with a new host-issued run ID. Log text and the terminal JSON or
diagnostic share one retained output cap. Result JSON has the same 1 MiB
structural ceiling and is measured within the active runtime scope before any
serialization. A discarded success result does not consume the failure receipt's
output allowance. Failures retain bounded code/message and verified source coordinates
only, never mutable runtime values or the linked
artifact. Compilation failure returns a failed run receipt with zero runtime
calls or logs. Cancellation and unexpected exceptions propagate through the
existing compiler/runtime cleanup boundaries.

OCH-266 will attach this core to a developer host owner, task scheduler,
permission checks and durable receipts. OCH-263/264 will supply admitted upload
receipts and complete changed-closure validation. This core does not add a
resolver, writable host root or live capability fallback.
