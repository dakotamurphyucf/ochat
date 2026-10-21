# Authoring and execution services

Ochat exposes reusable services for editors and developer clients: capture a
Project or ChatMD document, inspect it without running code, explicitly save or
upload it, then Check or execute that exact revision. This reference explains
how to compose those services. For a terminal you can use immediately, start
with the [ChatML REPL walkthrough](chatml-development-workflow.md).

These APIs are shipped; a desktop IDE, completion popup and embedded chat-TUI
REPL panel are future consumers. The standalone `chatml-repl` frontend provides
multiline input, highlighting, type inspection and explicit Task execution.
It does not expose all the editor services described here.

## Choose the operation you need

| Goal | Service | Runs authored code? |
| --- | --- | --- |
| Read captured sources or keep an unsaved document | Authoring context/source/document methods | No. |
| Find diagnostics, types, visible names or definitions | Analyze, Prefix and Query | No initializers, tools or model calls. |
| Highlight text or suggest markup, references and paths | Lexical and Assistance | No authored execution or implicit dependency acquisition. |
| Check a selected executable revision | Developer Check | Compiles/checks without evaluating initializers. |
| Exercise deterministic configured test steps | Developer Fixture | Uses the admitted fixture runner; not a Live run. |
| Run with the selected host tools, prompt and workspace | Developer Live | Yes, after current execution permission checks. |
| Explore persistent interactive bindings | Developer REPL | Successful submitted statements evaluate; returned Tasks stay inert until explicitly driven. |
| Inspect bounded execution observations | Developer Trace | Read-only; never replays execution. |

Analysis and Developer Check answer different questions. Analyze produces facts
for an editor's exact context and static environment. Check validates a selected
Developer execution capture and target. Passing either one grants no execution
or source-disclosure permission. Fixture success is not evidence that a provider
or native tool ran. Use the returned receipt's mode, outcome and
`execution_started` information to distinguish preparation from real execution.

## 1. Negotiate the installed services

Initialize the connection before other methods, requesting only the features
your client uses. Discover actual methods and finite bounds with
`authoring.capabilities`, `developer.capabilities`,
`developer.repl.capabilities` or `developer.trace.capabilities` as appropriate.
The [protocol reference](../agent-server/protocol.md#method-reference) lists
method names; the [type and codec reference](../agent-server/protocol-types.md)
defines exact request/result shapes. A type name in the table is not a JSON
object you can send verbatim: use its linked encoder or the typed client.

Authoring features are independent negotiated capabilities:

- `authoring.core.v1` owns contexts, captured sources and document overlays.
- `authoring.semantic.v1` adds compiler-backed analysis and queries; it needs core.
- `authoring.lexical.v1` adds bounded source highlighting; it needs core.
- `authoring.assistance.v1` adds markup and source-backed suggestions; it needs core.
- `authoring.bundle.v1` supports immutable uploads and can be installed alone.

The default document service can provide local markup help without an installed
semantic handler. `method_not_found` means the method was not installed or
selected. A semantic result of `Unavailable` instead means that the installed
service cannot provide the requested semantic evidence, for example because no
authentic selected script Build exists. Do not display either as an empty set
of diagnostics or a successful check.

Native source reads need configured roots and explicit read/selection grants.
Developer methods additionally need exact source/target/principal policy and
method scopes: `developer.check`, `developer.fixture.run`,
`developer.live.run` and `developer.repl` as applicable. Provider, native-tool,
Shell approval, prompt/workspace and result-read permission remain separate.
See [host configuration](../agent-server/configuration.md) for installation.

## 2. Open the revision you intend to inspect

For a host Project, enumerate authorized roots and members, then open the exact
Project/entry/environment selection. For caller-supplied ChatMD, open the
provided document and select a real parser-assigned script ID before requesting
script semantics. Source-only and Project-backed scripts retain their actual
selected compiler contract; a document name alone does not select a target.

Keep the returned context identity, `Expected` generation, source IDs and
digests. A context is an immutable captured view, not a live filesystem window.
`authoring.source.list` and `authoring.source.read` refer to that view;
`authoring.host.list` and `authoring.host.read` perform fresh authorized host
reads. Do not substitute a fresh host read for the bytes behind an old result.

Replace an editor document with `authoring.document.replace` using its exact
version and validated full UTF-8 text. Closing an overlay reverts it under the
same version rules. Neither action writes a file. A stale generation/version
must conflict: refresh the context and reconcile the user's edits before
issuing a new mutation. Never silently apply an older diagnostic or edit to a
newer document.

## 3. Analyze, then ask source-bound questions

Call `authoring.analysis` for the current Project or selected ChatMD script.
Retain its snapshot/evidence identity with every query. The real compiler,
selected environment, module exports and host signatures determine facts.
Queries do not rerun a different checker or silently recapture imports.

| Evidence | What a reader can rely on |
| --- | --- |
| Complete / certified | A complete successful check of the selected captured graph; semantic hover, types, signatures, visible declarations/modules/imports and definition navigation may be available. |
| Failed / syntax-only | Diagnostics and parsed syntax from that failed attempt. It does not certify semantic facts for a successfully parsed part of a failed graph. |
| Provisional prefix | A separately identified, parser-confirmed complete top-level prefix checked by the real compiler under the captured environment. It does not make the rest of the file valid. |
| Unavailable | No supported evidence for that request; show the reason. |

For incomplete implementation input, request `authoring.prefix` and use
`authoring.query` with that exact returned evidence. Companion `.chatmli`
interfaces are checked as part of the captured graph and constrain public
signatures, but provisional prefix analysis of an interface source is
unsupported; it returns `Unsupported_source_phase` rather than inferred facts. A prefix is not created by inserting imaginary
closing tokens. An unfinished local function scope can have syntax suggestions
while its inferred local types, hover or navigation remain unavailable. The
full-document result still owns diagnostics. Editing does not implicitly fetch
new imports; explicitly Analyze or refresh when the captured dependency view
needs to change.

Navigation follows source ownership and public signatures. A definition can be
in a captured dependency or older source; preserve its ID/digest rather than
opening current disk text at a guessed offset. Completion results from a
lexical fallback must stay labeled syntax/provisional rather than appearing as
compiler-certified names.

### Display diagnostics at the correct position

Native sites use the source ID, digest and a half-open byte range in the exact
captured bytes. A Unicode character can occupy several UTF-8 bytes; byte offsets
are not UTF-16 editor columns. Use the source-bound conversion/projection data
and the negotiated position encoding instead of adding offsets to the current
buffer.

For embedded ChatMD scripts, retain both the native ChatML site and its authored
projection when one exists. An external `src` body keeps that external source's
owner. Generated, imported or discontinuous text may have no exact authored
projection; show the native site and qualified related context. An approximate
projection must remain visibly approximate. Diagnostic clipping and omitted
related sites are explicit output limits, not proof that there are no other
errors.

## 4. Offer local assistance without executing the document

`authoring.lexical` returns shared source-bound language roles, open constructs
and scanned-prefix status for ChatML or authored/effective ChatMD. Its byte
ranges describe highlighting, not a successful compile. The terminal and other
frontends can render the same language roles with their own theme and layout.
Malformed or incomplete input can still have useful lexical spans.

`authoring.assistance` adds parser-backed tags, attributes and scope-sensitive
values. It can suggest actual captured references, configured paths, tools,
Projects and environments when the corresponding source/host evidence exists.
An unknown catalog, inaccessible path or ambiguous origin is different from an
empty result. Helpful names for permissive declarations are suggestions, not
an exhaustive validator.

Checked ChatML templates use the real compiler and selected target/environment.
Apply only their returned replacement range against the original version.
Some incomplete or external-source positions lack the exact reverse mapping
needed for a checked replacement and therefore cannot offer one. These queries
do not preprocess prompts, invoke scripts/tools/models, run initializers,
fetch Git/HTTP dependencies, or install packages.

## 5. Save locally or publish an immutable bundle revision

Local save belongs to the editor's filesystem owner:

1. Save the intended editor bytes explicitly.
2. Refresh the Authoring context against its current generation.
3. Analyze the returned revision and use that new identity for later requests.

For a remote source closure, use the bundle methods:

1. Begin an upload with its bounded manifest and, when required, exact parent.
2. Send exact member chunks at their expected offsets.
3. Commit with the explicit idempotency identity, then read Status until its
   validation/publication outcome is known.
4. Open or rebind to the published immutable revision under the installed policy.

Upload write, publication, bundle read and rebind are independent grants.
Acceptance of a commit can precede publication; do not execute it merely because
a commit request returned. Retrying an uncertain chunk/commit uses its original
identity and bytes. Changed bytes publish a new revision; they do not rewrite an
old bundle or an already admitted REPL capture. A bundle ID or digest is an
identity, not a permission token. See [host upload configuration](../agent-server/configuration.md)
and the [bundle method contracts](../agent-server/protocol.md#method-reference).

## 6. Check, choose Fixture or Live, then observe

Select the exact source, Project/environment, target and bounded input from the
revision you reviewed. Submit a Developer command with a fresh idempotency key.
Check evaluates no initializers. Fixture uses admitted deterministic steps;
Live may invoke real native/managed tools, child Agents and model work only
under the installed runtime policies. Neither a successful Check nor an imported
module grants these effects. `Model.spawn` additionally needs its selected
Developer compiler/tool contract and provider authority; the standalone
`One_off_v1` REPL does not become a model-enabled host by naming it. The
[host orchestration guide](../agent-server/chatml-orchestration.md) explains
managed tools, child Agents, retained jobs and provider-backed work.

Read the original command's immutable receipt through Observe. Keep its mode,
exact capture/selection, admission outcome, execution-started flag, diagnostics
and terminal disposition. Shared finite scheduler/capacity limits can deny or
delay work; multiple clients do not create independent execution capacity.
Root/native/child work stays owned by its admitted run and current permissions.
Nested jobs inherit the relevant selected source/runtime limits, including their
original runtime deadline; polling does not restart an execution budget.

Results and Blob reads have their own disclosure and retention rules. An expired
or undisclosable result is not an empty successful result. Durable command
recovery reconciles the original record; it does not replay external effects.
Cancellation signals the owned work and joins cleanup. A cancelled or failed
receipt cannot promise that earlier external effects were undone.

### Recover after a disconnect or notification gap

Retain the original key and exact request before transport handoff. If a reply
is lost, Observe that key; an absent observation does not prove it never ran.
An explicit retry repeats the original key and payload. A new intentional run
uses a new key. Changed payload under an existing key must conflict.

Register Watch, then Observe immediately after its acknowledgement for the
baseline. Lifecycle hints and `Resync_required` are bounded process-local
invalidations: Observe the exact watched key after either one. Re-registering
Watch is unnecessary for a hint; if you refresh the registration, always Observe
after that acknowledgement too. Registration itself does not manufacture a
resync event. Reconnect establishes a new authorized watch and baseline;
notifications are not a durable event log. Refresh approval lists separately
under their own permission.

A long `Await_terminal` occupies an ordinary request lane. Use Watch/Observe
or the client's supported control lane/connection when cancellation must remain
responsive. Do not treat a cancelled read as cancellation of the command.
REPL instance/epoch/submission keys and Task run/keys remain separate from
ordinary Developer command identities.

## 7. Use the REPL and inspect an optional trace

Create a REPL on that same admitted capture, submit a value or Task, and drive
the Task explicitly. Reset joins work and changes the epoch while retaining
the original source capture; create a new instance for different bytes or an
environment change. See the [walkthrough](chatml-development-workflow.md) and
[submission reference](../development/chatml-repl-submission.md) for partial
commit, old closures, key replay and process-local lifetime rules.

If recording is installed and enabled, page the exact Command, REPL statement
or Task locator through `developer.trace.page`. Match its execution ID and
capture fingerprints to the receipt before interpreting source sites. Traces
can be unavailable, incomplete, expired or truncated; they contain no source
text or runtime values. Read [bounded execution tracing](../design/chatml-execution-tracing.md)
for enablement, paging, source permission, redaction and terminal truth.

The repository's offline joined workflow exercises one authored edit/save or
upload through Analyze/Check, Fixture/Live, REPL/Task and trace correlation over
configured HTTP and local stdio. From a configured development checkout:

```sh
env -u OPENAI_API_KEY -u OPENAI_KEY -u ANTHROPIC_API_KEY \
  -u GEMINI_API_KEY -u GOOGLE_API_KEY dune build \
  @test/agent_server_e2e/agent-e2e-developer-workflow
```

This is an integration qualification command, not a CLI for submitting your
own program. Its provider-free results do not claim a live provider request,
a desktop UI or a full matrix of every operation over every transport.
