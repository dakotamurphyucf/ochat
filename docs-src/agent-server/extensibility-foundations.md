# ChatML extension records and capability discovery

The extension record/transaction foundations and strict declaration parsing are implemented. Model-visible
one-off scripts, moderator tools, subscription adapters and generated-child tools
are still under implementation; none of their feature flags is enabled yet.
This page describes the available storage and client protocol contracts, not a
runnable extension tutorial.

## Capability discovery

`protocol.initialize` can return optional `extensions` metadata. Older responses
omit it. Both the metadata version and record codec version are currently 1;
unsupported versions fail decoding instead of dropping required information.
The catalog distinguishes known contracts from qualified host functionality:

- `chatml.invocations.v1`
- `chatml.background.v1`
- `chatml.notifications.v1`
- `agent.delegation.v1`
- `chatml.authoring.v1`

`available_features` is empty on current hosts. Negotiation intersects requested
features, server options and host qualification. Adding an extension string to
server options cannot activate an unfinished service. `server.info` applies the
same qualification filter. Capability discovery does not confer execution
permission; eventual tool admission must still check inherited authority.

Host identity distinguishes `daemon`, `embedded_durable`, `embedded_transient`
and the reserved `direct` host. Embedded sessions with an explicit data root use
`embedded_durable`; temporary data roots use `embedded_transient`. Host identity
and persistence lifetime are separate from the selected journal flush boundary:

| Journal mode | Acknowledgement boundary |
|---|---|
| `synced` | The journal append completes `Eio.File.sync` before success. |
| `buffered` | The append returns without a sync guarantee. |
| `memory` | Reserved for a host with no persistent journal. |

The current daemon maps both configured `each` and `interval` to a synced append;
`unsafe_buffered` maps to buffered. Embedded hosts also sync their journals, but
transient roots are removed on close. These facts do not promise durable external
execution, continuation replay or recovery of a deleted temporary root. A
process-restart test is not evidence of survival through power loss.

## Status snapshots and events

Snapshots have an additive `extension_status` list. Each version-1 summary contains
only its kind, typed identity, generation and lifecycle state. Missing fields on
older snapshots default to an empty list. Summaries omit tool arguments, results,
error text, schemas, capability fingerprints and arbitrary correlation text.
The server exposes them only to principals with `security.read`.

A changed projection is included in an existing durable `session.updated` event
as an optional `extension_status` field. It replaces the entire status list; an
empty list clears old state. Older clients can ignore the field and still advance
the event cursor. New clients decode strict kind-specific states and reject
ambiguous duplicate identities. Replacement snapshots and ordinary snapshots use
the same scope filter. Identical repeated terminal commits produce no extra
status update or history insertion.

## Durable records and atomic commits

Invocations retain admission context separately from their initial outcome and
provider-history publication. Subscriptions retain an originating invocation,
expiry, epoch and one immutable terminal winner. Deliveries retain a completion,
source, wake policy, attempt and one history identity. The session aggregate
checks ownership, generation and cross-record acknowledgement/result correlation.
One terminal work item has one delivery owner.

Session state schema 4 upgrades schema 2 with empty extension records and schema 3
with its invocation records preserved. Inconsistent old fields and unknown future
schemas fail closed. Snapshot, journal and compaction archive restoration apply
the same version checks. An older binary is not a supported reader of schema 4;
retain compatible backups before testing a binary rollback.

The host-internal `Session_actor.commit_extensions` operation atomically commits
record changes, moderator state, queued work intents and notification publication.
It requires the expected session revision and generation. The actor installs
state and broadcasts events only after persistence succeeds. It is not an RPC or
an authorization boundary. No external effect is rolled back by rejecting a local
transaction.

Publication uses `Runtime_notification(delivery_id)` provenance and commits its
history entry and receipt together. It requires the originating initial response
to be published. The current foundation permits publication only while the
session is running and idle; active-turn safe-point delivery and provider data
framing remain unfinished execution-service work.

## Recovery classifications

These classifications define the execution-service recovery work. Record replay
and local transaction deduplication are implemented; automatic reconciliation of
all execution states is not yet available.

| Retained state | Required recovery action |
|---|---|
| No committed invocation | Retry admission with the same request identity. |
| Admitted or dispatching, no result | Determine whether execution began; preserve interruption or uncertainty rather than fabricate success. |
| Committed queued intent, no launch | Reconcile claim and launch; do not execute arbitrary effects twice. |
| Initial outcome resolved, unpublished | Publish/reconcile the retained response without rerunning its handler. |
| Published pending acknowledgement | Continue tracking its owned job or subscription. |
| Terminal work, pending delivery | Retain the business result; deliver only at an eligible history boundary. |
| Committed notification | Replay its existing history identity; never insert a second copy. |
| Failed delivery | Allow explicit bounded delivery retry; do not repeat external work. |
| Interrupted external execution or approval continuation | Report interrupted/uncertain state; retry only under an explicit safe or idempotent policy. |

Active and unresolved records must not be removed to meet a retention target.
Notification text and delivery receipts have different lifetimes: compacting text
does not clear the committed receipt. Full reset, shutdown and restart reconciliation
are part of the remaining recovery implementation.

See the [protocol interfaces](protocol-types.md) for exact codecs and
[session architecture](../lib/agent_session/architecture.doc.md) for actor APIs.

## Tool schema validation foundation

`Chatmd_shell_spec.Tool_schema` provides shared pure compilation and validation;
parsed ChatMD extension tools compile input, initial-output and optional completion
schemas through this service during source capture. The supported
subset consists of boolean schemas and these object keywords:

- `type` (one type or a nonempty type union)
- `properties`, `required`, `additionalProperties`
- `items`, `minItems`, `maxItems`
- `minLength`, `maxLength`
- `minimum`, `maximum`
- `enum`, `const`, `anyOf`
- String metadata: `title`, `description`, `$comment`

Unknown keywords, including `$ref`, `$schema`, `format`, `pattern` and `oneOf`, are
rejected explicitly. There is no file/network reference resolution. Primitive
constraints apply to their respective value types, and an empty object schema
accepts all valid JSON. Contradictory bounds may compile as an unsatisfiable schema.

Numbers use exact decimal comparison, including values beyond floating-point
integer precision; mathematically integral decimals satisfy `integer`. Enum and
const compare objects independently of key order and numbers by mathematical value.
String lengths count Unicode scalars, not UTF-8 bytes or grapheme clusters.
Malformed UTF-8, invalid number tokens and duplicate JSON object keys fail validation.

Schema sources and values are limited to 1 MiB, 128 levels and 100,000 nodes.
Source nesting is checked before JSON parsing. Literal decimal exponent magnitude
is limited to 1,000,000 without allocating exponent-sized strings; length/count
bounds must fit the host integer range. Compilation and validation limit structural,
branch and equality work to 1,000,000 charged steps. Exhausting that budget is a
resource error, even inside `anyOf`; a later permissive branch cannot hide it.
These are schema-service ceilings, separate from the narrower runtime invocation
budgets and the execution services' cancellation guarantees.

Diagnostics distinguish invalid schemas/JSON, resource exhaustion and value
mismatch and include the failing value or schema path. Compilation does not load
sources, instantiate ChatML modules, or invoke any tool.

## Parsed extension declarations

The following declaration shapes are now parsed, serialized and captured in pinned
prompt artifacts. Their runtime execution remains disabled while the invocation
and authoring services are implemented. Handler kind/reference checks are present;
full entrypoint type checking and effective capability binding remain unfinished.

```xml
<script id="worker" language="chatml" kind="tool" src="worker.chatml"/>
<tool name="process_report" type="chatml" script="worker" entrypoint="run"
      input_schema="schemas/input.json" output_schema="schemas/output.json">
  <uses tool="read_file"/>
</tool>
```

Standalone scripts require an explicit ID. `uses` names exact registered tools,
without changing their configuration. Omission selects zero tools. Duplicate names
and cyclic dependencies between declared extension tools are rejected. Resolving
these references against the final authorized tool manifest is still pending.

```xml
<script id="coordinator" language="chatml" kind="moderator"
        api="extensibility-v1" src="coordinator.chatml"/>
<tool name="watch_result" type="moderator" moderator="coordinator"
      input_schema="schemas/watch.json" output_schema="schemas/ack.json"
      completion_schema="schemas/result.json"/>
```

Moderator tools require the selected version-1 moderator; a legacy moderator
without `api` cannot receive the new invocation event. At most one conversation
moderator is allowed. Moderator tools use their owner's configured capabilities;
`uses` belongs to standalone tool declarations. Binding both implementations,
combining extension tools with shell/custom/MCP configuration, unknown attributes
and duplicate attributes fail before schema reads.

An authoring policy is a single top-level declaration:

```xml
<authoring_context policy="manual"/>
```

`auto` and `manual` reject a `topics` attribute. `preload` requires a nonempty,
unique whitespace-separated topic list. Topic existence, helper dependencies and
context injection are not implemented yet, and hosts explicitly reject execution
with these new declarations instead of silently ignoring them. Ordinary inline
`uses`/`authoring_context` markup remains text outside its declaration scope.

Schema and script dependencies must be relative local paths within the prompt's
source root. Each extension source read is bounded to 1 MiB before an observer can
capture it. The artifact captures exact schema/script bytes, qualified handler IDs,
and the declaring source context. It restores without consulting changed or deleted
live files. Capture rejects differing bytes for one dependency during a single
build, more than 256 total source files, or an aggregate larger than 8 MiB. This
source boundary does not replace execution capability checks or artifact symlink
verification.

New prompt artifacts use parser schema version 2 and a distinct revision identity.
Existing parser-version-1 artifacts with legacy declarations still restore; unknown
parser/runtime versions fail. Existing moderator binary record layouts are retained
by additive declaration variants. Extension declarations cannot be interpreted as
version-1 artifact contents.

## Static script contracts

`Chatml_host_runtime.compile_script` accepts optional `required_bindings` in the
host's type language. It checks final inferred bindings before resolving the
program, without evaluating initializers or invoking entrypoints. Missing names,
wrong arity, incompatible inputs/results and shadowed final definitions reject.
Shared type variables relate requirements such as moderator `initial_state` and
`on_event`. Source-level type aliases cannot redefine the host's expected types.
This is an internal compiler facility; complete admission and isolated compilation
with enforced time/resource budgets remain unfinished.

`Chatml.Chatml_extension_surface` defines explicit version-1 compiler surfaces:

- `one_off_v1` provides core computation, task composition, diagnostic logging and
  `Tool.call`. `main(input)` must return `json task`.
- `tool_v1` adds typed invocation context and outcome aliases. `run(ctx, input)`
  must return `tool_outcome task`, with exactly two arguments.

Neither surface provides stdout printing, direct model/process access, tool
approval/rewriting, conversation mutation, session administration, spawning, timers
or UI operations. Approved background operations will arrive with the job service.
Compiling a `Tool.call` does not select or authorize a tool; the host still needs
the exact admitted capability binding and per-call policy checks.

Tool outcomes are tagged ChatML variants, distinct from ordinary JSON:

```ocaml
let run : tool_context -> json -> tool_outcome task =
  fun ctx input -> Task.pure(`Complete(input))
```

The other script outcomes are `Pending(work_ref, acknowledgement)` and
`Fail(tool_error)`. Work references are tagged `Job(id)`/`Subscription(id)` values;
error records contain `code`, `message`, `retryable` and JSON `details`. These tags
use ChatML's backtick syntax. Host cancellation is not a script-constructible
success or outcome tag. A type-correct work reference still requires runtime
ownership and lifecycle validation.

`tool_context` includes the version, invocation/provider/session IDs, generation,
origin, parent invocation/job references, tool identity/revision, capability
fingerprint, creation/deadline milliseconds, execution limits and selected
capability descriptors. Descriptors include an opaque ID, name, implementation
revision, fingerprint and input schema. These are host-provided snapshots; copying
or modifying a script record cannot change the host's actual execution authority.
The separate `input` argument contains the validated request. The context does not
expose transcript items, credentials, filesystem handles or callable OCaml values.

## Live tool capability bindings

`Chat_response.Tool_capability` stores bindings to the actual constructed tools.
`Agent_runtime` retains the declaration revision for each resulting implementation,
including each name produced by MCP discovery, and offers a lazy capability registry.
The registry includes the configured host resource/manifest/policy fingerprint and
the actual provider descriptor. Descriptors are checked as bounded valid JSON;
this does not yet compile their schema keywords or validate invocation arguments.

Selecting a list of names returns a subset of those same bindings. Empty selects
none; duplicate or unavailable names reject. Selection cannot install another
implementation, add roots, change a shell configuration, reconnect an MCP endpoint
or select a capability that was removed from the supplied registry. The owning
invocation service receives the original silent/progress runners.

Every live binding has a fresh opaque `cap_` identity and fingerprint. Resolving a
reference checks both within the selected registry. A different registration,
owner, or configuration cannot accept an old reference by falling back to its name.
Registry fingerprints are independent of selection order. Runtime construction
captures configuration digests; the registry retains neither serialized credentials
nor serialized executable closures. Its in-memory implementation closures continue
to own their existing configured resources.

This is live binding infrastructure. It does not implement durable grant restoration,
child authority inheritance, per-call moderation, or the invocation admission and
result-disclosure services. In particular, implementation access is a trusted host
operation, not a model-facing bypass for calling a tool. Runtime reconstruction must
re-admit durable capabilities explicitly; a configuration digest alone cannot prove
the identity of a reconnected remote implementation or a rebuilt native binary.
