# ChatMD and ChatML authoring services

Status: M5 design, 2026-10-13. This document defines the selected service and
edit/save/run boundary. It does not describe an implemented remote language
service. `docs-src/design/chatml-developer-runtime.md` owns test, trace and
REPL execution after this service has selected an immutable source.

## Existing foundation

`Chatml_compilation.analyze_prepared` runs the existing checker in a joined,
bounded compiler domain without evaluating initializers. `Chatml_semantics`
publishes immutable, source-qualified symbols, lexical contexts, types, exports,
references, graph facts and diagnostics for a complete check. A failed check
publishes diagnostics and syntax from units parsed in that same attempt; it
publishes **no certified semantic facts for any unit** in the failed graph.
Queries already have result and work limits. `Chatml_prepared_program`
fingerprints the exact source IDs and digests, entry, provenance, selected
environment, scopes and compiler contract. These values describe code, not host
authorization or editor revision.

`Chatml_source.Location` is an exact half-open byte range tied to a source ID and
digest. `Chatmd_parser_input` retains authored/effective bytes and parser-owned
ranges; located import expansion retains each imported source owner. The current
ChatMD script conversion retains the extracted inline body and a declaration
range, but does not expose a proven byte map from every extracted body interval
through RAW delimiters, entities and serialization to original markup.

`Project_source_host` and its admitted roots own host source acquisition. Its
borrowed source handles are not a client path grant. `Source_loader` has bounded
and captured-only modes; its lexical confinement is explicitly not an OS symlink
boundary. The runtime `Workspace` protocol exposes configured execution workspace
summaries, not editor source roots or file-write permission. `Operator_authorization`
and `Connection_context` already bind authenticated actors to transport
connections. The OChat protocol already has one closed typed command dispatcher
over direct, HTTP and full-duplex NDJSON stdio paths.

`Chatml_project_build.restore` validates an exact captured lock/source inventory
without filesystem or network access. `compose` owns pure project closure and lock
generation, but an uploaded raw source manifest still needs a new adapter through
the existing decoder/catalog planner before `compose`; it is not already a public
bundle-upload API. `Chatmd_source_bundle` provides bounded captured-only ChatMD
sources. The HTTP `/v1/blobs` upload is message-input scoped, temporary and has
no stdio equivalent. Its storage mechanics may be reused, but its authority,
retention and wire contract cannot stand in for authoring upload.

## Service and authority

Implement one local typed authoring service with additive `authoring.*` methods
on the existing versioned OChat request envelope. The same validated domain
requests serve direct callers, daemon HTTP and stdio. Advertise method support,
limits and position encodings during protocol initialization. An optional LSP
adapter may translate into this service later; M5 and the neutral desktop editor
do not depend on a second LSP lifecycle.

A host-owned **authoring context** binds an authenticated principal, host
incarnation, connection lease, admitted source origin, selected project and
environment, authentic static compiler contract, graph identity and resource
policy. Its origin is either an explicitly authorized read-only host source root
or a principal-bound uploaded bundle. Logical organization projects and runtime
workspace definitions do not grant source access. Host source browsing, analysis,
bundle upload and execution have separate grants. A client-supplied path, digest,
workspace ID or upload receipt is never sufficient authority by itself. The host
reauthorizes every request, including a cache hit or rebound uploaded bundle.

Host browsing and path completion stay available: a read grant permits bounded
manifest and content reads from admitted roots, using their actual source owner.
Path candidates are selected from that host manifest, project declarations and
the uploaded bundle's own inventory. A denied root, missing dependency or
unavailable environment returns an explicit failure; the service does not search
the connecting client's filesystem, `cwd`, network or package manager. Captured
Git/package sources are immutable for this service. Upload never writes into a
host root, installed prompt catalog, Git checkout or active agent session.

The design-level types are deliberately separate from wire DTOs:

```ocaml
module Authoring_context : sig
  type t
  val identity : t -> Context_identity.t
  val capture : t -> overlays:Overlay_set.t -> (Chatml_prepared_program.t, Error.t) result
end

module Overlay : sig
  type t
  val replace
    :  t
    -> expected_revision:Revision.t
    -> next_revision:Revision.t
    -> contents:string
    -> (t, Error.t) result
end

module Bundle_receipt : sig
  type t
  val graph : t -> Chatml_source.Digest.t
  val sources : t -> Source_revision.t list
end
```

An overlay owns immutable exact bytes, source ID/digest, observed base revision
and a monotonically increasing editor revision. It is connection-scoped and has
no write or execution authority. A request observes one immutable overlay set;
later edits cannot change its result. The response echoes host/context, source
graph, environment/contract, document digest and editor revision. Clients reject
results for any superseded identity. HTTP reconnect opens a new context and
resends a complete overlay set; stdio EOF closes its transient context. Closing
or expiry cancels and joins work before borrowed source-host resources are
released. A durable uploaded bundle can be rebound after fresh authorization;
the old overlay lease cannot.

Initially retain at most the latest immutable analysis result per context under
an explicit byte bound. Replace it on graph, overlay, environment or host-policy
change. A multi-entry cache needs measured justification. Source count, total
bytes, metadata, parser depth/tokens, graph work, query work/results and
publication bytes have finite host ceilings charged across each complete request,
not reset per file. Cancellation is cooperative and joined; no domain or source
borrow is abandoned. Errors distinguish unauthorized, missing source,
incompatible environment, invalid position, stale revision, unavailable fact,
truncation/work limit, cancelled and disconnected states.

## Queries and incomplete input

`authoring.analyze` uses the captured project graph and
`Chatml_compilation.analyze_prepared`; it never builds a second checker or
resolver. Complete snapshots support diagnostics, visible-symbol completion,
hover/type, signature and definition/navigation. Exact selected host signatures,
module scopes, exports and source owners determine results. A failed whole-file
check exposes only same-run parsed syntax and diagnostics, with no certified
semantic facts for any unit in the failed graph.
`Unavailable` and `Ambiguous` remain distinct from a successful empty result.
No completion operation invokes prompt preprocessing, scripts, tools, model
inference, Git/HTTP fetch, install or initializer evaluation.

Incomplete syntax needs bounded cursor assistance. A lexer/parser-owned pass may
offer keywords, delimiters, ChatMD tags/attributes and selected authentic host
builtin names; every item is labeled syntax/provisional. For `let x = <cursor>`,
the unfinished expression and any local function scope are not certified, so
hover/type/definition and inferred local-variable completion can be unavailable.
For a parser-confirmed complete top-level prefix, a compiler-owned authoring
mode may check those exact prefix bytes under a distinct source/graph identity
and return an explicitly `Provisional` result. It uses the same graph/parser/
checker/recorder/freezer and authentic selected static surface, but skips only
final entrypoint requirements. This mode is necessary: the current
`Chatml_checked_graph.check` always calls `check_entrypoints` on the entry, so
normal analysis of a prefix lacking `main` fails host admission. It must not
insert synthetic closers, broaden build grammar, claim an unfinished inner scope
or turn prefix success into a whole-file build success. If the prefix itself
fails parse/type/dependency checking, only syntax assistance remains. Whole-file
diagnostics are authoritative. Root review accepted this narrow authoring behavior;
any change to build grammar or import precedence needs renewed review.

For embedded ChatML, extraction must publish a parser-owned map from each body
byte interval to an exact ChatMD input interval or mark it unmappable. Compose
this with imported-input provenance. External `src` errors point to the external
captured file with the declaring attribute as a related location. When RAW or
entity transformation prevents exact mapping, report a declaration-level range
marked approximate; do not search for matching text or invent an exact range.
Live analysis reads authored bytes with no effectful root preprocessing. These
mapping rules apply to diagnostics, navigation and highlighting alike.

`Builtin_instructions` already embeds and resolves `builtin:chatml`, which M4
captured-content tests exercise. M5 completion can enumerate actual installed
assets through that same catalog; unknown assets such as `builtin:skills` remain
unavailable until their CAP owner implements them. OCH-89 adds broader instruction
content and metadata, but is not needed to create a second catalog or block the
M5 service for presently installed assets.

## Positions

Compiler locations remain exact byte offsets. OCH-256 owns the proposed pure
`Chatml_editor_position` module in `lib/chatml_sources`, independent of runtime,
transport and UI. Its draft contract has `Encoding.Utf8_bytes | Utf16_units`,
source-bound index, abstract positions/ranges and typed errors. Wire requests
negotiate one encoding and use zero-based lines and units. CR, LF and CRLF retain
their original bytes and one logical line break. Conversions reject out-of-range
coordinates, CRLF midpoints, byte positions inside a valid UTF-8 scalar and
UTF-16 surrogate midpoints. They never clamp, normalize or change source text.

`Chatml_source` permits arbitrary byte strings. Invalid UTF-8 is therefore not
a new compiler error: byte-mode locations and analysis remain possible. UTF-16
conversion of such content returns a typed `Invalid_utf8` error. JSON overlay
text needs a lossless valid-UTF-8 representation; exact non-UTF-8 bytes use the
binary/base64 bundle path. Every converted range remains tied to the captured
source ID/digest and editor revision.

## Local save and remote upload

The selected M5 persistence workflow is **local editor save plus explicit
immutable bundle upload**. OChat's language-service overlay is not file
synchronization. The editor or user's chosen local editor owns its local file
write. OChat reobserves saved bytes through the existing local source owner and
captures a new graph; a concurrent external edit can change what is observed,
so the result must name exact captured bytes and never claim an atomic filesystem
CAS against uncooperative writers. A run/test selects that saved capture
explicitly. Existing agent sessions and REPLs retain their prior source until
an explicit new capture/reset/upgrade operation.

A remote author may read a permitted host source closure, save a local edited
copy, then upload a complete immutable closure. The manifest names portable
source IDs, exact byte lengths/digests, entry, project/environment selection,
declarations and all selected dependency/lock sources. The server validates
manifest count/size/path/collision limits before receiving chunks, verifies
every exact digest and aggregate quota, and uses existing captured-only ChatMD
and pure ChatML project composition/restoration to admit the closure. `restore`
accepts an unchanged exact lock; changed project files need the existing
declaration decoder/catalog planner/`compose` path to produce a new lock. Missing
or unavailable sources fail: upload never fetches a package or silently falls
back to current host files. A host-owned private store publishes only a complete
validated immutable bundle ID, receipt, graph/contract/environment identity,
source manifest and documented retention expiry. It does not expose the staged
files as a writable project resolver.

Use `authoring.bundle.begin/chunk/commit/status` over the common typed protocol.
Staging is bound to principal, host, context, intended origin and per-principal/
global storage quotas. An incomplete or cancelled upload publishes nothing.
Commit is idempotent under an explicit key; after a lost HTTP/stdio reply the
client queries status rather than retrying a possibly completed publication.
Storage/receipt ownership and pure closure validation/publication are separate
five-point implementation slices. Reuse checked blob-store streaming mechanics
where appropriate, but do not reuse message-input `Send_messages` scope,
one-hour temporary retention or `allowed_use` as authoring authority. Local,
HTTP and stdio clients share the same bundle receipt contract.

The user-facing flow must distinguish `host source`, `local saved`, `unsaved
overlay` and `uploaded revision`. Analysis and run/test name the exact chosen
origin. Uploaded source is private to its authorized principal and is not a host
file update. Execution has a separate grant and rechecks the current principal,
selected target, compiler contract and runtime capabilities before consuming
the immutable receipt. `docs-src/design/chatml-developer-runtime.md` defines
run, test and REPL behavior over that pinned source. REPL create/submit/
drive-task/reset/close operations share the source identity and typed transport,
not an agent conversation or editor lease. Accepted REPL behavior commits each
successful statement after whole-submission parsing; a top-level `Task` runs
only through the separate explicit drive action. A terminal Eio+Notty client,
chat TUI and future desktop editor consume the same portable core/client API.

## Implementation ownership and validation

OCH-132 and OCH-136 remain **unestimated tracking parents** so their complete
acceptance is not accidentally narrowed or double-counted. The concrete children
are: OCH-257 host-owned read-only source context and bounded overlay lease (8);
OCH-256 pure position bridge (3); OCH-258 checker/diagnostic projection (5);
OCH-259 parser-owned ChatMD extraction mapping (5); OCH-260 compiler-owned
provisional prefix assistance (5); OCH-261 typed protocol/client contract (5);
OCH-262 HTTP/stdio server adapters (5); OCH-263 authoring bundle
staging/storage/receipt (5); and OCH-264 pure closure validation/publication
(5). OCH-133, OCH-134 and OCH-135 retain their own assistance and highlighting
outcomes. The run/test/trace/REPL implementation belongs to its developer-runtime
tickets. Assign estimates before starting each ticket and link completion
dependencies, including the bundle path into editor save/reload integration.

Each implementation ticket updates affected `docs-src/` contracts and runs
relevant local checks. Draft PRs can skip CI; final integrated M5 validation
must run the combined CI gate under OCH-254. Validation includes
build/analysis agreement, cross-file scope and source ownership,
failed/partial/provisional separation, Unicode and CRLF boundaries, ChatMD
RAW/entity/import mapping, changed overlays
and environments, denied host roots, missing upload members, malformed/deep
sources, cancellation and disconnect cleanup, lost commit reply reconciliation,
quota exhaustion, no preprocessing/fetch/tool/provider side effects, and
equivalent direct/HTTP/stdio results with clean stdio stdout. Measure latency
and retained bytes on small/large graphs; compiler checkpoints are cooperative,
not a hard CPU or heap sandbox.
