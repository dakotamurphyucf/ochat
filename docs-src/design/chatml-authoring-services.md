# ChatMD and ChatML authoring services

Status: M5 design, 2026-10-13. The portable project/document context ledger and
host document capture are implemented; the remote language-service handlers
and wire document projection are still proposed. This document defines the
selected service and edit/save/run boundary. The
[developer runtime](chatml-developer-runtime.md) owns test and
REPL execution over selected immutable sources; the
[trace design](chatml-execution-tracing.md) owns execution observations.

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

OCH-277 implements the [host-only source read/capture adapter](../development/chatml-authoring-source-host.md)
over configured external roots, current operator policy and the existing local
project preparation path. It is not yet a protocol method or an overlay service.

OCH-278 implements the [pure immutable authoring context store](../development/chatml-authoring-context.md).
It owns exact pending overlay revisions and a bounded current-context source
ledger. The host authorizes access; OCH-264 owns changed-closure validation;
OCH-258 owns the single retained semantic result.

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
requests serve direct callers, daemon HTTP and stdio. Initialization selects
an authoring feature only when handlers are installed; the capability method
then reports actual methods, limits and position encodings. An optional LSP
adapter may translate into this service later; M5 and the neutral desktop editor
do not depend on a second LSP lifecycle.

A host-owned **authoring context** binds an authenticated principal, host
incarnation, connection lease, admitted source origin and resource policy.
A project-basis context also binds its selected project, environment,
authentic static compiler contract and graph identity. A document-basis
context binds one exact captured source without requiring those project fields.
Project origins are explicitly authorized read-only host source roots or
principal-bound uploaded bundles; a document origin may also be bounded
client-supplied bytes under a separate operator policy. Logical organization projects and runtime
workspace definitions do not grant source access. Host source browsing, analysis,
bundle upload and execution have separate grants. A client-supplied path, digest,
workspace ID or upload receipt is never sufficient authority by itself. The host
reauthorizes every request, including a cache hit or rebound uploaded bundle.

Host browsing and path completion stay available: a read grant permits bounded
live manifest and content reads from admitted roots, using their actual source
owner. These reads are fresh observations; reading an immutable context source
instead returns its pinned captured or overlay bytes. A standalone or malformed
ChatMD document can open a document-only context from an authorized host member
or bounded client-supplied bytes without preparing a project graph. The host
mints a fresh source ID and captures the exact bytes, including empty or
invalid UTF-8 input at this host/core boundary; wire admission is specified
separately. The document ledger shares project contexts' generation,
overlay revision and source-byte quotas but publishes no project analysis key.
Project-only graph operations reject its basis explicitly. Optional default
static environment data is a real separately admitted build; its absence still
permits lexical and basic markup assistance. A ChatMD file may contain scripts
selecting different environments, so this default does not resolve every
script. Project-level path/template/script references need their separate
source/catalog authorization and may be unavailable in a document-only context.
The source host rechecks the original and request actor and the document/root
operator policy before serving pinned bytes or an overlay, and again after
yielding work before a response is published. Supplied bytes confer no browse,
path read, project, run or write authority.
An explicit context refresh captures the current authorized host-document
member again; its stable root/member locator stays bound to the connection,
while the captured base source digest may change. A supplied document's base
capture stays immutable until a new context is opened. Context generations and
editor revisions are exact JSON integers up to 2^53−1; mutation receipts
advance generation and remain bound to the current source owner.
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
  val inputs : t -> overlays:Overlay_set.t -> (Pending_sources.t, Error.t) result
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

The pure context owns pending effective sources, not a trusted prepared graph.
The shared closure validator uses the existing project decoder/catalog planner
and `compose` to validate authorized base sources plus overlays, or an uploaded
bundle. Exact lock restoration cannot accept changed source bytes. This validator
does not require upload transport for local overlay analysis, and never fetches
missing dependencies. Only its validated result reaches compiler analysis or
separate execution admission.

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
The protocol also exposes source/context-revision-bound lexical spans and
syntax, tag, attribute, path and template assistance without requiring an
analysis stamp. The former uses the shared pure lexical role vocabulary;
the latter checks actual admitted project and environment facts. Both remain
distinct from certified semantic query results and can help on incomplete
source before a whole-file check succeeds.
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

The OCH-260 implementation recognizes outer `program_stmts` reductions from the
existing ChatML parser. It retains one latest completed statement or `;;`
boundary at or before the requested cursor while reading the actual source;
an incomplete later statement, nested module body, or failing lexer cannot
create a made-up prefix. It copies exactly those leading bytes into a distinct
captured revision, then runs one ordinary parse and configured graph check over
that revision. Its opaque checked result cannot enter the linked compiler.
The published frozen snapshot carries a `Provisional` wrapper with the full
graph fingerprint, original source digest, selected source ID and prefix end.
Its locations use the *prefix* digest, so a full-document point cannot query
it. A private complete-analysis constructor prevents a caller from relabeling
that snapshot `Complete`. The full-document check still owns diagnostics.

This first bounded mode supports implementation sources. Incomplete `.chatmli`
companions and non-program sources truthfully return semantic `Unavailable`,
leaving keyword, delimiter, authentic host-name and ChatMD markup assistance
to the syntax path; complete companions use ordinary full-graph analysis. A
prefix with a type, dependency, companion or rebase failure publishes no
semantic facts. A selected catalog implementation outside the checked entry
closure is also `Unavailable`, so another source's successful check cannot
stand in for the selected file. Input bytes are admitted before the parser pass, and one
semantic work ledger covers input chunks, tokens, completed reductions,
source revision, graph/checker, recording, freezing and publication. It keeps
one boundary and at most one prefix graph per request. Cancellation discards
that attempt without changing any full-document result.

For embedded ChatML, the owned ChatMD lexer records byte offsets for each text
fragment through the existing grammar and conversion. Each declaration retains
the map produced while serializing its actual script body. An exact inline
projection requires every queried byte to match one continuous authored input
interval; imported text keeps its imported input owner. RAW delimiters, generated
child serialization, mixed intervals and ambiguous empty boundaries yield the
declaration range as an approximate related site, while the original exact
ChatML location remains available. The root's effective bytes project to an
authored location only when the authored and effective inputs match exactly.
The one parse invocation charges a finite byte, segment and work ledger for
map recording; exhaustion only degrades location precision and does not alter
the script body. An external `src` first yields its actual successful read
member and body digest, never an inferred editor source ID. The host may turn
that into an exact external location only through the same captured-source
receipt and source construction used for the program; the declaring element
range remains related context. No consumer searches for matching text or rereads a
file to infer provenance. Live analysis reads authored bytes without effectful
root preprocessing. These mapping rules apply to diagnostics, navigation and
highlighting alike.

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
Staging is bound to the authenticated principal, host incarnation, intended
origin and per-principal/global storage quotas. A client may attach a current
authoring context for provenance, but an upload-only client does not need a host
source-read grant or an existing context. Begin and commit each use an explicit
idempotency key; repeated accepted chunks with identical bytes are no-ops,
while conflicting bytes fail. An incomplete or cancelled upload publishes
nothing. After a lost HTTP/stdio commit reply, the client queries status
instead of blindly repeating publication.
If the begin reply is lost, it repeats the exact begin key, manifest and optional
parent context to recover the upload ID; status only reconciles a known ID.
The portable bundle client requires the separately selected
`authoring.bundle.v1` feature, which does not require `authoring.core.v1`.
Wire receipts echo authenticated host/principal comparison coordinates and
the upload ID. A pending commit is distinct from a Published receipt; only
OCH-264's complete closure validation may publish graph, compiler-contract,
environment, manifest identity and finite expiry. The manifest fingerprint is
a versioned digest of the exact qualified project source, selection and sorted
member ID/digest/length inventory. `Source.Portable_path` rejects exact,
ASCII-case and file/directory prefix conflicts within each source namespace.
Storage/receipt ownership and pure closure validation/publication are separate
components. The private upload stage records a versioned immutable manifest,
atomic phase and accepted-offset documents, and numeric member data files under
the daemon's locked data root. It fsyncs accepted bytes before advancing an
offset. Recovery discards unacknowledged suffixes, rechecks sealed payloads,
and rejects missing or conflicting authority rather than guessing that a
partially written stage was accepted. A manifest without a phase can resume as
Open only when no member progress or data exists. Expiry first writes a durable
tombstone; pruning renames the upload directory to a recognizable cleanup
marker before deleting its records, so an interrupted deletion cannot recreate
an Open stage. Finite per-principal source reservations and active-count limits,
plus global metadata, source-byte and record limits, bound claim and tombstone
retention. The staging store cannot
publish a bundle or issue a Published receipt. Reuse checked blob-store streaming mechanics
where appropriate, but do not reuse message-input `Send_messages` scope,
one-hour temporary retention or `allowed_use` as authoring authority. Local,
HTTP and stdio clients share the same bundle receipt contract.

The host upload facade takes exclusive lifetime ownership of one staging store.
Its independent upload policy checks the original authenticated actor and
current grant before each request, again after waiting for the facade gate,
and within the store operation gate before recovery or private upload access.
An optional parent context is checked against current context authority at
begin; it is provenance, not a requirement for upload-only clients. The store
samples the host wall clock after its own gate and recovery, so queue time
cannot extend an expired upload. This timestamp governs operation admission;
completed I/O is not retroactively cancelled at the deadline. Revocation or
cancellation may leave durable accepted bytes with an uncertain reply, which
an authorized retry or status call reconciles. The facade joins active work
before closing the store and returns finite, path-free error codes; it does
not install a public handler or manufacture publication authority.

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
are: OCH-277 host read-grant/capture adapter (5) and OCH-278 pure immutable
context/overlay lifecycle (5), under unestimated tracking parent OCH-257;
OCH-256 pure position bridge (3); OCH-258 checker/diagnostic projection (5);
OCH-259 parser-owned ChatMD extraction mapping (5); OCH-260 compiler-owned
provisional prefix assistance (5); OCH-261 common/source/context protocol and
client contract (8);
OCH-280 analysis, lexical and assistance wire/client contract (5); OCH-281
bundle wire/client contract (5); OCH-282 document-only context host admission
(5); OCH-262 HTTP/stdio server adapters (5); OCH-263 authoring bundle
staging/storage/receipt (8); and OCH-264 pure closure validation/publication
(5). OCH-261 includes separate live host browse/read and captured source read.
OCH-133, OCH-134 and OCH-135 retain their own assistance and highlighting
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
