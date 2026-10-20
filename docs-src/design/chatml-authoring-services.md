# ChatMD and ChatML authoring services

Status: implemented opt-in Authoring service. The portable Project/Document
context ledger, native semantic and assistance handlers, upload publication,
and typed client/wire projections share the installed host policy and finite
limits. This document describes their source and edit/save/run contracts. The
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

When a host has selected an authentic custom compiler contract, the joined
full and prefix analysis entry points accept that exact contract. They share
canonical input and conservative conversion admission with explicit-contract
compilation, then use the same semantic checker and immutable result types.
The project and selected ChatMD script adapters preserve their existing
receipt, source and result-identity checks. The standard target entry points
still select their fixed contracts inside the bounded worker.

`Chatml_source.Location` is an exact half-open byte range tied to a source ID and
digest. `Chatmd_parser_input` retains authored/effective bytes and parser-owned
ranges; located import expansion retains each imported source owner. The [parser-owned extraction map](../../lib/chatmd/chatmd_extraction_map.mli)
binds the exact emitted ChatML body to its declaring input. A span has an exact
inline projection only when its bytes prove one continuous authored origin.
An external `src` origin additionally needs the host's immutable captured-source
custody. Transformed, synthetic, ambiguous or budget-degraded spans retain
explicit approximate or unavailable projections; not every extracted interval
has an exact markup range.

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
project preparation path. This adapter does not itself install protocol methods
or overlays; the installed service borrows it under the current host policy.

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
The host adapter can explicitly parse a current unsaved ChatMD root together
with its authorized imports, using the existing Generated parser and a retained
no-follow source owner. It returns one immutable bundle, genuine parsed receipt
and exact parser-input/source associations, with no retained file-read handle.
The authoring service will adopt this closure at an explicit Analyze or rebuild
boundary under the current context-generation check. Prefix and Query consume
only that admitted closure; a new or changed imported file is unavailable to
them until an explicit capture. An import read observes exact bytes, but the
host does not assert a multi-file atomic filesystem snapshot.
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

The captured project composer requires explicit owner-local dependency routes:
each selected authored dependency name maps to one exact qualified captured
project declaration. It checks those associations against the decoded project,
selected environment, namespace and existing project selection gate. For an
overlay, routes come from the previously admitted lock and the old lock source
must remain exact; the entire effective generation is recomposed under one
bounded budget. A prior Build is coherence evidence, not a live read or Git
grant. A standalone upload binds its routes in the durable tagged manifest
before validating a captured-only graph. These routes describe a virtual
captured closure. They do not prove that an authored Local directory resolves
to a native host directory, and cannot authorize later filesystem reads.

The project analysis core checks an OCH-294 overlay receipt against the same
OCH-278 context inputs, including exact captured source bytes and the old lock,
before handing its completed Build to joined semantic analysis with its actual
selected compiler contract.
It checks the returned compiler identity against the prepared input fingerprint,
required and selected contract, and environment. Failed checks retain same-run
syntax and diagnostics, not certified facts. Native primary and related
diagnostic sites keep their exact captured byte owners when editor-position
conversion is unavailable. Projection separately caps top-level diagnostics,
all primary/related sites, distinct captured revisions, total indexed source
bytes and actual line/checkpoint entries. It marks omitted site projections and
omitted diagnostics explicitly while retaining the native diagnostic.

The pure project cache retains at most one latest result per current context and
evicts on edit, close, refresh, or invalid generation. A candidate produced by a
joined worker can publish only against current source revisions, prepared
identity, environment, contract, and the same policy. The host must serialize
context transitions and publication, reauthorize current access, and join work
before releasing borrowed resources; this host gate belongs to the service
adapter. The OCH-278 analysis key records the validated captured input
fingerprint for both complete and failed checks; only a Complete snapshot carries
certified semantic graph facts. Result retention is bounded by context count and
compiler/semantic source, fact, edge, label, and work limits, not by an asserted OCaml heap-byte
estimate. Source count, total bytes, metadata, parser depth/tokens, graph work,
query work/results and publication bytes have finite host ceilings charged
across each complete request, not reset per file. Cancellation is cooperative
and joined; no domain or source borrow is abandoned. Errors distinguish unauthorized, missing source,
incompatible environment, invalid position, stale revision, unavailable fact,
truncation/work limit, cancelled and disconnected states.

The selected ChatMD-script adapter accepts a real parser-retained declaration
from a current document context. It compares the parser root's authored bytes
with that document revision, bounds declaration and input membership checks,
and binds the parser's extraction map to the exact entry source of a separately
host-admitted per-script Build. Its stamp records the document generation,
actual declaring parser input and range, selected source digest, prepared input
fingerprint, environment, and contracts. A missing selection, incomplete parse,
dynamic-only source, or absent validated project produces semantic
`Unavailable`; it does not manufacture a document graph. The adapter reuses the
same checked compiler and bounded native-site projection as project analysis.
Inline sites can retain parser-owned authored ranges, external sources retain
member/digest provenance pending host custody, and effective-only or generated
text remains approximate. Its pure document result map and the project result
map use one policy and a shared aggregate retention count. The installed
[semantic host](../../lib/agent_server/authoring_semantic_service.mli) serializes
these results with its context ledger and reauthorizes actor, parser selection,
source custody and currentness before publication or readback.

## Queries and incomplete input

`authoring.analysis` uses the captured project graph and
`Chatml_compilation.analyze_prepared`; it never builds a second checker or
resolver. Complete snapshots support diagnostics, visible-symbol completion,
hover/type, signature and definition/navigation. Exact selected host signatures,
module scopes, exports and source owners determine results. A failed whole-file
check exposes only same-run parsed syntax and diagnostics, with no certified
semantic facts for any unit in the failed graph.
The portable semantic wire contract has a separate request selector and
host-produced capture. A project request selects the current project context;
a document request selects an actual parser-assigned script ID. The client need
not invent a checked graph or compiler contract before analysis. A selected
script capture records the current document revision, parser/declaration input
fingerprints and declaration range, extracted script revision, selected graph,
environment and contracts. Raw document input returns typed semantic
`Unavailable` until a real selected script has been admitted. Complete,
Failed and Unavailable are distinct; a query identifies one snapshot and
current context, and a provisional prefix has its own source identity.
The current semantic service resolves Certified evidence only from its matching
Complete cache row and snapshot label, Syntax_only only from the same-run Failed
partial, and Provisional only from the exact retained prefix row. It checks the
requested source against captured revisions and reauthorizes a selected ChatMD
script against the current document and original host selection. Queries do not
recapture imports or publish new checker results. Native query sites retain their
compiler source; exact or approximate authored sites come only from the parser's
captured input associations. The host sets a finite source/selection work budget
alongside the lower query producer's item, display and traversal limits and
checks the finished response against the current context and response cap.
Semantic DTOs preserve native compiler sites and optional exact or approximate
ChatMD projections. They expose bounded type/signature display text from actual
selected bindings and explicit empty, unavailable, ambiguous and truncated
states, without exporting mutable inference graphs. The protocol/client layer
defines `authoring.semantic.v1` and checks selected feature, installed method
and response affinity; the host's Analyze/Prefix publication and read-only
Query paths share one context owner.
The typed methods are `authoring.analysis` (current context and project or
selected-script selector → Complete, Failed or Unavailable),
`authoring.prefix` (exact source/cursor → provisional prefix or typed
unavailability), and `authoring.query` (certified, provisional or syntax-only
evidence and one query kind → bounded, source-owned facts). Codec support alone
does not advertise an installed handler; capabilities list actual methods.
The [shared context owner](../../lib/agent_server/authoring_context_service_core.mli)
keeps the semantic, lexical and assistance siblings on the same joined
connection/context ledger. Context creation and editing require the core
feature; each sibling request checks its own negotiated feature. A borrowed
`Current` token expires when its callback returns. After yielding work, the
private owner repeats actor, source and selection checks, then compares the
context generation, both analysis cache heads and the retained document-capture
head in one nonyielding publication gate. Replacing an equal-byte capture does
not restore an earlier result's authority.

Only an explicit analyzed/rebuilt host document can admit a genuine
[parser/import capture](../../lib/agent_server/authoring_source_host.mli). The
owner retains at most one bounded immutable capture. An edit hides its old
Parsed facts immediately, while a later prefix operation may recompose the
current unsaved root with those already captured import bytes without a host
read. Refresh, close and owner replacement retire that material. For projects,
an explicit Analyze may also capture the project named by one genuine root or
imported script declaration. The source host resolves its authored project path
relative to the declaring parser input, reads through the same authorized
no-follow root and bounded context allowance, and decodes those exact bytes
independently of whether the script Build succeeds. The immutable receipt
retains its source, decoded environment names and fixed comparison identity;
later assistance reads only the current retained receipt. Exact project-byte
agreement with a selected Build is comparison evidence. The host must
separately prove the selected Build belongs to the admitted root and portable
member and recheck both actors; matching bytes or paths alone grant no source
authority. One explicitly analyzed parser/import capture now retains a finite,
complete row set for all genuine root and queued-agent declarations. Each row
holds independently validated selection, metadata-only evidence or a typed
unavailable reason. The host preflights the entire declaration count before
native preparation and bounds aggregate retained source/input bytes and work;
an over-cap analysis publishes no partial row set. An unrelated declaration's
missing Build does not erase a valid requested script or its project metadata.
Replacing the capture retires every row together, and a missing row requires
explicit Analyze or rebuild rather than an implicit completion-time read.

The [shared project overlay preparation](../../lib/agent_server/authoring_project_overlay.mli)
uses the actual admitted compiler contract and existing closure validator; it
chooses a fresh collision-checked lock identity under a finite work allowance.
The [selected-script policy](../../lib/agent_server/authoring_script_selection.mli)
compares an actual selected Build and compiler contract for the genuine parsed
declaration, without turning that comparison into a read or execution grant.
When an unsaved root edit changes that declaration's script body, the
[selected-entry overlay](../../lib/agent_server/authoring_selected_script_overlay.mli)
recomposes only the selected entry from the current parser body and frozen
Build sources. Analyze and Prefix use the resulting pure Build for checking;
the final gate still rechecks the original host-selected Build and contract.
An incomplete body can yield a provisional prefix only where the compiler
finds a completed leading statement. New imports and changed project routing
remain unavailable until an explicit authorized capture.
Prefix may use a prior selected row after an unsaved root edit only when the
current reparse uses exactly the frozen imports and a pure comparison proves
the same declaring source, project route/environment, external locator and
converted nonbody script metadata. A body-only inline change can then use the
existing bounded selected-entry overlay. Its provisional Query retains this
proof with the selected Script and input-source associations; both endpoints
repeat the original host selection and project-metadata authorization against
current Inputs before serving. The proof does not mint a new read or Build
grant.
Semantic wire labels, messages, names and environment identifiers require
valid UTF-8; their constructors and decoders return typed errors for malformed
text. Captured source bytes, digests and native byte sites remain unchanged.
The host projection must explicitly escape unrepresentable diagnostic bytes or
return a typed unavailable fact rather than silently dropping source evidence.

The semantic query producer reads only a matching immutable Complete snapshot,
an actual compiler provisional prefix, or same-run Partial syntax. It compares
the capture's input fingerprint, selected environment and contracts and the
request's exact captured source revision before projecting facts. A numeric
symbol ID is scoped to its snapshot; the producer validates its owner before
serializing it. Candidate and configured-module root slots are reserved before
expanding optional public members, so a tight aggregate result cap still shows
later root candidates with truthful nested-page truncation. A single bounded
work, item, member-edge and label ledger covers the request, and the final
payload must pass the portable query envelope constructor. An actual
provisional source remains the native site. Where the host also admits the
matching full capture, byte-for-byte prefix proof allows its genuine ChatMD
extraction map to add authored provenance without replacing that native site.
Failed input exposes only parsed syntax sites; lexical or selected-contract
fallback belongs to assistance and is labelled separately.
The [ChatML fallback producer](../../lib/chatml_authoring_fallback/chatml_authoring_fallback.mli)
uses bounded captured-byte lexical spans for keyword and delimiter templates.
When the host supplies a separately admitted selected Build, it compares the
actual compiler contract fingerprint with both the Prepared closure and its
required environment before suggesting names from that one static surface.
These selected-catalog references are possible even when checking the source
fails, but they are not visible-binding or type facts. No selected Build yields
lexical templates alone; a failed expected catalog admission is reported as
catalog unavailable, not as a certified empty set. Every result, including an
empty or unavailable one, must fit the encoded response-byte cap.

For an inline ChatMD script, the thin
[selected-script adapter](../../lib/chatmd_authoring_fallback/chatmd_authoring_fallback.mli)
requires the current authored document capture and the genuine parser-bound
[extraction map](../../lib/chatmd/chatmd_extraction_map.mli). A cursor maps to
compiled bytes only at one exact authored identity point, and the full token
replacement must project back to an exact range in that same current authored
input. RAW gaps, transformed input, external bodies and ambiguous boundaries
return source-map unavailability; no path or matching text supplies a map.
These producers do not install protocol handlers or grant source custody.
The portable `authoring.lexical` method returns byte-exact spans in all 16
shared highlighter roles, five open-construct issue kinds and a truthful
reached-EOF or truncated scanned prefix. Its owner distinguishes captured
ChatML source from ChatMD parser input and authored/effective view digests;
malformed UTF-8 source bytes remain valid lexical input. `authoring.assistance`
returns bounded tag, attribute, value, reference, path, template and
environment suggestions with exact authored replacement ranges. Static
grammar, genuinely parsed document/project declarations, installed builtin
assets, selected Build inputs and authorized host roots have distinct evidence
forms. A parsed environment name can be suggested before selecting a valid
Build; a declared path is not presented as a verified file. Both methods are
independently negotiated and unavailable until their actual handlers are
installed. Neither method certifies semantic facts or requires a successful
whole-file check.
The [lexical and assistance service](../../lib/agent_server/authoring_context_service.mli)
borrows the existing joined context owner; it does not create a second context
or capture imports while answering. Authored highlighting and local structural
help use the current source even when parsing fails. Effective highlighting and
parsed references require the immutable capture from an explicit Analyze for
that exact document generation. Project help uses the current validated overlay
and selected compiler contract; an invalid overlay retains bounded lexical
help without stale catalog names. Declared project environment names may come
from the one authorized captured declaration even when its selected Build
failed. Imported declarations keep the requested captured source and physical
parser-input association, while a transformed input without an exact authored
coordinate proof falls back to authored-only lexical help. Host path names and
selected tool references require their separate current host grants. The
service checks the selected method feature and final owner/selection state
before returning a bounded response; transport installation and advertised
capabilities remain separate.
An Analyze request can explicitly opt in to one
[checked ChatML example](../../lib/agent_server/authoring_checked_template.mli)
for the actual selected Standard target. Preparation replaces only the captured
entry in the validated Build, recomposes its frozen closure, and runs the joined
non-executing compiler under the selected contract and finite host policy. The
default Analyze request does no example work; Native contracts have no curated
example. A compiler or resource failure aborts the opt-in request without
publishing a partial analysis result. An originally Failed script can retain a
separately successful example witness. The context owner stores only a bounded
immutable comparison witness, evicts it on context revision or selection
change, and serves it through Assistance without compiling or reading sources.
The checked example takes one result slot before optional lexical candidates
and carries its own target and exact text digest. For inline ChatMD bodies the
full replacement must project through the genuine extraction map to one exact
current authored range. External `src` bodies currently lack that reverse
authored-point proof and receive no checked replacement; matching text or path
cannot supply it.
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

The ChatMD authoring schema now derives tag spelling and strict lexical
placement from the same AST vocabulary and scope state used by the batch lexer.
Script/tool family routing and exhaustive declaration attribute sets are shared
with their existing validators; nested shell names require the actual typed
ancestor chain, since `path`, `arg` and matchers have different meanings under
different parents. Permissive legacy declarations expose **Helpful** names,
which never claim to reject other attributes. Unconditional finite values use
the parser's typed spelling maps; comma-separated approval scopes identify
allowed tokens rather than pretending each token is an entire valid value.
Sibling-dependent lifecycle/protocol values remain unavailable until a precise
branch is known. An unfinished
declaration retains conditional family choices: for example a `read_file` tool
prefix may still become a command, agent, inherited or extension declaration.
Closed invalid script discriminators and duplicate strict attributes are
reported as authoring-malformed prefixes without changing batch acceptance.
The schema caps one authoring query at 128 completed attributes, 4096 bytes per
name or value and 65536 bytes total; exceeding it yields no attribute
candidates and does not reject the underlying document. The tolerant ChatMD
cursor now reads exact authored bytes through the same bounded highlighter
scan and the strict lexer's attribute spelling decoder. It returns
source-bound raw prefix and completed attribute ranges, strict-decoded
completed values, and explicit uncertainty for an unfinished name, value,
quote, escape or entity. It uses the validator's existing tag scope; an
irreparable fragment or mismatched official close is opaque rather than a guessed parent
scope. Invalid UTF-8 remains byte-exact, and a work-limited scan cannot claim
facts beyond its inspected prefix. The separate suggestion producer consumes
this cursor and schema; cursor output alone certifies no declaration or
semantic fact.

The pure ChatMD assistance producer now uses that exact authored cursor for
seven portable suggestion kinds: tag, attribute, finite value, reference,
path, template and environment. Static markup help remains available on an
incomplete document without a selected project. A successful current parse
supplies local and imported script IDs with each declaration's own captured
source; a separately selected tool registry supplies comparison-only tool
references. Environment names come from the captured decoded project map even
when Build selection fails. Installed instruction IDs come from the reviewed
embedded asset table, not a directory scan. Project source paths, decoded
project members, admitted host listings and selected closure members retain
distinct evidence. A closure candidate requires a qualified selected member
and a host-projected route from the current authored base; matching a path
suffix across package namespaces does not establish a valid edit or read
authority. Declared project fields are shown as declarations, never verified
files. Attribute edits preserve strict decoder semantics by encoding entities
in the current quote context; when an exact value cannot be represented,
the producer withholds that item with typed truncation. It also withholds
prefix replacement in the middle of a token, where the retained suffix could
otherwise duplicate text. One bounded work ledger covers capture checks,
cursor scanning and emitted candidates. The host still authenticates the
actor, capture, root and selected metadata before returning this DTO; the
pure library does not install a handler or fetch source files.

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

An explicit semantic Analyze request borrows the existing authoring context
owner and checks its negotiated semantic feature. Project analysis recomposes
the current admitted overlay and uses its actual selected compiler contract.
Document analysis captures imports only during this explicit request through
the current host source owner; a supplied document parses its supplied bytes
without a host read grant and cannot acquire missing imports. A successful
parse retains one bounded immutable parser bundle and a finite, complete row
inventory for its declared scripts. Rows admit each script's selected Build and
independently authorized decoded project metadata, or record why that script is
unavailable. A missing Build for one script does not erase another script's
facts. Later Query and Assistance calls must reauthorize the retained selection
and metadata through their own current host policies; omitting a policy cannot
downgrade a retained proof. Complete results publish a certified snapshot label;
Failed results retain same-run syntax and diagnostics without one. Native
diagnostic sites retain their exact source owner, with authored ChatMD
projections marked exact or approximate. Invalid or oversized display text is
visibly byte-escaped; diagnostic clipping is reported. The final no-yielding
gate rechecks actor, context generation, selected Build and every cache/capture
head before one pure publication. Source/parser/projection work, the joined
checker and response bytes each use their stated finite limits; cancellation
before publication changes no retained analysis facts.

`Builtin_instructions` already embeds and resolves `builtin:chatml`, which M4
captured-content tests exercise. M5 completion can enumerate actual installed
assets through that same catalog; unknown assets such as `builtin:skills` remain
unavailable until their CAP owner implements them. OCH-89 adds broader instruction
content and metadata, but is not needed to create a second catalog or block the
M5 service for presently installed assets.

## Positions

Compiler locations remain exact byte offsets. The pure
[`Chatml_editor_position`](../../lib/chatml_sources/chatml_editor_position.mli)
module is independent of runtime, transport and UI. Its immutable source-bound
index supports `Encoding.Utf8_bytes | Utf16_units`, abstract positions/ranges
and typed errors. Wire requests
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
explicitly. Existing agent sessions keep their selected source under their own
replacement lifecycle. A REPL keeps its original capture across reset; selecting
new saved bytes, a project graph or an environment requires a new Create.

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
source-read grant or an existing context. With a parent, the handler borrows the
actual current Core context on the same physical connection, checks original
and request actors, and retains that exact parent in the durable upload. A
parent-bearing request therefore needs the selected Core feature and current
context authority; echoed context coordinates grant nothing. Begin and commit
each use an explicit
idempotency key; repeated accepted chunks with identical bytes are no-ops,
while conflicting bytes fail. An incomplete or cancelled upload publishes
nothing. After a lost HTTP/stdio commit reply, the client queries status
instead of blindly repeating publication.
If the begin reply is lost, it repeats the exact begin key, manifest and optional
parent context to recover the upload ID; status only reconciles a known ID.
The portable bundle client requires the separately selected
`authoring.bundle.v1` feature, which does not require `authoring.core.v1`.
The bundle service handler borrows the same physical connection lifecycle and
joined request scope as source authoring, then delegates stage, validation and
publication to the one upload host. It can serve an upload-only principal with
no source context. The final protocol installer remains responsible for
advertising and routing the method on each transport.
When a complete sealed upload has no available publisher or current selected
contract, commit returns a typed selection error; it does not promise queued
work. The exact same commit key can retry the sealed claim after selection is
available. Status is read-only and reports a sealed pending claim as
`Validating`, the same coarse wire state used for an active durable validation
intent. That wire state promises neither automatic scheduling nor eventual
publication. A same-key Commit retry returns a known durable Published or
Rejected terminal after the upload host verifies it under the current
independent upload grant and rechecks any persisted parent. Such a retry after
parent refresh or revocation can fail without changing the durable result.
Read-only Status checks its upload grant without rechecking the parent.
Wire receipts echo authenticated host/principal comparison coordinates and
the upload ID. A pending commit is distinct from a Published receipt; only
the proof-gated store publisher may publish a validated graph, compiler-contract,
environment, manifest identity and finite expiry. The tagged manifest
fingerprint binds its explicit Project or ChatMD document basis, captured root
and selection, exact owner-local project routes when applicable, and sorted
member ID/digest/length inventory. `Source.Portable_path` rejects exact,
ASCII-case and file/directory prefix conflicts within each source namespace.
The current typed wire admits one shared inventory: each Project names an exact
member subset, and a selected inline ChatMD script names its authored owner and
body digest/length separately from any per-script Project. A captured Project
entry may be an uploaded member or a source derived from that exact script
selection. A compose request names the full immutable previous-lock source
revision when one is selected; its metadata is comparison evidence, not a read
grant. Constructors and decoders reject inconsistent subsets, repeated derived
source IDs with conflicting revisions, ambiguous paths, and out-of-range script
selectors. Client replies are checked against the request's tagged manifest;
these structural checks do not replace the host's captured-source validation.
The pure `Chatml_authoring_bundle_project` adapter admits one exact uploaded
manifest/source snapshot into a bounded Project validation attempt. A top-level
Project must account for the entire uploaded inventory; nested script Projects
select only their explicit qualified subsets from the same indexed snapshot.
An exact captured lock is restored unchanged. Composition binds a separately
admitted previous Build to the manifest's full prior-lock source revision, or
uses the existing Local-only gate without one. It reuses the project builder's
route, catalog, selection and compiler-contract checks. A captured Library entry
can be a planned catalog member or a standalone source: the existing catalog
planner makes that distinction from actual selected captures, and only the
matching standalone entry is excluded from catalog inputs. The adapter compares
the resulting Build's root, entry, selected environment (including declaration
defaults), routes and complete source revisions against the manifest. One
attempt owns the full snapshot index and aggregate work/input allowance across
all selected Projects; a failed or interrupted validation cannot be retried on
that attempt. The returned immutable value is captured-closure evidence, not
a sealed-upload claim, native provenance or execution permission.

The captured ChatMD document adapter maps each explicit portable document
member to its exact qualified uploaded source, then runs the existing generated
source-bundle parser over those bytes. Its checked path reports known invalid
source, syntax/declaration, missing source and parser-limit failures as typed
outcomes while preserving cancellation and unexpected exceptions. The parser's
read members and actual `Parsed.declarations`/`Binding` determine the complete
script set, registration and declaring input, byte range, extracted body and
external `src` origin; manifest selectors cannot invent declarations. A
single parser-owned `Chatml_prompt_binding` implementation supplies this
binding to both captured ChatMD validation and program custody;
`Chatml_prompt_programs.Binding` remains a type-equal public facade. A
`Source_only` script has no authored project and yields no fabricated graph.
Each selected Project must resolve from the declaration's actual portable input
to an explicit document member, match the authored environment and use a
separately supplied authentic compiler contract/previous Build. Its selected
entry is tied to the parser body by the wire selector and Project validator;
an uploaded external entry must also be the exact external document member,
even if another member has identical bytes. Every mapped document member must
be parser-read or a genuinely selected Project root, and the union of document
members and selected Project subsets must equal the uploaded snapshot. A
borrowed parser stage and all selected Builds debit one fail-closed Attempt
ledger. This remains in-memory closure evidence; publication still binds an
authenticated live sealed store claim.
Storage/receipt ownership and pure closure validation/publication are separate
components. The private upload store records a versioned immutable manifest,
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
retention. A sealed claim stays Ready until the publisher binds the exact
authenticated snapshot to a genuine captured Project or ChatMD validation
proof. It durably records an intent, installs any graph and lock captures, and
exposes Published only after a synced marker names their verified identities.
An incomplete intent remains Validating; authorized retry reconciles the same
bundle ID. Recovery verifies every installed source, rejects duplicate bundle
IDs, and reserves the aggregate retained source allowance before rereading
installed bytes. One protected published-capture borrow per store may run at a
time; its read lease survives uncertain recovery and blocks pruning or close
until the callback returns. The borrowed capture's bytes may be retained as
data, while the borrow itself conveys no lasting authority. Reuse checked
blob-store streaming mechanics where appropriate, but do not reuse
message-input `Send_messages` scope,
one-hour temporary retention or `allowed_use` as authoring authority. Local,
HTTP and stdio clients share the same bundle receipt contract.
The unreleased private staging document uses schema version 2 for this tagged
manifest. Recovery rejects a prior draft version instead of reinterpreting its
idempotency key or manufacturing a tagged basis; there is no released-upload
migration contract for these draft bytes.

The host upload facade takes exclusive lifetime ownership of one staging store.
Its independent upload policy checks the original authenticated actor and
current grant before each request, again after waiting for the facade gate,
and within the store operation gate before recovery or private upload access.
An optional parent context is checked against actual same-connection Core
authority at begin and again before publication. When Compose explicitly names
a prior lock, its original Build comes from current Project input or one exact
retained Document script row; a missing or changed independent selection is
unavailable. A Document parent that supplies no prior Build does not require an
Analyze request first. An
uploaded Project parent carries a same-Store publication pin checked at the
marker boundary. No parent is required for an upload-only client. The store
samples the host wall clock after its own gate and recovery, so queue time
cannot extend an expired upload. Publication separately rechecks the current
host-selected contracts, source grant, wall-clock deadline and daemon lock
immediately before the durable marker rename. Rechecking the exact same
immutable compiler-contract object avoids re-encoding its identity; a fresh
contract value still receives the full semantic fingerprint comparison. Current
policy selection, actor/grant checks and post-wait admission remain required.
A published read also checks the deadline and lock after materialization,
before lending the capture.
Rebinding that capture uses an independent current bundle-read grant under the
same host gate. Before this request materializes captured source bytes, the
host compares the saved manifest and receipt with its current selected target,
environment and compiler contract. The store may perform bounded internal
recovery reads before this comparison. A rebound Project restores the saved
**final** lock and exact captured graph under the current contract; it does
not need the original pre-composition Build. A rebound ChatMD document is
reparsed from its captured members, binds actual declarations and restores
each selected script graph under the same aggregate budget. Source-only scripts
remain graph-free. After waits, the host repeats actor, read-grant and selected
contract checks before returning. The receipt and rebound value are immutable
evidence, never a continuing read grant or execution authority; absent,
foreign, expired and stale captures have finite, path-free outcomes.
Ordinary staging timestamps govern operation admission; completed staging I/O
is not retroactively cancelled at the deadline. Revocation or
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
only through the separate explicit drive action. The shipped Eio/Notty
`chatml-repl` frontend uses the portable client API. Future chat-TUI REPL panels
and desktop editors can use the same contracts; those panels are not installed
by the language service.

## Implementation boundaries and validation

The [source host](../development/chatml-authoring-source-host.md) owns current
read grants and immutable capture; the [context store](../development/chatml-authoring-context.md)
owns overlay revisions and generation. The installed
[Authoring composition](../../lib/agent_server/authoring_installation.mli)
selects semantic, assistance and upload services independently under finite
limits. The parser owns extraction maps, the compiler owns complete and
provisional checking, and the typed client/wire adapters preserve their exact
source and result identities. Bundle validation and publication retain their
own Store-backed proof. Developer execution and historical trace disclosure
remain separately authorized services, rather than privileges conferred by
an analysis result.

The [frozen type display](chatml-type-display.md) is a bounded, presentation-only
view of immutable inferred types. Its text is not a type equality key or a
source-language annotation.

Integrated qualification exercises the [captured workflow](../guide/chatml-development-workflow.md)
and the focused service suites. Validation includes
build/analysis agreement, cross-file scope and source ownership,
failed/partial/provisional separation, Unicode and CRLF boundaries, ChatMD
RAW/entity/import mapping, changed overlays
and environments, denied host roots, missing upload members, malformed/deep
sources, cancellation and disconnect cleanup, lost commit reply reconciliation,
quota exhaustion, no preprocessing/fetch/tool/provider side effects, and
equivalent direct/HTTP/stdio results with clean stdio stdout. Measure latency
and retained bytes on small/large graphs; compiler checkpoints are cooperative,
not a hard CPU or heap sandbox.
