# ChatML semantic query contracts

`Chatml_semantics.analyze` consumes an already admitted `Prepared_program` and
the authentic static compiler contract. It calls the same configured graph checker
as linked compilation. `Chatml_compilation.analyze_prepared` runs analysis through
the existing joined Eio compiler worker. Neither path loads project files,
initializes modules or grants native tool authority.

Use `Complete` snapshots for types, definitions, references, lexical candidates,
configured module availability and public exports. Type groups are frozen together
after all importer, interface and host constraints settle. A symbol retains its
principal definition; a binding selects the actual lexical or interface-restricted
view of that symbol. Do not replace a public binding's scheme or member set with
the principal implementation view. Alias templates retain their original parameter
groups and recursive binders. Host origins carry contract/member identity instead
of invented source locations.

Construct zero-width byte points using the snapshot and its exact captured source
location. Points, symbol IDs and contexts carry snapshot ownership; equal text or
equal input fingerprints do not permit reuse across snapshots. Reference hits use
actual parser selections. Type lookup uses the smallest containing recorded
expression region, keeping conflicting equally specific facts ambiguous. Context
lookup uses actual reference sites, expression starts and statement/unit anchors,
including EOF. Unsupported gaps are unavailable, not nearest-neighbor guesses.
Configured module availability is separate from lexical values and type aliases;
availability alone proves neither checking nor initialization.

`Chatml_editor_position` converts editor line/column pairs against one immutable
`Chatml_source` capture. Lines are zero based. A byte column counts exact UTF-8
source bytes; a UTF-16 column counts code units, so a supplementary scalar takes
two units. CR, LF and CRLF delimit lines without normalization. The position
inside CRLF, the middle of a valid UTF-8 scalar and the middle of a UTF-16
surrogate pair are rejected. EOF is the end of the last line; a trailing line
break creates an empty final line. Malformed UTF-8 leaves byte conversion
available and makes UTF-16 conversion unavailable with a typed error; it does
not change compiler source admission. Converting to a compiler span uses
`Chatml_source.locate`, and a span from another source ID or digest is rejected
even if its byte offsets fit. The on-demand index has a caller-selected entry
limit and does not load files or confer authority. An editor range itself holds
only coordinates, so the service must qualify incoming ranges with the intended
capture ID and digest before applying them to an index.

An invalid program returns `Failed`, whose partial result contains only diagnostics
and syntax tables from units parsed in that same attempt. A failed parser has no
recovery tree. No provisional inferred type, export, lexical context or dependency
graph escapes through the failed result. Resource/admission failure is a typed
error, while cancellation and unexpected exceptions propagate with their original
identity and available backtrace. Domain cleanup completes before return.

`Chatml_semantics.analyze_prefix` is a separate, explicitly provisional analysis
for incomplete implementation sources. The real parser records completed
top-level statement/separator boundaries; analysis checks one prefix ending at
the last such boundary at or before the byte cursor. It uses the same configured
checker and exact dependency inventory, including companion interfaces, while
skipping only the selected entry's final host-entrypoint requirement. The
provisional checked graph has no execution conversion. A successful result
retains the full graph fingerprint, original source digest, prefix digest and
boundary so clients can label its scope accurately. It is not a `Complete`
result, and the full analysis remains authoritative for diagnostics.

Missing boundaries, rejected prefixes, sources outside the checked closure and
incomplete interface files produce explicit unavailable results. There is no
heuristic type recovery or repeated search through shorter prefixes. Grammar-only
completion and lexical highlighting can still provide separately labeled
assistance. `Chatml_compilation.analyze_prepared_prefix` provides the same joined,
cancellable Eio worker boundary as full analysis. Prefix construction, checking,
and publication share the analysis work and retention limits.

Analysis input, recorder, frozen graph and publication capacities form one bounded
plan, with a cumulative cooperative work budget. Publication admits retained facts,
edges and label bytes before allocation. Query calls separately bound work and
result counts and report truncation. These units are not CPU time or heap quotas;
third-party parser steps remain indivisible. The fixed host contract projection in
the joined adapter is covered by its elapsed policy before the analysis work ledger
begins. Snapshots retain no inference cells, recorder callbacks or execution state.

Within one analysis recorder, repeated source units using the exact same immutable
static host surface reuse its staged host definitions and type schemes. Each unit
still gets its own source context and authored bindings. The recorder keeps only
the most recently used surface as a lookup shortcut; another surface is checked
and staged independently. Type-scheme uses instantiate fresh inference cells, and
recursive binders are resolved within their original quantifier group. This keeps
multi-module checking within the same finite analysis limits without conflating
source facts or independent polymorphic uses.

`Chatml_authoring_query` projects those immutable facts into portable semantic
query payloads. It checks actual snapshot or provisional identity against the
request capture and exact source ID, digest and byte length. The host separately
owns authorization, current context and snapshot incarnation. The producer uses
one aggregate work, item, member-edge and label ledger, plus a shared frozen-type
display session. It reserves the bounded top-level candidate/module page before
spending remaining items on optional nested public members; omitted members
retain a truncated page flag. It validates a finished payload against the wire
envelope before returning it. Same-run partial results expose parser syntax
only. A provisional prefix keeps its own native source; an exact byte-prefix
witness can relate it to a separately admitted full capture for ChatMD authored
projection without relabelling the compiler site.

The public interface lives in `lib/chatml_semantics/chatml_semantics.mli`; private
modules own publication, budgets, query traversal and partial results. Behavioral
evidence in `test/chatml_semantics` covers public/implementation views, late shared
constraints, previous-snapshot immutability, identity rejection, actual lexical
anchors, partial failures, limits and joined-worker cancellation. The lower recorder
and type snapshot suites independently exercise their respective invariants.
