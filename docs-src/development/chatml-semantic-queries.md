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

An invalid program returns `Failed`, whose partial result contains only diagnostics
and syntax tables from units parsed in that same attempt. A failed parser has no
recovery tree. No provisional inferred type, export, lexical context or dependency
graph escapes through the failed result. Resource/admission failure is a typed
error, while cancellation and unexpected exceptions propagate with their original
identity and available backtrace. Domain cleanup completes before return.

Analysis input, recorder, frozen graph and publication capacities form one bounded
plan, with a cumulative cooperative work budget. Publication admits retained facts,
edges and label bytes before allocation. Query calls separately bound work and
result counts and report truncation. These units are not CPU time or heap quotas;
third-party parser steps remain indivisible. The fixed host contract projection in
the joined adapter is covered by its elapsed policy before the analysis work ledger
begins. Snapshots retain no inference cells, recorder callbacks or execution state.

The public interface lives in `lib/chatml_semantics/chatml_semantics.mli`; private
modules own publication, budgets, query traversal and partial results. Behavioral
evidence in `test/chatml_semantics` covers public/implementation views, late shared
constraints, previous-snapshot immutability, identity rejection, actual lexical
anchors, partial failures, limits and joined-worker cancellation. The lower recorder
and type snapshot suites independently exercise their respective invariants.
