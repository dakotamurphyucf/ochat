# Parser and program custody

`Chatml_prompt_programs` retains exact ChatMD parser inputs and complete immutable
source graphs in the prompt artifact's existing metadata/private payload bundle.
It performs no filesystem lookup, preprocessing, acquisition, compilation,
initializer execution or tool admission.

A binding belongs to an actual `Prompt.Chat_markdown.Parsed` declaration. It
records the effective input's role, authored/effective revisions, byte range,
qualified script ID, extracted body digest and registration Root. Initial capture
checks actual parser membership and byte equality with the graph entry. A stored
range alone does not prove AST membership: active selection and restoration check
it against the current replayed parser result.

Each program is either an actual provided `Prepared.t` or an actual selected
`Build.t`. The former delegates metadata and restoration to
`Chatml_program_document`; the latter retains the exact original lock bytes and
restores through `Build.restore`. Selected custody never substitutes a provided
projection or retargets a lock contract. Restored programs retain their authentic
input kind, including the admitted selected build.

Source IDs are scoped to a graph. Independent programs may use the same namespace
and relative path at different revisions. The outer inventory therefore keys
sources by ID, digest and byte length; program references and selected-lock
references name those exact revisions. Duplicate IDs within one program reject,
even during inactive structural admission. Globally equal revisions require exact
byte equality. The private SHA256 pool deduplicates equal physical bodies and
rejects unequal bytes at an equal digest. Original graph IDs remain unchanged.

The named `chatml.prompt-programs` version 1 document contains parser-input
associations, source-revision descriptors, complete provided metadata or selected
lock references, original program identities and current active binding mappings.
Opaque source namespaces and script IDs use explicit hexadecimal fields. Source
bodies, native owners, callbacks, compiler output and initialized state never enter
metadata. Exact effective parser bodies and graph source bodies live in verified
private payloads or refer to equal authored payloads.

Initial capture and extension consume admitted graphs. Storage admission checks
all active and inactive metadata, references, source revisions and exact payload
inventory. It does not infer historical compiler admission from a receipt.
Restoration requires authentic contracts only for the current active selection;
inactive records are retained but never exposed as executable prepared programs.
Unknown fields remain owned by the original document. Rerooting and extension use
that carrier; adoption additionally preserves the complete previous known
inventory and transfers compatible unknown ownership. Unsafe deletion or
conflicting unknown values reject.

Limits are explicit. Canonical program input bytes are summed across programs,
including shared bytes in each graph. Selected restoration consumes one total
project-work allowance. Source-revision counts and metadata expansion are admitted
before projection, hexadecimal output or maps. Physical bodies additionally share
the artifact's 256-file / 8 MiB ceiling with authored files; complete outer manifest
admission remains the artifact owner's responsibility.

The prompt owner uses a named structural artifact policy, with a separate 8 MiB
canonical-input sum. It hashes the complete admitted storage manifest before fresh
revision identity or retained-child source/reservation identity. That includes
preserved unknown metadata and payload digest/length inventory, and excludes outer
revision identity, creation time and self hashes. Source/package IDs are selected
before this hash, avoiding a revision-identity cycle. Zero-program custody still
retains and hashes exact effective Roots.

Present bundles replay retained effective Roots with captured imports and no
preprocessor. Invalid present custody never falls back to legacy source loading.
The explicitly absent-bundle path preserves legacy restoration. The fixed replay
helper uses the existing prompt-owner diagnostic policy: parser exceptions become
diagnostics, while Eio cancellation and timeout propagate. Host preparation
callbacks and pure codec checkpoints remain outside that parser diagnostic
boundary and preserve exception identity.

Execution and per-declaration project/environment selection belong to the host
runtime owner. `restore` supplies immutable active programs, not execution
permission, live tools or a module instance.

## Refresh associations

`Binding.Slot` identifies an actual logical registration Root, declaration Input
role/path and parser-qualified script ID. It omits revisions, body hashes and byte
ranges, so editing or moving a declaration within its input preserves the slot.
Exact `Binding` replay identity remains unchanged. Distinct import aliases and
registration Roots remain distinct; these labels never confer source authority.

`retained` projects all admitted program records, including inactive variants.
Its bounded exact-key lookup returns the original Selected lock capture, preserving
accepted noncanonical bytes. It rejects malformed, missing and Provided-only keys
and never restores an old graph against a new compiler contract. Projection and
lookup each have an explicit whole-call cooperative-work limit and preserve
original checkpoint/cancellation exceptions. No callback survives in the output.

The authoritative `Script_contract_plan` records actual Standard targets, explicit
legacy Ui/Headless invocation roles, and native hook kind/selected entrypoint.
These role labels remain distinct even when contract fingerprints coincide; the
actual supplied surface still determines compilation metadata. This pure boundary
supplies facts for a durable host association. It does not choose Initial/Build/Update,
persist a Catalog baseline or authorize dependency updates.
