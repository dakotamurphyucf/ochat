# Retaining ChatMD parser inputs and ChatML programs

A saved prompt revision retains the code it was prepared from. Its program bundle
contains the exact effective ChatMD parser inputs, their original authored bytes,
and any prepared ChatML graphs supplied by the host. Preparing and executing those
graphs remain separate operations: restoring custody never runs an initializer or
grants filesystem, tool, or provider access.

## Source ownership

A declaration binding comes from the actual located ChatMD parser. It identifies
its registration root, declaration source and byte range, qualified script ID, and
extracted script body. The range belongs to the effective parser input, which can
differ from authored text after preprocessing. Restart replays that retained input
without running preprocessing again. Imported files retain their captured bytes.

Original operator-selected source access is a separate runtime loan. Its bounded
receipt survives artifact restoration, but only a matching current catalog
selection can admit live reads; missing roots do not prevent captured replay.
See [original source ownership](../../lib/agent_session/original_sources.md) for
shared revocation, retained directory capabilities and scoped source preparation.

Source IDs are local to a prepared graph. Different script projects may use the
same package namespace and relative path with different contents. The outer
custody inventory distinguishes these by source ID, digest and byte length. Each
program references its exact revisions; its original source IDs and selected lock
remain unchanged. Duplicate source IDs inside one program are rejected. Identical
physical content is shared only after exact-byte comparison.

A `Provided` program retains its admitted graph description. A `Selected` program
retains its actual project lock and source inventory; restoration delegates to
`Chatml_project_build.restore`. A lock is not reconstructed from a provided graph,
and changing a role does not merely replace its saved compiler-contract digest.

## Parent and child revisions

One declaration can retain several separately prepared role variants. The bundle
stores all variants and an explicit active selection for its current root.
Restoration requires the authentic current static contracts for that selection.
Inactive variants retain structurally validated metadata and sources but are not
exposed as executable prepared values.

A retained child changes its registration context and qualified bindings while
preserving the exact declaration body and range. Its artifact retains the full
required source inventory and parser roots, so restart does not need the parent's
live registry or original files. Adding a role variant requires an actually
prepared graph before publishing the child's artifact.

## Publication and limits

`Prompt_revision_builder.build` can receive a one-shot preparation hook over its
immutable parser capture. The hook may close over explicitly supplied host
capabilities; those capabilities are not retained in custody. Without the hook,
the builder still saves parser custody, including child roots, with zero prepared
programs. A present but invalid bundle is an error, never a reason to fall back to
legacy parsing. Artifacts without a bundle retain the explicit legacy path.

The revision identity includes the complete program manifest, including preserved
unknown metadata and the private content inventory. Child source fingerprints
include their selected and retained program custody too. Revision timestamps and
the revision ID itself do not enter this input hash.

Private program payloads use the existing artifact transaction and retention
machinery. They are not exposed as authored import paths. Current artifact policy
bounds complete program metadata to 256 KiB and jointly bounds authored files and
private payloads to 256 files / 8 MiB. Canonical prepared input has a separate
aggregate 8 MiB ceiling, counting shared input once per program. Selected graph
restoration also consumes one shared host-supplied work allowance, not a fresh
allowance for every program.

This custody layer supplies validated inputs for session/runtime integration.
Compilation, binding live host capabilities, and initializer execution remain the
responsibility of their existing runtime owners.
