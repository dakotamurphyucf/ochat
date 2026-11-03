# Admitted instruction sources

OChat's lower instruction libraries discover and retain local project guidance
and skills under explicitly admitted source roots. They provide OCaml APIs for
host integrations. They do not install a native instruction tool, activate
instructions or insert content into a provider request. Those operations require
separate actor-owned integration.

## Explicit roots and read permission

`Agent_server.Instruction_sources` borrows an existing authenticated
`Authoring_source_host.Root`. The source host owns the retained, no-follow reader
and checks both the original principal and the current request principal before
and after yielding work. Bind an explicit administrative `Instruction_content.Root_id`
to that root. The ID is a stable label, not a host path or a read permission.

There is no default home or working directory, automatic search across roots,
upward search above a root or permission granted by document text. The same skill
slug in two roots has two identities: each identity includes its root label and
portable member path. An ephemeral discovery receipt also belongs to its exact
live root binding; an equal label cannot rebind it to a different owner.

A host must assign unique labels in its configured root map and supply genuine
principal read grants. Existing project access configuration couples its grants
to project selections; integrations must not invent an executable selection to
obtain instruction reads. A read-only instruction configuration and actor
installation remain separate integration work. Loaded content alone never
preserves an obsolete permission grant: activation and disclosure must check the
current host policy.

## Discovery, load and explicit refresh

`discover_project` accepts an explicit directory scope relative to the selected
root. It reads `AGENTS.md` at the root and then each ancestor directory through
the target, in root-to-leaf order. Missing exact `AGENTS.md` leaves are optional;
a missing parent, denied read, symlink, changed file or malformed governing
content fails the operation. Siblings and other roots never enter the chain.
`AGENTS.md` is always plain UTF-8 Markdown; it has no metadata requirement and a
leading `---` is ordinary text.

`discover_skills` enumerates one selected root and observes only direct
`<slug>/SKILL.md` files. It does not crawl deeper directories or load referenced
files, scripts or assets. A slug is 1–64 ASCII lowercase letters, digits and
single hyphens, begins and ends with a letter or digit, and has no consecutive
hyphens. Candidate order is portable member order, independent of filesystem
enumeration order. Invalid individual candidates produce bounded issues.
Revocation and aggregate, entry or work exhaustion fail the complete discovery.
Names excluded by the source owner's portable-path rules remain excluded.

Discovery reads bounded bytes to compute the full file digest and metadata, then
returns metadata and exact revision identities without disclosing the body.
`load` makes a fresh authorized read of the candidate's exact member. A changed
digest returns `Stale`, an absent source fails, and neither substitutes newer or
unrelated content. Loading returns exact immutable original bytes, including
frontmatter and line endings. It does not activate them.

Refreshing means explicitly discovering again. It produces new observations;
old loaded values remain unchanged. A collection is a sequence of individual
observations, not an atomic filesystem snapshot. The retained reader rejects
observed replacement or changes during an individual read, within its documented
filesystem guarantees.

## Supported skill metadata

The [Agent Skills specification](https://agentskills.io/specification) describes
YAML frontmatter with required name and description fields. OChat supports the
bounded string profile below. This is partial interoperability, not a complete
YAML or Agent Skills implementation. Unsupported syntax produces a diagnostic;
OChat does not repair or silently reinterpret it.

```markdown
---
name: code-review
description: Review changes and report concrete correctness findings.
license: MIT
compatibility: Requires a local source checkout.
allowed-tools: Read
metadata:
  owner: engineering
custom-note: "Retained as an inert annotation"
---

Read the requested changes and explain each actionable finding.
```

The opening delimiter must be the exact first line `---`, and the closing line
must also be exactly `---`. LF and CRLF are accepted. UTF-8 BOM, invalid UTF-8
and NUL bytes are rejected. When a header exists, `name` and `description` are
required and nonempty. The name must exactly equal the validated directory slug.
Descriptions allow at most 1024 Unicode scalar values and compatibility strings
at most 500, with additional byte limits.

Scalar values can be single-line plain strings, single-quoted strings using
`''` for an apostrophe, or JSON-compatible double-quoted strings. Values are
strings; plain `123` and `true` are not converted to numbers or booleans. Decoded
control characters are rejected. Quote text containing `: ` or ` #`; plain
values with those ambiguous YAML constructs are rejected. Blank lines and
full-line comments are accepted. Duplicate fields fail. Keys use ASCII
lowercase letters, digits, `_` and `-`, up to 64 bytes.

Optional `license`, `compatibility` and `allowed-tools` are inert annotations.
A `metadata:` map accepts exactly one level of scalar fields with two-space
indentation. Other top-level scalar fields are retained with an `Unknown_field`
warning. Block scalars (`|`, `>`), collections, deeper mappings, anchors, aliases,
tags and executable hooks are unsupported. In particular, `allowed-tools` grants
no tools or execution permission.

An entirely headerless Markdown skill is an OChat extension: discovery uses the
slug as its name, no description, and a `Headerless_skill` warning. This fallback
does not claim conformance to the external specification.

Headers have fixed limits of 8 KiB, 128 physical lines and 32 fields; decoded
metadata is at most 8 KiB and each scalar at most 4 KiB. The file, aggregate,
entry and cooperative work limits are separately validated. The standard
instruction profile is 128 files, 256 KiB per file, 4 MiB aggregate bytes and one
million cooperative steps. Existing source-host/project limits can only narrow
these bounds. Failed reads conservatively charge their attempted byte allowance,
except exact absent leaves. No partial discovery is published on exhaustion.

## Immutable capture and restore

`Instruction_content.Snapshot` captures an ordered set of loaded revisions in an
`ochat.instruction-content` version 1 universal document and the existing
content-addressed payload container. Duplicate qualified IDs reject; identical
physical bytes may share one payload. Snapshot order preserves guidance order.
The storage container's existing 256-payload, 8 MiB payload and 256 KiB metadata
limits remain in force; this is no additional mutable instruction store.

Restore verifies payload membership and digests, identity, byte counts, original
metadata and body offsets. Missing or corrupted captures fail. Compatible unknown
document and row fields survive an exact storage round trip. It never reads
current files, so exact retained content survives source edits and deletion.
Restored bytes describe custody, not activation or source permission.

## Durable selection foundation

`Agent_session.Instruction_selection` stores an inert, optional
`ochat.instruction-selection` version 1 subdocument inside the sole session
state. It is separate from editable conversation history. Legacy absence stays
absent on unrelated writes. A stored selection does not install tools, activate
provider guidance or preserve a historical source grant. Genuine native
activation, current source-use authorization and request-context insertion are
separate host integration work.

The pure `Instruction_capture.project` constructor checks a single root and
scope, with `AGENTS.md` members in root-to-target order. An explicitly selected
empty chain is distinct from no project selection; a real host must still prove
that discovery was complete. `Instruction_capture.skills` keeps distinct
qualified skills in explicit order. Replacing a group or removing individual
skills changes references; original manifests, exact bytes and compatible
unknown fields remain retained. Identical bytes share one payload across the
retained captures. Refreshing discovery cannot change this selection.

`Instruction_command.create` builds a consistency-only request with session,
generation, expected selection revision and idempotency key. The internal
`Instruction_transition.prepare` produces a checked plan over the actual session
carrier. It authenticates no principal and accepts no grant as data. A genuine
host owner must authorize before calling it, save through `Session_persistence`,
and publish the candidate state and receipt only after save acknowledgement.
Every fresh accepted command, including a semantic no-op, retains one immutable
principal/key/request-digest receipt. An exact retry returns that same receipt
without another write; a reused key with different input or stale fresh command
fails. Deactivation changes references without requiring new instruction bytes.

The fixed session selection profile is 64 selected files and 256 KiB of selected
raw contents. Its finite lifetime limits are 128 retained captures, 512 file
memberships, 8 MiB of cumulative membership bytes, 4 MiB of deduplicated payloads,
2 MiB of encoded capture manifests, 128 receipts, and 8 MiB for the complete
encoded subdocument. Restore has a ten-million-checkpoint aggregate work bound;
child snapshot limits still apply. Complete encoding bounds include compatible
unknown data. The enclosing session's potentially smaller byte, field and node
limits also apply, with independent component reservations added together.

While a project or skill remains selected, selection and whole-session document
admission reserve a final Clear receipt and positive document encoding/counter
headroom, including an empty selected
project chain. Unrelated commands and new no-op receipts cannot consume that
reserve. Receipts and captures are never evicted. Saturation rejects fresh
commands; continuing requires a new session or a future explicit maintenance
policy. Exact retries remain available. `Instruction_status` exposes only stored
revision, selection counts and retained counts, without bodies, paths or root
labels; it does not claim current authorization or activation.


This reserve covers the selection and session document profiles. The complete
transaction and journal frame have independent limits, so document admission
alone cannot guarantee that a later Clear can be persisted under an arbitrary
journal configuration. Before exposing actual activation, the genuine host
integration must also preflight Clear against its actual complete transaction
and journal limits, including the audit and previous-hash envelope. That gate
belongs to native activation integration; this foundation provides no public
activation endpoint.
