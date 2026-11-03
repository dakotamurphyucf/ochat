# Admitted instruction sources

OChat discovers and retains local project guidance and skills under explicitly
admitted source roots. Five explicitly selected native tools read, refresh,
activate, deactivate and inspect a session's instruction selection. The actor
saves exact selected bytes and receipts before acknowledging a mutation. Reading
or importing an instruction document does not activate it or grant any tool.

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

A host must supply genuine principal read grants. The existing
`authoring_project_access` configuration accepts nonempty `read_grants` with an
empty `selections` list for read-only use. This grants no Project Build or
execution. The installed instruction facade uses the same retained source owner.
Its root labels are domain-separated digests of the exact resolved configured
root spelling: reordering preserves labels, while remapping to another spelling
cannot silently authorize an old capture. These administrative labels are not
cryptographic inode identities. Same-path replacement follows the existing
fresh source-host capability policy. Loaded bytes never preserve an obsolete
grant: activation, disclosure and each provider submission check current policy.

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
absent on unrelated writes. A stored selection does not install tools or preserve a historical source grant.
The native host integration checks authentic current execution and source-use
authority before activation and provider submission.

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
headroom, including an empty selected project chain. Unrelated commands and new no-op receipts cannot consume that
reserve. Receipts and captures are never evicted. Saturation rejects fresh
commands; continuing requires a new session or a future explicit maintenance
policy. Exact retries remain available. `Instruction_status` exposes only stored
revision, selection counts and retained counts, without bodies, paths or root
labels; it does not claim current authorization or activation.

The persistence owner also preflights the future native Clear transaction
against its actual transaction and journal-frame limits, including the previous
hash, retained unknown content and the complete typed session.updated/status event.
The future event uses maximal checked host counter/timestamp widths; its validated
summary clears active counts and retains captures plus the final receipt. Native
instruction receipts belong to the selection transaction; they do not create server request-idempotency audit
records. A smaller writer profile can reject activation before any selection is
acknowledged. A future operator entry point must separately qualify its genuine
server-request audit envelope against that profile.

## Explicit native selection

Declare only the tools the agent needs:

```xml authoring=root
<tool name="instructions_refresh"/>
<tool name="instructions_read"/>
<tool name="instructions_activate"/>
<tool name="instructions_deactivate"/>
<tool name="instructions_status"/>
```

Selecting these names supplies no source roots or permissions. The invoking
session must have an installed source facade, explicit root grants and genuine
current operator execution. Native admission is specific to the selected effect:
a read callback cannot authorize activation or Clear.

1. Call `instructions_refresh` with `{"selection":null}` to list admitted root
   labels. Refresh an explicit selection using
   `{"kind":"skills","roots":["ROOT"],"scope":null}` or
   `{"kind":"project","roots":["ROOT"],"scope":"src"}`. The empty project
   scope means the root. The result contains bounded metadata, paths and digests.
2. Call `instructions_read` with `selection` and ordered `members`, each containing
   `root`, `member` and `sha256` from discovery. A project descriptor must include
   the complete ordered root-to-scope AGENTS.md chain; a skill descriptor selects
   the explicit ordered skills. Read returns exact bytes and `capture_id`.
3. Call `instructions_activate` with that same descriptor, `capture_id`, a fresh
   `key`, and `expected_revision` as a canonical decimal string from status.
   Project activation replaces project guidance; skills activation replaces the
   selected skill group. Missing, changed or incomplete content fails atomically.
4. Use `instructions_status` with `{}` for the stored selection revision and
   counts. `instructions_deactivate` takes `kind` (`all`, `project` or `skills`),
   `members` (null for all/project), `expected_revision` and `key`. Clear requires
   session mutation authority but works after source read permission is revoked.

Successful fresh no-op mutations retain idempotency evidence too. Exact retained
retries preserve their original receipt without another write; changing input
under the same principal/key conflicts. An exact activation retry may use its
already retained capture after live files disappear, but still requires current
root/use policy. A fresh key must make a genuine fresh read. Refresh never
silently replaces the active edition.

## Agent context and lifetime

For each actual agent-conversation provider request, OChat projects the retained
project chain followed by selected skills once into a fixed User-level text
prefix. File text is untrusted guidance. Authored System/Developer instructions,
request settings and canonical conversation history remain unchanged. No extra
history IDs are allocated, and repeated requests do not append copies.

Ordinary root turns, selected joined Agent/Fork conversations, background Agent
conversations and admitted agent initialization use their genuine physical
owners. Named Model.call recipes, literal one-off prompts, standalone developer
contexts and pre-admission ChatMD content-construction Agent prompts are explicit
separate prompts and do not inherit session guidance. This exclusion remains
sticky through recipe-to-Agent reentry. A newly created session owns its own
selection.

Authenticated Start can lend one pending lifecycle event for that runtime. Only
that event's exact acknowledged no-input follow-up intents may transfer custody;
Start is never a fallback for later unrelated inputs or mixed queued callers.
Duplicate Start cannot refresh or upgrade custody. Normal completed-worker
compaction uses an exact pre-retirement, completion-only handoff, consumed only
after terminal save acknowledgement. Compactor requests carry the same guidance
and reserve its framed local text-token estimate before history allocation.
This estimate is not provider-exact multimodal accounting or M-CTX functionality.

Physical reopen restores exact selected bytes but no operator authority. A new
genuine authenticated admission and current source grants are required; missing
or changed live files never replace retained active bytes. Automatic daemon
restart with durable schedules still needs an explicit configured host admission
policy. Source revocation before a new effect denies it. Revocation after a
completed provider call or acknowledged mutation does not rewrite that truthful
completion or receipt.

## Shared selection inspection

Current session snapshots expose `instruction_status` as the same validated
counts-only summary produced by `Instruction_selection.summary`. It records the
stored revision, whether project guidance is selected, and counts of project
files, skills, retained captures and command receipts. It contains no document
bodies, paths, root labels or source permission. Reading it never activates or
refreshes instructions and does not prove that the current host can use them.

The common session get/attach and durable client projection support this optional
field under transcript-read permission. Legacy, unavailable and redacted values
are omitted; a supported empty selection has an explicit zero summary. On the
existing `session.updated` event, a present summary replaces prior counts and an
absent extra preserves them. A replacement snapshot supplies its own value.
If it accompanies a present event extra, both must contain the same summary; a
replacement with no summary cannot be overridden by a present extra. Explicit
null, contradictory and malformed known values are rejected. Source-use and native
mutation integrations publish one final `session.updated` after each newly
acknowledged selection mutation, including a fresh no-op receipt. An exact
retained retry emits no new status event. A rejected save publishes neither a
receipt nor a status change. The projection adds no mutation route or separate
status store.

## Checked request helpers and Lifecycle composition

The installed `instruction-selection` source bundle includes the ordinary pure
`Instruction_requests` module and a checked example that imports the existing
`Lifecycle` library:

```sh
ochat libraries show instruction-selection
ochat libraries show instruction-selection -member examples/libraries/instruction-requests/src/instruction_requests.chatmli
ochat libraries export instruction-selection -parent /existing/workspace -directory instructions-copy
```

Its entry is `examples/learning/instruction-selection/agent.chatmd`. Configure
that prompt path and explicitly admit the exported sibling `examples/libraries`
directory for project preparation, as described in
[installed libraries](chatml-installed-libraries.md). This preparation grant is
separate from the principal's read-only instruction roots. The example needs a
genuine authenticated agent-conversation callback; it does not require a workflow
Run merely to inspect counts. Export and import neither select instructions nor
register tools or grant access.

`Instruction_requests` constructs the existing native JSON request shapes from
typed project/skill selections and ordered root/member/digest descriptors. It
rejects more than 64 roots or members before mapping. The actual native decoder
still validates identifiers, portable paths, capture completeness, canonical
revision text, keys and current authority. For example, after obtaining the exact
read descriptor and capture ID, authored code can construct an activation:

```chatml
match Instruction_requests.activate(descriptor, capture_id, revision, key) with
| `Error(error) -> Task.fail("The instruction request exceeds its bounded shape.")
| `Ok(request) -> Tool.call("instructions_activate", request)
```

The caller must explicitly choose that action. Reading a descriptor does not
activate it. Preserve an acknowledged command's exact key and input for retries;
use a new key and current stored revision for a distinct command.

The example keeps separate `instructions` and `review` state fields, composes two
named Lifecycle components in explicit order, and calls the selected
`instructions_status` tool at turn end. Its checkpoint retains the returned
counts alongside the inspection flag; this says nothing about current source permission or successful
activation. Pure Lifecycle decisions remain author policy. The example stages no
Run action and does not turn component completion into host root completion.
