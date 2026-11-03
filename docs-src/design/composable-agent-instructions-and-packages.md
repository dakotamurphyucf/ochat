# Composable instructions and source packages

This document records the accepted architecture for skills, project guidance,
built-in prompt assets and reusable ChatML source packages. It describes the
implementation contract, not a claim that all features are available. The
existing source capture, project resolver and native-tool owners remain
authoritative. Feature guides must identify the actually installed resources and
supported operations.

## Instruction discovery and activation

The runtime host explicitly configures and admits instruction roots. There is no
automatic home-directory lookup, ambient working-directory search or traversal
above an admitted root. A root identifier is a configured identity, not permission
to turn a supplied string into an arbitrary host path. Current principal and
source permissions still govern discovery, reading, activation and disclosure.

Project guidance uses `AGENTS.md` from the selected root through the ancestors of
an explicit target scope, in root-to-leaf order. Sibling directories and other
roots are excluded. More specific guidance may refine earlier prose, but cannot
change host permissions or override higher-priority instructions. Applying a
different scope is explicit; a tool invocation does not silently change the
active project instructions.

Skills are direct `<slug>/SKILL.md` children of a selected skills root. Discovery
does not recursively crawl arbitrary trees. Stable identities include the root
and portable member path, so equal slugs in different roots remain distinct.
Display names do not become lookup identities. Project guidance and skills can
be selected independently.

Four operations have different meanings:

| Operation | Result | Effect on active instructions |
| --- | --- | --- |
| Discover or refresh | Bounded candidate metadata and exact observed revision identities | None |
| Load | Exact content matching the requested candidate revision | None |
| Activate or replace | Actor-owned selection of retained content and explicit scope | Changes the active selection atomically |
| Deactivate | Actor-owned removal from the active selection | Removes guidance from subsequent context construction |

Discovery may need to read bounded file bytes to compute a real digest. That is
not activation or permission to disclose the body. Loading a stale revision
returns an explicit stale result; it does not silently return newer content.
Refreshing candidates never replaces already loaded or active revisions.

Activation keeps exact content and provenance. Resume reconstructs guidance from
those retained bytes, even if the original file changed or disappeared. Missing
or corrupt captured content is an error, never an invitation to reread live
files. Current host policy is revalidated before use and disclosure. Capturing
content preserves identity; it does not preserve an obsolete permission grant.

Skill metadata is inert authoring data. The discovery implementation must choose
and document a bounded supported metadata grammar before exposing it. Familiar
filenames do not imply support for every YAML construct or another product's
installation hooks. Unsupported syntax must produce useful diagnostics. Metadata
cannot register tools, grant paths, execute commands or activate itself. Plain
project `AGENTS.md` does not require a metadata header.

## Ownership and context integration

Reuse retained, no-follow source readers and explicit host root admission.
`Chatml_local_source` already supplies joined directory/read operations with
bounded batches and byte limits. `Authoring_project_access_configuration`
separates configured roots from principal read grants. Discovery must not bypass
these owners with a second ambient filesystem reader.

An observed collection is not an atomic snapshot of a concurrently edited
filesystem. Each loaded member carries exact bytes and a validated revision.
Candidate counts, per-file bytes, aggregate bytes, path lengths, metadata and
scan work all have explicit bounds. Expected failures distinguish denied,
missing, stale, malformed and exhausted limits without disclosing hidden host
paths. Cancellation must retain the existing Eio behavior.

The session actor owns the active instruction selection and mutation receipts.
Retained content belongs in the existing universal document/content custody,
with immutable loaded revisions and bounded references. There is no separate
writable conversation or instruction store. The provider-context builder
materializes the selected guidance once in deterministic order; activation does
not repeatedly append copies to conversation history.

Restore and existing context reconstruction preserve active instructions.
Automatic context-window sizing and the later local context-management system
are separate work. This feature must qualify the current reconstruction hooks
without claiming that future context policies already exist.

Native instruction operations must be registered and selected through the real
tool-capability path. Read operations return admitted metadata/content;
activation changes use actor revisions and receipts. ChatML helpers call those
same operations. Operator clients project the same state rather than owning a
parallel active-instruction list.

## Built-in prompt assets

Keep the existing `<doc src="builtin:ID" />` mechanism. The installed
`Builtin_instructions` catalog already provides validated stable IDs, exact
embedded bytes, hashes and bounded enumeration without filesystem or network
fallback. Captured ChatMD content retains the selected rendered bytes through
save and restore, including inactive child definitions.

The [installed instruction catalog](../guide/builtin-instructions.md) contains
`chatml`, `goals`, `recurrence`, `skills`, `project-instructions`,
`context-recovery`, `packages` and `lifecycle`. These concise assets describe policy
and the use of capabilities actually supplied by the host; they do not provision
native controls or activate skills. Context recovery supplies local/session and
explicit-sharing policy only, with no context-management functionality or storage
claim. Future context-management execution still requires its own integration.
A new installation affects new explicit captures; it cannot rewrite a saved
session's instructions. Unknown IDs remain useful errors, not authored paths or
URLs. Exact captured bytes/hash identify an edition; no new version syntax is
introduced.

Built-in prompt instructions are distinct from the longer source-pinned
`Authoring_sources`/`Authoring_corpus` reference documents. Existing
`Authoring_package_file` packages are bounded prose conventions; they are not an
executable dependency format. Keep their roles separate while linking truthful
help and compatibility metadata to the actual installed resources.

## Reusable executable source packages

A capability package uses the ordinary existing formats:

- `chatml.project.json` selects modules, interfaces, Local/Git dependencies and
  package environments. Its `capabilities` field expresses required tool names;
  compiler/target requirements use the existing checked contract.
- ChatMD explicitly selects native tool declarations and script environments.
- ChatMD document imports explicitly select built-in or authored instructions.

Importing a module never registers a native implementation or grants tools.
Requirements are checked against the genuine selected implementations and host
authority. The shared project acquisition/build/capture pipeline remains the
only executable resolver; no new package language, dependency source kind,
registry, version solver or implicit fetch is required.

Repository examples alone do not make libraries available to an installed OChat
user. Provide one immutable installed source inventory containing the exact
reviewed project/module/interface/template bytes, with validated portable paths,
member hashes and a complete resource digest. Derive discovery/help metadata
from that same inventory. Reuse the existing build-time embedding pattern rather
than adding an independently mutable catalog.

Expose bounded list/read access and an explicit export route in an installed
command. Export copies a complete ordinary source tree to a caller-selected new
directory; subsequent authoring uses unchanged Local dependencies. It does not
prepare or execute the code, install native tools, alter existing exports or
change captured sessions. Existing destinations and collisions must be rejected;
partial failure cannot be reported as a complete export. The filesystem adapter
must use explicit Eio capabilities and reviewed no-follow/exclusive-write
behavior. Keep this command separate from the later unified CLI restructuring.

Pure lifecycle composition and its moderator-only Run adapter are separate
ordinary projects, because the latter requires a genuine moderator surface.
Component state stays in explicit typed fields. Authored order, distinct
component obligations, schema/config migration and root completion are explicit;
the helper does not become another scheduler or mutable handler registry.
Feature-specific goal, recurrence and instruction wrappers contribute their
actual source resources to this same installed inventory as they are implemented.

## Implementation and acceptance boundaries

The following are independently reviewable outcomes; none alone proves the
complete capability library:

1. Bounded lifecycle composition and actual native Run staging, with checked
   module interfaces and two ordinary component dependencies.
2. Installed source discovery/read/export and real preparation of the exported
   complete tree outside the repository.
3. Admitted instruction discovery and exact loaded content custody.
4. Actor-owned activation, selected native dispatch, provider-context integration
   and captured resume behavior.
5. Reviewed built-in assets and feature-specific helpers for actual capabilities.
6. Shared operator projections and representative composed workflows through the
   real actor/host, followed by combined documentation and release qualification.

Qualification should use independent bounded reference models and informative
examples rather than duplicated case matrices. Essential properties include:

- Directory enumeration order cannot change qualified identities or guidance
  order; sibling and out-of-root paths never enter the result.
- Discovery/load does not activate; stale loads do not substitute content;
  refresh does not replace active bytes.
- Symlink/path replacement, permission revocation, cancellation and byte/count
  limits are enforced by the real retained reader and host admission.
- Activation/replace/deactivate commits atomically and retries preserve original
  receipts. Restored active content survives source replacement/deletion without
  repeated history insertion or current-catalog fallback.
- Installed resource members form a complete portable project closure. Exporting
  to unrelated fresh locations yields the same content identity; missing members
  and incompatible compiler/tool requirements fail honestly.
- Helpers stage at most their selected action, preserve peer obligations and
  defer actual Run admission, worker joining and terminal evidence to the host.

The final workflow evidence must include real native dispatch, actor persistence,
source replacement/resume and installed resource delivery. A pure helper test,
successful metadata lookup or research document is not that evidence.

## Original operator custody

Authenticated connection ingress, explicit Run admission and authenticated
runtime construction carry the original operator proof into the existing actor
execution owner. Principal-only hosts remain unbound. A stored creating
principal, replayed invocation or reopened session cannot reconstruct that proof.
Joined native callbacks borrow through their explicit executor and exact
invocation; retaining the returned object cannot extend the callback, worker,
initialization owner or original authentication lifetime. Authenticated runtime
construction checks the same proof before construction and at actual worker
installation; initialization completion and queued Start admission also recheck
it. Expiry closes an uninstalled returned runtime without undoing completed
external effects or preventing owner cleanup. Worker return revokes
its custody before terminal persistence, including when that persistence fails.

A single deferred authenticated input may transfer its original custody at
actual adoption; expired custody rejects before adoption is saved. An ambiguous
batch of independently admitted inputs carries no selected operator proof.
Explicit proof-bearing Run ownership is separate from that conservative input
batch rule. A selected background launch transfers private original custody to
its acknowledged job owner only after durable publication; an aborted or
unselected launch publishes none. Job execution still requires the actual claim,
attempt, generation and existing stop policy.

Authenticated initialization uses the existing actor initialization scope. Its
explicit borrow and acknowledged model-job transfer preserve the same lifetime
rules. This does not add native admission to a top-level authored `Tool.call`,
install a native instruction tool, grant source access, activate retained
instructions or materialize provider guidance. Those consumers must use the
actual explicit owner port and perform their own current source/effect checks.
Independent authored model recipes do not implicitly become agent-conversation
instruction consumers.

## Existing implementation reference points

- `lib/builtin_instructions/` and `lib/chatmd_content_capture/`:
  installed prompt assets and captured document content.
- `lib/chatmd_prompt_content/` and prompt content custody tests:
  exact retained content and replay.
- `lib/chatml_project/`, `lib/chatml_project_build/`,
  `lib/chatml_project_prepare/`: executable declarations, acquisition, checked
  graph construction and capture-only restoration.
- `lib/chatml_local_source/`: retained filesystem source access.
- `lib/instruction_content/` and `lib/agent_server/instruction_sources.*`:
  admitted discovery, exact loaded revisions and immutable captured instruction
  content. See [Admitted instruction sources](../guide/admitted-instructions.md)
  for the bounded metadata profile and current library-only boundary.
- `lib/agent_session/instruction_capture.*`, `instruction_selection*` and
  `instruction_transition.*`: pure bounded retained selections and receipts in
  the universal session document, with exact snapshot/payload custody and
  internal transactional admission. This foundation is inert; it supplies no
  authenticated native activation port or provider-context insertion.
- `lib/capability_sources/` and `lib/capability_source_export/`: the immutable
  installed ordinary-source inventory and explicit create-only Eio export.
  `ochat libraries` exposes list, exact member read and export without preparing
  or executing those sources.
- `lib/authoring_sources/`, `lib/chat_response/authoring_package_file.ml`:
  installed long-form references and prose-only authored conventions.
- `lib/agent_session/` and `lib/agent_server/`: session mutations, selected native
  capability ownership, source admission and operator views.

References describe the audited baseline. Implementation should keep this design
and the affected public interfaces/source guides aligned as the concrete APIs
are delivered.
