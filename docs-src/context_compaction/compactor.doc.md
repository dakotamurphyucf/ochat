# Context_compaction.Compactor

Build a replacement canonical history from retained instructions, previous
reminders, and a new summary. This is lossy context reduction, not a guarantee
that every important detail survives. Export first when the original transcript
matters. See [TUI behavior](../guide/chat_tui.md#context-compaction-compact--current-behavior)
and [daemon history](../agent-server/sessions-and-workspaces.md#history-and-synchronization).

## Overview

The pipeline loads [configuration](config.doc.md) through Eio when an environment
is available, optionally grades relevance, and calls
[Summarizer](../lib/context_compaction/summarizer.doc.md). Relevance grading is
disabled by default; ordinary compaction does not add grader requests.

## Algorithm in Detail

1. Partition the input while preserving occurrence IDs and relative order.
   Retain all system/developer input messages and up to ten most recent
   previous user-role reminders whose first text part, after stripping whitespace,
   starts with `<system-reminder>`.
2. Send the history to the summarizer, excluding older reminders beyond that
   ten-entry bound. Opt-in relevance selection keeps tool call/output groups
   together and always includes policy-containing groups and the latest group.
3. After successful summarization, validate the resulting history against
   `context_limit` using the documented local token estimate. Reject an oversized
   result without allocating a reminder or replacing history. Otherwise allocate
   one fresh `History_entry.Id` and
   construct a **user-role** message containing `<system-reminder>...</system-reminder>`.
   The tag is text, not a system-role conversion.
4. Return retained instruction entries, retained reminders, and the new reminder
   in that order. Up to eleven reminders can therefore be present immediately
   after compaction. No synthetic first instruction is added for empty history.
5. Return an explicit error on failure; propagate Eio cancellation. The caller
   installs the replacement only on success.

## Public Interface

```ocaml
val compact_entries
  :  allocator:History_entry.Allocator.t
  -> env:Eio_unix.Stdenv.base option
  -> history:History_entry.t list
  -> (History_entry.t list, exn) result
```

The function does not itself persist, archive, or reset a session. It checks the
configured resulting-history estimate; this is not exact provider token accounting.
The host owns the commit and synchronization
boundary. The allocator advances for the new reminder; it is not a pure
identity-free string operation.

## Usage Examples

```ocaml
let compact_offline ~allocator ~history =
  Context_compaction.Compactor.compact_entries ~allocator ~env:None ~history
```

`env:None` explicitly chooses deterministic summary truncation. It tests wiring,
not semantic summary quality. With an environment and provider key, summary
requests can incur costs; online failures do not silently switch to a stub.

## Interaction with Other Modules

- [Legacy TUI compaction](../lib/chat_tui/app_compaction.doc.md) saves the current
  snapshot when available and applies operation-ID-matching results on the UI owner.
- Native/daemon `Session_actor` commits an archive reference and replacement
  history together, advances the compaction generation, and publishes
  `history.replaced`. Durable hosts sync the checksummed archive file before
  committing the reference; failure leaves original history intact.
- `session.get` exposes `archived_revisions`; `session.export` with one of those
  revisions retrieves the old history using the same authorization/redaction
  as current-history exports. See [session history](../agent-server/sessions-and-workspaces.md#history-and-synchronization).

## Known Limitations

Summarization is lossy and may require several provider requests. The reminder
count bound is independent of the configurable token estimate. Agent-session
archives are separate from journal/fallback-snapshot retention and remain until
the session is removed. They enable export, not automatic undo or rollback of
tools. Transient embedded archives do not outlive the transient session. Legacy
TUI snapshot persistence is not this archive feature; export first in that host.

Sources: [implementation](../../lib/context_compaction/compactor.ml),
[interface](../../lib/context_compaction/compactor.mli).
