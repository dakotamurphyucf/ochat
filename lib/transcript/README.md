# Neutral transcript read views

`Transcript` is pure presentation state. It depends on Core, JSON, History_entry
and the shared document validator; it imports no provider, protocol IDs or Eio.
OCH-52 canonical history and storage version1 remain unchanged. A stream event,
draft or public projection never authorizes canonical append or tool execution.

`Header` derives directly from neutral semantic payload. Developer remains
Developer. Unknown items have their actual kind, and an orphan partial item has
no header until an observation supplies one. There is no guessed assistant role
or content position. Source, attempt, item and part keys are separate types.
Nested descriptors retain the actual parent scope and any actually known host
call entry ID; a provider call alias is separate metadata.

`Stream` admits the entire JSON envelope under supplied limits and retains it
immutably, including uninterpreted envelope fields. Its typed view describes
known observations. `Item_finalized` must carry the exact committed entry and a
matching actual host ID/header. The producer emits it only after owning admission
succeeds; provider item-done and response-completed observations are provisional.
The committed header/name may replace provisional classification after host
preparation, while ordinary live refinements remain strict.

`Admission` supplies the shared presentation profile: depth 160, one million
object fields and two million nodes, with a caller-selected compact JSON byte
ceiling (16 MiB by default). This leaves room around a canonical payload's depth
129 limit and preserves aggregate snapshot bounds when several payloads share an
envelope. Provider wire and canonical payload admission remain independent.

`Draft` is an immutable bounded reducer. It keeps current read views rather than
an event log, rejects conflicts and changes after completion, and marks missing
prefixes explicitly. Complete replacement repairs the replaced text without
inventing an unseen earlier prefix. Known numeric part positions order parts;
unavailable positions use stable keys. Removing an admitted item retires only
that item's draft, leaving sibling parts/sources intact.

Retained charges include scoped descriptors, part descriptors, escaped text,
origin/outcome, immutable finalized payloads and opaque observations. These are
compact encoded component bytes with conservative completeness flags, rather
than an OCaml heap-size estimate. Each component uses the configured JSON
structural limits; explicit scope/item/part/unknown counts bound aggregate
structure. Cached escaped text charges let an append check the candidate limit
before joining strings. Caller byte allowances can further reduce the configured
bound, including for exact no-op observations. A client additionally counts
receipts, future events and activity against its own total budget; retaining a
second encoded string for equality is unnecessary.

Expected admission/conflict errors leave the receiver unchanged. New typed
producer observers propagate unexpected failures, backtraces and cancellation.
Previously protected legacy observers keep their existing delivery contract.
During cancellation, strict tool terminal delivery is skipped; the original
cancellation/backtrace propagates and the authoritative operation terminal fences
activity without claiming an unavailable tool outcome. Legacy protected observers
still receive Finished Cancelled. Non-cancellation failure plus a second terminal
observer failure preserves both errors through Exn.protect.
Durable history and a fresh admitted snapshot provide repair after an interrupted
live projection; partial reconnect state is not a second canonical history.
