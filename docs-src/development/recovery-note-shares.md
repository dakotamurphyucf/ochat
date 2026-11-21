# Durable recovery note shares

A source session grants plain-text read access to one recipient session and one exact note revision. Each reference names a private source ledger entry. Looking up a reference requires the original intended recipient and current transcript-read authority in that session. The source owns the grant and its revocation.

## Source management from a connected client

Open the stock [stdio gateway](../agent-server/tutorials/stdio-client.md#gateway-to-a-detached-daemon)
over the daemon's Unix socket or use the same protocol over authenticated HTTP.
Select `recovery.note_share_control` in `protocol.initialize`. Keep one
authenticated connection open. The source session owner needs transcript
access, `grant.manage` and its live `owner_read_write` attachment on this
connection. The detached session's creating principal must match. For
owner-bound sessions the current live owner lease supplies that proof.

The source must already contain a note at a known virtual path. The
[foreground note tools](recovery-note-tools.md) can create it. Use the source
session's real generation and writer attachment from `session.create` or
`session.attach`. Obtain the exact host-qualified recipient reference through
an intentional handoff. The recipient must be a different durable session on
this daemon.

Send one complete JSON line at a time. Replace capitalized values with
actual IDs, and use the source's actual generation in all three requests.

```json
{"jsonrpc":"2.0","id":"inspect","method":"session.recovery.notes.inspect","params":{"source":{"server_id":"SERVER","session_id":"SOURCE"},"attachment_id":"WRITER","expected_generation":0,"path":"handoff/review"}}
```

The reply carries `basis.share_revision`, `basis.catalog_revision` and, for
an exact match, `selection.id` and `selection.revision`. Revisions and local
note IDs are decimal strings. Omit `path` to get the current revisions alone.
Inspection can return one selection and never lists recipient or grant
metadata. A source with no catalog reports revision zero.

Use those exact values for the grant, plus an absolute RFC 3339 expiry
up to seven days in the future. Replace the illustrative expiry below
with a valid value at the time of the request.

```json
{"jsonrpc":"2.0","id":"create","method":"session.recovery.notes.share","params":{"source":{"server_id":"SERVER","session_id":"SOURCE"},"attachment_id":"WRITER","expected_generation":0,"expected":{"share_revision":"0","catalog_revision":"1"},"selection":{"id":"1","revision":"1"},"recipient":{"server_id":"SERVER","session_id":"RECIPIENT"},"expires_at":"2026-11-22T12:00:00Z","key":"review-share-1"}}
```

The body-free reply contains the new `share_revision` and the complete
`reference` needed by the recipient's selected `recovery_note_read` with
`selection.kind: "shared"`. Pass that reference privately to the intended
recipient. Its source route, nonce and grant ID identify the ledger record.
The recipient's existing authority and current source grant still govern
disclosure. The share operation sends no message and adds no note to the
recipient's catalog.

To revoke, inspect again without a path. Use the current
`basis.share_revision`, the exact reference previously returned, and a new
application key.

```json
{"jsonrpc":"2.0","id":"revoke","method":"session.recovery.notes.revoke_share","params":{"source":{"server_id":"SERVER","session_id":"SOURCE"},"attachment_id":"WRITER","expected_generation":0,"expected_share_revision":"1","reference":{"source":{"server_id":"SERVER","session_id":"SOURCE"},"nonce":"SOURCE_NONCE","id":"1"},"key":"review-revoke-1"}}
```

The reply contains the committed share revision and that same reference.
Subsequent protected reads stop after revocation. The shared client module
`Agent_client.Recovery_windows.Note_shares` provides typed `inspect`,
`create` and `revoke` functions on the existing connection.

Use the same application key and exact original grant or revoke arguments
after losing an acknowledgment. On reconnect, initialize again and obtain a
current owner attachment as the same principal. The writer attachment is
authorization for this attempt. The original application intent remains
unchanged. A retained matching receipt is factual even if its grant has
since expired or been revoked. The live same-host recipient must still
match the original incarnation for a grant retry.

The daemon records the method and redacted outcome in its ordinary audit.
Note paths, plaintext, recipient and reference coordinates, and idempotency
keys stay out of that audit and ordinary public session projections. The
authorized command response is the handoff. Keep it private.

## Trusted host interface

`Session_factory.Recovery_note_shares` supplies the source host port. It
verifies the original source connection and the actual registered Store and
Actor. Use the same revision checks, exact reference and idempotency keys as
the connected protocol.

The source actor commits grant changes and body-free receipts together. A retry with the exact same authenticated writer, key and requested arguments replays a retained receipt. A different request with the same retained key conflicts. The ledger retains the latest 128 receipts. After eviction an old revision still prevents the original request from running again, and the caller must read current state before choosing a new intent. The ledger allocates each grant reference once, admits at most 128 active grants and limits the encoded state including extensions. The Store checks the actual journal and snapshot capacity before writing.

The host records a private incarnation stamp before a durable session first participates in sharing. Reopening the same canonical journal preserves that stamp. Recreating a session with the same external coordinates receives a fresh stamp. The grant also pins each actor's authority generation. A new branch or seed starts with its own note and share state. Grant targeting creates no destination note, source-catalog visibility, write authority or transitive rights.

## Read and disclosure

Resolve one explicit reference with `with_disclosure`. The Factory obtains the actual recipient witness before asking the source actor to select the grant. It retains the source entry across the callback, recovering an indexed inactive source without starting model execution. In the callback, call synchronous `Session_actor.Recovery_note_shares.materialize` immediately before disclosure. The disclosure witness carries no body and expires when the retained callback ends.

At materialization the actor checks both actual owner bindings, the original recipient authority, both incarnations and generations, the private grant entry and expiry, and the current exact note head. These checks run on the creating domain without another mailbox wait. An in-flight canonical write closes the publication path until the actor installs its committed state. Unavailable, expired, revoked and mismatched selectors produce the same error. A later edit, deletion or revoke stops future managed disclosure, including through a previously selected witness.

Protected provider delivery for a running operation uses `prepare_operation_delivery`. It reauthorizes the actual current recipient operation from its physical original execution custody. The current operator must still have transcript-read scope, and operation replacement or completion closes the proof. A completed native invocation and a retained operation ID cannot supply that authority.

A protected submission holds one source-session lease per distinct session in deterministic order. After transport selection, the host rechecks the original recipient, grant and note revision. It holds the source's Registry and writer reservation through the HTTP request or WebSocket application-frame write. A conflicting source mutation can retry after custody ends. The five-second custody limit covers waiting for the source and writing the request. Return, failure and cancellation release custody. A write that may have reached the provider is ineligible for replay.
