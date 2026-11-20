# Durable recovery note shares

A source session grants plain-text read access to one recipient session and one exact note revision. Each reference names a private source ledger entry. Looking up a reference requires the original intended recipient and current transcript-read authority in that session. The source owns the grant and its revocation.

## Source management

`Session_factory.Recovery_note_shares` is the trusted host interface. Grant management requires the source's authenticated owner attachment and the `grant.manage` scope. Owner-bound sessions require the current live lease for that principal. Detached sessions check the authenticated principal against the durable creating principal. The host verifies the original source connection and the actual registered Store and Actor.

1. Read the current share ledger revision with `management_revision`. Obtain the exact note selection and catalog revision through an independently authorized source catalog read.
2. Call `create` with those two revisions, the exact note selection, one same-host durable recipient and an absolute expiry within seven days. Pass a client idempotency key. Return the resulting reference only through the authorized handoff.
3. Call `revoke` with the current ledger revision, the exact granted reference and a new idempotency key.

The source actor commits grant changes and body-free receipts together. A retry with the exact same authenticated writer, key and requested arguments replays a retained receipt. A different request with the same retained key conflicts. The ledger retains the latest 128 receipts. After eviction an old revision still prevents the original request from running again, and the caller must read current state before choosing a new intent. The ledger allocates each grant reference once, admits at most 128 active grants and limits the encoded state including extensions. The Store checks the actual journal and snapshot capacity before writing.

The host records a private incarnation stamp before a durable session first participates in sharing. Reopening the same canonical journal preserves that stamp. Recreating a session with the same external coordinates receives a fresh stamp. The grant also pins each actor's authority generation. A new branch or seed starts with its own note and share state. Grant targeting creates no destination note, source-catalog visibility, write authority or transitive rights.

## Read and disclosure

Resolve one explicit reference with `with_disclosure`. The Factory obtains the actual recipient witness before asking the source actor to select the grant. It retains the source entry across the callback, recovering an indexed inactive source without starting model execution. In the callback, call synchronous `Session_actor.Recovery_note_shares.materialize` immediately before disclosure. The disclosure witness carries no body and expires when the retained callback ends.

At materialization the actor checks both actual owner bindings, the original recipient authority, both incarnations and generations, the private grant entry and expiry, and the current exact note head. These checks run on the creating domain without another mailbox wait. An in-flight canonical write closes the publication path until the actor installs its committed state. Unavailable, expired, revoked and mismatched selectors produce the same error. A later edit, deletion or revoke stops future managed disclosure, including through a previously selected witness.

Protected delivery for a running operation uses `with_operation_disclosure`. It reauthorizes the actual current recipient operation from its physical original execution custody. The current operator must still have transcript-read scope, and operation replacement or completion closes the proof. A completed native invocation and a retained operation ID cannot supply that authority.
