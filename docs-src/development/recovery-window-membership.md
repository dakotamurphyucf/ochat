# Recovery window membership

`Agent_protocol.Recovery_window` is the immutable ordered membership of an OChat
context window. An actor or storage reader can use this type to relate an exact
citation to an ordinal. Resolving the cited payload and checking who may read it
are separate operations on the session's current recovery state.

A scope contains the server-qualified session, source generation and opaque
window ID. A citation adds an existing history occurrence ID and its exact
version. A canonical citation uses the existing history content revision. An
effective citation also records the SHA-256 of its neutral member projection
and the origin of that projection. Moderator insertions and replacements retain
the revision that established their overlay namespace and the actual change ID.
A replacement also carries its canonical base revision. Use the captured source
revision when change provenance is unavailable. Derive these coordinates from
the admitted source. The type alone cannot attest that a provider received it.

For example, an admitted window may list a moderator replacement of `x@0` at
ordinal 0 and its canonical `x@0` at ordinal 1. Looking up the canonical
citation returns 1. Reading the other ordinal returns the effective citation.
Revising the text under the same occurrence ID creates a distinct version.
Repeating the exact same member within the same manifest fails validation.
The same member can legitimately occur in another window. Each window has its
own order.

`create` validates that every citation belongs to the supplied scope. `at`
uses zero-based positions. `ordinal` checks the entire scope and member, and
returns an option. The JSON representation stores the shared scope once and
the ordered member list separately. Decoding revalidates identities and
uniqueness and rebuilds the ordinal lookup. A manifest admits at most 65,536
members and 8 MiB of encoded JSON. An occurrence namespace is limited to
4,096 UTF-8 bytes. A storage owner with tighter limits applies those before
publication.

The protocol encodings describe recognized coordinates. A persistent session
document owns its extension fields, archive references and visibility decisions.
[Recovery design](../design/local-context-recovery.md)

### Reading a text part

Search and reads use half-open byte offsets in the original UTF-8 part. A
search for `fox` in the part `🦊 FOX` returns the range `[5,8)`. Reading
that range yields `FOX`. The highlight in a search snippet is relative to
the snippet, which may start later than byte zero of the original part.

`Agent_protocol.History_text` admits a validated part and keeps its original
index even when earlier parts were hidden. A range read defaults to 4 KiB
and accepts a budget of up to 16 KiB. If more text remains, use the returned
next offset as the start of the next read, with the same end offset and source
revision. A budget too small for a single scalar returns a resource-limit
error. The caller checks visibility and the revision each time it discloses
a result. Conversation search continues to admit its existing user and
assistant text and refusal parts.
