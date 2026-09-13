open Core

(** Bounded metadata for rediscovery, not retained documentation or authority.
    The owner must persist this index in trusted session state and select what
    may be disclosed under the current policy before rendering a pointer. *)
type t

type limits =
  { max_receipts : int
  ; max_bytes : int
  }

val default_limits : limits
val empty : ?limits:limits -> scope:string -> unit -> (t, Agent_protocol.Error.t) result
val scope : t -> string
val receipts : t -> Authoring_presence.receipt list
val truncated : t -> bool
val encoded_bytes : t -> int

(** Observe chronological, host-labelled canonical/archive entries. Keep recent
    fitting receipts; deduplicate by history identity and reject conflicting
    provenance. Payload lookalikes, modified payloads and new redacted entries
    do not add receipts. Rediscovery pointers are not indexed as new reads.
    Explicit redaction removes any prior retained receipt.
    Re-observation moves a valid receipt to the newest position. Historical
    receipts absent from this batch remain eligible for retention.

    The index stores entry identities and provenance only, never message bodies.
    Old context/policy/source identities remain unchanged for presence inspection;
    retaining a receipt does not mark its content present or current. Byte limits
    cover the complete JSON envelope. Truncation is sticky and explicit; this is
    bounded working memory, not a complete retrieval audit. No history is mutated. *)
val remember
  :  ?limits:limits
  -> t
  -> history:Agent_protocol.History.entry list
  -> (t, Agent_protocol.Error.t) result

(** Explicit history deletion forgets metadata for the deleted occurrences as
    well. This differs from compaction, where receipts remain useful pointers. *)
val forget : t -> Agent_protocol.History.Id.t list -> t

val to_json : t -> Jsonaf.t

(** Restore only from trusted persisted state. Strict version/field, identity,
    scope, provenance, count and encoded-size checks run before returning an
    index. Serialized JSON supplied by a model is not trusted provenance. *)
val of_json
  :  ?limits:limits
  -> scope:string
  -> Jsonaf.t
  -> (t, Agent_protocol.Error.t) result
