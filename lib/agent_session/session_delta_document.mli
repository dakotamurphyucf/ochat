open! Core

(** Immutable journal change document. The admitted complete child documents and
    unknown fields remain attached to the value and are never rebuilt for hashing. *)
type t

val value : t -> Session_delta.t
val document : t -> Document_schema.Document.t

val decode
  :  limits:Document_schema.Limits.t
  -> Document_schema.Document.t
  -> (t, Document_schema.Error.t) result

val create
  :  Session_delta.t
  -> limits:Document_schema.Limits.t
  -> state_document:(Session_state.t -> Session_state_document.t)
  -> (t, Document_schema.Error.t) result

module Transaction_metadata : sig
  (** A timestamp from the validated timestamp type and checked nonnegative
      counters, applied by the transaction owner after admitting the complete
      unstamped state. This is independent of the
      store's hash chain, owner and event-range validation. *)
  type t

  (** [last_event_sequence = None] preserves the post-delta event counter. *)
  val create
    :  updated_at:Agent_protocol.Timestamp.t
    -> revision:int64
    -> transaction_sequence:int64
    -> last_event_sequence:int64 option
    -> (t, Document_schema.Error.t) result
end

(** Apply current transition semantics and adopt captured child preservation
    context into the mutable state owner. Conflicts fail before any write. Optional
    transaction metadata changes only its four owned scalar fields; original
    native validation and complete bounds admission precede those changes, and
    the complete stamped document passes bounds and current domain validation. *)
val apply
  :  t
  -> ?transaction_metadata:Transaction_metadata.t
  -> limits:Document_schema.Limits.t
  -> Session_state_document.t
  -> (Session_state_document.t, Document_schema.Error.t) result
