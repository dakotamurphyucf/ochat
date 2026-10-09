(** Bounded archival custody for disposition records evicted by explicit retention.
    Stores only exact expired private records, never the canonical session history.
    The session's existing owner retains authority throughout durable publication. *)
module Reference : sig
  type t [@@deriving equal, sexp]

  val operation_id : t -> Agent_protocol.Id.Operation.t
  val to_jsonaf : t -> Jsonaf.t
  val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t
end

type t

val create
  :  Session_state.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> pending_revision:Agent_protocol.Pending_input.Revision.t
  -> records:Pending_disposition_document.t list
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) result

val reference : t -> Reference.t
val document : t -> Document_schema.Document.t
val filename : Reference.t -> string

(** Complete file and directory sync must precede journal publication of eviction.
    Closed/foreign session handles fail before I/O; cancellation propagates. *)
val write
  :  t
  -> env:Eio_unix.Stdenv.base
  -> handle:Agent_store.Session_store.Handle.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) result
