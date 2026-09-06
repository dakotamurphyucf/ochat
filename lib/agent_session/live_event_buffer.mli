open! Core

(** Bounded recoverable event ring for one foreground operation. *)

type t

val create
  :  capacity:int
  -> session_id:Agent_protocol.Id.Session.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> t

val publish
  :  t
  -> anchor_sequence:int64
  -> timestamp:Agent_protocol.Timestamp.t
  -> kind:Agent_protocol.Event.Recoverable.kind
  -> payload:Jsonaf.t
  -> Agent_protocol.Event.Recoverable.t

val after : t -> int64 -> Agent_protocol.Event.Recoverable.t list
val latest_sequence : t -> int64
