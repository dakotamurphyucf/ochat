(** Local observations about one provider attempt attached to a prepared item. *)
open! Core

type delivery =
  | Definitely_not_submitted
  | Possibly_submitted
  | Response_started

type phase =
  | Allocated
  | Running
  | Returned of delivery
  | Interrupted of delivery

type t =
  { ordinal : int64
  ; tracked : bool
  ; phase : phase
  }

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result
