(** Body-free inspection and exact-revision control of the current successor.
    Reinspect after an uncertain reply before making another decision. *)
open! Core

module Basis = Recovery_rollover_control.Basis

val feature : string
val method_name : string

type action =
  | Continue
  | Wait
  | Finish
  | Cancel

type selection =
  | Inspect of Session_ref.t
  | Apply of Basis.t * action

type t =
  { attachment_id : Id.Attachment.t
  ; selection : selection
  }

val session : t -> Session_ref.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result

module Reply : sig
  type phase =
    | Idle
    | Awaiting
    | Waiting
    | Preparing
    | Held_capacity
    | Held_capacity_unknown
    | Held_authority
    | Held_input
    | Held_interrupted
    | Admitted
    | Finished
    | Cancelled
  [@@deriving equal, sexp_of]

  type t =
    { basis : Basis.t
    ; phase : phase
    }
  [@@deriving equal, sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

val validate : Reply.t -> request:t -> (unit, Protocol_error.t) result
