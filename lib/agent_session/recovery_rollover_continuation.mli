(** Durable, body-free continuation state for one committed foreground rollover.
    A process must separately prove current authorization and the exact selected
    preparation before it can submit a transition using these data. *)
open! Core

module P = Agent_protocol

type hold =
  | Capacity
  | Capacity_unknown
  | Authority_unavailable
  | Required_input_unavailable
  | Preparation_interrupted
[@@deriving equal, sexp_of]

type finish =
  | Script_finished
  | Cancelled
[@@deriving equal, sexp_of]

type phase =
  | Awaiting
  | Waiting
  | Preparing of P.Id.Operation.t
  | Held of hold
  | Admitted of
      { operation : P.Id.Operation.t
      ; ordinal : int64
      }
  | Finished of finish
[@@deriving equal, sexp_of]

type t = private
  { owner : P.Id.Operation.t
  ; predecessor : P.Id.Operation.t
  ; phase : phase
  }
[@@deriving equal, sexp_of]

module Hook : sig
  type decision =
    | Continue
    | Wait
    | Finish

  type view =
    { window : P.Recovery_window.Scope.t
    ; checkpoint : [ `Checkpointed | `Archive_only ]
    }
end

type action =
  | Continue
  | Wait_for_wake
  | Finish_followup
  | Cancel_followup

type change =
  | Wait
  | Begin of P.Id.Operation.t
  | Hold of
      { operation : P.Id.Operation.t
      ; reason : hold
      }
  | Admit of
      { operation : P.Id.Operation.t
      ; ordinal : int64
      }
  | Finish of finish
  | Stop_preparation of
      { operation : P.Id.Operation.t
      ; reason : finish
      }

val create
  :  owner:P.Id.Operation.t
  -> predecessor:P.Id.Operation.t
  -> (t, P.Error.t) result

(** Compare exact phase and operation on both sides of admission. A committed
    attempt is terminal for this obligation, including uncertain delivery. *)
val apply : t -> change -> (t, P.Error.t) result

val is_pending : t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
val change_to_json : change -> Jsonaf.t
val change_of_json : Jsonaf.t -> (change, P.Error.t) result
