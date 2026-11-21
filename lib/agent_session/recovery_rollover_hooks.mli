open! Core

(** Durable authored dispositions owned by one rollover receipt. A retained
    claim is evidence of possible execution. It never permits running a
    handler again, including after restart. Authorization and physical source
    custody remain with the actor. *)
module P = Agent_protocol

module Hint_selection = Recovery_rollover_hint_ref.Selection

type phase =
  | Before
  | After
[@@deriving equal]

module Claim : sig
  type t = private
    { execution : P.Id.Moderator_execution.t
    ; predecessor : P.Id.Operation.t
    ; window : P.Recovery_window.Scope.t
    ; revision : int64
    ; observer : P.Invocation.observer
    }
  [@@deriving equal]

  val create
    :  execution:P.Id.Moderator_execution.t
    -> predecessor:P.Id.Operation.t
    -> window:P.Recovery_window.Scope.t
    -> revision:int64
    -> observer:P.Invocation.observer
    -> (t, P.Error.t) result
end

type continuation = Chat_response.Context_rollover_hook.continuation =
  | Continue
  | Wait
  | Finish
[@@deriving equal]

type before =
  | Veto
  | Proceed of
      { hints : Hint_selection.t option
      ; continuation : continuation option
      }

type failure =
  | Failed
  | Authority_lost
  | Interrupted
[@@deriving equal]

type 'a disposition =
  | Pending
  | Claimed of Claim.t
  | Completed of Claim.t * 'a
  | Stopped of Claim.t * failure

type change =
  | Claim of phase * Claim.t
  | Complete_before of P.Id.Moderator_execution.t * before
  | Complete_after of P.Id.Moderator_execution.t * continuation
  | Stop of phase * P.Id.Moderator_execution.t * failure

type t

val equal : t -> t -> bool
val sexp_of_t : t -> Sexp.t
val claim : 'a disposition -> Claim.t option
val phase : change -> phase
val empty : t
val before : t -> before disposition
val after : t -> continuation disposition
val apply : t -> change -> (t, P.Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
val change_to_json : change -> Jsonaf.t
val change_of_json : Jsonaf.t -> (change, P.Error.t) result
