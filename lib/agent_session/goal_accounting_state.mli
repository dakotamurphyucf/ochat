(** Internal shared representation; neither these facts nor stored claims grant
    runtime authority. Public Goal_accounting keeps its domain types abstract. *)
open! Core

module P = Agent_protocol
module G = P.Goal
module L = Inference_ledger
module O = Inference.Observation
module Usage = Goal_usage
module Counter = Goal_accounting_counter

module Error : sig
  type t =
    | Ledger of L.Error.t
    | Scope_mismatch
    | Conflicting_binding
    | Missing_bound_row
    | Invalid_retirement
    | Invalid_admission
    | Stale_plan
    | Counter_exhausted
    | Binding_capacity
    | Invalid_field of
        { field : string
        ; reason : string
        }
  [@@deriving equal, sexp_of]
end

val ledger : ('a, L.Error.t) result -> ('a, Error.t) result
val counter : ('a, Counter.error) result -> ('a, Error.t) result
val add : int64 -> int64 -> (int64, Error.t) result
val increment : int64 -> (int64, Error.t) result
val sum : int64 list -> (int64, Error.t) result

module Scope : sig
  type t =
    { goal_id : P.Id.Goal.t
    ; session : P.Session_ref.t
    }
  [@@deriving equal, sexp_of]

  val goal_id : t -> P.Id.Goal.t
  val session : t -> P.Session_ref.t
end

module Binding : sig
  type t =
    { handle : L.Handle.t
    ; run_id : P.Id.Run.t
    ; source : P.Run_source.t
    }

  val equal : t -> t -> bool
  val handle : t -> L.Handle.t
  val run_id : t -> P.Id.Run.t
  val source : t -> P.Run_source.t
end

type t =
  { scope : Scope.t
  ; revision : int64
  ; admitted_turns : int64
  ; admitted_attempts : int64
  ; untracked_turns : int64
  ; settled : Usage.t
  ; bindings : Binding.t Int64.Map.t
  ; max_bindings : int
  ; last_attempt_ordinal : int64
  ; last_turn_ordinal : int64
  ; last_ledger_revision : int64
  }

val scope : t -> Scope.t
val revision : t -> int64
val admitted_turns : t -> int64
val admitted_attempts : t -> int64
val untracked_turns : t -> int64
val untracked_attempts : t -> int64
val settled_tracked_attempts : t -> int64
val bindings : t -> Binding.t list
val equal : t -> t -> bool

type marks =
  { attempt : int64
  ; turn : int64
  ; untracked_attempts : int64
  ; untracked_turns : int64
  }

val fold_error : Canonical_usage_fold.Error.t -> Error.t
val marks : L.t -> (marks, Error.t) result
val check_session : t -> L.t -> (unit, Error.t) result
val ended : O.Attempt_record.t -> bool
