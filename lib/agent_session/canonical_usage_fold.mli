(** Pure accounting over one actual canonical ledger edge. No imported provider
    record, execution authority or second attempt registry is constructed here. *)
module Error : sig
  type t =
    | Scope_mismatch
    | Missing_bound_row
    | Conflicting_binding
    | Invalid_retirement
    | Counter_exhausted
  [@@deriving equal, sexp_of]
end

module Marks : sig
  type t =
    { attempt : int64
    ; turn : int64
    ; untracked_attempts : int64
    ; untracked_turns : int64
    }
  [@@deriving equal, sexp_of]

  (** Lifetime counts are checked sums of retained, retired and untracked rows.
      Changing generation does not reset these counters. *)
  val of_ledger : Inference_ledger.t -> (t, Error.t) result
end

module Plan : sig
  type t

  val settled : t -> Goal_usage.t
  val retired : t -> Inference_ledger.Handle.t list
end

(** Each supplied Handle must occur exactly once in the same-session before
    ledger. A retained row must keep that exact Handle. Only an actually ended
    row absent from the after-ledger is folded, once, into settled usage. The
    caller must separately validate the canonical update. This finite fold does
    not repeat complete carrier encoding for every accounting partition. Results
    preserve Handle order.
    This is shared by local Goal bindings and a delegated whole-ledger partition;
    it does not authenticate why those Handles belong to either partition. *)
val prepare
  :  Goal_usage.t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> handles:Inference_ledger.Handle.t list
  -> (Plan.t, Error.t) result
