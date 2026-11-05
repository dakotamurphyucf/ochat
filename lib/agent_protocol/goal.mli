(** Pure persistent objective domain. This module performs no IO and grants no
    execution authority. The actor authenticates all supplied Run evidence. Goal
    identity is its stable accounting scope; objective edits never replace it.
    Construction, mutation and decoding enforce a 128 KiB owned JSON projection
    at depth 64; independently valid Run proofs can exceed this embedding bound. *)
module Objective = Goal_text.Objective

module Progress = Goal_text.Progress
module Reason = Goal_text.Reason
module Budget = Goal_budget
module Episode = Goal_episode
module Achievement_request = Goal_achievement.Request
module Achievement = Goal_achievement
module Capacity = Goal_capacity

module Hold : sig
  type t =
    | Operator_pause
    | Needs_input of Reason.t
    | Budget_gate of Budget.Dimension.t
    | Usage_uncertain of Inference.Observation.Usage.Component.t
    | Deadline_reached
    | Source_review
  [@@deriving equal, sexp_of]
end

module Closure : sig
  type t =
    | Achieved of Achievement.t
    | Abandoned of Reason.t
  [@@deriving equal, sexp_of]
end

module State : sig
  type admission =
    | Enabled
    | Held of Hold.t
  [@@deriving equal, sexp_of]

  type t =
    | Open of admission
    | Closed of Closure.t
  [@@deriving equal, sexp_of]
end

type t [@@deriving equal, sexp_of]

(** New goals start held for explicit source review, with empty progress and
    revision zero. Source identity does not authorize running the goal. *)
val create
  :  id:Id.Goal.t
  -> session:Session_ref.t
  -> creator:Id.Principal.t
  -> source:Run_source.t
  -> objective:Objective.t
  -> budget:Budget.t
  -> (t, Error.t) result

val id : t -> Id.Goal.t
val session : t -> Session_ref.t
val creator : t -> Id.Principal.t
val source : t -> Run_source.t
val objective : t -> Objective.t
val progress : t -> Progress.t
val budget : t -> Budget.t
val state : t -> State.t
val episode : t -> Episode.t option
val achievement_request : t -> Achievement_request.t option

(** Optional immutable terminal association resolving the latest request. Its
    disposition is derived from actual Run evidence, not a writable status. *)
val request_terminal : t -> Run_receipt.t option

val request_settlement : t -> Episode.Settlement.t option

(** At most one immutable predecessor proof is retained. Other evidence remains
    Run-owned under its existing retention policy. *)
val previous_episode_settlement : t -> Episode.Settlement.t option

val revision : t -> int64

(** An unresolved achievement request freezes the objective whose completion
    was requested. Progress, holds, budgets and abandonment remain available. *)
val revise_objective : t -> Objective.t -> (t, Error.t) result

val record_progress : t -> Progress.t -> (t, Error.t) result
val change_budget : t -> Budget.t -> (t, Error.t) result
val hold : t -> Hold.t -> (t, Error.t) result

(** Pure policy change only. Actor authorization must reject clearing operator
    holds by a model and must recheck actual execution/source custody. *)
val resume : t -> (t, Error.t) result

(** Replacing an episode requires matching terminal evidence for the old one.
    An unresolved achievement request must first settle or be explicitly retired.
    When that Run already has a retained request settlement, the predecessor must
    reuse that exact settlement, including the original admission association. *)
val bind_episode
  :  t
  -> Episode.t
  -> previous:(Run.t * Run_receipt.t) option
  -> (t, Error.t) result

(** Enable and bind a new episode atomically, advancing the Goal revision
    exactly once. All [bind_episode] association and predecessor requirements
    apply. This pure transition grants no admission or execution authority. *)
val activate_episode
  :  t
  -> Episode.t
  -> previous:(Run.t * Run_receipt.t) option
  -> (t, Error.t) result

val request_achievement : t -> Achievement_request.t -> (t, Error.t) result

(** Retain adverse terminal association, preserving any existing hold (including
    an operator pause); enabled work becomes held for review. Completed evidence
    must use [achieve], never a fabricated failed request disposition. *)
val retire_request : t -> run:Run.t -> receipt:Run_receipt.t -> (t, Error.t) result

(** Settle the exact pending request, or atomically retain its embedded request
    and achievement when the actual Finish completed in the same transaction. *)
val achieve : t -> Achievement.t -> (t, Error.t) result

val abandon : t -> Reason.t -> (t, Error.t) result

(** Monotone installation/generation change; retains old episode/request sources
    verbatim and leaves the goal held. No source authority is reconstructed. *)
val adopt_source : t -> Run_source.t -> (t, Error.t) result

(** Structural update fence for persistence adoption: bounded projection, retained
    evidence, current-episode association for newly introduced requests, exact
    checked revision advance, identity and source invariants. The actor must
    authenticate evidence against its authoritative retained Run/receipt index. *)
val validate_update : t -> incoming:t -> (unit, Error.t) result

(** Storage-accounting upper bound, not domain state or authority. For an open
    goal retaining a request, reserves all bounded mutable metadata and receipt
    slots needed for completion, retirement and next-episode binding. Request
    admission rejects when this exceeds [Capacity.max_projection_bytes]. The
    actor must preflight the complete [Goal_document] carrier before staging the
    corresponding Run Finish; this pure projection cannot charge unknown data. *)
val reserved_capacity : t -> (Capacity.t, Error.t) result

val to_json : t -> Jsonaf.t

(** Required nullable fields stay distinct from malformed absent fields. Decoding
    validates stored relationships; it does not certify host receipt authenticity. *)
val of_json : Jsonaf.t -> (t, Error.t) result
