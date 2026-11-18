(** Pure local-source accounting plans for one stable Goal identity. The
    canonical ledger alone owns provider observations. [Stored] separately encodes
    bounded durable claims and resolves exact canonical handles. Neither layer
    performs IO, grants execution permission or owns another provider event store. *)
module Usage = Goal_usage

module Error : sig
  type t =
    | Ledger of Inference_ledger.Error.t
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

module Scope : sig
  type t [@@deriving equal, sexp_of]

  val goal_id : t -> Agent_protocol.Id.Goal.t
  val session : t -> Agent_protocol.Session_ref.t
end

module Binding : sig
  type t

  val equal : t -> t -> bool
  val handle : t -> Inference_ledger.Handle.t
  val run_id : t -> Agent_protocol.Id.Run.t
  val source : t -> Agent_protocol.Run_source.t
end

module Admission : sig
  type t =
    | Attempt of
        { episode : Agent_protocol.Goal.Episode.t
        ; handle : Inference_ledger.Handle.t
        ; tracking : Inference_ledger.tracking
        }
    | Turn of
        { episode : Agent_protocol.Goal.Episode.t
        ; operation : Agent_protocol.Operation.t
        ; handle : Inference_ledger.Turn_handle.t
        ; tracking : Inference_ledger.tracking
        }
end

type t

val equal : t -> t -> bool

(** Initial new-scope state, zero charges and fences at the supplied actual
    ledger. The actor must reject duplicate scope creation; this is not a reset
    API for an existing scope. Binding limit is positive, at most 256, default 256.
    Metadata edits, pause/resume and source adoption never reset this value. *)
val create
  :  ?max_bindings:int
  -> Agent_protocol.Goal.t
  -> ledger:Inference_ledger.t
  -> (t, Error.t) result

val scope : t -> Scope.t
val revision : t -> int64
val admitted_turns : t -> int64
val admitted_attempts : t -> int64
val untracked_turns : t -> int64
val untracked_attempts : t -> int64
val settled_tracked_attempts : t -> int64
val bindings : t -> Binding.t list

(** Fresh read of settled totals plus exact retained bound rows. Revisions replace
    complete contributions, never add deltas. Missing bound rows fail: they must
    first be folded in the retirement owner transaction. Unknown, Untracked and
    Estimated remain labelled independently for each component. A closed goal
    can still receive canonical retained usage corrections. Token overflow is
    reported, not wrapped; this observed evidence is never a hard spend cap. *)
val usage : t -> ledger:Inference_ledger.t -> (Usage.t, Error.t) result

module Plan : sig
  type accounting = t

  type admission_disposition =
    | Not_requested
    | Counted
    | Already_retained
  [@@deriving equal, sexp_of]

  type t

  val expected_accounting_revision : t -> int64
  val before_ledger_revision : t -> int64
  val after_ledger_revision : t -> int64
  val admission_disposition : t -> admission_disposition
  val retired_bindings : t -> Binding.t list
  val candidate : t -> accounting

  (** Ephemeral shared immutable canonical candidates, not durable copied rows.
      The actor publishes candidate accounting and canonical after-ledger in one
      persistence/CAS transaction, then acknowledges actual admission. *)
  val before_accounting : t -> accounting

  val before_ledger : t -> Inference_ledger.t
  val after_ledger : t -> Inference_ledger.t
end

(** One validated proposal reconciles bound-row removals and optional one actual
    admission. Each removed ended row's final before-ledger contribution is
    folded once before its binding disappears. Unchanged live usage is read from
    the canonical after-ledger; duplicate/stale usage never mutates this account.
    Source/session, ordinal/counter progress, current Goal episode and available
    actual Handle/operation facts are checked. New Attempt/Turn admission requires
    the episode's source to equal the adopted Goal source and its generation to
    equal the after-ledger generation. Ended old/closed goal retirements remain
    valid with admission=None; old bound rows still accept retained corrections.
    Old admission plans cannot recount.

    These checks do NOT authenticate actual source/operation attribution, confer
    authority, or enforce one-goal-per-handle across partitions. An Untracked
    Turn_handle exposes only operation ID/generation, no session accessor,
    ordinal or retained row: counts cannot prove fresh qualified host operation
    identity. The actual operation owner must prove that admission.

    The actor must preflight integrated storage quota and protected future-field
    compatibility, including binding retirement, BEFORE canonical effects/dispatch;
    then commit all affected partitions (including closed old goals) atomically.
    This module provides no durable codec or permission to erase unknown data.
    Counter/ordinal exhaustion rejects before goal-owned dispatch. *)
val plan
  :  t
  -> goal:Agent_protocol.Goal.t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> admission:Admission.t option
  -> (Plan.t, Error.t) result

(** Context-free compact durable claims. These values contain no Handle and do
    not authorize attribution or execution. Resolution requires exact canonical
    membership; stored strings never allocate a missing ledger occurrence. *)
module Stored : sig
  type accounting = t
  type t

  val capture : accounting -> t
  val equal : t -> t -> bool
  val goal_id : t -> Agent_protocol.Id.Goal.t
  val session : t -> Agent_protocol.Session_ref.t
  val revision : t -> int64
  val admitted_attempts : t -> int64
  val admitted_turns : t -> int64
  val untracked_attempts : t -> int64
  val untracked_turns : t -> int64
  val ordinals : t -> int64 list
  val run_bindings : t -> (Agent_protocol.Id.Run.t * Agent_protocol.Run_source.t) list
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

  val resolve
    :  t
    -> goal:Agent_protocol.Goal.t
    -> ledger:Inference_ledger.t
    -> (accounting, Error.t) result

  val validate_update : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

  val validate_transition
    :  t
    -> incoming:t
    -> before:Inference_ledger.t
    -> after:Inference_ledger.t
    -> (unit, Agent_protocol.Error.t) result

  (** Shape/capacity probes only, never decoded as accounting evidence. *)
  val retirement_projection : t -> ordinal:int64 option -> Jsonaf.t
end
