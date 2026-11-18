(** Bounded, nonactivating Goal inspection. Callers can read without attaching.
    An optional current attachment informs advisory actions only. *)
module Features : sig
  val inspect : string
end

val max_envelope_bytes : int

module Request : sig
  type target =
    | All
    | One of Id.Goal.t
  [@@deriving sexp_of]

  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t option
    ; target : target
    }
  [@@deriving sexp_of]

  val methods : string list
  val method_name : t -> string
  val params : t -> Jsonaf.t
  val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result
end

module Availability : sig
  (** [Stopped] reflects the canonical observed Stopped lifecycle. [Active]
      requires a selected owner on an execution-enabled host. [Unavailable]
      can carry retained Goal data when no such owner is available. *)
  type t =
    | Active
    | Stopped
    | Unavailable
  [@@deriving equal, sexp_of]
end

module Run_observation : sig
  type status =
    | Admitted
    | Active
    | Waiting
    | Retained
    | Terminal
  [@@deriving equal, sexp_of]

  (** [Observed] carries the authorized Run revision from the same session
      snapshot. [Retained] is nonterminal Run evidence on a selected, stopped
      owner with execution enabled. It can supply a basis for an explicit cold
      Resume. [Terminal] can also be observed from an indexed snapshot.
      [Unavailable] makes no claim about termination, retention or authorization.
      Full Run details are available through the separately authorized [Run_query]. *)
  type t =
    | No_episode
    | Unavailable
    | Observed of
        { run_id : Id.Run.t
        ; revision : int64
        ; status : status
        }
  [@@deriving sexp_of]
end

module Usage : sig
  (** An aggregate partition. Counts cover the same attempt set in each metric.
      Actual, estimated, unknown and untracked contributions remain explicit. *)
  type t = private
    { attempts : int64
    ; untracked_attempts : int64
    ; components : Inference_query.Summary.components
    }

  val create
    :  attempts:int64
    -> untracked_attempts:int64
    -> components:Inference_query.Summary.components
    -> (t, Error.t) result
end

module Accounting : sig
  type counts =
    { turns : int64
    ; attempts : int64
    }

  (** Local and descendant partitions come from the Goal accounting owner.
      Pending counts reserve capacity but do not create observed usage. *)
  type t =
    { revision : int64
    ; local : Usage.t
    ; physical : Usage.t
    ; causal : Usage.t
    ; physical_unknown : bool
    ; historical_causal_unknown : bool
    ; admitted : counts
    ; pending : counts
    }
end

module Action : sig
  type t =
    | Revise_objective
    | Record_progress
    | Change_budget
    | Adopt_source
    | Hold
    | Abandon
    | Start
    | Resume
  [@@deriving compare, equal, sexp_of]
end

module Goal_summary : sig
  type state =
    | Open of Goal.State.admission
    | Abandoned of Goal.Reason.t
    | Achieved

  (** Objective metadata for a transcript-authorized operator. Run receipts,
      source input and achievement command evidence stay with the Run owner. *)
  type t = private
    { id : Id.Goal.t
    ; session : Session_ref.t
    ; revision : int64
    ; source : Run_source.t
    ; objective : Goal.Objective.t
    ; progress : Goal.Progress.t
    ; budget : Goal.Budget.t
    ; state : state
    ; achievement_pending : bool
    }

  val of_goal : Goal.t -> t
end

module Execution_blocker : sig
  type t =
    | Stopped
    | Source_unavailable
    | Source_changed
    | Accounting_unavailable
    | Usage_uncertain
    | Policy_hold
    | Active_run
    | No_current_run
    | Unavailable
  [@@deriving compare, sexp_of]
end

module View : sig
  type row =
    { goal : Goal_summary.t
    ; run : Run_observation.t
    ; accounting : Accounting.t option
    ; actions : Action.t list
    ; execution_blockers : Execution_blocker.t list
    }

  (** All data is from one captured authorized basis. Missing historical
      accounting stays absent. The source and actions are advisory observations
      and carry no read, write or execution authority. *)
  type t = private
    { session : Session_ref.t
    ; generation : int
    ; session_revision : int64
    ; catalog_revision : int64
    ; source : Run_source.t option
    ; availability : Availability.t
    ; can_create : bool
    ; goals : row list
    }

  val create
    :  session:Session_ref.t
    -> generation:int
    -> session_revision:int64
    -> catalog_revision:int64
    -> source:Run_source.t option
    -> availability:Availability.t
    -> can_create:bool
    -> goals:row list
    -> (t, Error.t) result
end

module Result : sig
  type outcome =
    | Available of View.t
    | Unavailable of Session_ref.t

  (** An available single-Goal lookup can contain zero rows for an absent Goal.
      Failure to authorize the session remains an error. *)
  type t =
    | List of outcome
    | Get of outcome

  val session : outcome -> Session_ref.t
  val method_name : t -> string
  val payload : t -> Jsonaf.t
  val of_method_and_result : method_:string -> result:Jsonaf.t -> (t, Error.t) result
end
