(** Nonactivating recurrence inspection on the current authenticated host.
    A read may omit its attachment. Any reported actions are advisory. *)

val max_envelope_bytes : int

module Features : sig
  val inspect : string
end

module Request : sig
  type selector =
    | List
    | Get of Id.Recurrence.t
    | Status of Id.Recurrence.t
    | History of Id.Recurrence.t

  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t option
    ; selector : selector
    }

  val methods : string list
  val method_name : t -> string
  val params : t -> Jsonaf.t
  val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result
end

module Availability : sig
  type t =
    | Active
    | Stopped
    | Unavailable
end

module Basis : sig
  type t =
    { session : Session_ref.t
    ; generation : int
    ; state_revision : int64
    ; catalog_revision : int64
    ; lifecycle_revision : Session_lifecycle.Revision.t
    ; automatic : bool
    ; source : Run_source.t option
    ; availability : Availability.t
    }
end

module Definition : sig
  type pause_reason =
    | Operator_pause
    | Controller_stop
    | Source_review
    | Recovery_review
    | Target_review

  type retire_reason =
    | Operator_retire
    | Lifecycle_changed

  type control =
    | Enabled
    | Paused of pause_reason
    | Retired of retire_reason

  type target_kind =
    | Existing_session
    | Fresh_child

  (** [control] is effective controller control. Target details, input and
      private source pins remain with the target owner. *)
  type t =
    { id : Id.Recurrence.t
    ; controller : Session_ref.t
    ; definition_revision : int64
    ; control_revision : int64
    ; control : control
    ; source : Run_source.t
    ; birth_lifecycle_revision : Session_lifecycle.Revision.t
    ; creator : Id.Principal.t
    ; created_at : Timestamp.t
    ; schedule : Recurrence_schedule.t
    ; target_kind : target_kind
    ; policy : Recurrence_policy.t
    ; actions : Recurrence_control.Verb.t list
    }
end

module Planning : sig
  type reason =
    | Not_configured
    | Not_granted
    | Expired
    | Closing
    | Lifecycle_not_automatic
    | Controller_not_running
    | Source_unavailable
    | Controller_not_loaded
    | Controller_not_selected

  type t =
    | Available
    | Unavailable of reason
end

module Totals : sig
  (** Lifetime controller coverage. Retained ranges/attempts are finite. *)
  type t =
    { consumed : int64
    ; omitted : int64
    ; withdrawn : int64
    ; settled : int64
    }
end

module Definition_id : sig
  type t =
    { controller : Session_ref.t
    ; recurrence_id : Id.Recurrence.t
    ; revision : int64
    }
end

module Occurrence_id : sig
  type t =
    { definition : Definition_id.t
    ; slot : int64
    }
end

module Nominal : sig
  type t =
    { occurrence : Occurrence_id.t
    ; at : Timestamp.t
    }
end

module Outcome : sig
  type t =
    | Completed
    | Failed of Error.code option
    | Cancelled
    | Limited
    | Interrupted
end

module Run_observation : sig
  type status =
    | Admitted
    | Active
    | Waiting
    | Terminal of Outcome.t

  type t =
    { id : Id.Run.t
    ; revision : int64
    ; status : status
    ; retained_outcome : Outcome.t option
    }
end

module Target : sig
  (** Denied carries no target identifier or outcome. A visible session can
      still omit Run detail when its actual Run is unavailable or not visible. *)
  type t =
    | Denied
    | Unavailable
    | Visible of
        { session : Session_ref.t
        ; session_revision : int64
        ; run : Run_observation.t option
        }
end

module Attempt : sig
  type t =
    { number : int
    ; rejection : Error.code option
    ; uncertainty : Error.code option
    ; retry_resolution_recorded : bool
    ; admission_recorded : bool
    ; terminal_recorded : bool
    ; target : Target.t
    }
end

module Occurrence : sig
  type t =
    { nominal : Nominal.t
    ; attempts : Attempt.t list
    }
end

module Protected_creation : sig
  type t =
    { nominal : Nominal.t
    ; link_recorded : bool
    ; execution_recorded : bool
    }
end

module Status : sig
  type issue =
    | Zone_unavailable
    | Coverage_unavailable
    | Timing_exhausted
    | Capacity_saturated
    | Initial_delay_missed
    | Delay_nominal_missed
    | Execution_interrupted
    | Delay_successor_exhausted

  type t =
    { entry : Definition.t
    ; planning : Planning.t
    ; planning_revision : int64
    ; pending_count : int
    ; unresolved_count : int
    ; issue : issue option
    ; totals : Totals.t
    ; ready : Nominal.t list
    ; lookahead : Nominal.t option
    ; alarm_due_at : Timestamp.t option
    ; protected_creations : Protected_creation.t list
    ; occurrences : Occurrence.t list
    }
end

module History : sig
  type kind =
    | Skipped
    | Withdrawn

  type reason =
    | Expired
    | Policy_skip
    | Superseded
    | Pending_capacity
    | Control_withdrawal

  type range =
    { definition : Definition_id.t
    ; first_slot : int64
    ; last_slot : int64
    ; kind : kind
    ; reason : reason
    }

  type t =
    { entry : Definition.t
    ; totals : Totals.t
    ; retained_ranges : range list
    ; protected_creations : Protected_creation.t list
    ; occurrences : Occurrence.t list
    }
end

module Result : sig
  type 'a view =
    { basis : Basis.t
    ; can_create : bool
    ; value : 'a
    }

  type 'a outcome =
    | Available of 'a view
    | Unavailable of Session_ref.t

  type t =
    | List of Definition.t list outcome
    | Get of Definition.t outcome
    | Status of Status.t outcome
    | History of History.t outcome

  val session : 'a outcome -> Session_ref.t
  val method_name : t -> string
  val payload : t -> Jsonaf.t
  val of_method_and_result : method_:string -> result:Jsonaf.t -> (t, Error.t) result
end
