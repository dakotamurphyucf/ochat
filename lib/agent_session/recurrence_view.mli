(** Safe metadata projection. Captured artifacts, input content, workspace paths,
    native selectors and runtime configuration are deliberately not included.
    Effective control is computed under the read owner's genuine lifecycle
    observation; this value never establishes execution authority. *)
module Target : sig
  type t =
    | Existing_session
    | Fresh_child
  [@@deriving equal, sexp_of]
end

type t = private
  { id : Agent_protocol.Id.Recurrence.t
  ; controller : Agent_protocol.Session_ref.t
  ; definition_revision : int64
  ; control_revision : int64
  ; control : Recurrence_entry.control
  ; source : Agent_protocol.Run_source.t
  ; birth_lifecycle_revision : Agent_protocol.Session_lifecycle.Revision.t
  ; creator : Agent_protocol.Id.Principal.t
  ; created_at : Agent_protocol.Timestamp.t
  ; rule : Recurrence_rules.Rule.t
  ; target : Target.t
  ; policy : Recurrence_policy.t
  }
[@@deriving sexp_of]

val of_entry : Recurrence_entry.t -> effective_control:Recurrence_entry.control -> t
