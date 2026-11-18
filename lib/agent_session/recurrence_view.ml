open! Core
module P = Agent_protocol
module E = Recurrence_entry
module F = Recurrence_definition

module Target = struct
  type t =
    | Existing_session
    | Fresh_child
  [@@deriving equal, sexp_of]
end

type t =
  { id : P.Id.Recurrence.t
  ; controller : P.Session_ref.t
  ; definition_revision : int64
  ; control_revision : int64
  ; control : E.control
  ; source : P.Run_source.t
  ; birth_lifecycle_revision : P.Session_lifecycle.Revision.t
  ; creator : P.Id.Principal.t
  ; created_at : P.Timestamp.t
  ; rule : Recurrence_rules.Rule.t
  ; target : Target.t
  ; policy : Recurrence_policy.t
  }
[@@deriving sexp_of]

let of_entry entry ~effective_control =
  let definition = E.current entry in
  { id = E.id entry
  ; controller = E.controller entry
  ; definition_revision = Recurrence_identity.Definition.revision (F.key definition)
  ; control_revision = E.control_revision entry
  ; control = effective_control
  ; source = F.source definition
  ; birth_lifecycle_revision = F.birth_lifecycle_revision definition
  ; creator = F.creator definition
  ; created_at = F.created_at definition
  ; rule = F.Schedule.rule (F.schedule definition)
  ; target =
      (match Recurrence_target.view (F.target definition) with
       | Existing_session _ -> Target.Existing_session
       | Fresh_child _ -> Target.Fresh_child)
  ; policy = F.policy definition
  }
;;
