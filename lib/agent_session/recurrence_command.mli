(** Original bounded operator intent. Data and captured selectors grant no
    authority; the actor rechecks original caller, current source and genuine
    lifecycle Current before metadata persistence. *)
module Capture : sig
  type t =
    { schedule : Recurrence_definition.Schedule.t
    ; target : Recurrence_target.t
    ; policy : Recurrence_policy.t
    }
end

module Mutation : sig
  type t =
    | Create of Agent_protocol.Id.Recurrence.t * Capture.t
    | Edit of Agent_protocol.Id.Recurrence.t * Capture.t
    | Enable of Agent_protocol.Id.Recurrence.t
    | Pause of Agent_protocol.Id.Recurrence.t
    | Retire of Agent_protocol.Id.Recurrence.t

  val id : t -> Agent_protocol.Id.Recurrence.t
  val needs_source : t -> bool
end

type t = private
  { session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; source : Agent_protocol.Run_source.t option
  ; expected_lifecycle_revision : Agent_protocol.Session_lifecycle.Revision.t option
  ; expected_catalog_revision : int64
  ; expected_control_revision : int64 option
  ; key : Agent_protocol.Idempotency_key.t
  ; mutation : Mutation.t
  }

(** Create has no expected entry frontier; every other mutation requires one.
    Source and lifecycle expectations are required for Create/Edit/Enable and
    absent for safe Pause/Retire. All CAS fields enter the original digest.
    Capture roots are immutable custody; no mutable source reads occur here. *)
val create
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> source:Agent_protocol.Run_source.t option
  -> expected_lifecycle_revision:Agent_protocol.Session_lifecycle.Revision.t option
  -> expected_catalog_revision:int64
  -> expected_control_revision:int64 option
  -> key:Agent_protocol.Idempotency_key.t
  -> mutation:Mutation.t
  -> (t, Agent_protocol.Error.t) result

val digest : t -> (string, Agent_protocol.Error.t) result
