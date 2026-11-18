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

(** Original Goal selection for a fresh-child capture. The revision belongs to
    the request digest. Callers supply no creator episode or grant. *)
module Goal_owner_request : sig
  type t = private
    { goal_id : Agent_protocol.Id.Goal.t
    ; expected_goal_revision : int64
    }

  val create
    :  goal_id:Agent_protocol.Id.Goal.t
    -> expected_goal_revision:int64
    -> (t, Agent_protocol.Error.t) result
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
  ; goal_owner : Goal_owner_request.t option
  ; shared_intent_digest : string option
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

val with_goal_owner : t -> Goal_owner_request.t -> (t, Agent_protocol.Error.t) result

(** Bind a shared client's complete, scoped intent to a host-captured target.
    A matching durable receipt can be recognized without recapturing that target.
    The selector must agree with the supplied capture; simple controls accept none. *)
val of_shared_intent
  :  session:Agent_protocol.Session_ref.t
  -> Agent_protocol.Recurrence_control.Intent.t
  -> target:Recurrence_target.t option
  -> (t, Agent_protocol.Error.t) result

val digest : t -> (string, Agent_protocol.Error.t) result

(** Trusted native producer fingerprint. Historical unbound native requests
    keep the ordinary digest. A bound native request hashes the originally
    acknowledged frozen definition, distinct from the operator selection. *)
val digest_native
  :  t
  -> binding:Recurrence_definition.Goal_binding.t
  -> (string, Agent_protocol.Error.t) result
