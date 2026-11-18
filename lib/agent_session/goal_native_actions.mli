(** Private provisional native Goal actions. This module does not authenticate
    supplied admissions or owners: Actor callsites select the actual execution
    and repeat physical callback/source checks. No serializer restores a loan. *)
module Value : sig
  type t =
    | Edit of Goal_command.t
    | Start of Agent_protocol.Goal_native_start_intent.t
    | Resume of Agent_protocol.Goal_native_resume_intent.t
end

module Entry : sig
  type t

  val create
    :  invocation_id:Agent_protocol.Id.Invocation.t
    -> value:Value.t
    -> admission:Execution_custody.Admission.t
    -> authorize:
         (Operator_authorization.t
          -> Session_state.t
          -> (unit, Agent_protocol.Error.t) result)
    -> t

  val invocation_id : t -> Agent_protocol.Id.Invocation.t
  val value : t -> Value.t
  val actor : t -> (Operator_authorization.t, Agent_protocol.Error.t) result

  val authorize
    :  t
    -> Operator_authorization.t
    -> Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  val check : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
  val completed : t -> unit
  val discard : t -> unit
  val discard_speculative : t -> unit

  (** Called only after the actual selected owner ACK; lexical closure then
      retires speculative entries but preserves this same intent-owned loan. *)
  val retain : t -> unit

  val select_completed : t -> (unit, Agent_protocol.Error.t) result
  val take_admission : t -> (Execution_custody.Admission.t, Agent_protocol.Error.t) result
end

type t

val create : unit -> t

val install_scope
  :  t
  -> Chat_response.Goal_native_scope.t
  -> (unit, Agent_protocol.Error.t) result

val matches_scope : t -> Chat_response.Goal_native_scope.t option -> bool
val stage : t -> Entry.t -> (int, Agent_protocol.Error.t) result
val deliverable : t -> int -> (bool, Agent_protocol.Error.t) result
val accept : t -> int -> (unit, Agent_protocol.Error.t) result
val rollback : t -> int -> unit
val prepare : t -> int list -> (unit, Agent_protocol.Error.t) result
val for_commit : t -> (Entry.t list, Agent_protocol.Error.t) result
val recorded : t -> (Entry.t list, Agent_protocol.Error.t) result
val selected : t -> (Entry.t list, Agent_protocol.Error.t) result
val close : t -> unit

module Fresh_control : sig
  (** Immutable planning DATA only. A receipt belongs to the same successful
      command preparation and nonempty delta. It grants no callback ACK. *)
  type t = private
    { receipt : Goal_command_receipt.t
    ; mutation : Agent_protocol.Goal_native_control_request.Mutation.t
    }
end

module Plan : sig
  type t

  val prepare
    :  Entry.t list
    -> state:Session_state.t
    -> session:Agent_protocol.Session_ref.t
    -> descendant_profile:Goal_descendants.Profile.t
    -> now:Agent_protocol.Timestamp.t
    -> (t, Agent_protocol.Error.t) result

  val state : t -> Session_state.t
  val deltas : t -> Session_delta.t list

  (** Selected Start/Resume entries in actual callback order. *)
  val dispatches : t -> Entry.t list

  (** Selected fresh controls only; retained receipt replay has no control effect.
      IDs are planning DATA, consumed by the same Actor's actual writer boundary. *)
  val fresh_holds : t -> Agent_protocol.Id.Goal.t list

  (** Fresh Hold/Abandon receipts in actual selection order, excluding replay.
      [state] is the final projection, including edits after these controls. *)
  val fresh_controls : t -> Fresh_control.t list

  (** Reject a new Continue/turn request for the exact Goal controlled in this
      plan. Passive Wait/Finish and unrelated Run owners remain unchanged. *)
  val check_scheduling
    :  t
    -> run_id:Agent_protocol.Id.Run.t option
    -> action:Agent_protocol.Run_action.t option
    -> request_turn:bool
    -> (unit, Agent_protocol.Error.t) result

  val check : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
end
