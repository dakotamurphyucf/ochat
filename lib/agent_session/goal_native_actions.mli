(** Private provisional native Goal actions. This module does not authenticate
    supplied admissions or owners: Actor callsites select the actual execution
    and repeat physical callback/source checks. No serializer restores a loan. *)
module Value : sig
  type t =
    | Edit of Goal_command.t
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

module Plan : sig
  type t

  val prepare
    :  Entry.t list
    -> state:Session_state.t
    -> session:Agent_protocol.Session_ref.t
    -> now:Agent_protocol.Timestamp.t
    -> (t, Agent_protocol.Error.t) result

  val state : t -> Session_state.t
  val deltas : t -> Session_delta.t list
  val resumes : t -> Entry.t list
  val check : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
end
