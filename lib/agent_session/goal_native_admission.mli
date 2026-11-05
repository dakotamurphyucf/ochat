module Effect : sig
  type t =
    | Get
    | Objective
    | Progress
    | Resume
  [@@deriving equal, enumerate]

  val name : t -> string
end

(** Exact admitted native effect and its actual checked execution borrow.
    No decoder, ambient actor lookup or principal-derived proof constructor. *)
type t

val create
  :  Native_tool_invocation.Admission.t
  -> binding:Chat_response.Tool_capability.binding
  -> effect_kind:Effect.t
  -> (t, Agent_protocol.Error.t) result

val check_now
  :  t
  -> Session_state.t
  -> effect_kind:Effect.t
  -> (unit, Agent_protocol.Error.t) result

val actor : t -> Operator_authorization.t
val invocation : t -> Agent_protocol.Invocation.t

(** Routing DATA only. The Actor independently matches its registered callback. *)
val callback_scope : t -> Chat_response.Goal_native_scope.t option
