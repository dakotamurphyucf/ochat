open! Core

module Effect : sig
  type t =
    | Refresh
    | Read
    | Activate
    | Deactivate
    | Status
  [@@deriving equal, enumerate]

  val name : t -> string
end

(** Exact admitted native effect plus its original checked503 execution borrow.
    No arbitrary actor, authorizer callback or serialized permission is accepted. *)
type t

val create
  :  Native_tool_invocation.Admission.t
  -> binding:Chat_response.Tool_capability.binding
  -> effect_kind:Effect.t
  -> (t, Agent_protocol.Error.t) result

(** Nonyielding exact effect, session/generation, original/physical epoch and
    protocol scope check. A token for Read cannot authorize Activate or Clear. *)
val check_now
  :  t
  -> Session_state.t
  -> effect_kind:Effect.t
  -> (unit, Agent_protocol.Error.t) result

val actor : t -> Operator_authorization.t
