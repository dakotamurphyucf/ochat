open! Core

module Effect : sig
  type t =
    | Create
    | List
    | Get
    | Edit
    | Pause
    | Resume
    | Cancel
    | Status
    | History
  [@@deriving equal, enumerate]

  val name : t -> string
end

(** Exact selected conversation-native callback, not a serialized grant. *)
type t

val create
  :  Native_tool_invocation.borrowed
  -> binding:Chat_response.Tool_capability.binding
  -> effect_kind:Effect.t
  -> (t, Agent_protocol.Error.t) result

val check_now
  :  t
  -> Session_state.t
  -> effect_kind:Effect.t
  -> (unit, Agent_protocol.Error.t) result

val actor : t -> Operator_authorization.t

(** Rechecks the SAME stored borrowed/native epochs before returning the actual
    selected ceiling. No ambient lookup or resource realization. *)
val capabilities : t -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

(** Immutable actual dispatch selectors, not permission. The Factory uses them
    ONLY together with check_now and the real current host metadata check. *)
val invocation : t -> Agent_protocol.Invocation.t

val binding : t -> Chat_response.Tool_capability.binding

(** Immutable dispatch selector, not an admission issuer. *)
val effect_kind : t -> Effect.t
