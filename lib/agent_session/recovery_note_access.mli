open! Core

(** Live original authority for an owned note effect, separate from persistent
    mutation identities. Attachments and invocation IDs alone convey no authority. *)
module Effect : sig
  type t =
    | Read
    | List
    | Write
    | Append
    | Delete
  [@@deriving equal]

  val name : t -> string
  val of_mutation : Recovery_notes.Catalog.mutation -> t
end

type t

(** [authorize] is the host's nonyielding original connection/visibility check
    for this session and, for writes, the connection's exact attachment. The
    actor also checks the real live writer role. *)
val operator
  :  Operator_authorization.t
  -> owner:Agent_protocol.Session_ref.t
  -> generation:int
  -> attachment_id:Agent_protocol.Id.Attachment.t option
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> t

val native
  :  Native_tool_invocation.Admission.t
  -> binding:Chat_response.Tool_capability.binding
  -> effect_kind:Effect.t
  -> (t, Agent_protocol.Error.t) result

type checked =
  | Operator of
      { principal : Agent_protocol.Id.Principal.t
      ; attachment_id : Agent_protocol.Id.Attachment.t option
      }
  | Native of Agent_protocol.Invocation.t

val check_now
  :  t
  -> Session_state.t
  -> owner:Agent_protocol.Session_ref.t
  -> effect_kind:Effect.t
  -> (checked, Agent_protocol.Error.t) result
