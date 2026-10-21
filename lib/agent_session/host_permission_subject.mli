open Core

(** The subject offered to the existing permission policy/reviewer. Conversation
    requests retain the exact Session-owned Permission record; developer requests
    bind a live invocation and authenticated developer owner, with no Session
    attachment or session-scoped grant. This is request data, not approval.
    [developer] requires a Dispatching developer invocation; the host actor
    verifies it is current before opening a request and after any approval wait. *)
type t

val of_conversation : Agent_protocol.Permission.t -> (t, Agent_protocol.Error.t) result

val developer
  :  invocation:Agent_protocol.Host_invocation.t
  -> id:Agent_protocol.Id.Permission.t
  -> identity_digest:string
  -> invocation_display:string
  -> effects:string list
  -> created_at:Agent_protocol.Timestamp.t
  -> expires_at:Agent_protocol.Timestamp.t option
  -> (t, Agent_protocol.Error.t) result

val owner : t -> Agent_protocol.Execution_owner.t
val invocation_id : t -> Agent_protocol.Id.Invocation.t option
val request : t -> Permission_policy.invocation

(* Developer subjects offer only Approve_once/Deny until a separate real
   developer grant store/policy is installed. Approve_session and prefix grants
   must never be reinterpreted against a synthetic Session. *)
val choices : t -> Agent_protocol.Permission.choice list
val expires_at : t -> Agent_protocol.Timestamp.t option
val to_conversation : t -> Agent_protocol.Permission.t option
