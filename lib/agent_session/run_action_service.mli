(** One ephemeral native staging owner per actual moderator callback. The actor
    creates the scope only for its borrowed execution, closes it with that borrow,
    and revalidates the prepared action at the atomic checkpoint commit. *)
type t

val create : scope:Run_scope.t -> t
val scope : t -> Run_scope.t
val transaction : t -> Chat_response.Run_operations.transaction

(** Close before releasing callback ownership, including cancellation/failure.
    No staged receipt can become durable after this point. *)
val close : t -> unit

(** Preparation is one-shot and seals staging. Any failed/repeated preparation,
    rollback after preparation, or close invalidates the selection permanently.
    Reads and verification also require the scope to remain open. Persistence
    failure must close/rollback before releasing the borrow. Rollback of an unknown
    or already-removed receipt is an idempotent no-op. Only a receipt actually
    retained by this service can remove its staging or invalidate its selection. *)
val prepared : t -> (Agent_protocol.Run_action.t option, string) result

val verify : t -> Agent_protocol.Run_action.t option -> (unit, string) result

(** Verify the one-shot prepared selection and compose its scheduling request with
    the existing runtime intent. Continue requests the existing turn admission;
    Wait/Finish reject Request_turn. Continue with End_session rejects rather than
    silently losing either decision. Other session requests retain their meaning.
    This does not persist or schedule work; the actor rechecks at its commit. *)
val compose_requests
  :  t
  -> action:Agent_protocol.Run_action.t option
  -> requests:Agent_protocol.Invocation.follow_up
  -> (Agent_protocol.Invocation.follow_up, string) result
