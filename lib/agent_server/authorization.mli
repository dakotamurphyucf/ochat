(** Method and requested-attachment-mode authorization under the current
    principal, before execution or idempotency replay. Create/attach retries
    cannot disclose an attachment or reclaim credential after its required
    transcript, send or owner scopes have been removed. Session actors perform
    a second current-state attachment check for mutations. Pending controls also
    require current transcript scope here, including reconstructed original
    commands during receipt reconciliation. *)

val authorize
  :  ?installed_authoring:bool
  -> Agent_protocol.Principal.t
  -> Agent_protocol.Command.t
  -> (unit, Agent_protocol.Error.t) result

(** [installed_authoring] defaults to false. The Command_handler passes true
    only when it owns the actual Authoring_installation; this generic check
    supplies no source/upload grant or feature selection. The installed
    handler repeats current actor, epoch, feature and origin authorization.
    Reconstructed command receipts keep the default false because authoring
    commands have no generic idempotency receipt. *)

(** Shared current session visibility used after method-scope authorization.
    Administer_configuration permits management visibility; otherwise the retained
    creator must equal this authenticated principal. No attachment, root path,
    receipt or prior inspection grants visibility. Pure; no runtime activation. *)
val session_visible_to : Agent_protocol.Principal.t -> Agent_protocol.Session.t -> bool
