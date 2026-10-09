(** Method and requested-attachment-mode authorization under the current
    principal, before execution or idempotency replay. Create/attach retries
    cannot disclose an attachment or reclaim credential after its required
    transcript, send or owner scopes have been removed. Session actors perform
    a second current-state attachment check for mutations. *)

val authorize
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Command.t
  -> (unit, Agent_protocol.Error.t) result

(** Shared current session visibility used after method-scope authorization.
    Administer_configuration permits management visibility; otherwise the retained
    creator must equal this authenticated principal. No attachment, root path,
    receipt or prior inspection grants visibility. Pure; no runtime activation. *)
val session_visible_to : Agent_protocol.Principal.t -> Agent_protocol.Session.t -> bool
