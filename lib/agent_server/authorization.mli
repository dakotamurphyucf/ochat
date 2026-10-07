(** Method and requested-attachment-mode authorization under the current
    principal, before execution or idempotency replay. Create/attach retries
    cannot disclose an attachment or reclaim credential after its required
    transcript, send or owner scopes have been removed. Session actors perform
    a second current-state attachment check for mutations. *)

val authorize
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Command.t
  -> (unit, Agent_protocol.Error.t) result
