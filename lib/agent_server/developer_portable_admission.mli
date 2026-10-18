open! Core

(** Pure compatibility check for one explicit portable Developer installation.
    It grants no method, source, approval or Host authority. The daemon still
    constructs the physical Hub, Host and Endpoint in order and checks their
    exact pairing before advertising any method. *)
val check
  :  policy:Developer_endpoint.Policy.t
  -> initialize:Agent_protocol.Initialize.Limits.t
  -> budgets:Developer_command_budgets.Configuration.t
  -> approval:Developer_approval_owner.Limits.t option
  -> repl:Developer_repl_epoch.Configuration.t option
  -> (unit, Agent_protocol.Error.t) result
