open! Core

(** Pure compatibility check for one explicit portable Developer installation.
    It grants no method, source, approval or Host authority. The daemon still
    constructs the physical Hub, Host and Endpoint in order and checks their
    exact pairing before advertising any method. Trace admission validates
    complete method JSON plus bounded JSON-RPC envelope and request ID against
    negotiated Initialize capacity; opted-in wire requires an actual trace
    validated retention capacities, never a fabricated read policy. Its
    conservative page-fit criterion reserves enough encoded room for one
    maximal wire-representable retained event, Header, terminal, repeated
    historical site and cursors. It also validates maximal supported cursors
    and resumed requests through the actual wire constructors. Native source
    metadata outside the portable domain can still yield a typed
    [Resource_limit]; runtime paging measures every complete response. *)
val check
  :  policy:Developer_endpoint.Policy.t
  -> initialize:Agent_protocol.Initialize.Limits.t
  -> budgets:Developer_command_budgets.Configuration.t
  -> approval:Developer_approval_owner.Limits.t option
  -> repl:Developer_repl_epoch.Configuration.t option
  -> trace:Developer_trace_retention.Limits.t option
  -> (unit, Agent_protocol.Error.t) result
