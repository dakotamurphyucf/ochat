open! Core

(** Private presentation waiter over the actual Run_views observer. Confined to
    its creating Eio domain/Switch; no attachment, reader or executor ownership. *)
type t

val create
  :  Agent_client.Run_views.t
  -> sw:Eio.Switch.t
  -> request:Agent_protocol.Run_query.Lookup_request.t
  -> admission:Agent_protocol.Run_receipt.t
  -> expected_mode:Agent_protocol.Run.Mode.t option
  -> t

(** Nonyielding notification callback: signals caller presentation and forwards
    current projection to the existing read-only Run_views watch. Feed the actual
    current handle projection immediately after post-admission creation. *)
val observe : t -> Agent_client.Projection.t -> unit

(** Requires exact original admission and actual terminal receipt, while current
    handle/query authority remains valid. Runs user presentation outside the reader,
    races actual reader closure and joins all race fibers. Callback/cancellation
    exceptions propagate unchanged. No waiter/callback escapes its owning Switch. *)
val await_terminal
  :  t
  -> handle:Agent_client.Session_handle.t
  -> on_view:(Agent_client.Projection.t -> unit)
  -> (Agent_protocol.Run_query.View.t, Agent_protocol.Error.t) Result.t
