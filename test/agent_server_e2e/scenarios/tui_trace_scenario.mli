open Core

(** Canonical message/identity and presentation-isolation journeys. Each uses
    the production TUI session adapter over embedded, Unix, and HTTP connections.
    This initial trace slice does not claim live provider/tool/overlay coverage. *)
val messages : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

val presentation : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

(** Use only inside a credential-stripped fixture subprocess whose provider URL
    is pinned to the gated loopback server. Unlike the offline smoke wrapper,
    this host permits its supplied network capability. *)
val with_embedded_provider
  :  Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> (Chat_tui.Agent_session_client.t -> Agent_client.Connection.t -> 'a)
  -> 'a

val model : string -> Chat_tui.Model.t

val apply
  :  Chat_tui.Agent_event_apply.t
  -> Chat_tui.Model.t
  -> Chat_tui.Agent_projection.t
  -> unit

val connected
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> bool
  -> Agent_client.Connection.t
