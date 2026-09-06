(** [run env ~case] creates a private Unix relay with cut/resume/status/quit
    operator controls. [case] names the existing manual fixture launcher
    directory, or ["self-check"] for a bounded forwarding/cut/resume test.
    The generated launcher copies the fixture environment and changes only its
    endpoint. Cleanup removes only the relay's own temporary roots. Closing this
    relay does not stop the upstream daemon. No normal runtest alias invokes it. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit

(** [operator env link] handles cut/resume/status/quit on operator stdin. *)
val operator : Eio_unix.Stdenv.base -> Support.Tui_manual_link.t -> unit

(** [self_check_connections ~sw env address link] verifies round trips through
    an echo relay, connection closure on cut, refusal while cut, and forwarding
    after resume. The caller supplies an outer deadline. *)
val self_check_connections
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Eio.Net.Sockaddr.stream
  -> Support.Tui_manual_link.t
  -> unit
