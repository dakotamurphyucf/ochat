val prompt : string

(** Exercise the typed Goal controls against one genuine daemon Unix socket.
    The HTTP client uses a distinct authenticated principal. *)
val check
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> spec:Agent_protocol.Session.Spec.t
  -> unit
