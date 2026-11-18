(** Install one real project source and grant the fixture bearer principal access. *)
val configure : env:Eio_unix.Stdenv.base -> Support.Config_fixture.t -> unit

(** Exercise all five typed Instruction RPCs through the authenticated HTTP daemon. *)
val check
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> spec:Agent_protocol.Session.Spec.t
  -> unit
