val run_child
  :  Eio_unix.Stdenv.base
  -> root:string
  -> boundary:string
  -> recover:bool
  -> unit

val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

(** Shared real factory fixture; no generated child storage is fabricated. *)
val create_child
  :  ?start_immediately:bool
  -> ?lifetime:Agent_server.Session_factory.generated_lifetime
  -> Eio_unix.Stdenv.base
  -> string
  -> Agent_server.Daemon.t
  -> Agent_protocol.Id.Session.t
  -> Agent_server.Session_registry.entry
