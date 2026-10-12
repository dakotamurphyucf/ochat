open! Core

(** Genuine Embedded host, existing captured prompt, no bootstrap attachment.
    Callbacks borrow its initialized connection until the host closes. *)
val with_host
  :  remove_source:bool
  -> prompt:string
  -> requests:int ref
  -> f:
       (env:Eio_unix.Stdenv.base
        -> host:Agent_server.Embedded.host
        -> Agent_client.Connection.t
        -> unit)
  -> unit

val create : Agent_client.Connection.t -> key:string -> Agent_protocol.Session.t

val snapshot
  :  Agent_client.Connection.t
  -> Agent_protocol.Id.Session.t
  -> Agent_protocol.Public.Snapshot.Fields.t

val run
  :  Agent_client.Connection.t
  -> env:Eio_unix.Stdenv.base
  -> session:Agent_protocol.Session.t
  -> request:Managed_completion_cli.Request.t
  -> ?prepare:
       (Agent_client.Session_handle.t
        -> Agent_protocol.Session.t
        -> ( Managed_completion_cli.Request.t * Agent_protocol.Session.t
             , Managed_completion_cli.Error.t )
             Result.t)
  -> ?checkpoint:(unit -> unit)
  -> ?on_view:(Agent_client.Projection.t -> unit)
  -> ?on_admission:(Agent_protocol.Run_receipt.t -> unit)
  -> unit
  -> (Managed_completion_cli.Completed.t, Managed_completion_cli.Error.t) Result.t

val checked : ('a, Managed_completion_cli.Error.t) Result.t -> 'a
