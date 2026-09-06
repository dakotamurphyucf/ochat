open Core

(** [run env ~case] exercises real ChatML model jobs through an isolated daemon
    process and a gated loopback Responses endpoint. [None] runs every case. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit

(** [run_load_capacity env ~record] saturates each configured concurrency
    dimension independently, verifies queued cancellation and lease reuse,
    with all unrelated concurrency limits set to 64. Root jobs use depth zero;
    nested-depth rejection remains a separate scheduler unit-test concern. *)
val run_load_capacity
  :  Eio_unix.Stdenv.base
  -> record:
       (Support.Config_fixture.t
        -> Support.Daemon_process.t
        -> Support.Http_driver.t
        -> string
        -> unit)
  -> unit
