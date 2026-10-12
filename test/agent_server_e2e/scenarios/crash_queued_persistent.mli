(** Actual process-boundary queued persistent source regression. Reuses the
    existing qualified daemon, journal sync barrier and subprocess/HTTP owners.
    No production fault adapter or source-selection API is introduced. *)
val run_child : Eio_unix.Stdenv.base -> config_path:string -> boundary:string -> unit

(** Queue selected File B before dispatch, SIGKILL, delete originals and reopen
    twice. The first restart executes persistent B and its explicitly Captured
    nested B; the second performs no inference. Observe immutable source, job,
    delegation and actual submission/notification identities across restart. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
