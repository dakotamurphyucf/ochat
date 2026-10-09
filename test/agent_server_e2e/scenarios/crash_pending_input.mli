(** Actual SIGKILL after acknowledged after-root input and graceful stop, followed
    by daemon reopen, Interrupted reconciliation, and explicit resume. *)
val run_child : Eio_unix.Stdenv.base -> root:string -> recover:bool -> unit

val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
