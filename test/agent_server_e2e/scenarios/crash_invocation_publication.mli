(** Actual journal-sync SIGKILL at model invocation admission and at committed
    handler outcome before provider publication, followed by two fresh daemons. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
