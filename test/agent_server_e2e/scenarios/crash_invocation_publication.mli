(** Actual journal-sync SIGKILL at model invocation admission and at committed
    handler outcome before provider publication, and during nested native approval,
    followed by two fresh daemons. Late approval cannot revive the old call. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
