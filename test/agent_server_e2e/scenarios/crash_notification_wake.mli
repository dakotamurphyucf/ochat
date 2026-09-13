(** Actual compiled notification publication, journal-sync SIGKILL, and two
    independent reopenings at pending publication, committed pending wake and
    accepted wake boundaries. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
