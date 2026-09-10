(** SIGKILL after a compiled ChatML handler commits a selected job intent, before
    worker publication/launch. Two fresh daemons must run that same job once. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
