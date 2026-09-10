(** Kill after ingress journal sync but before acknowledgement; reopen twice and
    retry over HTTP, preserving one event, handler, notification and continuation. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
