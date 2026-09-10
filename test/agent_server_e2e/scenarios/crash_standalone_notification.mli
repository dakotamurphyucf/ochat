(** Native standalone completion and artifact reference delivery through the real
    daemon. SIGKILL after terminal job commit and each durable admission/publication/wake boundary, followed
    by two independent reopenings, authorized artifact reads and effect checks. *)
val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
