(** Real qualified daemon, paused after the first ingress receipt journal sync.
    Recovery uses a fake provider and never requests another tool. *)
val run : Eio_unix.Stdenv.base -> config_path:string -> recover:bool -> unit
