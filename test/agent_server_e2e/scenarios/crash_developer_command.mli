(** Actual developer Live command interrupted by SIGKILL after a native effect. *)

val run_child
  :  Eio_unix.Stdenv.base
  -> config_path:string
  -> marker:string
  -> handoff:string
  -> recover:bool
  -> unit

val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
