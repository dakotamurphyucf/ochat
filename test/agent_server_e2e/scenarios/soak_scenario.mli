(** Persistent detached orchestration across repeated client closure, graceful
    restart and SIGKILL restart. Explicit opt-in only; required runs last at
    least one hour. The self-check exercises the same loop with shorter timing. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
