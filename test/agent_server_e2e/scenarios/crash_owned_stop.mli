val run_child
  :  ?interrupt_recovery:string
  -> Eio_unix.Stdenv.base
  -> root:string
  -> recover:bool
  -> unit

val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
