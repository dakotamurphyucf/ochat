val run_child
  :  Eio_unix.Stdenv.base
  -> root:string
  -> boundary:string
  -> recover:bool
  -> unit

val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
