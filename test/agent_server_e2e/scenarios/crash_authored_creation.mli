(** Real native authored invocations interrupted by external SIGKILL. *)
val run_child
  :  ?lose_ack:bool
  -> Eio_unix.Stdenv.base
  -> root:string
  -> boundary:string
  -> mode:string
  -> recover:bool
  -> unit

val test : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit

(** Unlike [test], these cases unwind a failed write acknowledgement and allow
    normal invocation cleanup before restarting twice. *)
val test_lost_ack : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
