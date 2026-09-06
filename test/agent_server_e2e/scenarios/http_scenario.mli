open Core

(** Runs direct HTTP RPC and SSE end-to-end scenarios against the daemon. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
