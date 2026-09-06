open Core

(** Runs shared protocol scripts across the supported client transports. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
