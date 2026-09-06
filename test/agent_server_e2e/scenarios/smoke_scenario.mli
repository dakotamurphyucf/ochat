open Core

(** Real configuration and daemon smoke checks. *)

(** [run env ~case] runs every smoke subcase, or only [case] when supplied. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
