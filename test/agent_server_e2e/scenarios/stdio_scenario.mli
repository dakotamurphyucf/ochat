open Core

(** End-to-end local and gateway stdio checks against the real executable. *)

(** [run env ~case] runs every stdio subcase, or only [case] when supplied. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
