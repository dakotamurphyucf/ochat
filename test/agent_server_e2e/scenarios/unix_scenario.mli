open Core

(** End-to-end Unix transport checks against the real daemon executable. *)

(** [run env ~case] runs every Unix subcase, or only [case] when supplied. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
