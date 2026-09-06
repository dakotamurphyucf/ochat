open Core

(** Isolation and artifact checks for the E2E harness. *)

(** [run env ~case] runs every harness-isolation subcase, or only [case] when
    supplied. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
