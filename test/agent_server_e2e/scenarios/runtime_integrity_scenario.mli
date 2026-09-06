open Core

(** Exercises canonical tool pairs, deferred adoption, moderator overlays and
    boundaries, bounded self-triggered turns, and exact history after restart. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
