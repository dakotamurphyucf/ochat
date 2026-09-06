open Core

(** Run the session-liveness E2E scenario or one named subcase. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
