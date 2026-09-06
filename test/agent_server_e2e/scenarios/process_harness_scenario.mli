open Core

(** Process, pipe, port, socket, and deadline checks for the E2E harness. *)

(** [run env ~case] runs every process-harness subcase, or only [case] when
    supplied. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit

(** [run_child env behavior] executes an internal fixture-child behavior. *)
val run_child : Eio_unix.Stdenv.base -> string -> unit
