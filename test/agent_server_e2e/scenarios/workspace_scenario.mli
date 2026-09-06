open Core

(** Runs workspace-variable and tool-authority checks through real hosts. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
