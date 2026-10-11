open Core

(** Test-only interception of the actual native domain provider, retaining its
    cancellation and joining. The supplied gate runs inside that real domain. *)
val environment
  :  Eio_unix.Stdenv.base
  -> before:(unit -> unit)
  -> finally:(unit -> unit)
  -> Eio_unix.Stdenv.base
