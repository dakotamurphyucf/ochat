(** Inject EIO/ENOSPC exceptions at real Eio persistence boundaries in private
    fixture stores. These are store-integration tests, not host disk exhaustion
    or hardware power-loss simulations. *)

val replacements : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
val snapshots : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
val rotations : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
val writers : Eio_unix.Stdenv.base -> Support.Temporary_environment.t -> unit
