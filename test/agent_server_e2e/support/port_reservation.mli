open Core

(** Loopback-only TCP port reservations owned by an Eio switch. *)

type t

(** [create] binds an ephemeral IPv4 loopback listener. *)
val create : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> t

(** [port t] is the uniquely bound local port. *)
val port : t -> int

(** [release t] closes the listener while preserving [port t] for immediate use. *)
val release : t -> unit
