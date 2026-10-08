open! Core

(** Immutable private-file secret revisions. Does not manage credentials, login,
    authoritative pointers, CAS, or session authority. No Keychain fallback. *)
module Error : sig
  type operation =
    | Open
    | Create
    | Read
    | Delete
    | Confirm_absent
    | Close
  [@@deriving sexp_of]

  type code =
    | Missing
    | Exists
    | Denied
    | Unavailable
    | Unsupported_context
    | Corrupt
    | Too_large
    | Invalid_identifier
    | Busy
    | Closed
  [@@deriving equal, sexp_of]

  type t

  val code : t -> code
  val operation : t -> operation

  (** None only for nonmutations. Delete/create errors preserve whether the
      target was mutated before durability became uncertain. *)
  val publication : t -> Private_storage.Error.publication option

  val sexp_of_t : t -> Sexp.t
end

module Namespace : sig
  type t

  val create : string -> (t, Error.t) result
end

module Revision : sig
  (** Nonsecret caller-issued immutable operation identity, bounded safe ASCII. *)
  type t

  val create : string -> (t, Error.t) result
  val to_string : t -> string
end

module Secret : sig
  type t

  val maximum_bytes : int

  (** Copies caller bytes. Positive fixed ceiling applies at construction,
      read and publication. No token-specific parsing or equality oracle. *)
  val of_bytes : bytes -> (t, Error.t) result

  (** Trusted authorized consumers only; borrowed material must not be logged.
      This API cannot prevent deliberate copies or guarantee forensic erasure. *)
  val with_string : t -> f:(string -> 'a) -> 'a

  val length : t -> int
end

type t

val open_private_files
  :  sw:Eio.Switch.t
  -> directory:Private_storage.Directory.t
  -> namespace:Namespace.t
  -> (t, Error.t) result

(** Create-only; Exists never compares secret bytes. Do not publish an
    authoritative pointer before success. Cancellation joins native work and
    attempts cleanup only of this invocation's newly created unreferenced
    revision. Failed cleanup leaves an orphan for lifecycle reconciliation. *)
val create : t -> revision:Revision.t -> Secret.t -> (unit, Error.t) result

val read : t -> revision:Revision.t -> (Secret.t, Error.t) result

(** Logical unlink only, not physical/forensic erasure. *)
val delete : t -> revision:Revision.t -> (unit, Error.t) result

(** Confirms absence in this backend's own retained directory after syncing it.
    Any existing entry returns Exists, including symlinks, without following or
    deleting it. Caller owns lifecycle locking. No recreation or physical erasure
    is implied; errors have publication=None and must not become cleanup proof. *)
val confirm_absent : t -> revision:Revision.t -> (unit, Error.t) result

(** Joins outstanding backend operations. Borrows Directory: close never closes
    it; caller must keep Directory alive until backend close. Earlier Directory
    close makes subsequent operations fail Closed. Idempotent. *)
val close : t -> unit
