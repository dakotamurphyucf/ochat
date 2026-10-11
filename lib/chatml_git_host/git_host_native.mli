module Native_unix = Unix
open Core

(** Private descriptor operations used in joined Eio system threads. Resource
    owners are allocated closed before acquisition, filled in stable native
    memory before runtime reentry, and explicitly closed by their brackets.
    No allocation/callback/OCaml heap access occurs while the runtime is released. *)
module Code : sig
  type t =
    | Missing
    | Exists
    | Denied
    | Unsupported
    | Corrupt
    | Busy
    | Too_large
    | Unavailable
  [@@deriving equal, sexp_of]
end

module Descriptor : sig
  type t

  val create : unit -> t
  val close : t -> unit

  (** Duplicate a retained private, current-operator-owned 0700 directory on a
      supported local filesystem, with no ACL. Never adopt the input fd. *)
  val root : t -> Native_unix.file_descr -> (unit, Code.t) Result.t

  val validate_root : t -> (unit, Code.t) Result.t
  val trusted_directory : t -> Native_unix.file_descr -> (unit, Code.t) Result.t

  (** Exact one-component nofollow opens. Descendants are operator owned with no
      ACL, special mode bits or world-write permission. Group-write from host
      umask002 is permitted only below the retained 0700/noACL root, which the
      host keeps private throughout the owner lifetime. Files have one link;
      the private root prevents traversal by other users. Lease admission remains
      the strict existing 0600/noACL policy. *)
  val directory
    :  t
    -> parent:t
    -> name:string
    -> create:bool
    -> exclusive:bool
    -> (unit, Code.t) Result.t

  val file : t -> parent:t -> name:string -> create:bool -> (unit, Code.t) Result.t

  (** Native fd remains borrowed from this owner. Caller retains the owner for
      the complete Eio wrapper/process lifetime; it is never closed by the wrapper. *)
  val borrowed_fd : t -> Native_unix.file_descr

  (** Exclusive nonblocking flock on an already owned regular file; lease ends
      only when the descriptor is closed. *)
  val lock : t -> (unit, Code.t) Result.t
end

module Entry : sig
  type kind =
    | Directory
    | Regular
    | Symlink
  [@@deriving equal, sexp_of]

  type t =
    { kind : kind
    ; payload_bytes : int64
    }
end

(** Observe only the named entry without following it. Files with multiple links,
    unexpected kind/owner/mode/ACL reject; symbolic links are reported for bounded
    removal only and are never read. *)
val entry : Descriptor.t -> name:string -> (Entry.t, Code.t) Result.t

val read : Descriptor.t -> Bigstring.t -> max_bytes:int -> (int, Code.t) Result.t
val write : Descriptor.t -> Bigstring.t -> bytes:int -> (int, Code.t) Result.t
val sync : Descriptor.t -> (unit, Code.t) Result.t
val unlink : Descriptor.t -> name:string -> directory:bool -> (unit, Code.t) Result.t

(** Atomic same-root create-only directory publication. Unsupported native
    no-replace rename fails; no racy existence check/fallback. *)
val publish
  :  Descriptor.t
  -> name:string
  -> destination:Descriptor.t
  -> destination_name:string
  -> (unit, Code.t) Result.t

module Cursor : sig
  type t

  val create : unit -> t
  val open_ : t -> Descriptor.t -> (unit, Code.t) Result.t
  val close : t -> unit

  (** Independent cursor; one bounded name, <=1024 bytes. Dot entries skipped. *)
  val next : t -> Bigstring.t -> (string option, Code.t) Result.t
end
