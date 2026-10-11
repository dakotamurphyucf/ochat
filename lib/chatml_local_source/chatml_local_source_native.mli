module Native_unix = Unix
open Core

(** Private native primitives, used only in joined Eio system-thread operations.
    New native resources have closed custom-block owners with stable malloc payload
    allocated in the Eio fiber BEFORE installing cleanup/starting worker acquisition.
    Acquisition fills that payload before runtime reentry. No movable heap access
    occurs while unlocked. Normal explicit close joins; GC finalizers are fallback only.
    Native errors are finite, OCaml allocation failures are never errno results.
    No callback invocation or OCaml allocation with runtime lock released. *)
module Code : sig
  type t =
    | Missing
    | Denied
    | Unsupported
    | Kind
    | Symlink_or_non_directory
    | Changed
    | Too_large
    | Unavailable
  [@@deriving equal, sexp_of]
end

module Descriptor : sig
  type t

  (** Borrowed input is retained by Eio_unix.Fd.use_exn until worker joins; output
      owns only a new duplicate, never the caller's original descriptor. *)
  val create : unit -> t

  val clone_directory : t -> parent:t -> (unit, Code.t) Result.t
  val duplicate_directory : t -> Native_unix.file_descr -> (unit, Code.t) Result.t

  (** ONE validated native component, relative to descriptor; kernel nofollow.
      Newly acquired file must be fstat regular before success. Nonblocking flags
      avoid FIFO admission hangs. Constructors allocate owner/result BEFORE IO. *)
  val open_directory : t -> parent:t -> string -> (unit, Code.t) Result.t

  val open_file : t -> parent:t -> string -> (unit, Code.t) Result.t
  val close : t -> unit
end

module Observation : sig
  type t

  val byte_length : t -> int64
end

val observe_file : Descriptor.t -> (Observation.t, Code.t) Result.t
val unchanged : Descriptor.t -> Observation.t -> (unit, Code.t) Result.t

val same_entry
  :  Descriptor.t
  -> name:string
  -> file:Descriptor.t
  -> (unit, Code.t) Result.t

(** Buffer is nonmoving bigarray storage allocated before native IO. One read,
    <= requested bytes; no allocation while runtime lock is released. *)
val read : Descriptor.t -> Bigstring.t -> max_bytes:int -> (int, Code.t) Result.t

module Directory : sig
  type t

  (** A fresh openat(".") descriptor refers to the acquired directory object with
      independent cursor; fdopendir adopts it. Owner/result allocated before IO. *)
  val create : unit -> t

  val open_ : t -> Descriptor.t -> (unit, Code.t) Result.t
  val close : t -> unit

  (** At most 1024 bytes per native name via fixed nonmoving buffer. Dot entries skipped;
      None is EOF. Names copied to OCaml only with runtime lock held. Overlong
      native names reject. Allocation exceptions leave the already owned stream
      reachable for bracket cleanup, not a separate unowned native allocation. *)
  val next : t -> Bigstring.t -> (string option, Code.t) Result.t
end

module Directory_identity : sig
  type t =
    { device : int64
    ; inode : int64
    }
  [@@deriving equal, compare, sexp_of]
end

(** fstat of a retained directory; exact device/inode bits. No new resource.
    Result allocation only with runtime lock held, not errno conversion of OOM. *)
val observe_directory : Descriptor.t -> (Directory_identity.t, Code.t) Result.t
