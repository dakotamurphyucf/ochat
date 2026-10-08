open! Core

(** Descriptor-bound private local storage. No provider/session dependencies.
    Errors contain finite tags only; cancellation is not converted to an error. *)
module Error : sig
  type operation =
    | Validate
    | Open_directory
    | Open_lock
    | Read
    | Create
    | Replace
    | Delete
  [@@deriving sexp_of]

  type code =
    | Missing
    | Exists
    | Denied
    | Unavailable
    | Unsupported_filesystem
    | Corrupt
    | Too_large
    | Invalid_name
    | Busy
    | Closed
  [@@deriving equal, sexp_of]

  type publication =
    | Not_published
    | Published_durability_unknown
  [@@deriving equal, sexp_of]

  type t

  val code : t -> code
  val operation : t -> operation

  (** None only for nonmutating operations. Post-publication sync failure and
      post-unlink sync failure always report Published_durability_unknown. *)
  val publication : t -> publication option

  val sexp_of_t : t -> Sexp.t
end

module Name : sig
  type t

  val create : string -> (t, Error.t) result
end

module Directory : sig
  type t

  (** Fixed positive input/read ceiling, including create/replace. *)
  val maximum_bytes : int

  (** Anchor is an explicitly trusted native OS capability, not HOME discovery.
      Descendants are opened descriptor-relative with nofollow. All descendant
      directories must be current-operator-owned 0700 with no extended ACL.
      An empty component list is rejected; the anchor itself is not private data.
      Only local filesystems are admitted; platform qualification is separate. *)
  val open_or_create
    :  sw:Eio.Switch.t
    -> anchor:Eio.Fs.dir_ty Eio.Path.t
    -> components:Name.t list
    -> (t, Error.t) result

  val read_bounded : t -> Name.t -> max_bytes:int -> (bytes, Error.t) result

  (** Create-only atomic publication, file+directory sync. Never overwrites an
      existing target. Cancellation joins and attempts cleanup only of this call's newly
      created unreferenced revision, retaining original cancellation/backtrace.
      Failed cleanup can leave an orphan; cancellation does not imply rollback.
      Do not publish an authoritative reference until this returns Ok. *)
  val create_immutable : t -> Name.t -> bytes -> (unit, Error.t) result

  (** Nonsecret metadata only. Caller owns CAS/coordination. Cancellation may
      have committed replacement; reread exact metadata, never assume rollback. *)
  val replace_metadata : t -> Name.t -> bytes -> (unit, Error.t) result

  (** Exact validated private file; logical removal, not forensic erasure. *)
  val delete : t -> Name.t -> (unit, Error.t) result

  (** Join active operations and reject new ones. Idempotent. Existing lock
      leases own separate descriptors and are released by their own switch. *)
  val close : t -> unit

  module For_testing : sig
    type fault =
      | Before_publication
      | Before_directory_sync

    (** Real native publication with one injected finite fault, no mocked IO. *)
    val create_with_fault : t -> Name.t -> bytes -> fault:fault -> (unit, Error.t) result

    (** Runs the hook after joined native creation, before cancellation check.
        Used to exercise ownership cleanup without a timing-dependent sleep. *)
    val create_with_completion_hook
      :  t
      -> Name.t
      -> bytes
      -> after_native:(unit -> unit)
      -> (unit, Error.t) result
  end
end

module Lock : sig
  type mode =
    | Shared
    | Exclusive
  [@@deriving sexp_of]

  type t

  (** Nonblocking kernel admission, Busy on contention. Each successful lease
      owns a distinct open FD, including within one process. Stable lock inode
      must never be removed/replaced. Caller controls polling/deadline/order. *)
  val acquire
    :  Directory.t
    -> Name.t
    -> sw:Eio.Switch.t
    -> mode:mode
    -> (t, Error.t) result

  val release : t -> unit
end
