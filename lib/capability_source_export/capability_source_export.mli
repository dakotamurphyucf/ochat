open! Core

(** Explicit create-only source copying. This does not prepare or execute a
    project, register native tools, or replace any existing tree. *)
module Error : sig
  type progress =
    | Not_created
    | Incomplete
  [@@deriving equal, sexp_of]

  type code =
    | Invalid_name
    | Exists
    | Denied
    | Unsupported_provider
    | Changed
    | Limit_exceeded
    | Io
  [@@deriving equal, sexp_of]

  type t

  val progress : t -> progress
  val code : t -> code
  val sexp_of_t : t -> Sexp.t
end

module Directory_name : sig
  (** One validated portable child component, never a path or an ambient root. *)
  type t

  val create : string -> (t, Error.t) Result.t
  val to_string : t -> string
end

module Receipt : sig
  type t

  val resource : t -> Capability_sources.Id.t
  val digest : t -> Chatml_source.Digest.t
  val entry : t -> Source.Portable_path.t
  val members : t -> int
  val bytes : t -> int
end

(** [parent] is an already retained/admitted closeable directory with an empty
    Eio path. Borrow only through this joined call; no callback or FD escapes.
    Existing destinations, including symlinks, reject. Directories and files are
    created exclusively with 0700/0600. Directory admission checks no-follow
    entry identity before/after opening and binds writes to that retained FD;
    final entry checks reject observed replacement. Files use O_CREAT|O_EXCL,
    never truncation, under a retained admitted parent and one validated name.

    Success follows every complete write, file/directory sync and identity check.
    Failure after destination creation reports Incomplete and leaves that tree;
    cancellation propagates unchanged and may also leave a partial tree. There is
    no recursive rollback, overwrite, crash-atomic publication, or guarantee
    against privileged namespace/content mutation after the checked boundary.
    A retry must choose a new absent destination or explicitly remove the old
    one. Work is capped at one million checked steps; exhaustion is an error.
    Caller keeps its parent owner alive and joins this call before release. *)
val export
  :  Capability_sources.Resource.t
  -> parent:[ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> directory:Directory_name.t
  -> checkpoint:(unit -> unit)
  -> (Receipt.t, Error.t) Result.t
