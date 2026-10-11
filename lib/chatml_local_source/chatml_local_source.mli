open Core

(** Bounded read-only local source acquisition through admitted Eio capabilities. One native read-only acquisition owner; no module/config/graph,
    private-storage mode/ACL, runtime or global filesystem policy. *)
module Error : sig
  type operation =
    | Borrow_root
    | Open_parent
    | Open_directory
    | Open_file
    | Enumerate
    | Read
    | Check_revision
  [@@deriving equal, sexp_of]

  type code =
    | Missing
    | Denied
    | Unsupported_provider
    | Unsupported_kind
    | Symlink_or_non_directory
    | Changed
    | Too_large
    | Entry_limit
    | Invalid_limit
    | Not_retained_directory
    | Closed
    | Unavailable
  [@@deriving equal, sexp_of]

  type t

  val operation : t -> operation
  val code : t -> code
  val sexp_of_t : t -> Sexp.t
end

type t

(** Caller passes an ALREADY OPEN host-admitted closeable directory with empty
    Eio path; nonempty paths reject. Borrow its native FD by opening "." through
    that retained capability, validating descriptor kind Directory and using
    Eio_unix.Resource.fd_opt/Fd.use_exn. No native-path re-resolution. Unsupported
    providers reject instead of widening to env.fs or falling back to an unbounded
    directory listing. Caller keeps its root switch alive during this bracket.

    Private child FDs/native directory handles own duplicated or newly acquired
    resources only. Each native operation runs in a joined Eio system thread;
    protected cleanup closes every owned handle exactly once on success/failure/
    cancellation. An owner retained past the bracket is closed and rejects operations;
    no usable descriptor or callback is retained past its owned invocation. [f] runs in the Eio
    fiber and its exception/backtrace is preserved; only native filesystem failures
    are mapped to finite Error.t. No catch-all around caller/checkpoint code.
    max_path_components must be positive. A scoped lifecycle gate admits operations
    while open; no gate is held during IO or callbacks. Reentrant read/fold from a
    callback is supported with independent handles. Bracket close rejects new
    operations with Closed, joins active operations, then closes its root duplicate. *)
val with_root
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> max_path_components:int
  -> checkpoint:(unit -> unit)
  -> f:(t -> ('a, 'error) Result.t)
  -> ('a, [ `Filesystem of Error.t | `Callback of 'error ]) Result.t

(** None means the retained root itself. Some path is opened component by component,
    one kernel openat(O_RDONLY|O_DIRECTORY|O_NOFOLLOW|O_CLOEXEC) at a time. No parent
    traversal, source locators or symlink resolution. fstat verifies each acquired
    directory FD. Cancellation polls between bounded component operations.

    fdopendir owns a fresh openat(".") descriptor for the exact acquired directory
    object, so its cursor does not alter the retained root or other streams. Read
    at most 128
    entries per joined batch; exclude '.'/'..', count every other name, charge the
    remaining limit BEFORE retaining each returned name. One bounded lookahead may
    distinguish exact-limit EOF from overflow. No complete-directory materialization.
    [f] runs in the Eio fiber after checkpoint, not in the native worker. Caller
    candidate admission retains only needed names. Unspecified enumeration order;
    canonical selection is pure and independent of order. The first callback error
    stops further batches and invokes joined close; the current batch was already
    read within its bounded allowance. Native batches are bounded but OS
    calls have no wall-clock deadline guarantee. Polls check Eio cancellation and
    then invoke the supplied checkpoint; callback exceptions propagate unchanged.
    max_entries must be nonnegative; returned count is exact on success. *)
val fold_directory
  :  t
  -> directory:Source.Portable_path.t option
  -> max_entries:int
  -> init:'a
  -> checkpoint:(unit -> unit)
  -> f:('a -> name:string -> ('a, 'error) Result.t)
  -> ('a * int, [ `Filesystem of Error.t | `Callback of 'error ]) Result.t

(** Open parents descriptor-relative as above. Final leaf uses kernel
    O_RDONLY|O_NONBLOCK|O_NOFOLLOW|O_CLOEXEC followed by fstat regular-file BEFORE
    reading; FIFO/device/socket/directory/symlink is never an accepted source.
    Same retained FD supplies all bytes. No precheck/open pathname race or native-
    path re-resolution. Read at most 4096 bytes per joined operation, poll outside
    the worker, never retain more than max_bytes plus one bounded overflow probe.
    Grow a bounded OCaml buffer only after per-chunk budget checks. The caller
    supplies min(per-source cap, remaining aggregate cap), not an invented 8 MiB cap.

    Compare opened device/inode/size/mtime/ctime before and after the read; reject
    observed changes/deletion or differing length. Directory-entry replacement may
    be detected by descriptor-relative fstatat(no follow) identity comparison, but
    no all-file atomic snapshot or proof against metadata-preserving rewrites.
    Missing is recoverable as an absent optional companion ONLY when operation is
    Open_file (exact leaf); missing parent/root and all other errors fail capture.
    Errors/exception/cancellation return no partial bytes. No callback retained.
    max_bytes must be nonnegative; zero admits only an empty file. *)
val read
  :  t
  -> member:Source.Portable_path.t
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> (string, Error.t) Result.t
