(** Bounded, read-only access to retained files beneath an owned root. The caller
    must serialize storage mutation and validate each file's format and identity
    before using reference absence as deletion evidence. This module supplies no
    deletion authority. Use one reader for an entire collection attempt. *)
type t

(** The entry budget covers directory opens, names returned and file reads; the
    byte budget is shared across reads. Serialize access to the reader itself. *)
val create
  :  env:Eio_unix.Stdenv.base
  -> root:string
  -> max_entries:int
  -> max_bytes:int
  -> (t, Store_error.t) result

(** Borrow an existing directory capability without acquiring an ambient root.
    The directory must expose a native path for bounded directory streaming.
    This reader cannot select a different root with [at_root]. *)
val create_at
  :  root:Eio.Fs.dir_ty Eio.Path.t
  -> max_entries:int
  -> max_bytes:int
  -> (t, Store_error.t) result

val root : t -> string

(** Physical Eio filesystem affinity for host-root rebinding. A borrowed reader
    never gains authority over an unrelated host root. *)
val uses_filesystem : t -> env:Eio_unix.Stdenv.base -> bool

(** Remaining bytes in this reader's existing shared invocation allowance.
    Serialized owner operations may admit a fixed header's declared remainder
    before allocating it, then charge each actual chunk through [charge_bytes]. *)
val remaining_bytes : t -> int

(** Narrow a borrowed scan without resetting its parent allowance. Every later
    entry and byte charge must fit both the new local ceiling and the original
    shared reader; exhausted attempts debit no budget for the failed charge.
    The returned reader retains the same filesystem authority and root. Nesting
    beyond eight scopes is rejected before any budget changes. *)
val with_local_limits : t -> max_entries:int -> max_bytes:int -> (t, Store_error.t) result

(** Charge serialized in-memory roots against the same aggregate byte allowance
    as filesystem reads. Negative sizes and exhausted budgets fail the attempt. *)
val charge_bytes : t -> int -> (unit, Store_error.t) result

(** Charge one explicit no-follow kind probe of an authentic owner-derived
    root, including a missing structural directory. This shares the same
    allowance as list/read and cannot mint a filesystem capability. *)
val charge_entry : t -> (unit, Store_error.t) result

(** Inspect a child without following links; consumes one entry. Only regular
    files/directories are accepted, and all parent directories are validated. *)
val kind : t -> path:string -> ([ `Directory | `File ], Store_error.t) result

(** The same charged, no-follow inspection with proved absence distinguished
    from links/unsupported entries. Startup inventories use this for optional
    owned namespaces without treating a failed [kind] as absence. *)
val kind_or_missing
  :  t
  -> path:string
  -> ([ `Directory | `File ] option, Store_error.t) result

(** Select another host-owned absolute root while sharing the same remaining
    entry/byte budgets. This does not reset allowances or authorize paths. The
    caller must own and serialize both roots; reads still reject linked paths. *)
val at_root : t -> root:string -> (t, Store_error.t) result

(** Stream directory names with a native directory handle in an Eio system thread,
    stopping at the budget. Returns sorted names. Rejects linked directories. *)
val list : t -> directory:string -> (string list, Store_error.t) result

(** Cooperative bounded directory enumeration. Checkpoints run in the calling
    Eio domain, including during enumeration, and their original exceptions and
    cancellation propagate. Shares this reader's entry allowance. *)
val list_with_checkpoint
  :  t
  -> directory:string
  -> checkpoint:(unit -> unit)
  -> (string list, Store_error.t) result

(** Read a regular file without following observed links or accepting parent-path
    traversal. Enforces both the per-file and remaining aggregate byte ceilings,
    including growth after stat. Every error invalidates the collection attempt. *)
val read : t -> path:string -> max_bytes:int -> (string, Store_error.t) result

(** The same bounded read, with calling-domain checkpoints between bounded
    chunks. Callback exceptions and cancellation propagate unchanged. *)
val read_with_checkpoint
  :  t
  -> path:string
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> (string, Store_error.t) result
