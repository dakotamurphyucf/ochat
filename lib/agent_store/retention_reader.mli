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

val root : t -> string

(** Charge serialized in-memory roots against the same aggregate byte allowance
    as filesystem reads. Negative sizes and exhausted budgets fail the attempt. *)
val charge_bytes : t -> int -> (unit, Store_error.t) result

(** Inspect a child without following links; consumes one entry. Only regular
    files/directories are accepted, and all parent directories are validated. *)
val kind : t -> path:string -> ([ `Directory | `File ], Store_error.t) result

(** Select another host-owned absolute root while sharing the same remaining
    entry/byte budgets. This does not reset allowances or authorize paths. The
    caller must own and serialize both roots; reads still reject linked paths. *)
val at_root : t -> root:string -> (t, Store_error.t) result

(** Stream directory names with a native directory handle in an Eio system thread,
    stopping at the budget. Returns sorted names. Rejects linked directories. *)
val list : t -> directory:string -> (string list, Store_error.t) result

(** Read a regular file without following observed links or accepting parent-path
    traversal. Enforces both the per-file and remaining aggregate byte ceilings,
    including growth after stat. Every error invalidates the collection attempt. *)
val read : t -> path:string -> max_bytes:int -> (string, Store_error.t) result
