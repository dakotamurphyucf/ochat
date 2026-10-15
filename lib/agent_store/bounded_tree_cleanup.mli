open! Core

(** No-follow deletion mechanism shared by exact Store and Workspace owners.
    It grants NO deletion authority: callers first prove their root/marker,
    device/inode, protected-path exclusion and live lease as applicable. *)

module Limits : sig
  type t

  val create : max_entries:int -> max_bytes:int -> max_work_steps:int -> t Or_error.t
end

(** Remove all descendants except [marker_name], leaving the root and marker
    for the caller's protected final step. Enumerate with Retention_reader's
    bounded streaming/no-follow directory reader; inner symbolic links are
    unlinked, never followed. Every entry/stat/unlink/rmdir is charged and
    cooperatively checked. An interruption retains the root and marker for a
    later finite retry; already removed descendants remain removed. *)
val remove_children
  :  env:Eio_unix.Stdenv.base
  -> root:string
  -> expected_root:int64 * int64
  -> marker_name:string
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (unit, Store_error.t) result
