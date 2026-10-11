open Core
module Native = Git_host_native

(** Joined descriptor ownership and bounded private tree operations. No Git
    commands, network authority, callback verification or attempt ledger. *)
type t

val close : t -> unit

val borrow_root
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> checkpoint:(unit -> unit)
  -> (t, Native.Code.t) Result.t

val borrow_deployment
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> checkpoint:(unit -> unit)
  -> (t, Native.Code.t) Result.t

val borrowed_fd : t -> Native.Native_unix.file_descr
val validate_root : t -> (unit, Native.Code.t) Result.t
val with_cwd : t -> f:(Eio_unix.Fd.t -> 'a) -> 'a

val directory
  :  t
  -> name:string
  -> create:bool
  -> exclusive:bool
  -> (t, Native.Code.t) Result.t

val with_directory
  :  t
  -> name:string
  -> create:bool
  -> exclusive:bool
  -> f:(t -> ('a, Native.Code.t) Result.t)
  -> ('a, Native.Code.t) Result.t

val lease : t -> name:string -> (t, Native.Code.t) Result.t

val names
  :  t
  -> max_entries:int
  -> checkpoint:(unit -> unit)
  -> (string list, Native.Code.t) Result.t

val entry : t -> name:string -> (Native.Entry.t, Native.Code.t) Result.t

val read_file
  :  t
  -> name:string
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> (string, Native.Code.t) Result.t

val create_file
  :  t
  -> name:string
  -> contents:string
  -> checkpoint:(unit -> unit)
  -> (unit, Native.Code.t) Result.t

module Inventory : sig
  type file =
    { path : string list
    ; payload_bytes : int
    }

  type t

  val files : t -> file list
  val directories : t -> string list list
  val entries : t -> int
  val payload_bytes : t -> int
  val work_steps : t -> int
end

(** Traverse only bounded depth and entries, reject symlinks, charge exact regular
    extents with checked arithmetic. The caller validates the profile's allowed
    directory/file names and config before invoking Git on an admitted cache. *)
val inventory
  :  t
  -> max_entries:int
  -> max_depth:int
  -> max_payload_bytes:int
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (Inventory.t, Native.Code.t) Result.t

(** Remove only this owned child tree without following links, with a bounded
    entry/depth budget. Protected caller cleanup may leave bounded leftovers on
    failure; never treat an incomplete tree as admitted. *)
val remove_tree
  :  t
  -> name:string
  -> max_entries:int
  -> max_depth:int
  -> checkpoint:(unit -> unit)
  -> (unit, Native.Code.t) Result.t

(** Copy admitted inventory using exclusive files and 4096-byte checkpoints;
    reject changes/length mismatch, sync copied files and directory, then publish
    create-only within the same retained store filesystem. Copy overlap must be
    reserved by the caller before invocation. *)
val copy_inventory
  :  t
  -> destination:t
  -> Inventory.t
  -> checkpoint:(unit -> unit)
  -> (unit, Native.Code.t) Result.t

val publish
  :  t
  -> name:string
  -> destination:t
  -> destination_name:string
  -> (unit, Native.Code.t) Result.t
