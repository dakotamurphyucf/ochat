(** Private ordinal-file layout for a validated authoring upload directory. Source
    IDs and client paths are never used as native filenames. The caller owns the
    directory capability and serializes these operations with phase updates. *)

module B = Agent_protocol.Authoring_bundle.Bundle

val manifest_name : string
val phase_name : string
val progress_name : int -> string
val data_name : int -> string

(** The accepted offset document, rather than file length, is authoritative.
    Recovery truncates any fsynced suffix that was not followed by an accepted
    offset replacement. A missing positive prefix fails closed. *)
val truncate_suffix
  :  Eio.Fs.dir_ty Eio.Path.t
  -> int
  -> int
  -> (unit, Store_error.t) result

val append_member
  :  Eio.Fs.dir_ty Eio.Path.t
  -> int
  -> accepted:int
  -> bytes:string
  -> (unit, Store_error.t) result

val file_bytes
  :  Eio.Fs.dir_ty Eio.Path.t
  -> int
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> (string, Store_error.t) result

val read_repeat
  :  Eio.Fs.dir_ty Eio.Path.t
  -> int
  -> offset:int
  -> bytes:string
  -> checkpoint:(unit -> unit)
  -> (bool, Store_error.t) result

(** Admission checks all names against the exact ordinal manifest layout,
    including recognized Durable_file temporary records. *)
val validate_entry_names : B.Member.t list -> string list -> bool

val is_member_filename : string -> bool

val remove_temporary_files
  :  Eio.Fs.dir_ty Eio.Path.t
  -> string list
  -> (unit, Store_error.t) result

val cleanup_member_files
  :  Eio.Fs.dir_ty Eio.Path.t
  -> string list
  -> (unit, Store_error.t) result
