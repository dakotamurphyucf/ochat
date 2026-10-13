(** Private ordinal-file layout for a validated authoring upload directory. Source
    IDs and client paths are never used as native filenames. The caller owns the
    directory capability and serializes these operations with phase updates. *)

module B = Agent_protocol.Authoring_bundle.Bundle

val manifest_name : string
val phase_name : string
val intent_name : string
val marker_name : string
val progress_name : int -> string
val data_name : int -> string
val supplemental_name : int -> string

(** Exact ordinal supplementary captures. Source IDs never become filenames.
    A prior final file must match the same bytes; a missing one is atomically
    written and synced. Admission and comparison poll [checkpoint]; a final
    rename rechecks admission. Known conflicts are typed; callback exceptions
    and cancellation retain identity. *)
val install_supplemental
  :  Eio.Fs.dir_ty Eio.Path.t
  -> ordinal:int
  -> bytes:string
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> (unit, Store_error.t) result

val read_supplemental
  :  Eio.Fs.dir_ty Eio.Path.t
  -> ordinal:int
  -> expected_bytes:int
  -> checkpoint:(unit -> unit)
  -> (string, Store_error.t) result

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
val validate_entry_names
  :  ?intent:Authoring_upload_document.Intent.t
  -> ?marker:Authoring_upload_document.Marker.t
  -> B.Member.t list
  -> string list
  -> bool

val is_member_filename : string -> bool
val is_publication_filename : string -> bool

val remove_temporary_files
  :  Eio.Fs.dir_ty Eio.Path.t
  -> string list
  -> (unit, Store_error.t) result

val cleanup_member_files
  :  Eio.Fs.dir_ty Eio.Path.t
  -> string list
  -> (unit, Store_error.t) result
