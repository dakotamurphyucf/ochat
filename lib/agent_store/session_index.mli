(** Rebuildable data-root index for session listing and scheduling hints. *)

module Entry = Session_index_entry

type t

(** [open_or_create] loads the atomic index snapshot or creates an empty one. *)
val open_or_create : env:Eio_unix.Stdenv.base -> path:string -> (t, Store_error.t) result

(** [open_or_rebuild ~env ~path ~rebuild] invokes [rebuild] only when [path]
    does not exist. Atomically install the complete result without publishing
    an intermediate empty index. Preserve existing corrupt or nonregular paths
    and fail closed. The caller must hold the data-root ownership lock. *)
val open_or_rebuild
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> rebuild:(unit -> (Entry.t list, Store_error.t) result)
  -> (t, Store_error.t) result

(** Last validated observation; authoritative consumers use [list_checked]. *)
val list : t -> Entry.t list

val find : t -> Agent_protocol.Id.Session.t -> Entry.t option

(** Mutations are serialized and durably replace the rebuildable snapshot. *)
val upsert : t -> Entry.t -> (unit, Store_error.t) result

val remove : t -> Agent_protocol.Id.Session.t -> (unit, Store_error.t) result

(** [replace_all] atomically installs a fully rebuilt index. *)
val replace_all : t -> Entry.t list -> (unit, Store_error.t) result

(** Validate and encode the complete replacement before [publish_authority].
    Hold the index mutation lock through the callback and durable projection
    replacement. The callback must not call index operations. Its successful
    authoritative publication is not rolled back when projection I/O fails.
    Once this callback begins, failure leaves checked reads unavailable unless
    the exact requested complete projection is proven installed. A valid old
    snapshot alone cannot prove reconciliation with newer metadata. *)
val with_prepared_upsert
  :  ?expected_entry:Entry.t option
  -> t
  -> Entry.t
  -> publish_authority:(unit -> ('a, Store_error.t) result)
  -> ('a, Store_error.t) result

(** Prepare exact logical catalog absence before authoritative terminal proof.
    Ownership, failure and [expected_entry] rules match [with_prepared_upsert]. *)
val with_prepared_remove
  :  ?expected_entry:Entry.t option
  -> t
  -> Agent_protocol.Id.Session.t
  -> publish_authority:(unit -> ('a, Store_error.t) result)
  -> ('a, Store_error.t) result

(** Uncertain publication refreshes from disk before returning its original
    failure. If refresh fails, checked reads and mutations reject until reopen. *)
val availability : t -> (unit, Store_error.t) result

val list_checked : t -> (Entry.t list, Store_error.t) result

val find_checked
  :  t
  -> Agent_protocol.Id.Session.t
  -> (Entry.t option, Store_error.t) result

(** Prevalidate the complete preserved replacement without any filesystem effect.
    Canonical commit admission holds the projection owner before this index lock. *)
val validate_upsert : t -> Entry.t -> (unit, Store_error.t) result

(** Prevalidate the complete preserved index after logical absence without effects. *)
val validate_remove : t -> Agent_protocol.Id.Session.t -> (unit, Store_error.t) result
