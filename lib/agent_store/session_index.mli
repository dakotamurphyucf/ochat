(** Rebuildable data-root index for session listing and scheduling hints. *)

module Entry : sig
  type t =
    { session : Agent_protocol.Session.t
    ; runnable_job_count : int
    ; deliverable_job_count : int
    ; earliest_schedule_due : Agent_protocol.Timestamp.t option
    ; owner_grace_deadline : Agent_protocol.Timestamp.t option
    ; archived : bool
    }
  [@@deriving sexp]
end

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

val list : t -> Entry.t list
val find : t -> Agent_protocol.Id.Session.t -> Entry.t option

(** Mutations are serialized and durably replace the rebuildable snapshot. *)
val upsert : t -> Entry.t -> (unit, Store_error.t) result

val remove : t -> Agent_protocol.Id.Session.t -> (unit, Store_error.t) result

(** [replace_all] atomically installs a fully rebuilt index. *)
val replace_all : t -> Entry.t list -> (unit, Store_error.t) result
