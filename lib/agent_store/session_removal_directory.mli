(** One validated removal namespace capability. The owning store retains its
    daemon lock and per-session reservation throughout every operation. *)
open! Core

module R = Session_archive_record

type t

(** Admit a terminal source under [sessions], without namespace mutation. Caller
    must retire runtime/resources and close its Handle before [stage]. *)
val create
  :  env:Eio_unix.Stdenv.base
  -> data_root:Data_root.t
  -> Agent_protocol.Id.Session.t
  -> (t, Store_error.t) Result.t

(** Admit recognized deleted-session containers. Root/payload marker alternatives
    are validated before effects. Empty private containers are retired with a
    parent sync and never create a session outcome or mutate an active source. *)
val discover
  :  env:Eio_unix.Stdenv.base
  -> data_root:Data_root.t
  -> (t list, Store_error.t) Result.t

val session_id : t -> Agent_protocol.Id.Session.t
val receipts : t -> R.Receipt.t list

(** Current pinned Applied Remove outcome, absent for an empty cleanup-only
    container. Absence grants no lifecycle mutation or execution selection. *)
val terminal_outcome : t -> R.Outcome.t option

(** Move source to stable container/payload, sync both parents, then move the
    original terminal marker to container root and sync both marker parents.
    No copies or resurrection rollback. Errors retain discoverable proof. *)
val stage : t -> (unit, Store_error.t) Result.t

(** Complete exact original host-owned generic receipts outside all projection
    and index locks, then durably acknowledge each immutable proof. Generic
    Success must outlive this session payload; no custom receipt store is used. *)
val complete_receipts
  :  t
  -> complete:(R.Receipt.t -> (unit, Store_error.t) Result.t)
  -> (unit, Store_error.t) Result.t

(** Reject any unacknowledged proof. Before destructive payload cleanup, move
    any nested workspace directory whole beside the stable root marker and sync
    both rename parents. Workspace and original proof remain permanently after
    payload absence; retained containers never become indexed sessions. Without
    a workspace, retire marker/container only after payload absence and its
    parent sync, then sync lost+found. Symlinks are never followed. *)
val cleanup : t -> (unit, Store_error.t) Result.t
