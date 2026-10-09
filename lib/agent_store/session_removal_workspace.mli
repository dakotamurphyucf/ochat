open! Core

type t

module Disposition : sig
  type t =
    | Retained
    | Absent
end

(** Validate the exact private removal container under the owned data root and
    its same-session Removed terminal document. Caller retains the store lock
    and removal reservation; this capability grants no session authority. *)
val create
  :  env:Eio_unix.Stdenv.base
  -> data_root:Data_root.t
  -> session_id:Agent_protocol.Id.Session.t
  -> container:string
  -> document:Session_archive_document.t
  -> (t, Store_error.t) Result.t

(** Move payload/workspace whole to container/workspace, without traversing its
    contents, then sync both rename parents before destructive payload cleanup.
    Retries recognize a completed rename. Collisions/symlinks fail closed. *)
val preserve : t -> (unit, Store_error.t) Result.t

(** Caller first establishes payload absence and syncs its parent. Every existing workspace directory,
    including an empty directory, is retained with the original terminal proof.
    Absence permits ordinary terminal-container cleanup. Never deletes content. *)
val disposition : t -> (Disposition.t, Store_error.t) Result.t
