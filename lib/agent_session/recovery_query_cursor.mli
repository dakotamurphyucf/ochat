(** Actor-local, opaque recovery query cursors. Only the issuing actor can
    open them. An authenticated payload has no authority of its own. *)
open! Core

type t

val create : unit -> t
val expired : unit -> Agent_protocol.Error.t

val seal
  :  t
  -> now:Mtime.t
  -> audience:string
  -> Recovery_history_query.Continuation.t
  -> (string, Agent_protocol.Error.t) result

val read
  :  t
  -> now:Mtime.t
  -> audience:string
  -> Agent_protocol.Page.Cursor.t
  -> (Recovery_history_query.Continuation.t, Agent_protocol.Error.t) result
