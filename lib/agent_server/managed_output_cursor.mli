(** Authenticated, append-stable output positions. These are pagination data,
    never bearer authority. Authorize the actual caller before resolving a cursor
    and again before disclosure. A process restart expires cursors explicitly;
    callers can obtain a bounded fresh snapshot instead of assuming no output.
    The caller supplies ordered, deterministic serialized output entries. *)
type t

type query =
  | All_outputs
  | Submission of Agent_protocol.History.Id.t
[@@deriving equal, sexp]

type context =
  { relationship : Agent_store.Delegation_store.Reference.t
  ; generation : int
  ; compaction_generation : int
  ; history_epoch : Agent_session.Durable_event_log.history_epoch
  ; access_revision : string
  ; query : query
  }

type position =
  { entry : int
  ; byte : int
  }
[@@deriving equal, sexp]

val create : unit -> t

(** Binds the consumed prefix, plus the current entry when it is partially read.
    Later appends within the retained history epoch do not invalidate the cursor.
    History replacement or replay-window eviction expires even an unread tail.
    Changing scope/query/generation or using another host instance also expires it.
    Offsets are bytes; the output pager must choose valid encoding boundaries. *)
val issue
  :  t
  -> context:context
  -> entries:string list
  -> position
  -> (Agent_protocol.Page.Cursor.t, Agent_protocol.Error.t) result

(** An absent cursor starts at zero. Invalid, tampered or stale supplied cursors
    return [Cursor_expired] with [snapshot_required=true], never an empty page. *)
val resolve
  :  t
  -> context:context
  -> entries:string list
  -> Agent_protocol.Page.Cursor.t option
  -> (position, Agent_protocol.Error.t) result
