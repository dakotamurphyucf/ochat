(** Principal-projected session state used by all connected clients. *)

type t =
  { session : Session.t
  ; canonical_history : History.Window.t
  ; archived_revisions : int64 list [@sexp.list]
  ; effective_history : History.Window.t option
  ; deferred_entries : History.entry list
  ; permissions : Permission.t list
  ; grants : Grant.t list
  ; jobs : Job.t list
  ; schedules : Schedule.t list
  ; active_tool_calls : Jsonaf.t list
  ; active_agent_calls : Jsonaf.t list
  ; halted : bool
  ; halt_reason : string option
  ; failure : Error.t option
  ; revision : int64
  ; latest_event_sequence : int64
  }
[@@deriving sexp]

(** [to_json t] encodes a rendering-neutral client snapshot. *)
val to_json : t -> Jsonaf.t

(** [of_json json] rejects snapshots whose top-level revision differs from the
    embedded session summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result
