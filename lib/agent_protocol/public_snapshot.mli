(** Validated client read state, separate from the persisted internal snapshot.
    These fields and their codecs do not change session storage version 1. *)
module Fields : sig
  type t =
    { session : Session.t
    ; canonical_history : Public_history.Window.t
    ; archived_revisions : int64 list
    ; effective_history : Public_history.Window.t option
    ; deferred_entries : Public_history.t list
    ; permissions : Permission.t list
    ; grants : Grant.t list
    ; jobs : Job.t list
    ; extension_status : Extension_status.t list
    ; schedules : Schedule.t list
    ; active_tool_calls : Activity.Tool.summary list
      (** All currently running foreground tool calls, unique by activity key. *)
    ; active_agent_calls : Activity.Tool.summary list
      (** Exact classified subset of [active_tool_calls], including shell scripts.
          Nonempty activity requires the snapshot's active foreground operation. *)
    ; halted : bool
    ; halt_reason : string option
    ; failure : Error.t option
    ; revision : int64
    ; latest_event_sequence : int64
    }
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

(** Checks positions against the session summary, ownership and counter bounds,
    and extension generations. Fields remain immutable after admission. *)
val create : Fields.t -> (t, Error.t) result

val fields : t -> Fields.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
