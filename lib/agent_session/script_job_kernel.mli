open Core

(** Lexical bookkeeping shared by script-job owners. This module does not own a
    job, authenticate an actor, publish a reservation, read a result, or grant a
    capability. The caller's borrowed stage/read/cancel operations must enforce
    their own physical owner, current binding, and result-custody checks. In
    particular, an ID recorded here is only a comparison coordinate. *)
type t

(** [with_scope] checks the selected registry before entry and again after a
    successful callback, including one Eio yield and its optional execution
    checkpoint. It expires [t] on every return, exception, and cancellation.
    Every recorded provisional start is offered to [abort] on an error or
    exception, even if an earlier abort raises. Callback exceptions, including
    cancellation, retain their original identity and backtrace over cleanup
    failures. Without an original exception, the first cleanup exception is
    raised after all aborts have been attempted. A prepared but unacknowledged
    parent commit fails and aborts. [abort] is the exact borrowed owner's
    cleanup operation, not an authority supplied by this module. It must be
    idempotent for each ID. *)
val with_scope
  :  selected:Chat_response.Tool_capability.t
  -> current_capabilities:(unit -> Chat_response.Tool_capability.t)
  -> abort:(Agent_protocol.Id.Job.t -> unit)
  -> error:(string -> 'error)
  -> (t -> ('a, 'error) result)
  -> ('a, 'error) result

(** Checks lexical liveness and exact selected pins. [check_open] requires the
    parent commit still open; [check_selection] also permits the prepared phase
    for coupled notification validation. Neither checks actor or job custody. *)
val check_open : t -> (unit, string) result

(** Trusted physical managed admission may replace the initial target selection
    exactly once with the already admitted implementation's declared dependency
    selection. The scope must still be open and must never have recorded a start,
    including one later rolled back. This is comparison state, not permission or
    a capability grant; the caller proves the prepared implementation and live
    physical invocation before calling. *)
val rebind_selected
  :  t
  -> selected:Chat_response.Tool_capability.t
  -> current_capabilities:(unit -> Chat_response.Tool_capability.t)
  -> (unit, string) result

val check_selection : t -> (unit, string) result

(** Check cancellation in the current fiber and the installed execution control before or after a
    yielding source preparation. Preserves cancellation; does not consume fuel
    beyond the supplied control. *)
val checkpoint : t -> unit

(** Install the authentic Job.* operation handlers for this lexical scope, and
    retain [control] only until the scope exits. Handler callbacks belong to
    the concrete owner adapter; this call does not manufacture host authority. *)
val install
  :  ?control:Chatml.Chatml_lang.execution_control
  -> t
  -> handlers:Chat_response.Background_job_operations.handlers
  -> Chatml_host_runtime.runtime_config
  -> Chatml_host_runtime.runtime_config

(** Record the exact ID returned by a successful physically owned stage.
    Rechecks live/open scope and exact selected pins after that potentially
    yielding stage. The caller must invoke this under the same cancellation
    protection as its stage and abort the newly staged reservation on any
    rejection, so it cannot escape cleanup. Duplicate IDs reject too. *)
val record_start : t -> Agent_protocol.Id.Job.t -> (unit, string) result

(** Cleanup-only rollback of one physically staged ID, including after lexical
    expiry. Safe to repeat for the same ID. The borrowed [abort] operation is
    called only if this scope recorded the ID. [with_scope] also cleans up all
    still-issued IDs on failure/cancellation. *)
val rollback_start : t -> Agent_protocol.Id.Job.t -> unit

(** A Pending result may reference only an ID returned from this lexical
    scope. The actual job and parent transaction remain the caller's proof. *)
val validate_work : t -> Agent_protocol.Invocation.work -> (unit, string) result

(** Parse the actual returned Job.start effects with the sole existing
    Background_job_operations grammar and verify all surviving IDs belong to
    this scope. The concrete parent still selects/persists them and aborts
    unselected work. This returns no publication capability. *)
val split_starts
  :  t
  -> Chatml.Chatml_lang.eff list
  -> (Agent_protocol.Id.Job.t list * Chatml.Chatml_lang.eff list, string) result

(** The concrete owner calls [mark_prepared] only after its own selected-parent
    prepare succeeds. The acknowledgement may run only once, inside this live
    lexical scope and after that parent's durable save. Repeated or late calls
    raise [Invalid_argument] and never revive a closed scope. A prepared scope
    that exits without acknowledgement aborts provisional work. No application
    effect or persistence occurs here. *)
val mark_prepared : t -> (unit -> unit, string) result
