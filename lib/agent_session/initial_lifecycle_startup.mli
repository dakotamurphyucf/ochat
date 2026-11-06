(** Runtime-bound first extension startup DATA validation. This module issues no
    original, admission or callback and never imports Actor authority. *)
type t = Runtime_builder.Authored_start.t

(** Physical binding only; no Manager entry, script state or authorization. *)
val matches_runtime
  :  t
  -> worker:Operation_worker.t option
  -> manager:Chat_response.Moderator_manager.t option
  -> bool

(** Pure candidate eligibility: ordinary Ready/Running first startup with no
    historical Runs or prior same-source/generation startup claim of any outcome.
    False leaves ordinary runtime installation unchanged. True grants no authority
    and does not replace the strict physical/source/currentness checks below. *)
val eligible : t -> state:Session_state.t -> bool

(** Validate first unconsumed extension startup against the actual Ready/Running
    source and physical resources. Reads the idle Manager's full checkpoint only
    before runtime installation exposes it to callbacks. *)
val check_capture
  :  t
  -> state:Session_state.t
  -> worker:Operation_worker.t option
  -> manager:Chat_response.Moderator_manager.t option
  -> (unit, Agent_protocol.Error.t) result

(** Same source and resource currentness without entering the Manager. The actual
    queued-event planner separately validates its supplied full snapshot. Safe
    while that exact Manager owns the callback gate. *)
val check_claim
  :  t
  -> state:Session_state.t
  -> worker:Operation_worker.t option
  -> manager:Chat_response.Moderator_manager.t option
  -> (unit, Agent_protocol.Error.t) result
