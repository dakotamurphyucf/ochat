open! Core

(** Find positive retained causal evidence for an exact queued [Internal_event].
    Reads owned, relinquished and terminal work across all supplied Runs before
    live/source filtering. Other phases and missing exact occurrences return
    [None], which grants neither Unbound provenance nor execution authority.
    Conflicting retained owners reject. A positive owner must not be replaced by
    the draining operation merely because the causal Run is terminal or stale. *)
val find
  :  Agent_protocol.Run.t list
  -> executing:Agent_protocol.Moderator_execution.t
  -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result
