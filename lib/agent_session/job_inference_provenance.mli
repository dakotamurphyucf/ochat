(** Stored attribution evidence only. No live owner, original authentication,
    provider Handle or execution permission is created by these checks. Historical
    missing origin stays unavailable; it is never inferred from current policy.

    A Run origin must name the sole retained Run containing an actual occurrence
    of this same Job ID/generation, including relinquishment or terminal evidence.
    This lifetime relationship does not prove the current attempt is admitted;
    queued-before-claim and queued-retry cancellations retain their real history. *)
val validate
  :  jobs:Agent_protocol.Job.t list
  -> runs:Run_state.t option
  -> (unit, Agent_protocol.Error.t) result

(** Retained records keep their exact classification, including historical
    absence. Call after an ordered batch or whole-state adoption; the occurrence
    relationship is validated only against the complete resulting Session. *)
val validate_transition
  :  previous:Agent_protocol.Job.t list
  -> incoming:Agent_protocol.Job.t list
  -> (unit, Agent_protocol.Error.t) result
