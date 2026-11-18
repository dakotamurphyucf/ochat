(** Pure lookup of an immutable Goal/Run association. These values are evidence,
    never execution permits. The actor separately authenticates the exact live
    operation, invocation, event, initializer or acknowledged job attempt. *)
type t

val goal_id : t -> Agent_protocol.Id.Goal.t
val episode : t -> Agent_protocol.Goal_episode.t

val of_run
  :  Session_state.t
  -> Agent_protocol.Run.t
  -> (t option, Agent_protocol.Error.t) result

(** Include retained relinquishment/terminal provenance so loss of current Run
    ownership cannot silently downgrade known goal work to unrelated work.
    Conflicting historical assignments reject rather than select an owner. *)
val of_work
  :  Session_state.t
  -> Agent_protocol.Run_work.t
  -> (t option, Agent_protocol.Error.t) result

(** Resolve retained evidence without losing a Run that has no Goal. These
    lookups grant no current ownership; actual allocation separately requires
    the exact current occurrence and live physical scope. Conflicting Run
    origins reject rather than choose a Goal or downgrade to unbound work. *)
val run_of_work
  :  Session_state.t
  -> Agent_protocol.Run_work.t
  -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result

(** Read the immutable stored classification. Historical missing provenance
    rejects as unavailable; only positively Unbound work returns None. A named
    Run must retain the same Job ID/generation lifetime occurrence, including
    truthful queued cancellation history. It need not have a Goal link. *)
val run_of_job
  :  Session_state.t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result

(** Resolve the exact stored Run's immutable Goal link. There is no launch
    ancestry or absent-field fallback; current attempt/authority remain actor
    responsibilities. A positively unbound Job or a Run without a Goal has no
    Goal attribution, while unavailable provenance rejects. *)
val of_job
  :  Session_state.t
  -> Agent_protocol.Job.t
  -> (t option, Agent_protocol.Error.t) result

(** Resolve an actual invocation's immutable admitted origin, including delayed
    observations whose originating operation is no longer retained. Missing
    classification rejects; terminal Run evidence remains named. This lookup
    grants no physical callback lifetime or current Run permission. *)
val run_of_invocation
  :  Session_state.t
  -> Agent_protocol.Invocation.t
  -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result

(** Pure original-parent resolution for native child work, separate from the
    strict immutable inference classification above. A retained endpoint stops
    traversal even when relinquished or terminal; callers must check live Run
    authority and exact owned membership before using it. No result grants
    execution permission or changes missing provenance to Unbound. *)
module Retained_parent : sig
  type t = private
    { run : Agent_protocol.Run.t
    ; work : Agent_protocol.Run_work.t
    }

  (** Resolve the exact current occurrence while retaining conflicting or stale
      evidence from every attempt in this Job ID/generation lifetime. A known
      lifetime without its current occurrence rejects rather than returns None. *)
  val of_job
    :  Session_state.t
    -> Agent_protocol.Job.t
    -> (t option, Agent_protocol.Error.t) result

  (** Prefer the retained event occurrence, then its actual matching Job attempt.
      Missing or mismatched referenced attempts reject. *)
  val of_event
    :  Session_state.t
    -> Agent_protocol.Moderator_execution.t
    -> (t option, Agent_protocol.Error.t) result

  (** Prefer retained invocation work, then original parent invocation, Job or
      event in that order. Missing links and ancestry beyond the retained
      invocation count reject. Never consult an unrelated draining operation. *)
  val of_invocation
    :  Session_state.t
    -> Agent_protocol.Invocation.t
    -> (t option, Agent_protocol.Error.t) result
end
