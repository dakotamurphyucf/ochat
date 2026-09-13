(** Private queue frame for a retained generic job's terminal result. Decoding
    validates structure only; the actor must authorize the source, generation,
    exact retained attempt/result and prior claims before invoking a moderator. *)
type t = private
  { session_id : Agent_protocol.Id.Session.t
  ; job_id : Agent_protocol.Id.Job.t
  ; generation : int
  ; attempt : int
  ; source : Agent_protocol.Invocation.observer
  ; completed_at : Agent_protocol.Timestamp.t
  ; result : Agent_protocol.Stored_completion.t
  }

val create
  :  source:Agent_protocol.Invocation.observer
  -> Agent_protocol.Job.t
  -> (t, string) result

val equal : t -> t -> bool
val capture : t -> Chatml.Chatml_lang.value
val decode : Chatml.Chatml_lang.value -> (t option, string) result

(** Internal_event JSON with kind=background_job_completed, job_id, exact decimal
    attempt and bounded stored completion. Artifact references remain references;
    script Job.read_result performs the authorized full-result read. *)
val script_event : Chatml.Chatml_lang.value -> (Chatml.Chatml_lang.value, string) result
