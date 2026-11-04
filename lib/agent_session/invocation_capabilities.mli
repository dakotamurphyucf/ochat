(** Checked host capabilities belonging to the exact dispatched invocation.
    This record is not an execution identity or a permit. Its actor-issued
    closures retain physical owner, original authentication and callback epochs;
    copying or constructing the record cannot mint any of those facts. *)
type t =
  { borrow_agent_actor :
      invocation:Agent_protocol.Invocation.t
      -> (Operator_authorization.t option, Agent_protocol.Error.t) result
  ; allocate_inference : Inference_allocation.Port.t
  ; model_jobs : Model_job_admission.t
  }

(** Explicit legacy scope with no authenticated actor and no inference or model
    admission capability. Never used as fallback from an expired owned scope. *)
val unavailable : t
