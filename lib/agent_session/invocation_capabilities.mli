(** Checked host capabilities belonging to the exact dispatched invocation.
    This record is not an execution identity or a permit. Its actor-issued
    closures retain physical owner, original authentication and callback epochs;
    copying or constructing the record cannot mint any of those facts. *)
type t =
  { refresh_effect_current :
      invocation:Agent_protocol.Invocation.t -> (unit, Agent_protocol.Error.t) result
    (** Negative refresh for SAME physical execution and admitted operation
        views, outside the Actor mailbox/Owner state mutex. Checks exact owner
        before/after yielding and invalidates stale views before waits. Does not
        acquire new admission, count/token capacity or agent-creation custody.
        Cancellation propagates and cannot restore an older cached view. *)
  ; check_effect_current :
      invocation:Agent_protocol.Invocation.t -> (unit, Agent_protocol.Error.t) result
    (** Negative currentness of this exact dispatched effect. Actor-issued
        closures check the physical execution/borrow lifetime and inherited
        admitted operation guard, callback-free on the issuing Actor domain.
        This does not require or grant original agent-creation custody,
        child eligibility, model admission or authentication. *)
  ; borrow_agent_actor :
      invocation:Agent_protocol.Invocation.t
      -> (Operator_authorization.t option, Agent_protocol.Error.t) result
  ; allocate_inference : Inference_allocation.Port.t
  ; model_jobs : Model_job_admission.t
  }

(** Explicit legacy scope with no authenticated actor and no inference or model
    admission capability. Its negative effect check succeeds only as the
    explicit compatibility value; the lending Native_tool_invocation scope
    independently checks exact dispatch and lexical lifetime. Never used as
    fallback from an expired owned scope. *)
val unavailable : t
