(** Checked host capabilities belonging to the exact dispatched invocation.
    This record is not an execution identity or a permit. Its actor-issued
    closures retain physical owner, original authentication and callback epochs;
    copying or constructing the record cannot mint any of those facts. *)
type t =
  { refresh_effect_current :
      invocation:Agent_protocol.Invocation.t -> (unit, Agent_protocol.Error.t) result
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

let unavailable =
  let denied () =
    Agent_protocol.Error.create
      Permission_denied
      ~message:"model execution has no authenticated physical owner"
      ~retryable:false
      ()
  in
  { refresh_effect_current = (fun ~invocation:_ -> Ok ())
  ; check_effect_current = (fun ~invocation:_ -> Ok ())
  ; borrow_agent_actor = (fun ~invocation:_ -> Ok None)
  ; allocate_inference = (fun ~graph:_ ~relation:_ ~configuration:_ -> Error (denied ()))
  ; model_jobs = Model_job_admission.Host.create ~admit:(fun _ -> Error (denied ()))
  }
;;
