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

let unavailable =
  let denied () =
    Agent_protocol.Error.create
      Permission_denied
      ~message:"model execution has no authenticated physical owner"
      ~retryable:false
      ()
  in
  { borrow_agent_actor = (fun ~invocation:_ -> Ok None)
  ; allocate_inference = (fun ~graph:_ ~relation:_ ~configuration:_ -> Error (denied ()))
  ; model_jobs = Model_job_admission.Host.create ~admit:(fun _ -> Error (denied ()))
  }
;;
