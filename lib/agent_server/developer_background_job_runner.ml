open! Core
module P = Agent_protocol
module O = Developer_root_owner
module Store = Agent_store.Developer_run_store
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module M = Chat_response.Managed_tool_registry
module N = Developer_native_executor
module H = P.Host_invocation
module Jobs = Developer_script_job_service
module Script = Developer_script_job_execution
module Runtime = Chat_response.Agent_runtime
module Claim_scope = Agent_session.Developer_job_scope
module Claimed = Agent_session.Claimed_child_runtime
module Deadline = Developer_claim_deadline
module Approval_owner = Developer_approval_owner

type t =
  { root : O.run
  ; native : N.t
  ; results : O.Job_result.t
  ; env : Eio_unix.Stdenv.base
  ; agent_runtime : Runtime.t option
  ; jobs : Jobs.t
  ; authorize : H.t -> C.binding -> (unit, P.Error.t) result
  ; prepare_output : Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, P.Error.t) result
  ; approval_owner : Approval_owner.t option
  }

module Availability = struct
  type t =
    | Runnable
    | Malformed_supported
    | Agent_artifact_installer_unavailable
    | Issuer_source_installer_unavailable
    | Unsupported_kind
  [@@deriving equal, sexp_of]
end

let invalid message = Error (P.Error.invalid_request message)
let ( let* ) result f = Result.bind result ~f

let create
      ~root
      ~results
      ~env
      ~managed_definition
      ~agent_runtime
      ~jobs
      ~authorize
      ~prepare_output
  =
  if not (O.Job_result.issued_by_root results root)
  then invalid "developer Tool runner has another result owner"
  else
    let* () =
      match agent_runtime with
      | None -> Ok ()
      | Some runtime ->
        Runtime.admit_native_selection
          runtime
          ~selected:(O.native_ceiling root)
          ~max_bindings:(O.job_policy root).execution.max_array_items
          ~checkpoint:(fun () -> ())
    in
    let* native = N.create ~root in
    let* native = N.with_job_service native jobs in
    let* native =
      match managed_definition with
      | None -> Ok native
      | Some definition -> N.with_managed_definition native ~definition ~env
    in
    Ok
      { root
      ; native
      ; results
      ; env
      ; agent_runtime
      ; jobs
      ; authorize
      ; prepare_output
      ; approval_owner = None
      }
;;

let issued_by_root t root = phys_equal t.root root

let with_approval_owner t owner =
  if not (Approval_owner.issued_by_root owner t.root)
  then invalid "developer background approval owner belongs to another root"
  else (
    match t.approval_owner with
    | None ->
      let* native = N.with_approval_owner t.native owner in
      Ok { t with native; approval_owner = Some owner }
    | Some existing when phys_equal existing owner -> Ok t
    | Some _ -> invalid "developer background runner already owns another approval owner")
;;

let with_claimed_approval t active f =
  match t.approval_owner with
  | None -> f ()
  | Some owner -> Approval_owner.with_claimed_job owner ~active f
;;

let decode t job =
  if not (P.Job.equal_kind (P.Developer_job.kind job) P.Job.Async_tool)
  then invalid "developer Tool runner requires an async tool job"
  else B.of_json ~policy:(O.job_policy t.root) (P.Developer_job.payload job)
;;

let classify t request =
  match B.issuer_source request with
  | Some _ ->
    (match B.target_kind request, B.agent_artifact request with
     | B.Target_kind.Tool, (Some _ | None) | Script, None ->
       if Option.is_some (Jobs.queued_issuer t.jobs)
       then Availability.Runnable
       else Issuer_source_installer_unavailable
     | Script, Some _ -> Availability.Unsupported_kind)
  | None ->
    (match B.target_kind request, B.agent_artifact request with
     | B.Target_kind.Tool, None -> Availability.Runnable
     | Tool, Some _ ->
       (match t.agent_runtime with
        | None -> Agent_artifact_installer_unavailable
        | Some _ -> Runnable)
     | Script, _ -> Runnable)
;;

let availability t candidate =
  let job = O.Job_candidate.job candidate in
  if not (P.Job.equal_kind (P.Developer_job.kind job) P.Job.Async_tool)
  then Ok Availability.Unsupported_kind
  else (
    match decode t job with
    | Ok request -> Ok (classify t request)
    | Error _ -> Ok Malformed_supported)
;;

let selected_schema t ~managed_definition capabilities reference =
  let* binding =
    C.resolve capabilities ~id:reference.C.id ~fingerprint:reference.fingerprint
    |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
  in
  match C.implementation binding, managed_definition with
  | C.Native _, _ -> Ok (reference.implementation_revision, None)
  | Managed _, None -> invalid "managed Tool dispatcher is not installed"
  | Managed _, Some managed ->
    let* revision =
      M.execution_revision managed binding
      |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
    in
    let* prepared =
      M.resolve managed binding
      |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
    in
    let schema =
      Chat_response.Extension_compiler.completion_schema prepared
      |> Option.map ~f:Chatmd_shell_spec.Tool_schema.to_json
    in
    Ok (revision, schema)
;;

let completed_state t claim completion =
  let job = Store.Claim.job claim in
  let state = P.Developer_job.state job in
  let status =
    match completion with
    | P.Completion.Succeeded _ -> P.Job.Succeeded
    | Failed error ->
      P.Job.Failed
        (P.Error.create
           Invalid_state
           ~message:error.P.Invocation.message
           ~retryable:error.retryable
           ())
    | Cancelled _ -> P.Job.Cancelled
    | Expired ->
      P.Job.Failed
        (P.Error.create
           Invalid_state
           ~message:"background job expired"
           ~retryable:false
           ())
  in
  { state with
    status
  ; next_run_at = None
  ; completed_at = Some (O.native_now t.root)
  ; result = Some (P.Developer_job.Result_value.Inline completion)
  ; retry_failure = None
  }
;;

let waiting_state claim ~invocation ~deadline ~policy ~completion_schema work =
  let state = P.Developer_job.state (Store.Claim.job claim) in
  { state with
    status =
      P.Job.Waiting_completion
        { invocation_id = (H.context invocation).id
        ; work
        ; deadline
        ; completion_schema
        ; max_output_bytes = policy.Chat_response.One_off_request.max_output_bytes
        ; max_output_depth = policy.execution.max_depth
        }
  ; next_run_at = None
  ; completed_at = None
  ; result = None
  ; retry_failure = None
  }
;;

let execute_with_budget t active claim job request deadline effective control ~issuer =
  let current_capabilities () =
    match issuer with
    | None -> O.native_selected t.root
    | Some issuer -> Claimed.selected issuer
  in
  let* prepared =
    B.prepare ~env:t.env ~policy:(O.job_policy t.root) ~current_capabilities request
  in
  match prepared with
  | B.Script _ -> invalid "developer Script target has no installed executor"
  | Tool { capabilities; reference; input; policy } ->
    let* binding =
      C.resolve capabilities ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
    in
    let managed_definition =
      match issuer with
      | None -> N.managed_definition t.native
      | Some issuer -> Some (Claimed.managed_definition issuer)
    in
    let* revision, completion_schema =
      selected_schema t ~managed_definition capabilities reference
    in
    let context : H.context =
      { id = P.Id.Invocation.create ()
      ; owner = P.Execution_owner.Developer (O.owner t.root)
      ; origin = P.Invocation.Script
      ; parent_invocation = None
      ; parent_job = Some (P.Developer_job.id job)
      ; tool_name = reference.name
      ; implementation_revision = revision
      ; capability_fingerprint = C.fingerprint capabilities
      ; input
      ; created_at = O.native_now t.root
      ; deadline = Some deadline
      }
    in
    let* invocation = H.create_developer context in
    Option.iter control ~f:(fun control ->
      control.Chatml.Chatml_lang.before_effect ~name:"Tool.call" ~spawned:false);
    let run_native () =
      match issuer with
      | None ->
        N.run_job_child
          ~selected:capabilities
          t.native
          ~active
          ~reference
          ~invocation
          ~authorize:t.authorize
          ~prepare_output:t.prepare_output
      | Some issuer ->
        N.Claimed.with_issuer ~parent:t.native ~active ~issuer (fun selected ->
          N.Claimed.run
            selected
            ~reference
            ~invocation
            ~authorize:t.authorize
            ~prepare_output:t.prepare_output)
    in
    let* terminal =
      match B.agent_artifact request with
      | None -> run_native ()
      | Some _ ->
        let* agent_runtime =
          match issuer with
          | Some issuer -> Ok (Claimed.native_runtime issuer)
          | None ->
            t.agent_runtime
            |> Result.of_option
                 ~error:
                   (P.Error.invalid_request "saved Agent.run runtime is not installed")
        in
        let* parent = O.job_parent t.root active in
        Agent_session.Captured_agent_runner.Claimed_developer.with_saved_artifact
          ~active
          ~parent
          ~request
          ~policy:(O.job_policy t.root)
          ~prepared
          ~binding
          ~agent_runtime
          ~max_registrations:(O.job_policy t.root).execution.max_array_items
          ~checkpoint:(fun () ->
            Option.iter
              control
              ~f:(fun (control : Chatml.Chatml_lang.execution_control) ->
                control.checkpoint ()))
          ~invocation
          ~check_current:(fun () ->
            let owner = O.owner t.root in
            let capture_fingerprint =
              Chatml_developer_capture.fingerprint (O.capture t.root)
            in
            let* () = Claim_scope.Parent.check parent ~owner ~capture_fingerprint in
            Claim_scope.Parent.check_now parent ~owner ~capture_fingerprint)
          run_native
    in
    let* outcome =
      H.outcome terminal
      |> Result.of_option
           ~error:(P.Error.invalid_request "developer Tool invocation did not resolve")
    in
    let* () = P.Invocation.validate_outcome outcome in
    let outcome_json = P.Invocation.outcome_to_json outcome in
    let* () =
      P.Json_codec.validate_limits
        ~max_depth:effective.Chatml_execution.max_depth
        ~max_bytes:(Int.min (B.policy request).max_output_bytes effective.max_value_bytes)
        outcome_json
    in
    Option.iter control ~f:(fun control ->
      control.Chatml.Chatml_lang.before_json_import outcome_json);
    (match outcome with
     | P.Invocation.Complete value ->
       Ok (completed_state t claim (P.Completion.Succeeded value))
     | Fail error -> Ok (completed_state t claim (P.Completion.Failed error))
     | Cancelled reason -> Ok (completed_state t claim (P.Completion.Cancelled reason))
     | Pending ((P.Invocation.Job _ as work), _) ->
       Ok
         (waiting_state
            claim
            ~invocation:terminal
            ~deadline
            ~policy
            ~completion_schema
            work)
     | Pending (Subscription _, _) ->
       invalid "background Pending subscription is not owned Job work")
;;

(* The Tool and Script paths share the saved request deadline and one host
   budget. [expired] remains target-specific because Script must validate even
   an unevaluated Expired completion against its saved output envelope. *)
let with_claim_budget t ~job ~request ~expired run =
  let* timing = Deadline.create ~now:(O.native_now t.root) ~job ~request in
  let stored = B.policy request in
  match Deadline.disposition timing with
  | Expired -> expired stored
  | Active effective ->
    Chatml_execution.with_host_budget
      ~env:t.env
      ~policy:(Bounded effective)
      (run ~deadline:(Deadline.deadline timing) ~effective)
    |> Result.map_error ~f:(fun error ->
      P.Error.create Resource_limit ~message:error.message ~retryable:false ())
    |> Result.join
;;

let execute_claimed t active =
  let claim = Agent_session.Developer_job_scope.claim active in
  let job = Store.Claim.job claim in
  let* request = decode t job in
  let* () =
    match classify t request with
    | Runnable -> Ok ()
    | Malformed_supported
    | Agent_artifact_installer_unavailable
    | Issuer_source_installer_unavailable
    | Unsupported_kind -> invalid "developer background target has no installed executor"
  in
  with_claim_budget
    t
    ~job
    ~request
    ~expired:(fun _ -> Ok (completed_state t claim P.Completion.Expired))
    (fun ~deadline ~effective control ->
       let run ~issuer =
         execute_with_budget t active claim job request deadline effective control ~issuer
       in
       match B.issuer_source request with
       | None -> run ~issuer:None
       | Some _ ->
         let* service =
           Jobs.queued_issuer t.jobs
           |> Result.of_option
                ~error:
                  (P.Error.invalid_request
                     "claimed child issuer installer is unavailable")
         in
         Developer_queued_issuer.with_claimed
           service
           ~active
           ~request
           ~checkpoint:(fun () ->
             Eio.Fiber.check ();
             Option.iter control ~f:(fun control -> control.diagnostic_checkpoint ()))
           (fun issuer -> run ~issuer:(Some issuer)))
;;

let call_script_tool t ~active ~claim ~deadline ~prepared ~issuer ~name ~args =
  let capabilities = Chat_response.One_off_script.capabilities prepared in
  let* binding =
    C.find capabilities ~name
    |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
  in
  let reference = C.reference binding in
  let managed_definition =
    match issuer with
    | None -> N.managed_definition t.native
    | Some issuer -> Some (Claimed.managed_definition issuer)
  in
  let* revision, _completion_schema =
    selected_schema t ~managed_definition capabilities reference
  in
  let context : H.context =
    { id = P.Id.Invocation.create ()
    ; owner = P.Execution_owner.Developer (O.owner t.root)
    ; origin = P.Invocation.Script
    ; parent_invocation = None
    ; parent_job = Some (P.Developer_job.id (Store.Claim.job claim))
    ; tool_name = reference.name
    ; implementation_revision = revision
    ; capability_fingerprint = C.fingerprint capabilities
    ; input = args
    ; created_at = O.native_now t.root
    ; deadline = Some deadline
    }
  in
  let* invocation = H.create_developer context in
  let* terminal =
    match issuer with
    | None ->
      N.run_job_child
        ~selected:capabilities
        t.native
        ~active
        ~reference
        ~invocation
        ~authorize:t.authorize
        ~prepare_output:t.prepare_output
    | Some issuer ->
      N.Claimed.with_issuer ~parent:t.native ~active ~issuer (fun selected ->
        N.Claimed.run
          selected
          ~reference
          ~invocation
          ~authorize:t.authorize
          ~prepare_output:t.prepare_output)
  in
  H.outcome terminal
  |> Result.of_option
       ~error:(P.Error.invalid_request "developer Script Tool.call did not resolve")
;;

let script_claim t active =
  let claim = Agent_session.Developer_job_scope.claim active in
  let job = Store.Claim.job claim in
  let* request = decode t job in
  let* () =
    match B.target_kind request, B.agent_artifact request with
    | B.Target_kind.Script, None -> Ok ()
    | Script, Some _ | Tool, _ ->
      invalid "claimed developer Script target changed before execution"
  in
  Ok (claim, job, request)
;;

let expired_script_state t ~active =
  let* claim, _job, request = script_claim t active in
  let* executed = Script.expired ~policy:(B.policy request) in
  Ok (completed_state t claim (Script.completion executed))
;;

let execute_script_in_budget
      t
      ~active
      ~scope
      ~issuer
      ~deadline
      ~(control : Chatml.Chatml_lang.execution_control option)
  =
  let* claim, _job, request = script_claim t active in
  Option.iter control ~f:(fun control -> control.checkpoint ());
  let* prepared =
    B.prepare
      ~env:t.env
      ~policy:(O.job_policy t.root)
      ~current_capabilities:(fun () ->
        match issuer with
        | None -> O.native_selected t.root
        | Some issuer -> Claimed.selected issuer)
      request
  in
  Option.iter control ~f:(fun control -> control.checkpoint ());
  match prepared with
  | B.Tool _ -> invalid "claimed developer Script prepared as a Tool"
  | Script { prepared; input; policy } ->
    let* executed =
      Script.run
        ~env:t.env
        ~scope
        ~prepared
        ~input
        ~policy
        ~deadline
        ~now:(fun () -> O.native_now t.root)
        ~call_tool:(call_script_tool t ~active ~claim ~deadline ~prepared ~issuer)
    in
    Ok
      (completed_state t claim (Script.completion executed), Script.local_effects executed)
;;

let execute_script_claimed t ~active ~scope ~issuer ~budget =
  match budget with
  | Some budget ->
    execute_script_in_budget
      t
      ~active
      ~scope
      ~issuer
      ~deadline:(Jobs.Claim_budget.deadline budget)
      ~control:(Some (Jobs.Claim_budget.control budget))
  | None ->
    let* _claim, job, request = script_claim t active in
    with_claim_budget
      t
      ~job
      ~request
      ~expired:(fun _ ->
        Result.map (expired_script_state t ~active) ~f:(fun state -> state, []))
      (fun ~deadline ~effective:_ control ->
         execute_script_in_budget t ~active ~scope ~issuer ~deadline ~control)
;;

let run_candidate t ?on_capacity_decision candidate =
  let signalled = ref false in
  let notify_once decision =
    if not !signalled
    then (
      signalled := true;
      Option.iter on_capacity_decision ~f:(fun notify -> notify decision))
  in
  Exn.protect
    ~finally:(fun () -> notify_once Developer_job_worker.Capacity_decision.Rejected)
    ~f:(fun () ->
      let* ready = availability t candidate in
      match ready with
      | Agent_artifact_installer_unavailable
      | Issuer_source_installer_unavailable
      | Unsupported_kind ->
        invalid "developer background target has no installed executor"
      | Malformed_supported ->
        O.Job_candidate.with_claimed
          t.root
          candidate
          ~on_capacity_decision:notify_once
          ~select:(O.Job_result.select t.results)
          (fun ~active ~sw:_ ->
             with_claimed_approval t active (fun () -> execute_claimed t active))
      | Runnable ->
        (match decode t (O.Job_candidate.job candidate) with
         | Error _ -> invalid "developer background target changed after availability"
         | Ok request ->
           (match B.target_kind request with
            | B.Target_kind.Tool ->
              O.Job_candidate.with_claimed
                t.root
                candidate
                ~on_capacity_decision:notify_once
                ~select:(O.Job_result.select t.results)
                (fun ~active ~sw:_ ->
                   with_claimed_approval t active (fun () -> execute_claimed t active))
            | Script ->
              Jobs.with_claimed_worker
                t.jobs
                ~candidate
                ~on_capacity_decision:notify_once
                ~expired_state:(expired_script_state t)
                ~select:(O.Job_result.select t.results)
                (fun ~active ~scope ~issuer ~budget ~sw:_ ->
                   with_claimed_approval t active (fun () ->
                     execute_script_claimed t ~active ~scope ~issuer ~budget)))))
;;
