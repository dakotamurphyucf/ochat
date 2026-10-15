open! Core
module P = Agent_protocol
module O = Developer_root_owner
module Store = Agent_store.Developer_run_store
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module M = Chat_response.Managed_tool_registry
module N = Developer_native_executor
module Publisher = Agent_store.Developer_result_publisher
module H = P.Host_invocation

type t =
  { root : O.run
  ; native : N.t
  ; publisher : Publisher.t
  ; env : Eio_unix.Stdenv.base
  ; managed : Agent_session.Native_tool_invocation.selected_managed_dispatch option
  ; storage : Publisher.Storage.t
  ; authorize : H.t -> C.binding -> (unit, P.Error.t) result
  ; prepare_output : Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, P.Error.t) result
  }

module Availability = struct
  type t =
    | Runnable
    | Script_target_unavailable
    | Agent_artifact_installer_unavailable
  [@@deriving equal, sexp_of]
end

let invalid message = Error (P.Error.invalid_request message)
let ( let* ) result f = Result.bind result ~f

let create
      ~sw
      ~root
      ~store
      ~blobs
      ~publisher_limits
      ~storage
      ~env
      ~managed
      ~jobs
      ~authorize
      ~prepare_output
  =
  if not (O.issued_by_store root store)
  then invalid "developer Tool runner has a foreign Store"
  else
    let* result_root = O.result_root root in
    let* publisher =
      Publisher.create ~sw ~runs:store ~root:result_root ~blobs ~limits:publisher_limits
    in
    let* native = N.create ~root in
    let* native = N.with_job_service native jobs in
    Ok { root; native; publisher; env; managed; storage; authorize; prepare_output }
;;

let decode t job =
  if not (P.Job.equal_kind (P.Developer_job.kind job) P.Job.Async_tool)
  then invalid "developer Tool runner requires an async tool job"
  else B.of_json ~policy:(O.job_policy t.root) (P.Developer_job.payload job)
;;

let classify request =
  match B.target_kind request, B.agent_artifact request with
  | B.Target_kind.Tool, None -> Availability.Runnable
  | Tool, Some _ -> Agent_artifact_installer_unavailable
  | Script, _ -> Script_target_unavailable
;;

let availability t candidate =
  let* request = decode t (O.Job_candidate.job candidate) in
  Ok (classify request)
;;

let deadline job request =
  Result.try_with (fun () ->
    Time_ns.add
      (P.Timestamp.to_time_ns (P.Developer_job.created_at job))
      (Time_ns.Span.of_sec (B.policy request).execution.wall_seconds)
    |> P.Timestamp.of_time_ns)
  |> Result.map_error ~f:(fun _ ->
    P.Error.invalid_request "background deadline is outside the supported timestamp range")
;;

let selected_schema t capabilities reference =
  let* binding =
    C.resolve capabilities ~id:reference.C.id ~fingerprint:reference.fingerprint
    |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
  in
  match C.implementation binding, t.managed with
  | C.Native _, _ -> Ok (reference.implementation_revision, None)
  | Managed _, None -> invalid "managed Tool dispatcher is not installed"
  | Managed _, Some managed ->
    let* revision =
      M.execution_revision managed.definition binding
      |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
    in
    let* prepared =
      M.resolve managed.definition binding
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

let execute_with_budget t active claim job request deadline effective control =
  let* prepared =
    B.prepare
      ~env:t.env
      ~policy:(O.job_policy t.root)
      ~current_capabilities:(fun () -> O.native_selected t.root)
      request
  in
  match prepared with
  | B.Script _ -> invalid "developer Script target has no installed executor"
  | Tool { capabilities; reference; input; policy } ->
    let* revision, completion_schema = selected_schema t capabilities reference in
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
    let* terminal =
      N.run_job_child
        ~selected:capabilities
        t.native
        ~active
        ~managed:t.managed
        ~reference
        ~invocation
        ~authorize:t.authorize
        ~prepare_output:t.prepare_output
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

let execute_claimed t active =
  let claim = Agent_session.Developer_job_scope.claim active in
  let job = Store.Claim.job claim in
  let* request = decode t job in
  let* () =
    match classify request with
    | Runnable -> Ok ()
    | Script_target_unavailable | Agent_artifact_installer_unavailable ->
      invalid "developer background target has no installed executor"
  in
  let* deadline = deadline job request in
  if P.Timestamp.compare deadline (O.native_now t.root) <= 0
  then Ok (completed_state t claim P.Completion.Expired)
  else (
    let stored = B.policy request in
    let remaining =
      Time_ns.diff
        (P.Timestamp.to_time_ns deadline)
        (P.Timestamp.to_time_ns (O.native_now t.root))
      |> Time_ns.Span.to_sec
      |> Float.min stored.execution.wall_seconds
    in
    if Float.(remaining <= 0.)
    then Ok (completed_state t claim P.Completion.Expired)
    else (
      let effective = { stored.execution with wall_seconds = remaining } in
      Chatml_execution.with_host_budget
        ~env:t.env
        ~policy:(Bounded effective)
        (execute_with_budget t active claim job request deadline effective)
      |> Result.map_error ~f:(fun error ->
        P.Error.create Resource_limit ~message:error.message ~retryable:false ())
      |> Result.join))
;;

let select_result t ~claim ~proposed =
  match proposed.P.Developer_job.State.result with
  | Some (P.Developer_job.Result_value.Inline _) ->
    let* selection =
      Publisher.select
        t.publisher
        ~storage:t.storage
        ~claim
        ~proposed
        ~selected_at:(O.native_now t.root)
    in
    let* wrapped =
      Developer_job_worker.Selected.create
        ~claim
        ~proposed
        ~after:(Publisher.Selected.after selection)
        ~ensure_attempted:(fun () -> Publisher.Selected.ensure_attempted selection)
        ~acknowledge_installed:(fun installed ->
          Publisher.Selected.acknowledge_installed selection installed)
    in
    Ok (Some wrapped)
  | None -> Ok None
  | Some (Artifact _) -> invalid "developer Tool runner cannot select a prior artifact"
;;

let run_candidate t candidate =
  let* ready = availability t candidate in
  match ready with
  | Script_target_unavailable | Agent_artifact_installer_unavailable ->
    invalid "developer background target has no installed executor"
  | Runnable ->
    O.Job_candidate.with_claimed
      t.root
      candidate
      ~select:(select_result t)
      (fun ~active ~sw:_ -> execute_claimed t active)
;;
