open! Core
module P = Agent_protocol
module O = Developer_root_owner
module Store = Agent_store.Developer_run_store
module Model_call = P.Developer_model_call
module Request = P.Model_spawn_request
module Runtime = Inference_runtime
module Ctx = Chat_response.Ctx
module Executor = Chat_response.Model_executor
module Capabilities = Chat_response.Moderation.Capabilities
module Job_scope = Agent_session.Developer_job_scope

module Sources = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; prompt_dir : Eio.Fs.dir_ty Eio.Path.t
    ; tool_dir : Eio.Fs.dir_ty Eio.Path.t
    ; response_dir : Eio.Fs.dir_ty Eio.Path.t
    ; cache : Chat_response.Cache.t
    ; manifest_authorizer : Shell_runtime.Manifest_authorizer.t
    ; approval_provider : Shell_runtime.Approval_broker.provider
    }

  let create
        ~env
        ~prompt_dir
        ~tool_dir
        ~response_dir
        ~cache
        ~manifest_authorizer
        ~approval_provider
    =
    { env
    ; prompt_dir
    ; tool_dir
    ; response_dir
    ; cache
    ; manifest_authorizer
    ; approval_provider
    }
  ;;
end

type t =
  { root : O.run
  ; results : O.Job_result.t
  ; sources : Sources.t
  ; limits : Document_schema.Limits.t
  }

module Availability = struct
  type t =
    | Runnable
    | Malformed_supported
    | Unsupported_kind
  [@@deriving equal, sexp_of]
end

let invalid message = Error (P.Error.invalid_request message)
let ( let* ) result f = Result.bind result ~f

let create ~root ~results ~sources =
  if not (O.Job_result.issued_by_root results root)
  then invalid "developer Model runner has another result owner"
  else
    let* limits = O.document_limits root in
    let* _ =
      O.selected_model root
      |> Option.value_map
           ~default:(invalid "developer Model provider is unavailable")
           ~f:Provider_operator_port.Model_selection.context
    in
    Ok { root; results; sources; limits }
;;

let issued_by_root t root = phys_equal t.root root

let decode t job =
  if not (P.Job.equal_kind (P.Developer_job.kind job) P.Job.Model_call)
  then invalid "developer Model runner requires a model job"
  else
    Model_call.of_json (P.Developer_job.payload job) ~limits:t.limits
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (P.Protocol_error.sexp_of_t error)))
;;

let availability t candidate =
  let job = O.Job_candidate.job candidate in
  if not (P.Job.equal_kind (P.Developer_job.kind job) P.Job.Model_call)
  then Ok Availability.Unsupported_kind
  else (
    match decode t job with
    | Ok _ -> Ok Runnable
    | Error _ -> Ok Malformed_supported)
;;

let tool_error message : P.Invocation.tool_error =
  { code = "model_error"; message; retryable = false; details = `Null }
;;

let completed_state t claim completion =
  let state = P.Developer_job.state (Store.Claim.job claim) in
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
        (P.Error.create Invalid_state ~message:"model job expired" ~retryable:false ())
  in
  { state with
    status
  ; next_run_at = None
  ; completed_at = Some (O.native_now t.root)
  ; result = Some (P.Developer_job.Result_value.Inline completion)
  ; retry_failure = None
  }
;;

let fetch_prompt scope ~ctx ~prompt ~is_local =
  O.Model_inference.check_current scope;
  let xml = Chat_response.Fetch.get ~ctx prompt ~is_local in
  let prompt_dir =
    if is_local then Chat_response.Fetch.resolve_local_dir ~ctx prompt else None
  in
  O.Model_inference.check_current scope;
  Ok (xml, prompt_dir)
;;

let run_agent t scope ?history_compaction ?prompt_dir ?session_id ~ctx prompt items =
  O.Model_inference.check_current scope;
  Chat_response.Driver.run_agent
    ?history_compaction
    ?prompt_dir
    ?session_id
    ~response_dir:t.sources.response_dir
    ~shell_manifest_authorizer:t.sources.manifest_authorizer
    ~shell_approval_provider:t.sources.approval_provider
    ~ctx
    prompt
    items
;;

let context t scope recipe =
  let selected = O.selected_model t.root |> Option.value_exn in
  let provider_guard =
    Runtime.Submission_guard.create ~refresh:(fun () ->
      O.Model_inference.check_current scope;
      fun () ->
        match Provider_operator_port.Model_selection.check_current selected with
        | Ok () -> ()
        | Error _ -> raise Chat_response.Ctx.Inference_admission_rejected)
  in
  Ctx.create
    ~inference_context:(O.Model_inference.context scope)
    ~inference_identity:(O.Model_inference.identity scope)
    ~on_inference_attempt:(O.Model_inference.on_attempt scope)
    ~on_inference_completion:(O.Model_inference.on_completion scope)
    ~on_inference_observation:(O.Model_inference.on_observation scope)
    ~on_admitted_call:(O.Model_inference.on_admitted_call scope)
    ~before_inference_dispatch:(O.Model_inference.before_dispatch scope)
    ~agent_prompt_v1_recipe:recipe
    ~env:t.sources.env
    ~dir:t.sources.prompt_dir
    ~tool_dir:t.sources.tool_dir
    ~cache:t.sources.cache
    ()
  |> Fn.flip Ctx.with_submission_guard (O.Model_inference.submission_guard scope)
  |> Fn.flip Ctx.with_submission_guard provider_guard
;;

let strict_recipe t scope =
  let rec recipe : Capabilities.model_recipe =
    { call =
        (fun ~payload ->
          O.Model_inference.check_current scope;
          let open Result.Let_syntax in
          let%bind request = Request.decode payload in
          let ctx = context t scope recipe in
          let exec_context : Executor.exec_context =
            { ctx; fetch_prompt = fetch_prompt scope; run_agent = run_agent t scope }
          in
          Executor.call_agent_prompt_v1_strict
            exec_context
            ~request
            ~capture_recipe_target:(fun target ->
              O.Model_inference.check_current scope;
              if
                Inference.Request.Target.equal
                  target
                  (Runtime.Context.target (O.Model_inference.context scope))
              then Ok ()
              else Error "developer Model target differs from the selected provider")
            ())
    ; spawn =
        (fun ~payload ->
          O.Model_inference.check_current scope;
          let open Result.Let_syntax in
          let%bind request =
            Request.admit payload ~limits:t.limits
            |> Result.map_error ~f:(fun error ->
              Sexp.to_string_hum (P.Protocol_error.sexp_of_t error))
          in
          O.Model_inference.admit_external_model
            scope
            ~request
            ~checkpoint:Eio.Fiber.check
          |> Result.map ~f:P.Id.Job.to_string
          |> Result.map_error ~f:(fun error -> error.P.Error.message))
    }
  in
  recipe
;;

let execute_claimed t active =
  let claim = Job_scope.claim active in
  let job = Store.Claim.job claim in
  let* saved = decode t job in
  let* () =
    let selected =
      O.selected_model t.root
      |> Option.value_map
           ~default:(invalid "developer Model provider is unavailable")
           ~f:Provider_operator_port.Model_selection.check_current
    in
    selected
  in
  O.Model_inference.with_claimed t.root ~active ~f:(fun scope ~sw:_ ->
    O.Model_inference.check_current scope;
    let target = Runtime.Context.target (O.Model_inference.context scope) in
    let completion =
      if not (Inference.Request.Target.equal target (Model_call.target saved))
      then P.Completion.Failed (tool_error "saved Model target changed")
      else (
        let request_json = Model_call.request saved in
        let recipe = strict_recipe t scope in
        match recipe.call ~payload:request_json with
        | Ok (Capabilities.Model_ok value) -> P.Completion.Succeeded value
        | Ok (Model_refused message | Model_error message) ->
          P.Completion.Failed (tool_error message)
        | Error message -> P.Completion.Failed (tool_error message))
    in
    completed_state t claim completion)
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
      | Unsupported_kind -> invalid "developer Model runner cannot execute this job kind"
      | Runnable | Malformed_supported ->
        O.Job_candidate.with_claimed
          t.root
          candidate
          ~on_capacity_decision:notify_once
          ~select:(O.Job_result.select t.results)
          (fun ~active ~sw:_ -> execute_claimed t active))
;;
