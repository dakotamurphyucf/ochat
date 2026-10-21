open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Receipt = Store.Command.Receipt
module Row = Store.Command.Row
module Lifecycle = Developer_command_lifecycle
module Original = Developer_command_original
module Budgets = Developer_command_budgets
module Task_receipt = Developer_repl_task_receipt
module Capture = Authoring_context_service.Developer
module Root = Developer_root_owner
module Jobs = Developer_script_job_service
module Claimed = Agent_session.Claimed_child_runtime
module Native = Developer_native_executor
module Background = Developer_background_job_runner
module Model = Developer_model_job_runner
module Live_resources = Developer_live_resources
module Child_runner = Developer_child_agent_runner
module Repl_epoch = Developer_repl_epoch
module Approval_owner = Developer_approval_owner

module Change = struct
  type kind =
    | Command_changed
    | Approval_changed
end

type live_services =
  { jobs : Jobs.t
  ; native : Native.t
  ; background : Background.t
  ; model : Model.t option
  ; native_authorizer : Live_resources.Native_authorizer.t
  }

let error code message ~retryable = P.Error.create code ~message ~retryable ()

let store_error = function
  | Store.Error.Store failure -> Agent_store.Store_error.to_protocol_error failure
  | Conflict | Prewrite_conflict ->
    error P.Error.Conflict "developer command store is busy" ~retryable:true
  | Foreign_or_stale ->
    error P.Error.Lease_stale "developer command store owner changed" ~retryable:false
  | Limit_exceeded ->
    error P.Error.Resource_limit "developer command store limit reached" ~retryable:false
  | Closed ->
    error P.Error.Invalid_state "developer command store is closed" ~retryable:false
;;

let store result = Result.map_error result ~f:store_error

let trace_disposition receipt =
  let open Chatml_trace.Terminal.Disposition in
  match Receipt.value receipt with
  | Check Ready -> Succeeded
  | Check (Rejected _) -> Failed Typecheck
  | Check Interrupted -> Interrupted
  | Check Cancelled -> Cancelled
  | Fixture (Completed { outcome = Succeeded _; _ }) -> Succeeded
  | Fixture (Completed { outcome = Tool_failed _ | Failed _; _ }) -> Failed Runtime
  | Fixture Interrupted -> Interrupted
  | Fixture Cancelled -> Cancelled
  | Live (Succeeded _) -> Succeeded
  | Live (Failed _) -> Failed Runtime
  | Live (Limited _) -> Failed Resource_limit
  | Live Interrupted -> Interrupted
  | Live Cancelled -> Cancelled
  | Task (Succeeded _) -> Succeeded
  | Task (Failed _) -> Failed Runtime
  | Task (Limited _) -> Failed Resource_limit
  | Task Interrupted -> Interrupted
  | Task Cancelled -> Cancelled
;;

module Limits = struct
  type t =
    { roots : Root.Limits.t
    ; max_active_commands : int
    ; disclosure : Developer_result_disclosure.Limits.t
    }

  let create ~roots ~max_active_commands ~disclosure =
    if max_active_commands <= 0
    then Or_error.error_string "developer host requires command capacity"
    else Ok { roots; max_active_commands; disclosure }
  ;;
end

module Command_view = struct
  type view = { row : Row.t }

  module Original = Original

  module Disclosure_policy = struct
    type t = { authorize : Operator_authorization.t -> Row.t -> bool }

    let create ~authorize = { authorize }
    let authorize t actor row = t.authorize actor row
  end

  module Control_policy = struct
    type t = { authorize : Operator_authorization.t -> Row.t -> bool }

    let create ~authorize = { authorize }
    let authorize t actor row = t.authorize actor row
  end

  let of_row row = { row }
  let of_lifecycle value = { row = Lifecycle.View.row value }
  let row t = t.row
  let id view = Row.id (row view)
  let owner view = Row.owner (row view)
  let kind view = Row.kind (row view)
  let phase view = Row.phase (row view)
  let capture_fingerprint view = Row.capture_fingerprint (row view)
  let request_fingerprint view = Row.request_fingerprint (row view)

  let receipt view =
    match Row.phase (row view) with
    | Row.Phase.Terminal { receipt; _ } -> Some receipt
    | Reserved | Waiting_permission | Execution_started -> None
  ;;
end

type live_owner =
  | Prepared of
      { resources : Developer_live_resources.live
      ; origin_row : Row.t
      ; approval_owner : Approval_owner.t option
      }
  | Root_owned of
      { root : Root.run
      ; resources : Developer_live_resources.live
      ; origin_row : Row.t
      ; services : live_services
      ; approval_owner : Approval_owner.t option
      }
  | Root_provisional of
      { root : Root.run
      ; resources : Developer_live_resources.live
      ; origin_row : Row.t
      ; approval_owner : Approval_owner.t option
      }

let live_origin_row = function
  | Prepared { origin_row; _ }
  | Root_owned { origin_row; _ }
  | Root_provisional { origin_row; _ } -> origin_row
;;

let live_resources = function
  | Prepared { resources; _ }
  | Root_owned { resources; _ }
  | Root_provisional { resources; _ } -> resources
;;

let live_root = function
  | Prepared _ -> None
  | Root_owned { root; _ } | Root_provisional { root; _ } -> Some root
;;

let live_approval_owner = function
  | Prepared { approval_owner; _ }
  | Root_owned { approval_owner; _ }
  | Root_provisional { approval_owner; _ } -> approval_owner
;;

type live_selection =
  { prompt : P.Id.Prompt_definition.t
  ; workspace : P.Id.Workspace_definition.t
  }

let live_selection_of_original original =
  match Original.prompt original, Original.workspace original with
  | Some prompt, Some workspace -> Some { prompt; workspace }
  | None, None -> None
  | None, Some _ | Some _, None ->
    failwith "validated developer command has an incomplete Live selector"
;;

let equal_live_selection left right =
  P.Id.Prompt_definition.equal left.prompt right.prompt
  && P.Id.Workspace_definition.equal left.workspace right.workspace
;;

type retirement_step =
  | Join_callbacks
  | Detach_scheduler
  | Close_root
  | Close_resources
  | Select_epoch

type repl_retirement_step =
  | Retire_task_lifecycle
  | Detach_task_scheduler
  | Close_task_root
  | Retire_task_admission
  | Close_task_resources
  | Select_task_epoch

type repl_retirement =
  { mutable step : repl_retirement_step
  ; instance : Repl_epoch.Instance.t
  ; custody : Repl_epoch.Task_cleanup.custody
  }

type ready_run =
  { handle : Store.handle
  ; mutable actor : Operator_authorization.t
  ; capture_fingerprint : Chatml_source.Digest.t
  ; repl_selection : Capture.Source_selection.t option
  ; mutable live_selection : live_selection option
  ; mutable live_owner : live_owner option
  ; live_gate : Eio.Mutex.t
  ; mutable repl_retirement : repl_retirement option
  }

type retiring_run =
  { old : ready_run
  ; target_key : P.Idempotency_key.t
  ; target_request : Chatml_source.Digest.t
  ; target_capture : Chatml_source.Digest.t
  ; target_repl_selection : Capture.Source_selection.t option
  ; principal : P.Principal.t
  ; mutable step : retirement_step
  }

type appending_epoch =
  { owner : P.Execution_owner.developer
  ; key : P.Idempotency_key.t
  ; request_fingerprint : Chatml_source.Digest.t
  ; capture_fingerprint : Chatml_source.Digest.t
  ; repl_selection : Capture.Source_selection.t option
  ; previous : Store.Observation.t option
  ; principal : P.Principal.t
  }

type admitted_run =
  | Ready of ready_run
  | Retiring of retiring_run
  | Appending of appending_epoch

type t =
  { sw : Eio.Switch.t
  ; env : Eio_unix.Stdenv.base
  ; session_store : Agent_store.Session_store.t
  ; blobs : Agent_store.Blob_store.t
  ; capacity : Job_capacity.t
  ; store : Store.t
  ; roots : Root.Registry.t
  ; lifecycle : Lifecycle.t
  ; capture : Capture.t
  ; authoring : Authoring_installation.t
  ; repl : Repl_epoch.t option
  ; trace : Developer_trace_retention.t option
  ; live_resources : Developer_live_resources.t
  ; approval : Approval_owner.Configuration.t option
  ; on_change :
      (run_id:P.Id.Developer_run.t -> key:P.Idempotency_key.t -> kind:Change.kind -> unit)
        option
  ; disclosure_policy : Command_view.Disclosure_policy.t
  ; control_policy : Command_view.Control_policy.t
  ; provider : Provider_operator_port.t option
  ; model_target : Inference.Request.Target.t option
  ; job_policy : Chat_response.One_off_request.policy
  ; runtime_policy : Chat_response.Runtime_semantics.policy
  ; prepare_output : Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, P.Error.t) result
  ; result_disclosure : Developer_result_disclosure.t
  ; publisher_limits : Agent_store.Developer_result_publisher.Limits.t
  ; publisher_storage : Agent_store.Developer_result_publisher.Storage.t
  ; budgets : Budgets.t
  ; max_runs : int
  ; now : unit -> P.Timestamp.t
  ; checkpoint : unit -> unit
  ; runs : (P.Id.Developer_run.t, admitted_run) Hashtbl.t
  ; mutable scheduler : Job_scheduler.t option
  ; mutable closed : bool
  }

let registry t = t.roots
let approval_installed t = Option.is_some t.approval
let trace_installed t = Option.is_some t.trace

let repl_registry_required_entries t =
  if t.closed
  then Error (error P.Error.Invalid_state "developer host is closed" ~retryable:false)
  else (
    match t.repl with
    | None ->
      Error
        (error P.Error.Invalid_state "developer REPL is not installed" ~retryable:false)
    | Some repl -> Repl_epoch.registry_required_entries repl)
;;

let uses_authoring t authoring = phys_equal t.authoring authoring

let install_scheduler t scheduler =
  if t.closed
  then Error (error P.Error.Invalid_state "developer host is closed" ~retryable:false)
  else if Option.is_some t.scheduler
  then
    Error
      (error P.Error.Conflict "developer scheduler already installed" ~retryable:false)
  else if not (Job_scheduler.uses_developer_registry scheduler t.roots)
  then
    Error
      (error
         P.Error.Invalid_state
         "developer scheduler does not own this root registry"
         ~retryable:false)
  else (
    t.scheduler <- Some scheduler;
    Ok ())
;;

let prepare_agent_job_source ~root ~resources ~parent ~binding ~request ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = Jobs.Source_parent.check parent ~binding in
  let%bind () = Live_resources.check_current resources in
  let%bind launch = Jobs.Source_parent.launch parent in
  let%bind artifact =
    match Jobs.Source_parent.runtime_source parent with
    | Selected_child_runtime child ->
      Developer_child_selection.prepare_agent_source
        child
        ~live:resources
        ~binding
        ~launch
        ~request
        ~max_registrations:(Root.job_policy root).execution.max_array_items
        ~checkpoint
    | Claimed_runtime issuer ->
      let%bind () = Claimed.check issuer in
      let%bind artifact =
        Chat_response.Agent_runtime.prepare_agent_source
          (Claimed.native_runtime issuer)
          ~binding
          ~launch
          ~request
          ~max_registrations:(Root.job_policy root).execution.max_array_items
          ~checkpoint
      in
      let%map () = Claimed.check issuer in
      artifact
    | Root_runtime ->
      Chat_response.Agent_runtime.prepare_agent_source
        (Live_resources.native_runtime resources)
        ~binding
        ~launch
        ~request
        ~max_registrations:(Root.job_policy root).execution.max_array_items
        ~checkpoint
  in
  let%bind () = Jobs.Source_parent.check parent ~binding in
  let%map () = Live_resources.check_current resources in
  artifact
;;

let create_live_services t ~root ~resources ~approval_owner ~current_actor =
  let open Result.Let_syntax in
  let%bind native_authorizer =
    Live_resources.native_authorizer resources ~root ~current_actor
  in
  let authorize_for_runtime =
    Live_resources.Native_authorizer.for_runtime native_authorizer
  in
  let%bind queued_issuer =
    Developer_queued_issuer.create ~live:resources ~root ~runtime_policy:t.runtime_policy
  in
  let%bind jobs =
    Jobs.create
      ~root
      ~store:t.store
      ~disclosure:t.result_disclosure
      ~env:t.env
      ~current_actor
      ~queued_issuer:(Some queued_issuer)
      ~prepare_source:(prepare_agent_job_source ~root ~resources)
  in
  let%bind native = Native.create ~root in
  let%bind native = Native.with_job_service native jobs in
  let%bind native =
    match approval_owner with
    | None -> Ok native
    | Some owner -> Native.with_approval_owner native owner
  in
  let%bind native =
    Native.with_managed_definition
      native
      ~definition:(Live_resources.managed_definition resources)
      ~env:t.env
  in
  let%bind results =
    Root.Job_result.bind
      root
      ~blobs:t.blobs
      ~limits:t.publisher_limits
      ~storage:t.publisher_storage
  in
  let%bind background =
    Background.create
      ~root
      ~results
      ~env:t.env
      ~managed_definition:(Native.managed_definition native)
      ~agent_runtime:(Some (Live_resources.native_runtime resources))
      ~jobs
      ~authorize_for_runtime:(function
        | Some runtime -> authorize_for_runtime runtime
        | None ->
          fun _ _ ->
            Error (P.Error.invalid_request "developer native runtime is not installed"))
      ~prepare_output:t.prepare_output
  in
  let%bind background =
    match approval_owner with
    | None -> Ok background
    | Some owner -> Background.with_approval_owner background owner
  in
  let%bind model =
    match Root.selected_model root with
    | None -> Ok None
    | Some _ ->
      let%bind model =
        Model.create ~root ~results ~sources:(Live_resources.model_sources resources)
      in
      let%map model =
        match approval_owner with
        | None -> Ok model
        | Some owner -> Model.with_approval_owner model owner
      in
      Some model
  in
  let%bind run_selected_target =
    Child_runner.create
      ~live:resources
      ~root
      ~native
      ~runtime_policy:t.runtime_policy
      ~authorize_for_runtime
      ~prepare_output:t.prepare_output
  in
  let%bind run_selected_content =
    Child_runner.create_content
      ~live:resources
      ~root
      ~native
      ~runtime_policy:t.runtime_policy
      ~authorize_for_runtime
      ~prepare_output:t.prepare_output
  in
  let%bind selected_tool_callbacks =
    Child_runner.create_tool
      ~live:resources
      ~root
      ~native
      ~runtime_policy:t.runtime_policy
      ~authorize_for_runtime
      ~prepare_output:t.prepare_output
  in
  let%map () =
    Live_resources.bind_agent_sources
      resources
      ~root
      ~models:model
      ~run_selected_target
      ~run_selected_content
      ~selected_tool_callbacks
      ~checkpoint:t.checkpoint
  in
  { jobs; native; background; model; native_authorizer }
;;

let check_method t ~actor ~kind =
  if t.closed
  then Error (error P.Error.Invalid_state "developer host is closed" ~retryable:false)
  else Developer_method_authorization.check_now kind ~original:actor ~current:actor
;;

let select_live_model t ~actor ~resources =
  let open Result.Let_syntax in
  match t.model_target, t.provider with
  | None, _ -> Ok None
  | Some _, None ->
    Error
      (error
         P.Error.Invalid_state
         "developer model provider is unavailable"
         ~retryable:false)
  | Some target, Some provider ->
    let%bind selected = Provider_operator_port.select_model provider ~actor ~target in
    let%map () = Live_resources.check_current resources in
    Some selected
;;

let admit_live_root t ~actor ~capture ~candidate ~ready ~resources ~active =
  let open Result.Let_syntax in
  let%bind selected_model = select_live_model t ~actor ~resources in
  let final_admission ~actor:now_actor ~capture:now_capture ~checkpoint =
    if not (phys_equal now_actor actor && phys_equal now_capture capture)
    then Error (P.Error.invalid_request "developer root selection changed")
    else (
      let%bind current =
        Capture.Candidate.final_admission candidate ~actor ~capture ~checkpoint
      in
      let%bind () = Live_resources.check_current resources in
      let%bind () = current () in
      let%map () =
        match selected_model with
        | None -> Ok ()
        | Some selection -> Provider_operator_port.Model_selection.check_current selection
      in
      fun () ->
        let%bind () = check_method t ~actor ~kind:Receipt.Kind.Live_run in
        let%bind () = current () in
        let%bind () = Live_resources.check_current resources in
        match selected_model with
        | None -> Ok ()
        | Some selection -> Provider_operator_port.Model_selection.check_current selection)
  in
  Root.Registry.admit_recovered
    ~final_admission
    ~checkpoint:t.checkpoint
    t.roots
    ~actor
    ~capture
    ~registry:(Live_resources.native_registry resources)
    ~selected:(Live_resources.selected_native resources)
    ~selected_model
    ~job_policy:t.job_policy
    ~handle:ready.handle
    ~workspace_conflict_domain:(Live_resources.conflict_domain resources)
    ~transfer_finished_claim:(fun ~accept ->
      Lifecycle.Active.transfer_finished_claim active ~handle:ready.handle ~accept)
;;

let check_repl_selection t ~actor selection =
  let open Result.Let_syntax in
  let%bind () = check_method t ~actor ~kind:Receipt.Kind.Repl_task_run in
  let%bind checked =
    Capture.Source_selection.Shared.check_current
      selection
      ~actor
      ~checkpoint:t.checkpoint
  in
  let%map () = Capture.Source_selection.Shared.check_now selection ~checked ~actor in
  checked
;;

let admit_repl_root t ~actor ~selection ~ready ~resources ~active ~borrowed =
  let open Result.Let_syntax in
  let%bind selected_model = select_live_model t ~actor ~resources in
  let final_admission ~actor:now_actor ~selection:now_selection ~checkpoint =
    if not (phys_equal now_actor actor && phys_equal now_selection selection)
    then Error (P.Error.invalid_request "developer REPL Root selection changed")
    else (
      let%bind checked =
        Capture.Source_selection.Shared.check_current selection ~actor ~checkpoint
      in
      let%bind () = Live_resources.check_current resources in
      let%bind () = Capture.Source_selection.Shared.check_now selection ~checked ~actor in
      let%bind () = Repl_epoch.check_now borrowed in
      let%bind () =
        match selected_model with
        | None -> Ok ()
        | Some selected -> Provider_operator_port.Model_selection.check_current selected
      in
      Ok
        (fun () ->
          let%bind () = check_method t ~actor ~kind:Receipt.Kind.Repl_task_run in
          let%bind () =
            Capture.Source_selection.Shared.check_now selection ~checked ~actor
          in
          let%bind () = Repl_epoch.check_now borrowed in
          let%bind () = Live_resources.check_current resources in
          match selected_model with
          | None -> Ok ()
          | Some selected -> Provider_operator_port.Model_selection.check_current selected))
  in
  Root.Registry.admit_repl_recovered
    ~final_admission
    ~checkpoint:t.checkpoint
    t.roots
    ~original_actor:(Repl_epoch.original_actor borrowed)
    ~current_actor:actor
    ~selection
    ~registry:(Live_resources.native_registry resources)
    ~selected:(Live_resources.selected_native resources)
    ~selected_model
    ~job_policy:t.job_policy
    ~handle:ready.handle
    ~workspace_conflict_domain:(Live_resources.conflict_domain resources)
    ~transfer_finished_claim:(fun ~accept ->
      Lifecycle.Active.transfer_finished_claim active ~handle:ready.handle ~accept)
;;

let same_principal (left : P.Principal.t) (right : P.Principal.t) =
  P.Id.Principal.equal left.id right.id
  && String.equal left.authentication_kind right.authentication_kind
;;

let check_old_live_control t ~actor (ready : ready_run) =
  match ready.live_owner with
  | None -> Ok ()
  | Some live ->
    let row = live_origin_row live in
    let principal = Operator_authorization.principal actor in
    if
      (match Row.kind row with
       | Receipt.Kind.Live_run | Repl_task_run -> true
       | Check | Fixture_run -> false)
      && P.Id.Principal.equal (Row.owner row).principal_id principal.id
      && P.Execution_owner.equal_developer (Row.owner row) (Store.owner ready.handle)
      && Result.is_ok (check_method t ~actor ~kind:(Row.kind row))
      && Command_view.Control_policy.authorize t.control_policy actor row
      && Operator_authorization.is_current actor
    then Ok ()
    else
      Error
        (error
           P.Error.Permission_denied
           "developer live retirement requires control"
           ~retryable:false)
;;

let persisted_owner_now t ~run_id ~expected =
  let open Result.Let_syntax in
  let%bind existing = store (Store.Result_root.find_existing t.store run_id) in
  let%bind observed =
    match existing with
    | None ->
      Error (error P.Error.Lease_stale "developer run disappeared" ~retryable:false)
    | Some existing -> store (Store.Result_root.observe_latest existing)
  in
  let persisted = Store.Observation.checkpoint observed |> Store.Checkpoint.owner in
  if P.Execution_owner.equal_developer persisted expected
  then Ok ()
  else Error (error P.Error.Lease_stale "developer run owner changed" ~retryable:false)
;;

let select_next_epoch
      t
      ~run_id
      ~key
      ~request_fingerprint
      ~(principal : P.Principal.t)
      ~capture_fingerprint
      ~repl_selection
  =
  let open Result.Let_syntax in
  let%bind previous =
    let%bind existing = store (Store.Result_root.find_existing t.store run_id) in
    match existing with
    | None -> Ok None
    | Some existing ->
      let%map observed = store (Store.Result_root.observe_latest existing) in
      Some observed
  in
  let host_id = Agent_store.Session_store.server_id t.session_store in
  let%bind epoch =
    match previous with
    | None -> Ok 0
    | Some observed ->
      let persisted = Store.Observation.checkpoint observed |> Store.Checkpoint.owner in
      if
        not
          (P.Id.Developer_run.equal persisted.run_id run_id
           && P.Id.Server.equal persisted.host_id host_id
           && P.Id.Principal.equal persisted.principal_id principal.id)
      then
        Error
          (error
             P.Error.Permission_denied
             "developer run belongs to another principal"
             ~retryable:false)
      else if Int.equal persisted.epoch Int.max_value
      then
        Error
          (error
             P.Error.Resource_limit
             "developer run epoch limit reached"
             ~retryable:false)
      else Ok (persisted.epoch + 1)
  in
  let%map owner =
    P.Execution_owner.developer ~host_id ~run_id ~epoch ~principal_id:principal.id
    |> Result.bind ~f:(function
      | P.Execution_owner.Developer owner -> Ok owner
      | Conversation _ -> Error (P.Error.invalid_request "invalid developer owner"))
  in
  { owner
  ; key
  ; request_fingerprint
  ; capture_fingerprint
  ; repl_selection
  ; previous
  ; principal
  }
;;

let append_selected_epoch t ~run_id ~actor ~authorize_now ~live_selection selected =
  let open Result.Let_syntax in
  if not (same_principal selected.principal (Operator_authorization.principal actor))
  then
    Error
      (error P.Error.Permission_denied "developer run principal changed" ~retryable:false)
  else (
    let%bind () = authorize_now () in
    let%bind handle, _ =
      store
        (Store.admit_epoch
           t.store
           ~owner:selected.owner
           ~capture_fingerprint:selected.capture_fingerprint
           ~previous:selected.previous)
    in
    let ready =
      { handle
      ; actor
      ; capture_fingerprint = selected.capture_fingerprint
      ; repl_selection = selected.repl_selection
      ; live_selection
      ; live_owner = None
      ; live_gate = Eio.Mutex.create ()
      ; repl_retirement = None
      }
    in
    Hashtbl.set t.runs ~key:run_id ~data:(Ready ready);
    let%map () = authorize_now () in
    ready)
;;

let rec retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring =
  let open Result.Let_syntax in
  match retiring.step with
  | Join_callbacks ->
    Option.iter retiring.old.live_owner ~f:(fun live ->
      Option.iter (live_approval_owner live) ~f:Approval_owner.close);
    let%bind () = Lifecycle.retire_run t.lifecycle slot ~actor in
    retiring.step <- Detach_scheduler;
    retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring
  | Detach_scheduler ->
    let%bind () =
      match retiring.old.live_owner with
      | None -> Ok ()
      | Some live ->
        (match live_root live, t.scheduler with
         | None, _ -> Ok ()
         | Some _, None ->
           Error
             (error
                P.Error.Invalid_state
                "developer scheduler is unavailable"
                ~retryable:true)
         | Some root, Some scheduler -> Job_scheduler.retire_developer scheduler ~root)
    in
    retiring.step <- Close_root;
    retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring
  | Close_root ->
    let%bind () =
      match retiring.old.live_owner with
      | None -> Ok ()
      | Some live ->
        (match live_root live with
         | None -> Ok ()
         | Some root -> Root.close_and_wait root)
    in
    let%bind () = Root.Registry.retire_admission t.roots ~handle:retiring.old.handle in
    let%bind () =
      Lifecycle.confirm_transferred_retired t.lifecycle slot ~handle:retiring.old.handle
    in
    retiring.step <- Close_resources;
    retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring
  | Close_resources ->
    let%bind () =
      match retiring.old.live_owner with
      | None -> Ok ()
      | Some live -> Developer_live_resources.close_and_wait (live_resources live)
    in
    retiring.step <- Select_epoch;
    retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring
  | Select_epoch ->
    let%bind () = authorize_now () in
    let%bind () = Lifecycle.release_finished_epoch t.lifecycle slot in
    let%bind selected =
      select_next_epoch
        t
        ~run_id
        ~key:retiring.target_key
        ~request_fingerprint:retiring.target_request
        ~principal:retiring.principal
        ~capture_fingerprint:retiring.target_capture
        ~repl_selection:retiring.target_repl_selection
    in
    Hashtbl.set t.runs ~key:run_id ~data:(Appending selected);
    append_selected_epoch t ~run_id ~actor ~authorize_now ~live_selection selected
;;

let admit_fresh_run
      t
      slot
      ~repl_selection
      ~actor
      ~run_id
      ~key
      ~request_fingerprint
      ~capture_fingerprint
      ~live_selection
      ~authorize_now
  =
  let open Result.Let_syntax in
  let%bind () = authorize_now () in
  let principal = Operator_authorization.principal actor in
  match Hashtbl.find t.runs run_id with
  | None ->
    let%bind () =
      if Hashtbl.length t.runs < t.max_runs
      then Ok ()
      else
        Error
          (error
             P.Error.Resource_limit
             "developer host run limit reached"
             ~retryable:false)
    in
    let%bind retained = Lifecycle.ready_epoch t.lifecycle slot in
    let%bind reused =
      match retained with
      | None -> Ok None
      | Some handle ->
        let owner = Store.owner handle in
        let%bind () =
          if P.Id.Principal.equal owner.principal_id principal.id
          then Ok ()
          else
            Error
              (error
                 P.Error.Permission_denied
                 "developer run belongs to another principal"
                 ~retryable:false)
        in
        let%bind active = store (Store.is_latest_active handle) in
        let%bind () = authorize_now () in
        if
          active
          && Chatml_source.Digest.equal
               (Store.capture_fingerprint handle)
               capture_fingerprint
        then (
          let ready =
            { handle
            ; actor
            ; capture_fingerprint
            ; repl_selection
            ; live_selection
            ; live_owner = None
            ; live_gate = Eio.Mutex.create ()
            ; repl_retirement = None
            }
          in
          Hashtbl.set t.runs ~key:run_id ~data:(Ready ready);
          Ok (Some ready))
        else (
          let%map () = Lifecycle.release_finished_epoch t.lifecycle slot in
          None)
    in
    (match reused with
     | Some ready -> Ok ready
     | None ->
       let%bind selected =
         select_next_epoch
           t
           ~run_id
           ~key
           ~request_fingerprint
           ~principal
           ~capture_fingerprint
           ~repl_selection
       in
       Hashtbl.set t.runs ~key:run_id ~data:(Appending selected);
       append_selected_epoch t ~run_id ~actor ~authorize_now ~live_selection selected)
  | Some (Appending selected) ->
    if
      P.Idempotency_key.equal selected.key key
      && Chatml_source.Digest.equal selected.request_fingerprint request_fingerprint
      && Chatml_source.Digest.equal selected.capture_fingerprint capture_fingerprint
    then append_selected_epoch t ~run_id ~actor ~authorize_now ~live_selection selected
    else
      Error
        (error P.Error.Conflict "developer epoch admission is pending" ~retryable:true)
  | Some (Retiring retiring) ->
    if
      same_principal retiring.principal principal
      && P.Idempotency_key.equal retiring.target_key key
      && Chatml_source.Digest.equal retiring.target_request request_fingerprint
      && Chatml_source.Digest.equal retiring.target_capture capture_fingerprint
      &&
      match repl_selection, retiring.target_repl_selection with
      | None, None -> true
      | Some left, Some right -> phys_equal left right
      | None, Some _ | Some _, None -> false
    then
      Eio.Cancel.protect (fun () ->
        retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring)
    else
      Error (error P.Error.Conflict "developer run retirement is pending" ~retryable:true)
  | Some (Ready ready) ->
    let%bind () =
      match ready.repl_retirement with
      | None -> Ok ()
      | Some _ ->
        Error
          (error
             P.Error.Conflict
             "developer REPL Task retirement is pending"
             ~retryable:true)
    in
    let%bind () = persisted_owner_now t ~run_id ~expected:(Store.owner ready.handle) in
    let%bind () = authorize_now () in
    if not (same_principal (Operator_authorization.principal ready.actor) principal)
    then
      Error
        (error
           P.Error.Permission_denied
           "developer run principal changed"
           ~retryable:false)
    else if
      Chatml_source.Digest.equal ready.capture_fingerprint capture_fingerprint
      && (match repl_selection, ready.repl_selection with
          | None, None -> true
          | Some requested, Some installed -> phys_equal requested installed
          | None, Some _ | Some _, None -> false)
      && (match repl_selection, ready.live_owner with
          | None, _ -> true
          | Some requested, Some (Root_owned { root; _ }) ->
            (match Root.source root with
             | Root.Source.Repl installed -> phys_equal requested installed
             | Command _ -> false)
          | Some _, None | Some _, Some (Prepared _ | Root_provisional _) -> false)
      && (match live_selection, ready.live_selection with
          | None, _ -> true
          | Some _, None -> true
          | Some requested, Some installed -> equal_live_selection requested installed)
      &&
      match live_selection with
      | None -> true
      | Some _ ->
        (match ready.live_owner with
         | None ->
           Option.is_none ready.live_selection
           || phys_equal ready.actor actor
           || Option.is_some repl_selection
         | Some (Root_owned { root; _ }) ->
           phys_equal ready.actor actor
           ||
             (match repl_selection, Root.source root with
             | Some requested, Root.Source.Repl installed ->
               phys_equal requested installed
             | None, _ | Some _, Command _ -> false)
         | Some (Prepared _ | Root_provisional _) -> false)
    then (
      if Option.is_some repl_selection then ready.actor <- actor;
      (match live_selection with
       | None -> ()
       | Some requested ->
         if Option.is_none ready.live_selection then ready.actor <- actor;
         ready.live_selection <- Some requested);
      Ok ready)
    else (
      let%bind active = Lifecycle.has_active t.lifecycle slot in
      let%bind () =
        match ready.live_owner with
        | None when active && Option.is_some ready.live_selection ->
          Error
            (error
               P.Error.Conflict
               "developer Live selection is still active"
               ~retryable:true)
        | Some (Prepared _ | Root_provisional _)
          when active
               && Chatml_source.Digest.equal ready.capture_fingerprint capture_fingerprint
          ->
          Error
            (error
               P.Error.Conflict
               "developer live preparation is still active"
               ~retryable:true)
        | None | Some (Root_owned _ | Prepared _ | Root_provisional _) -> Ok ()
      in
      let%bind () = check_old_live_control t ~actor ready in
      let%bind () = Lifecycle.check_retirement t.lifecycle slot ~actor in
      let retiring =
        { old = ready
        ; target_key = key
        ; target_request = request_fingerprint
        ; target_capture = capture_fingerprint
        ; target_repl_selection = repl_selection
        ; principal
        ; step = Join_callbacks
        }
      in
      Hashtbl.set t.runs ~key:run_id ~data:(Retiring retiring);
      Eio.Cancel.protect (fun () ->
        retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring))
;;

let submit_fresh
      t
      ~actor
      ~run_id
      ~key
      ~original
      ~resolve_fresh
      ~request_current
      ~validate_capture
      ~kind
      ~run
  =
  let open Result.Let_syntax in
  let check_request () =
    if request_current ()
    then Ok ()
    else
      Error
        (error
           P.Error.Permission_denied
           "developer request lease changed"
           ~retryable:false)
  in
  let%bind () = check_request () in
  let%bind () = check_method t ~actor ~kind in
  Lifecycle.with_slot t.lifecycle ~run_id ~f:(fun slot ->
    let open Result.Let_syntax in
    let%bind () = check_request () in
    let%bind () = check_method t ~actor ~kind in
    let%bind found =
      Lifecycle.lookup
        t.lifecycle
        slot
        ~actor
        ~key
        ~kind
        ~request_fingerprint:(Original.fingerprint original)
    in
    let%bind found =
      match found with
      | Some view ->
        (match Row.phase (Lifecycle.View.row view) with
         | Row.Phase.Terminal _ -> Ok (Some view)
         | Reserved | Waiting_permission | Execution_started ->
           let%bind () = Lifecycle.ensure_startup_recovered t.lifecycle slot in
           let%map refreshed =
             Lifecycle.lookup
               t.lifecycle
               slot
               ~actor
               ~key
               ~kind
               ~request_fingerprint:(Original.fingerprint original)
           in
           refreshed)
      | None -> Ok None
    in
    let%bind () = check_method t ~actor ~kind in
    let%bind () = check_request () in
    match found with
    | Some view -> Ok view
    | None ->
      let%bind () = Lifecycle.ensure_startup_recovered t.lifecycle slot in
      let%bind () = check_request () in
      let%bind fresh = resolve_fresh () in
      let%bind () = check_request () in
      let%bind () = check_method t ~actor ~kind in
      let%bind candidate =
        Developer_command_source.prepare
          t.capture
          ~original
          ~fresh
          ~actor
          ~clock:(Eio.Stdenv.mono_clock t.env)
          ~checkpoint:t.checkpoint
      in
      let capture = Capture.Candidate.capture candidate in
      let%bind () = validate_capture capture in
      let capture_fingerprint = Chatml_developer_capture.fingerprint capture in
      let%bind budgets = Budgets.for_original t.budgets original in
      let%bind () = check_request () in
      let%bind () = check_method t ~actor ~kind in
      let%bind before_retirement =
        Capture.Candidate.final_admission
          candidate
          ~actor
          ~capture
          ~checkpoint:t.checkpoint
      in
      let authorize_now () =
        let open Result.Let_syntax in
        let%bind () = check_request () in
        let%bind () = check_method t ~actor ~kind in
        before_retirement ()
      in
      let%bind admitted =
        admit_fresh_run
          t
          slot
          ~repl_selection:None
          ~actor
          ~run_id
          ~key
          ~request_fingerprint:(Original.fingerprint original)
          ~capture_fingerprint
          ~live_selection:(live_selection_of_original original)
          ~authorize_now
      in
      let handle = admitted.handle in
      let%bind () = Lifecycle.ensure_admitted_claim t.lifecycle slot ~handle in
      let%bind final_check =
        Capture.Candidate.final_admission
          candidate
          ~actor
          ~capture
          ~checkpoint:t.checkpoint
      in
      let admit () =
        Result.is_ok (check_method t ~actor ~kind) && Result.is_ok (final_check ())
      in
      let%bind () = check_request () in
      let%bind reserved =
        Lifecycle.reserve
          t.lifecycle
          slot
          ~handle
          ~key
          ~kind
          ~request_fingerprint:(Original.fingerprint original)
          ~admit:(fun () -> request_current () && admit ())
      in
      let id = Lifecycle.View.row reserved |> Row.id in
      Lifecycle.launch t.lifecycle slot ~handle ~reserved ~f:(fun active ~sw ->
        let trace_entry =
          match t.trace, Capture.Candidate.historical_origin candidate with
          | None, _ | _, None -> None
          | Some traces, Some historical_origin ->
            let row = Lifecycle.Active.view active |> Lifecycle.View.row in
            let prepared = Chatml_developer_capture.prepared capture in
            let provenance =
              Developer_trace_retention.Provenance.of_prepared
                ~action:(Command kind)
                ~original_actor:actor
                ~route:(Original.route original)
                ~historical_origin
                ~capture_fingerprint:(Some capture_fingerprint)
                ~selection_fingerprint:
                  (Chatml_developer_capture.graph_fingerprint capture)
                ~prepared
                ~limits:(Developer_trace_retention.limits traces)
            in
            let association = Developer_trace_retention.Association.of_reserved_row row in
            (match provenance, association with
             | Ok provenance, Ok association ->
               let entry =
                 Developer_trace_retention.begin_prepared
                   traces
                   ~association
                   ~provenance
                   ~submission_fingerprint:(Original.fingerprint original)
                   ~prepared
                   ~checkpoint:Eio.Fiber.check
               in
               entry
             | Error _, _ | _, Error _ -> None)
        in
        match t.trace with
        | None ->
          run t ~actor ~candidate ~admitted ~capture ~budgets ~id ~active ~sw ~admit
        | Some traces ->
          Developer_trace_retention.within traces trace_entry (fun () ->
            run t ~actor ~candidate ~admitted ~capture ~budgets ~id ~active ~sw ~admit)))
;;

let submit_pure
      t
      ~actor
      ~run_id
      ~key
      ~original
      ~resolve_fresh
      ~request_current
      ~validate_capture
      ~kind
  =
  submit_fresh
    t
    ~actor
    ~run_id
    ~key
    ~original
    ~resolve_fresh
    ~request_current
    ~validate_capture
    ~kind
    ~run:
      (fun
        t ~actor:_ ~candidate ~admitted ~capture ~budgets ~id ~active:_ ~sw:_ ~admit ->
      let open Result.Let_syntax in
      let open Chatml.Chatml_compiler_contract.Target in
      let selected = Capture.Candidate.selected_contract candidate in
      let%bind tool_selection =
        match kind, Original.target original with
        | Receipt.Kind.Check, _ | Fixture_run, One_off_v1 -> Ok None
        | Fixture_run, Tool_v1 ->
          (match Capture.Selected_contract.tool_fixture selected with
           | None ->
             Error (P.Error.invalid_request "Tool_v1 policy has no selected schema")
           | Some selected_tool ->
             let%bind run_id =
               Chatml_developer_test.Run_id.create (Receipt.Id.to_string id)
               |> Result.map_error ~f:(fun _ ->
                 P.Error.invalid_request "invalid developer fixture identity")
             in
             let owner = Store.owner admitted.handle in
             Chatml_developer_test.Tool_v1.Selection.create
               ~capture
               ~contract:(Capture.Selected_contract.contract selected)
               ~run:owner.run_id
               ~id:run_id
               ~generation:owner.epoch
               ~tool_name:(Capture.Selected_contract.Tool_fixture.name selected_tool)
               ~input_schema:
                 (Capture.Selected_contract.Tool_fixture.input_schema selected_tool)
               ~output_schema:
                 (Capture.Selected_contract.Tool_fixture.output_schema selected_tool)
               ~created_at:(t.now ())
               ~deadline:None
               ~limits:(Budgets.execution budgets)
               ~max_output_bytes:(Budgets.max_output_bytes budgets)
               ~checkpoint:t.checkpoint
             |> Result.map ~f:Option.some
             |> Result.map_error ~f:(fun _ ->
               P.Error.invalid_request "Tool_v1 fixture selection is invalid"))
        | ( (Fixture_run | Live_run)
          , (Developer_tool_v1 | Moderator_v1 | Delegated_moderator_v1) ) ->
          Error (P.Error.invalid_request "unsupported developer fixture target")
        | Live_run, (One_off_v1 | Tool_v1) ->
          Error (P.Error.invalid_request "live command cannot use fixture runner")
        | Repl_task_run, _ ->
          Error (P.Error.invalid_request "REPL Task cannot use command fixture runner")
      in
      Developer_command_portable.run_kind
        ~kind
        ~capture
        ~tool_selection
        ~admit
        ~id
        ~budgets
        ~env:t.env)
;;

let run_live_locked
      t
      ~original
      ~actor
      ~candidate
      ~admitted
      ~capture
      ~budgets
      ~id:_
      ~active
      ~sw:_
      ~admit
  =
  let open Result.Let_syntax in
  let%bind scheduler =
    t.scheduler
    |> Result.of_option
         ~error:
           (error
              P.Error.Invalid_state
              "developer scheduler is unavailable"
              ~retryable:true)
  in
  let%bind () =
    match live_selection_of_original original, admitted.live_selection with
    | Some requested, Some selected when equal_live_selection requested selected -> Ok ()
    | None, _ | Some _, None | Some _, Some _ ->
      Error
        (error
           P.Error.Lease_stale
           "developer Live selection changed before execution"
           ~retryable:false)
  in
  let%bind preparing = Lifecycle.Active.start_preparation active ~actor ~admit in
  let origin_row = Lifecycle.Active.view active |> Lifecycle.View.row in
  let%bind root, resources, services, approval_owner, started =
    match admitted.live_owner with
    | Some (Root_owned { root; resources; services; approval_owner; _ }) ->
      let%bind () = Live_resources.check_current resources in
      let%map started = Lifecycle.Active.Preparing.bind_root preparing ~root in
      root, resources, services, approval_owner, started
    | Some (Prepared _ | Root_provisional _) ->
      Error
        (error
           P.Error.Conflict
           "developer live preparation is incomplete"
           ~retryable:true)
    | None ->
      let%bind prompt, workspace =
        match Original.prompt original, Original.workspace original with
        | Some prompt, Some workspace -> Ok (prompt, workspace)
        | None, _ | _, None ->
          Error
            (P.Error.invalid_request
               "developer Live requires configured prompt and workspace")
      in
      let%bind approval_owner =
        match t.approval with
        | None -> Ok None
        | Some configuration ->
          Approval_owner.create_with_change
            ~sw:t.sw
            ~env:t.env
            ~store:t.store
            ~preparing
            ~configuration
            ~on_change:(fun row ->
              Option.iter t.on_change ~f:(fun notify ->
                notify
                  ~run_id:(Row.owner row).run_id
                  ~key:(Row.key row)
                  ~kind:Change.Approval_changed))
          |> Result.map ~f:Option.some
      in
      let build_initial () =
        let%bind resources =
          Live_resources.prepare
            t.live_resources
            ~handle:admitted.handle
            ~approval_owner
            ~original_actor:actor
            ~prompt
            ~workspace
            ~checkpoint:t.checkpoint
            ~retain:(fun resources ->
              admitted.live_owner
              <- Some (Prepared { resources; origin_row; approval_owner }))
        in
        let%bind root =
          admit_live_root t ~actor ~capture ~candidate ~ready:admitted ~resources ~active
        in
        admitted.live_owner
        <- Some (Root_provisional { root; resources; origin_row; approval_owner });
        let%bind started = Lifecycle.Active.Preparing.bind_root preparing ~root in
        let%bind () =
          match approval_owner with
          | None -> Ok ()
          | Some owner -> Approval_owner.bind_root owner ~root ~started
        in
        let%bind services =
          create_live_services
            t
            ~root
            ~resources
            ~approval_owner
            ~current_actor:(fun () -> actor)
        in
        let%bind () =
          Job_scheduler.attach_developer scheduler ~root ~runner:services.background
        in
        let%map () =
          match services.model with
          | None -> Ok ()
          | Some model ->
            Job_scheduler.attach_developer_model scheduler ~root ~runner:model
        in
        root, resources, services, approval_owner, started
      in
      (match build_initial () with
       | Ok value -> Ok value
       | Error error ->
         Option.iter approval_owner ~f:Approval_owner.close;
         Error error
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         Option.iter approval_owner ~f:Approval_owner.close;
         Exn.raise_with_original_backtrace exn backtrace)
  in
  let%bind current = Root.current root in
  let%bind () = Root.check_effect_now root current in
  let%bind execution_capture =
    Root.command_capture root
    |> Result.of_option
         ~error:
           (error
              P.Error.Lease_stale
              "developer live command source changed"
              ~retryable:false)
  in
  let%bind () =
    if
      Chatml_source.Digest.equal
        (Chatml_developer_capture.fingerprint execution_capture)
        admitted.capture_fingerprint
      && Chatml_source.Digest.equal
           (Chatml_developer_capture.fingerprint capture)
           admitted.capture_fingerprint
    then Ok ()
    else
      Error
        (error
           P.Error.Lease_stale
           "developer live capture changed before execution"
           ~retryable:false)
  in
  let%bind execution =
    Developer_command_execution.Live.create
      ~root
      ~jobs:services.jobs
      ~native:services.native
      ~authorize:
        (Live_resources.Native_authorizer.for_runtime
           services.native_authorizer
           (Live_resources.native_runtime resources))
      ~prepare_output:t.prepare_output
      ~capture:execution_capture
      ~budgets
      ~env:t.env
  in
  admitted.live_owner
  <- Some (Root_owned { root; resources; services; origin_row; approval_owner });
  match approval_owner with
  | None -> Developer_command_execution.Live.run execution ~started
  | Some owner ->
    Approval_owner.with_started_command owner ~active ~started ~actor (fun () ->
      Developer_command_execution.Live.run execution ~started)
;;

let with_live_gate gate f =
  (* Callers finish provisional-owner cleanup in their own [Exn.protect]
     before cancellation reaches this boundary. Retained Root custody still
     checks and reconciles independently; expected cancellation alone does not
     corrupt the serialization gate for a later command. *)
  let outcome =
    Eio.Mutex.use_rw ~protect:false gate (fun () ->
      match f () with
      | value -> `Returned value
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        `Cancelled (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | `Returned value -> value
  | `Cancelled (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let run_live
      t
      ~original
      ~actor
      ~candidate
      ~admitted
      ~capture
      ~budgets
      ~id
      ~active
      ~sw
      ~admit
  =
  with_live_gate admitted.live_gate (fun () ->
    Exn.protect
      ~finally:(fun () ->
        match admitted.live_owner with
        | Some (Prepared { approval_owner; _ } | Root_provisional { approval_owner; _ })
          -> Option.iter approval_owner ~f:Approval_owner.close
        | None | Some (Root_owned _) -> ())
      ~f:(fun () ->
        run_live_locked
          t
          ~original
          ~actor
          ~candidate
          ~admitted
          ~capture
          ~budgets
          ~id
          ~active
          ~sw
          ~admit))
;;

let run_repl_task_locked
      t
      ~actor
      ~budgets
      ~selection
      ~admitted
      ~active
      ~borrowed
      ~task_handle
      ~origin
      ~admit
  =
  let open Result.Let_syntax in
  let%bind scheduler =
    t.scheduler
    |> Result.of_option
         ~error:
           (error
              P.Error.Invalid_state
              "developer scheduler is unavailable"
              ~retryable:false)
  in
  let%bind () = Repl_epoch.check_now borrowed in
  let%bind () = check_repl_selection t ~actor selection |> Result.map ~f:ignore in
  let%bind preparing = Lifecycle.Active.start_preparation active ~actor ~admit in
  let origin_row = Lifecycle.Active.view active |> Lifecycle.View.row in
  let%bind root, resources, services, approval_owner, started =
    match admitted.live_owner with
    | Some (Root_owned { root; resources; services; approval_owner; _ }) ->
      let%bind () = Live_resources.check_current resources in
      let%map started =
        Lifecycle.Active.Preparing.bind_repl_root
          preparing
          ~root
          ~borrowed
          ~checkpoint:t.checkpoint
      in
      root, resources, services, approval_owner, started
    | Some (Prepared _ | Root_provisional _) ->
      Error
        (error
           P.Error.Conflict
           "developer REPL Task preparation is incomplete"
           ~retryable:true)
    | None ->
      let%bind approval_owner =
        match t.approval with
        | None -> Ok None
        | Some configuration ->
          Approval_owner.create_with_change
            ~sw:t.sw
            ~env:t.env
            ~store:t.store
            ~preparing
            ~configuration
            ~on_change:(fun row ->
              Option.iter t.on_change ~f:(fun notify ->
                notify
                  ~run_id:(Row.owner row).run_id
                  ~key:(Row.key row)
                  ~kind:Change.Approval_changed))
          |> Result.map ~f:Option.some
      in
      let build_initial () =
        let%bind resources =
          Live_resources.prepare
            t.live_resources
            ~handle:admitted.handle
            ~approval_owner
            ~original_actor:(Repl_epoch.original_actor borrowed)
            ~prompt:(Repl_epoch.prompt borrowed)
            ~workspace:(Repl_epoch.workspace borrowed)
            ~checkpoint:t.checkpoint
            ~retain:(fun resources ->
              admitted.live_owner
              <- Some (Prepared { resources; origin_row; approval_owner }))
        in
        let%bind root =
          admit_repl_root t ~actor ~selection ~ready:admitted ~resources ~active ~borrowed
        in
        admitted.live_owner
        <- Some (Root_provisional { root; resources; origin_row; approval_owner });
        let%bind started =
          Lifecycle.Active.Preparing.bind_repl_root
            preparing
            ~root
            ~borrowed
            ~checkpoint:t.checkpoint
        in
        let%bind () =
          match approval_owner with
          | None -> Ok ()
          | Some owner -> Approval_owner.bind_root owner ~root ~started
        in
        let%bind services =
          create_live_services
            t
            ~root
            ~resources
            ~approval_owner
            ~current_actor:(fun () -> admitted.actor)
        in
        let%bind () =
          Job_scheduler.attach_developer scheduler ~root ~runner:services.background
        in
        let%map () =
          match services.model with
          | None -> Ok ()
          | Some model ->
            Job_scheduler.attach_developer_model scheduler ~root ~runner:model
        in
        root, resources, services, approval_owner, started
      in
      (match build_initial () with
       | Ok value -> Ok value
       | Error error ->
         Option.iter approval_owner ~f:Approval_owner.close;
         Error error
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         Option.iter approval_owner ~f:Approval_owner.close;
         Exn.raise_with_original_backtrace exn backtrace)
  in
  let%bind () = Repl_epoch.check_now borrowed in
  let%bind () = Live_resources.check_current resources in
  let%bind () = check_repl_selection t ~actor selection |> Result.map ~f:ignore in
  admitted.live_owner
  <- Some (Root_owned { root; resources; services; origin_row; approval_owner });
  let run () =
    let%bind result =
      Developer_repl_task_execution.run
        ~root
        ~jobs:services.jobs
        ~native:services.native
        ~authorize:
          (Live_resources.Native_authorizer.for_runtime
             services.native_authorizer
             (Live_resources.native_runtime resources))
        ~prepare_output:t.prepare_output
        ~borrowed
        ~origin
        ~handle:task_handle
        ~started
        ~action_policy:(Chatml_execution.Bounded (Budgets.execution budgets))
        ~check_current:(fun () ->
          let%bind () = Repl_epoch.check_now borrowed in
          let%bind () = Live_resources.check_current resources in
          let%map _checked = check_repl_selection t ~actor selection in
          ())
    in
    Task_receipt.project ~limits:(Budgets.receipt budgets) ~poll:t.checkpoint result
  in
  match approval_owner with
  | None -> run ()
  | Some owner -> Approval_owner.with_started_command owner ~active ~started ~actor run
;;

let run_repl_task
      t
      ~actor
      ~budgets
      ~selection
      ~admitted
      ~active
      ~borrowed
      ~task_handle
      ~origin
      ~admit
  =
  with_live_gate admitted.live_gate (fun () ->
    Exn.protect
      ~finally:(fun () ->
        match admitted.live_owner with
        | Some (Prepared { approval_owner; _ } | Root_provisional { approval_owner; _ })
          -> Option.iter approval_owner ~f:Approval_owner.close
        | None | Some (Root_owned _) -> ())
      ~f:(fun () ->
        run_repl_task_locked
          t
          ~actor
          ~budgets
          ~selection
          ~admitted
          ~active
          ~borrowed
          ~task_handle
          ~origin
          ~admit))
;;

let submit_repl_task_resolving
      t
      ~actor
      ~key
      ~original
      ~resolve_fresh_epoch
      ~request_current
  =
  let open Result.Let_syntax in
  let kind = Receipt.Kind.Repl_task_run in
  let reference = Developer_repl_task_original.reference original in
  let run_id = Repl_epoch.Task_reference.run_id reference in
  let request_fingerprint = Developer_repl_task_original.fingerprint original in
  let%bind () = check_method t ~actor ~kind in
  let%bind existing =
    Lifecycle.with_slot t.lifecycle ~run_id ~f:(fun slot ->
      let%bind () = check_method t ~actor ~kind in
      let%bind found =
        Lifecycle.lookup t.lifecycle slot ~actor ~key ~kind ~request_fingerprint
      in
      match found with
      | Some view ->
        (match Row.phase (Lifecycle.View.row view) with
         | Row.Phase.Terminal _ -> Ok (Some view)
         | Reserved | Waiting_permission | Execution_started ->
           let%bind () = Lifecycle.ensure_startup_recovered t.lifecycle slot in
           Lifecycle.lookup t.lifecycle slot ~actor ~key ~kind ~request_fingerprint)
      | None -> Ok None)
  in
  match existing with
  | Some view ->
    if request_current ()
    then Ok view
    else Error (error P.Error.Lease_stale "REPL request lease expired" ~retryable:false)
  | None ->
    let%bind budgets =
      Budgets.for_repl_task
        t.budgets
        ~requested_execution:(Developer_repl_task_original.requested_execution original)
        ~requested_max_output_bytes:
          (Developer_repl_task_original.requested_max_output_bytes original)
    in
    let%bind () =
      if request_current ()
      then Ok ()
      else Error (error P.Error.Lease_stale "REPL request lease expired" ~retryable:false)
    in
    let%bind instance, epoch = resolve_fresh_epoch () in
    let%bind repl =
      t.repl
      |> Result.of_option
           ~error:(P.Error.invalid_request "developer REPL is not installed")
    in
    Repl_epoch.Task_action.start
      repl
      ~actor
      ~instance
      ~epoch
      ~key
      ~f:(fun action ~publish_initial ->
        let launched = ref false in
        let admitted_handle = ref None in
        let attempt () =
          let%bind selection =
            Repl_epoch.Task_action.preflight action ~actor ~reference
          in
          let capture_fingerprint =
            Capture.Source_selection.repl_root_fingerprint selection
          in
          Lifecycle.with_slot t.lifecycle ~run_id ~f:(fun slot ->
            let%bind () = check_method t ~actor ~kind in
            let%bind found =
              Lifecycle.lookup t.lifecycle slot ~actor ~key ~kind ~request_fingerprint
            in
            let%bind () =
              match found with
              | None -> Ok ()
              | Some _ ->
                Error
                  (error
                     P.Error.Conflict
                     "developer REPL Task key was installed concurrently"
                     ~retryable:true)
            in
            let%bind () = Lifecycle.ensure_startup_recovered t.lifecycle slot in
            let%bind checked = check_repl_selection t ~actor selection in
            let authorize_now () =
              let%bind () = check_method t ~actor ~kind in
              let%bind () =
                Capture.Source_selection.Shared.check_now selection ~checked ~actor
              in
              if Operator_authorization.is_current actor && request_current ()
              then Ok ()
              else
                Error
                  (error
                     P.Error.Permission_denied
                     "developer REPL Task actor is no longer current"
                     ~retryable:false)
            in
            let%bind admitted =
              admit_fresh_run
                t
                slot
                ~repl_selection:(Some selection)
                ~actor
                ~run_id
                ~key
                ~request_fingerprint
                ~capture_fingerprint
                ~live_selection:None
                ~authorize_now
            in
            admitted_handle := Some admitted.handle;
            let%bind () =
              Lifecycle.ensure_admitted_claim t.lifecycle slot ~handle:admitted.handle
            in
            let%bind checked = check_repl_selection t ~actor selection in
            let admit () =
              Result.is_ok (check_method t ~actor ~kind)
              && Result.is_ok
                   (Capture.Source_selection.Shared.check_now selection ~checked ~actor)
            in
            let admit_reserve () = request_current () && admit () in
            let%bind reserved =
              Lifecycle.reserve
                t.lifecycle
                slot
                ~handle:admitted.handle
                ~key
                ~kind
                ~request_fingerprint
                ~admit:admit_reserve
            in
            let%bind () = Repl_epoch.Task_action.mark_launching action in
            launched := true;
            Lifecycle.launch
              t.lifecycle
              slot
              ~handle:admitted.handle
              ~reserved
              ~f:(fun active ~sw ->
                Repl_epoch.Task_action.enter action ~sw ~f:(fun () ->
                  Repl_epoch.Task_action.with_current
                    action
                    ~actor
                    ~reference
                    ~f:(fun borrowed task_handle origin ->
                      let trace_entry =
                        Option.bind t.trace ~f:(fun traces ->
                          match
                            Repl_epoch.Task_origin.check_current
                              origin
                              ~actor
                              ~checkpoint:Eio.Fiber.check
                          with
                          | Error _ -> None
                          | Ok () ->
                            let row =
                              Lifecycle.Active.view active |> Lifecycle.View.row
                            in
                            (match Repl_epoch.Task_origin.trace_provenance origin with
                             | None -> None
                             | Some lineage ->
                               let provenance =
                                 Developer_trace_retention.Provenance.for_retained_task
                                   lineage
                                   ~capture_fingerprint:(Row.capture_fingerprint row)
                                   ~limits:(Developer_trace_retention.limits traces)
                               in
                               let association =
                                 Developer_trace_retention.Association.of_reserved_row row
                               in
                               (match provenance, association with
                                | Some provenance, Ok association ->
                                  let entry =
                                    Developer_trace_retention.begin_submitted
                                      traces
                                      ~association
                                      ~provenance
                                      ~submission_fingerprint:request_fingerprint
                                      ~source:(Repl_epoch.Task_origin.source origin)
                                      ~checkpoint:Eio.Fiber.check
                                  in
                                  entry
                                | None, _ | _, Error _ -> None)))
                      in
                      let run () =
                        run_repl_task
                          t
                          ~actor
                          ~budgets
                          ~selection
                          ~admitted
                          ~active
                          ~borrowed
                          ~task_handle
                          ~origin
                          ~admit
                      in
                      match t.trace with
                      | None -> run ()
                      | Some traces ->
                        Developer_trace_retention.within traces trace_entry run))))
        in
        let initial = attempt () in
        publish_initial initial;
        match initial with
        | Error error ->
          (match
             Lifecycle.with_slot t.lifecycle ~run_id ~f:(fun slot ->
               Lifecycle.reconcile_unlaunched
                 t.lifecycle
                 slot
                 ~actor
                 ~key
                 ~kind
                 ~request_fingerprint
                 ~handle:!admitted_handle)
           with
           | Ok disposition ->
             let disposition =
               match disposition with
               | Lifecycle.Unlaunched.No_durable_row ->
                 Repl_epoch.Task_action.No_durable_row
               | Terminal_row -> Terminal_row
             in
             Repl_epoch.Task_action.finish_unlaunched action ~disposition
           | Error _ -> Error error)
        | Ok initial ->
          if not !launched
          then
            Error
              (error
                 P.Error.Invalid_state
                 "developer REPL Task was returned without Launch"
                 ~retryable:true)
          else (
            let%bind terminal =
              Lifecycle.await_terminal t.lifecycle ~actor ~run_id ~key
            in
            let row = Lifecycle.View.row terminal in
            if
              Row.same_identity (Lifecycle.View.row initial) row
              && Receipt.Kind.equal (Row.kind row) kind
              &&
              match Row.phase row with
              | Row.Phase.Terminal _ -> true
              | Reserved | Waiting_permission | Execution_started -> false
            then Repl_epoch.Task_action.mark_settled action
            else
              Error
                (error
                   P.Error.Persistence_error
                   "developer REPL Task terminal identity changed"
                   ~retryable:true)))
;;

let submit_repl_task t ~actor ~key ~original ~fresh_epoch =
  submit_repl_task_resolving
    t
    ~actor
    ~key
    ~original
    ~resolve_fresh_epoch:(fun () ->
      fresh_epoch
      |> Result.of_option
           ~error:(P.Error.invalid_request "developer REPL Task instance is unavailable"))
    ~request_current:(fun () -> true)
;;

let submit_resolving_command
      t
      ~actor
      ~run_id
      ~key
      ~original
      ~resolve_fresh
      ~request_current
      ~validate_capture
  =
  let kind = Original.kind original in
  (match kind with
   | Receipt.Kind.Check | Fixture_run ->
     submit_pure
       t
       ~actor
       ~run_id
       ~key
       ~original
       ~resolve_fresh
       ~request_current
       ~validate_capture
       ~kind
   | Live_run ->
     submit_fresh
       t
       ~actor
       ~run_id
       ~key
       ~original
       ~resolve_fresh
       ~request_current
       ~validate_capture
       ~kind
       ~run:(run_live ~original)
   | Repl_task_run ->
     Error (P.Error.invalid_request "REPL Task requires its retained interpreter"))
  |> Result.map ~f:Command_view.of_lifecycle
;;

let submit_command t ~actor ~run_id ~key ~original ~fresh_source =
  submit_resolving_command
    t
    ~actor
    ~run_id
    ~key
    ~original
    ~request_current:(fun () -> true)
    ~validate_capture:(fun _ -> Ok ())
    ~resolve_fresh:(fun () ->
      fresh_source
      |> Result.of_option
           ~error:(P.Error.invalid_request "developer source is unavailable"))
;;

let observe_once t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  if not (Operator_authorization.is_current actor)
  then
    Error
      (error
         P.Error.Permission_denied
         "developer command actor is no longer current"
         ~retryable:false)
  else (
    let%bind existing = Lifecycle.observe t.lifecycle ~actor ~run_id ~key in
    match existing with
    | None ->
      if Operator_authorization.is_current actor
      then Ok None
      else
        Error
          (error
             P.Error.Permission_denied
             "developer command actor is no longer current"
             ~retryable:false)
    | Some existing ->
      let principal = Operator_authorization.principal actor in
      let row = Lifecycle.View.row existing in
      if Receipt.Kind.equal (Row.kind row) Repl_task_run
      then Error (P.Error.invalid_request "REPL Task requires its own receipt method")
      else (
        let%bind () = check_method t ~actor ~kind:(Row.kind row) in
        if
          P.Id.Principal.equal (Row.owner row).principal_id principal.id
          && Command_view.Disclosure_policy.authorize t.disclosure_policy actor row
        then Ok (Some (Command_view.of_row row))
        else
          Error
            (error
               P.Error.Permission_denied
               "developer command receipt access denied"
               ~retryable:false)))
;;

let observe_command t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  let%bind before = observe_once t ~actor ~run_id ~key in
  match before with
  | None -> Ok None
  | Some view ->
    (match Command_view.phase view with
     | Row.Phase.Terminal _ -> Ok (Some view)
     | Reserved | Waiting_permission | Execution_started ->
       Lifecycle.with_recovered_observation t.lifecycle ~run_id ~f:(fun () ->
         observe_once t ~actor ~run_id ~key))
;;

let await_command t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  let%bind () =
    if t.closed
    then Error (error P.Error.Invalid_state "developer host is closed" ~retryable:false)
    else Ok ()
  in
  let%bind prior = observe_command t ~actor ~run_id ~key in
  let%bind () =
    match prior with
    | Some _ -> Ok ()
    | None -> Error (P.Error.invalid_request "developer command is absent")
  in
  let%bind view = Lifecycle.await_terminal t.lifecycle ~actor ~run_id ~key in
  let%bind () =
    if Receipt.Kind.equal (Row.kind (Lifecycle.View.row view)) Repl_task_run
    then Error (P.Error.invalid_request "REPL Task requires its own receipt method")
    else Ok ()
  in
  let%bind () = check_method t ~actor ~kind:(Row.kind (Lifecycle.View.row view)) in
  Ok (Command_view.of_lifecycle view)
;;

let cancel_command t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  let%bind found = observe_command t ~actor ~run_id ~key in
  match found with
  | None -> Error (P.Error.invalid_request "developer command is absent")
  | Some view ->
    (match Command_view.phase view with
     | Row.Phase.Terminal _ -> Ok view
     | Reserved | Waiting_permission | Execution_started ->
       let%bind view = Lifecycle.cancel_and_wait t.lifecycle ~actor ~run_id ~key in
       let%map () = check_method t ~actor ~kind:(Row.kind (Lifecycle.View.row view)) in
       Command_view.of_lifecycle view)
;;

module Approval = struct
  module Pending = Approval_owner.Pending

  type decision = Approval_owner.decision =
    | Approve_once
    | Deny of string

  let current_owner t ~run_id =
    if t.closed
    then Error (P.Error.invalid_request "developer approval host is closed")
    else (
      match Hashtbl.find t.runs run_id with
      | Some
          (Ready { live_owner = Some (Root_owned { approval_owner = Some owner; _ }); _ })
        -> Ok owner
      | None | Some (Ready _ | Retiring _ | Appending _) ->
        Error (P.Error.invalid_request "developer approval owner is unavailable"))
  ;;

  let pending t ~actor ~run_id =
    let open Result.Let_syntax in
    let%bind owner = current_owner t ~run_id in
    Approval_owner.pending owner ~actor
  ;;

  let respond t ~actor ~run_id ~id decision =
    let open Result.Let_syntax in
    let%bind owner = current_owner t ~run_id in
    Approval_owner.respond owner ~actor ~id decision
  ;;

  let cancel t ~actor ~run_id ~id =
    let open Result.Let_syntax in
    let%bind owner = current_owner t ~run_id in
    Approval_owner.cancel owner ~actor ~id
  ;;
end

let repl_cleanup_pair repl ~instance ready =
  let open Result.Let_syntax in
  let%bind run_id = Repl_epoch.instance_run_id repl instance in
  let owner = Store.owner ready.handle in
  let%bind selection =
    ready.repl_selection
    |> Result.of_option
         ~error:(P.Error.invalid_request "developer REPL Task source is unavailable")
  in
  let%bind () =
    if
      P.Id.Developer_run.equal owner.run_id run_id
      && Chatml_source.Digest.equal
           ready.capture_fingerprint
           (Capture.Source_selection.repl_root_fingerprint selection)
      && Repl_epoch.instance_uses_selection repl instance selection
    then Ok ()
    else Error (P.Error.invalid_request "developer REPL Task owner changed")
  in
  let%bind () =
    match ready.live_owner with
    | None -> Ok ()
    | Some live ->
      let row = live_origin_row live in
      let%bind () =
        if
          Receipt.Kind.equal (Row.kind row) Repl_task_run
          && P.Execution_owner.equal_developer (Row.owner row) owner
          && Chatml_source.Digest.equal
               (Row.capture_fingerprint row)
               ready.capture_fingerprint
        then Ok ()
        else Error (P.Error.invalid_request "developer REPL Task row changed")
      in
      (match live_root live with
       | None -> Ok ()
       | Some root ->
         (match Root.source root with
          | Root.Source.Repl installed
            when phys_equal installed selection
                 && P.Execution_owner.equal_developer (Root.owner root) owner
                 && Repl_epoch.instance_paired_to
                      repl
                      instance
                      ~selection:installed
                      ~original_actor:(Root.original_actor root) -> Ok ()
          | Command _ | Repl _ ->
            Error (P.Error.invalid_request "developer REPL Task Root changed")))
  in
  Ok ()
;;

let authorize_repl_retirement t repl ~actor ~instance =
  let open Result.Let_syntax in
  let%bind run_id = Repl_epoch.instance_run_id repl instance in
  match Hashtbl.find t.runs run_id with
  | None -> Ok ()
  | Some (Ready ready) ->
    let%bind () = check_method t ~actor ~kind:Receipt.Kind.Repl_task_run in
    let%bind () = repl_cleanup_pair repl ~instance ready in
    (match ready.repl_retirement with
     | None -> check_old_live_control t ~actor ready
     | Some _ -> Ok ())
  | Some (Retiring _ | Appending _) ->
    Error
      (error
         P.Error.Conflict
         "developer REPL Task run retirement is occupied"
         ~retryable:true)
;;

let retire_repl_instance t repl ~actor instance =
  let open Result.Let_syntax in
  let%bind run_id = Repl_epoch.instance_run_id repl instance in
  match Hashtbl.find t.runs run_id with
  | None -> Ok ()
  | Some (Retiring _ | Appending _) ->
    Error
      (error
         P.Error.Conflict
         "developer REPL Task run retirement is occupied"
         ~retryable:true)
  | Some (Ready ready) ->
    let%bind () = check_method t ~actor ~kind:Receipt.Kind.Repl_task_run in
    let%bind () = repl_cleanup_pair repl ~instance ready in
    Lifecycle.with_slot t.lifecycle ~run_id ~f:(fun slot ->
      let check_custody retirement =
        let%bind custody = Repl_epoch.Task_cleanup.current_custody repl instance in
        match Hashtbl.find t.runs run_id with
        | Some (Ready current)
          when phys_equal current ready
               && Repl_epoch.Instance.equal retirement.instance instance
               && Repl_epoch.Task_cleanup.same_custody retirement.custody custody ->
          repl_cleanup_pair repl ~instance ready
        | None | Some (Retiring _ | Appending _ | Ready _) ->
          Error (P.Error.invalid_request "developer REPL Task cleanup owner changed")
      in
      let rec advance retirement =
        let%bind () = check_custody retirement in
        match retirement.step with
        | Retire_task_lifecycle ->
          Option.iter ready.live_owner ~f:(fun live ->
            Option.iter (live_approval_owner live) ~f:Approval_owner.close);
          let%bind () = Lifecycle.retire_run t.lifecycle slot ~actor in
          retirement.step <- Detach_task_scheduler;
          advance retirement
        | Detach_task_scheduler ->
          let%bind () =
            match ready.live_owner with
            | None -> Ok ()
            | Some live ->
              (match live_root live, t.scheduler with
               | None, _ -> Ok ()
               | Some _, None ->
                 Error
                   (error
                      P.Error.Invalid_state
                      "developer scheduler is unavailable"
                      ~retryable:true)
               | Some root, Some scheduler ->
                 Job_scheduler.retire_developer scheduler ~root)
          in
          retirement.step <- Close_task_root;
          advance retirement
        | Close_task_root ->
          let%bind () =
            match ready.live_owner with
            | None -> Ok ()
            | Some live ->
              (match live_root live with
               | None -> Ok ()
               | Some root -> Root.close_and_wait root)
          in
          retirement.step <- Retire_task_admission;
          advance retirement
        | Retire_task_admission ->
          let%bind () = Root.Registry.retire_admission t.roots ~handle:ready.handle in
          let%bind () =
            match ready.live_owner with
            | Some live when Option.is_some (live_root live) ->
              Lifecycle.confirm_transferred_retired t.lifecycle slot ~handle:ready.handle
            | None | Some _ -> Ok ()
          in
          retirement.step <- Close_task_resources;
          advance retirement
        | Close_task_resources ->
          let%bind () =
            match ready.live_owner with
            | None -> Ok ()
            | Some live -> Developer_live_resources.close_and_wait (live_resources live)
          in
          retirement.step <- Select_task_epoch;
          advance retirement
        | Select_task_epoch ->
          let%bind () = Lifecycle.release_finished_epoch t.lifecycle slot in
          Hashtbl.remove t.runs run_id;
          Ok ()
      in
      match ready.repl_retirement with
      | None ->
        let%bind () =
          Repl_epoch.Task_cleanup.check_retiring_current repl ~actor ~instance
        in
        let%bind () = repl_cleanup_pair repl ~instance ready in
        let%bind () = check_old_live_control t ~actor ready in
        let%bind custody = Repl_epoch.Task_cleanup.current_custody repl instance in
        let retirement = { step = Retire_task_lifecycle; instance; custody } in
        ready.repl_retirement <- Some retirement;
        advance retirement
      | Some retirement -> advance retirement)
;;

let close_physical_runs t =
  let open Result.Let_syntax in
  let live_runs =
    Hashtbl.data t.runs
    |> List.filter_map ~f:(function
      | Ready ready -> Option.map ready.live_owner ~f:(fun live -> ready.handle, live)
      | Retiring retiring ->
        Option.map retiring.old.live_owner ~f:(fun live -> retiring.old.handle, live)
      | Appending _ -> None)
  in
  let%bind () =
    List.fold_result live_runs ~init:() ~f:(fun () (_, live) ->
      match live_root live, t.scheduler with
      | None, _ -> Ok ()
      | Some _, None ->
        Error
          (error
             P.Error.Invalid_state
             "developer scheduler is unavailable"
             ~retryable:true)
      | Some root, Some scheduler -> Job_scheduler.retire_developer scheduler ~root)
  in
  let%bind () =
    List.fold_result live_runs ~init:() ~f:(fun () (_, live) ->
      match live_root live with
      | None -> Ok ()
      | Some root -> Root.close_and_wait root)
  in
  let%bind () =
    List.fold_result live_runs ~init:() ~f:(fun () (handle, _) ->
      Root.Registry.retire_admission t.roots ~handle)
  in
  let%bind () =
    List.fold_result live_runs ~init:() ~f:(fun () (_, live) ->
      Developer_live_resources.close_and_wait (live_resources live))
  in
  Hashtbl.clear t.runs;
  Ok ()
;;

let install_repl_task_cleanup t repl =
  Repl_epoch.Task_cleanup.create
    ~authorize_retirement:(fun ~actor ~instance ->
      authorize_repl_retirement t repl ~actor ~instance)
    ~retire_instance:(retire_repl_instance t repl)
    ~retire_all:(fun () ->
      let open Result.Let_syntax in
      let%bind () = Lifecycle.stop_and_join t.lifecycle in
      close_physical_runs t)
  |> Repl_epoch.Task_cleanup.install_once repl
;;

let create
      ~sw
      ~env
      ~session_store
      ~blobs
      ~capacity
      ~authoring
      ~provider
      ~model_target
      ~prompts
      ~catalogs
      ~workspace_leases
      ~physical_roots
      ~workspace_cleanup
      ~script_preparation
      ~tool_dir
      ~home
      ~cache
      ~approval
      ~max_native_bindings
      ~child_sources
      ~content_limits
      ~remote_content
      ~agent_source_limits
      ~active_agent_calls
      ~job_policy
      ~runtime_policy
      ~native_permission
      ~prepare_output
      ~capture_policy
      ~capture_limits
      ~command_disclosure
      ~command_control
      ~result_disclosure
      ~publisher_limits
      ~publisher_storage
      ~store_limits
      ~(limits : Limits.t)
      ~budget_configuration
      ~repl
      ~trace
      ?on_change
      ?on_repl_change
      ?on_task_change
      ~now
      ~checkpoint
      ()
  =
  let open Result.Let_syntax in
  let%bind capture =
    Authoring_installation.developer_capture
      authoring
      ~historical_recording:
        (Option.map trace ~f:Developer_trace_retention.Configuration.historical_recording)
      ~policy:capture_policy
      ~limits:capture_limits
  in
  let%bind artifact_store =
    Agent_store.Prompt_artifact_store.create
      ~env
      ~root:
        (Agent_store.Data_root.prompt_artifacts_path
           (Agent_store.Session_store.data_root session_store))
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let%bind live_resources =
    Developer_live_resources.create
      ~sw
      ~env
      ~prompts
      ~catalogs
      ~workspace_leases
      ~physical_roots
      ~workspace_cleanup
      ~script_preparation
      ~tool_dir
      ~home
      ~cache
      ~max_native_bindings
      ~child_sources
      ~content_limits
      ~remote_content
      ~artifact_store
      ~agent_source_limits
      ~active_agent_calls
      ~native_permission
  in
  let%bind store_owner =
    Store.open_or_create session_store ~sw ~limits:store_limits ~checkpoint |> store
  in
  let acquired_roots = ref None in
  let acquired_lifecycle = ref None in
  let acquired_repl = ref None in
  let acquired_trace = ref None in
  let build () =
    let%bind () =
      Agent_store.Blob_store.bind_developer_runs blobs ~session_store store_owner
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    in
    let%bind budgets = Budgets.bind budget_configuration ~store:store_owner in
    let%bind configured_store_limits = store (Store.limits store_owner) in
    let%bind result_disclosure =
      Developer_result_disclosure.create
        ~runs:store_owner
        ~blobs
        ~policy:result_disclosure
        ~limits:limits.disclosure
    in
    let roots =
      Root.Registry.create ~sw ~store:store_owner ~capacity ~now ~limits:limits.roots
    in
    acquired_roots := Some roots;
    let disclosure =
      Lifecycle.Disclosure_policy.create ~authorize:(fun actor row ->
        Result.is_ok
          (Developer_method_authorization.authorize_principal
             (Row.kind row)
             ~principal:(Operator_authorization.principal actor))
        && Command_view.Disclosure_policy.authorize command_disclosure actor row)
    in
    let control =
      Lifecycle.Control_policy.create ~authorize:(fun actor row ->
        Result.is_ok
          (Developer_method_authorization.authorize_principal
             (Row.kind row)
             ~principal:(Operator_authorization.principal actor))
        && Command_view.Control_policy.authorize command_control actor row)
    in
    let recovery =
      Lifecycle.Recovery_adapter.create ~recover:(fun ~claim:_ ~handle:_ ~window ->
        Developer_command_recovery.recover window ~now)
    in
    let lifecycle_change row =
      match Row.kind row with
      | Receipt.Kind.Check | Fixture_run | Live_run ->
        Option.iter on_change ~f:(fun notify ->
          notify
            ~run_id:(Row.owner row).run_id
            ~key:(Row.key row)
            ~kind:Change.Command_changed)
      | Repl_task_run ->
        Option.iter on_task_change ~f:(fun notify ->
          notify ~run_id:(Row.owner row).run_id ~key:(Row.key row))
    in
    let trace_owner =
      Option.map trace ~f:(fun configuration ->
        Developer_trace_retention.create
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~configuration)
    in
    acquired_trace := trace_owner;
    let trace_joined row =
      Option.iter trace_owner ~f:(fun traces ->
        match Developer_trace_retention.find_command_entry traces row, Row.phase row with
        | Some entry, Row.Phase.Terminal { receipt; _ } ->
          (match
             Chatml_trace.Terminal.create
               ~receipt_version:1
               ~disposition:(trace_disposition receipt)
           with
           | Ok terminal -> Developer_trace_retention.finish traces entry terminal
           | Error _ -> ())
        | None, _ | Some _, (Reserved | Waiting_permission | Execution_started) -> ())
    in
    let%bind lifecycle =
      Lifecycle.create_with_hook
        ~on_committed:(Some lifecycle_change)
        ~on_joined_terminal:(Some trace_joined)
        ~sw
        ~store:store_owner
        ~max_active_commands:limits.max_active_commands
        ~disclosure
        ~control
        ~recovery
    in
    acquired_lifecycle := Some lifecycle;
    let repl =
      Option.map repl ~f:(fun configuration ->
        Repl_epoch.install
          ~sw
          ~capture_host:capture
          ~traces:trace_owner
          ~env
          ~clock:(Eio.Stdenv.mono_clock env)
          ~configuration
          ?on_change:on_repl_change
          ~checkpoint
          ())
    in
    acquired_repl := repl;
    let host =
      { sw
      ; env
      ; session_store
      ; blobs
      ; capacity
      ; store = store_owner
      ; roots
      ; lifecycle
      ; capture
      ; authoring
      ; repl
      ; trace = trace_owner
      ; live_resources
      ; approval
      ; on_change
      ; disclosure_policy = command_disclosure
      ; control_policy = command_control
      ; provider
      ; model_target
      ; job_policy
      ; runtime_policy
      ; prepare_output
      ; result_disclosure
      ; publisher_limits
      ; publisher_storage
      ; budgets
      ; max_runs = Store.Limits.max_runs configured_store_limits
      ; now
      ; checkpoint
      ; runs = Hashtbl.create (module P.Id.Developer_run)
      ; scheduler = None
      ; closed = false
      }
    in
    let%map () =
      match repl with
      | None -> Ok ()
      | Some repl -> install_repl_task_cleanup host repl
    in
    host
  in
  let cleanup () =
    Eio.Cancel.protect (fun () ->
      let open Result.Let_syntax in
      let%bind () =
        match !acquired_repl with
        | None -> Ok ()
        | Some repl -> Repl_epoch.shutdown_and_wait repl
      in
      Option.iter !acquired_trace ~f:Developer_trace_retention.close;
      let%bind () =
        match !acquired_lifecycle with
        | None -> Ok ()
        | Some lifecycle -> Lifecycle.close_and_wait lifecycle
      in
      let%bind () =
        match !acquired_roots with
        | None -> Ok ()
        | Some roots -> Root.Registry.close_and_wait roots
      in
      store (Store.close_and_wait store_owner))
  in
  let outcome =
    match build () with
    | result -> `Returned result
    | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  match outcome with
  | `Returned (Ok _ as value) -> value
  | `Returned (Error original) ->
    let closed = cleanup () in
    (match closed with
     | Ok () -> Error original
     | Error cleanup_error ->
       Error
         (P.Error.create
            P.Error.Persistence_error
            ~message:"developer host startup and cleanup failed"
            ~retryable:true
            ~data:
              (`Object
                  [ "startup", P.Error.to_json original
                  ; "cleanup", P.Error.to_json cleanup_error
                  ])
            ())
     | exception _ ->
       Error
         (P.Error.create
            P.Error.Persistence_error
            ~message:"developer host startup cleanup raised"
            ~retryable:true
            ~data:(`Object [ "startup", P.Error.to_json original ])
            ()))
  | `Raised (exn, backtrace) ->
    (try ignore (cleanup () : (unit, P.Error.t) result) with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let close_and_wait t =
  t.closed <- true;
  Eio.Cancel.protect (fun () ->
    let open Result.Let_syntax in
    Hashtbl.iter t.runs ~f:(function
      | Ready ready ->
        Option.iter ready.live_owner ~f:(fun live ->
          Option.iter (live_approval_owner live) ~f:Approval_owner.close)
      | Retiring retiring ->
        Option.iter retiring.old.live_owner ~f:(fun live ->
          Option.iter (live_approval_owner live) ~f:Approval_owner.close)
      | Appending _ -> ());
    let%bind () =
      match t.repl with
      | None -> Ok ()
      | Some repl -> Repl_epoch.shutdown_and_wait repl
    in
    let%bind () = Lifecycle.stop_and_join t.lifecycle in
    let%bind () = close_physical_runs t in
    let%bind () = Root.Registry.close_and_wait t.roots in
    let%bind () = Lifecycle.close_and_wait t.lifecycle in
    let%bind () = Developer_live_resources.drain_unresolved t.live_resources in
    Option.iter t.trace ~f:Developer_trace_retention.close;
    store (Store.close_and_wait t.store))
;;

module Command = struct
  include Command_view

  let submit = submit_command
  let submit_resolving = submit_resolving_command
  let observe = observe_command
  let await_terminal = await_command
  let cancel_and_wait = cancel_command
end

module Repl = struct
  module Original = Repl_epoch.Original
  module Instance = Repl_epoch.Instance
  module Epoch = Repl_epoch.Epoch
  module Revision = Repl_epoch.Revision
  module View = Repl_epoch.View
  module Submission = Repl_epoch.Submission
  module Task_origin = Repl_epoch.Task_origin
  module Task_reference = Repl_epoch.Task_reference
  module Type_selector = Repl_epoch.Type_selector
  module Type_inspection = Repl_epoch.Type_inspection
  module Task_reference_page = Repl_epoch.Task_reference_page

  type borrowed = Repl_epoch.borrowed

  let with_owner t ~f =
    if t.closed
    then Error (error P.Error.Invalid_state "developer host is closed" ~retryable:false)
    else (
      match t.repl with
      | None ->
        Error
          (error P.Error.Invalid_state "developer REPL is not installed" ~retryable:false)
      | Some repl -> f repl)
  ;;

  let create t ~actor ~run_id ~key ~original ~fresh_source =
    with_owner t ~f:(fun repl ->
      Repl_epoch.create repl ~actor ~run_id ~key ~original ~fresh_source)
  ;;

  let create_resolving t ~actor ~run_id ~key ~original ~resolve_fresh ~request_current =
    with_owner t ~f:(fun repl ->
      Repl_epoch.create_resolving
        repl
        ~actor
        ~run_id
        ~key
        ~original
        ~resolve_fresh
        ~request_current)
  ;;

  let submit_with_request
        t
        ~actor
        ~instance
        ~epoch
        ~expected_revision
        ~key
        ~source
        ~request_current
    =
    with_owner t ~f:(fun repl ->
      Repl_epoch.submit_with_request
        repl
        ~actor
        ~instance
        ~epoch
        ~expected_revision
        ~key
        ~source
        ~request_current)
  ;;

  let submit t ~actor ~instance ~epoch ~expected_revision ~key ~source =
    with_owner t ~f:(fun repl ->
      Repl_epoch.submit repl ~actor ~instance ~epoch ~expected_revision ~key ~source)
  ;;

  let observe t ~actor ~instance ~key =
    with_owner t ~f:(fun repl -> Repl_epoch.observe repl ~actor ~instance ~key)
  ;;

  let inspect_type t ~actor ~instance ~epoch ~key ~statement_index ~selector ~max_bytes =
    with_owner t ~f:(fun repl ->
      Repl_epoch.inspect_type
        repl
        ~actor
        ~instance
        ~epoch
        ~key
        ~statement_index
        ~selector
        ~max_bytes)
  ;;

  let task_references_page t ~actor ~instance ~epoch ~key ~after ~limit =
    with_owner t ~f:(fun repl ->
      Repl_epoch.task_references_page repl ~actor ~instance ~epoch ~key ~after ~limit)
  ;;

  let cancel_and_wait_with_request t ~actor ~instance ~key ~request_current =
    with_owner t ~f:(fun repl ->
      Repl_epoch.cancel_and_wait_with_request repl ~actor ~instance ~key ~request_current)
  ;;

  let cancel_and_wait t ~actor ~instance ~key =
    with_owner t ~f:(fun repl -> Repl_epoch.cancel_and_wait repl ~actor ~instance ~key)
  ;;

  let reset_and_wait_with_request t ~actor ~instance ~expected_epoch ~key ~request_current
    =
    with_owner t ~f:(fun repl ->
      Repl_epoch.reset_and_wait_with_request
        repl
        ~actor
        ~instance
        ~expected_epoch
        ~key
        ~request_current)
  ;;

  let reset_and_wait t ~actor ~instance ~expected_epoch ~key =
    with_owner t ~f:(fun repl ->
      Repl_epoch.reset_and_wait repl ~actor ~instance ~expected_epoch ~key)
  ;;

  let close_and_wait_with_request t ~actor ~instance ~key ~request_current =
    with_owner t ~f:(fun repl ->
      Repl_epoch.close_and_wait_with_request repl ~actor ~instance ~key ~request_current)
  ;;

  let close_and_wait t ~actor ~instance ~key =
    with_owner t ~f:(fun repl -> Repl_epoch.close_and_wait repl ~actor ~instance ~key)
  ;;

  let with_current t ~actor ~instance ~epoch ~f =
    with_owner t ~f:(fun repl -> Repl_epoch.with_current repl ~actor ~instance ~epoch ~f)
  ;;

  let evaluator = Repl_epoch.evaluator
  let check_now = Repl_epoch.check_now
  let task_origin = Repl_epoch.task_origin

  module Task = struct
    module Original = Developer_repl_task_original

    type view = { row : Row.t }

    let of_lifecycle value = { row = Lifecycle.View.row value }
    let row value = value.row

    let submit t ~actor ~key ~original ~fresh_epoch =
      submit_repl_task t ~actor ~key ~original ~fresh_epoch |> Result.map ~f:of_lifecycle
    ;;

    let submit_resolving t ~actor ~key ~original ~resolve_fresh_epoch ~request_current =
      submit_repl_task_resolving
        t
        ~actor
        ~key
        ~original
        ~resolve_fresh_epoch
        ~request_current
      |> Result.map ~f:of_lifecycle
    ;;

    let receipt value =
      match Row.phase value.row with
      | Row.Phase.Terminal { receipt; _ } -> Some receipt
      | Reserved | Waiting_permission | Execution_started -> None
    ;;

    let check_row t ~actor row =
      if not (Receipt.Kind.equal (Row.kind row) Repl_task_run)
      then Error (P.Error.invalid_request "developer row is not a REPL Task")
      else
        let open Result.Let_syntax in
        let%bind () = check_method t ~actor ~kind:Repl_task_run in
        let principal = Operator_authorization.principal actor in
        if
          P.Id.Principal.equal (Row.owner row).principal_id principal.id
          && Command_view.Disclosure_policy.authorize t.disclosure_policy actor row
          && Operator_authorization.is_current actor
        then Ok ()
        else
          Error
            (error
               P.Error.Permission_denied
               "developer REPL Task receipt access denied"
               ~retryable:false)
    ;;

    let observe_once t ~actor ~run_id ~key =
      let open Result.Let_syntax in
      let%bind () = check_method t ~actor ~kind:Repl_task_run in
      let%bind existing = Lifecycle.observe t.lifecycle ~actor ~run_id ~key in
      match existing with
      | None ->
        let%map () = check_method t ~actor ~kind:Repl_task_run in
        None
      | Some existing ->
        let row = Lifecycle.View.row existing in
        let%map () = check_row t ~actor row in
        Some { row }
    ;;

    let observe t ~actor ~run_id ~key =
      let open Result.Let_syntax in
      let%bind found = observe_once t ~actor ~run_id ~key in
      match found with
      | None -> Ok None
      | Some ({ row; _ } as view) ->
        (match Row.phase row with
         | Row.Phase.Terminal _ -> Ok (Some view)
         | Reserved | Waiting_permission | Execution_started ->
           Lifecycle.with_recovered_observation t.lifecycle ~run_id ~f:(fun () ->
             observe_once t ~actor ~run_id ~key))
    ;;

    let await_terminal t ~actor ~run_id ~key =
      let open Result.Let_syntax in
      let%bind prior = observe t ~actor ~run_id ~key in
      let%bind () =
        match prior with
        | None -> Error (P.Error.invalid_request "developer REPL Task is absent")
        | Some _ -> Ok ()
      in
      let%bind view = Lifecycle.await_terminal t.lifecycle ~actor ~run_id ~key in
      let%map () = check_row t ~actor (Lifecycle.View.row view) in
      of_lifecycle view
    ;;

    let cancel_and_wait_with_request t ~actor ~run_id ~key ~request_current =
      let open Result.Let_syntax in
      let%bind prior = observe t ~actor ~run_id ~key in
      match prior with
      | None -> Error (P.Error.invalid_request "developer REPL Task is absent")
      | Some view ->
        (match Row.phase view.row with
         | Row.Phase.Terminal _ -> Ok view
         | Reserved | Waiting_permission | Execution_started ->
           let%bind () =
             if request_current ()
             then Ok ()
             else
               Error
                 (error P.Error.Lease_stale "Task request lease expired" ~retryable:false)
           in
           let%bind settled = Lifecycle.cancel_and_wait t.lifecycle ~actor ~run_id ~key in
           let%map () = check_row t ~actor (Lifecycle.View.row settled) in
           of_lifecycle settled)
    ;;

    let cancel_and_wait t ~actor ~run_id ~key =
      cancel_and_wait_with_request t ~actor ~run_id ~key ~request_current:(fun () -> true)
    ;;
  end
end

module Trace = struct
  type lookup =
    | Command of
        { run_id : P.Id.Developer_run.t
        ; key : P.Idempotency_key.t
        }
    | Repl_statement of
        { instance : Repl.Instance.t
        ; epoch : Repl.Epoch.t
        ; key : P.Idempotency_key.t
        }
    | Repl_task of
        { run_id : P.Id.Developer_run.t
        ; key : P.Idempotency_key.t
        }

  let read_row t ~actor row =
    match t.trace with
    | None -> Ok Developer_trace_retention.Availability.Unavailable
    | Some traces ->
      (match Developer_trace_retention.find_command_entry traces row with
       | None -> Ok Developer_trace_retention.Availability.Unavailable
       | Some entry ->
         let open Result.Let_syntax in
         Developer_trace_retention.with_read traces ~actor entry ~f:(fun request ->
           let%bind checked =
             Developer_trace_retention.check_read_current
               traces
               request
               ~current_actor:actor
               ~checkpoint:t.checkpoint
           in
           let run_id = (Row.owner row).run_id in
           let key = Row.key row in
           let%bind current =
             match Row.kind row with
             | Receipt.Kind.Repl_task_run ->
               let%map view = Repl.Task.observe t ~actor ~run_id ~key in
               Option.map view ~f:Repl.Task.row
             | Check | Fixture_run | Live_run ->
               let%map view = Command.observe t ~actor ~run_id ~key in
               Option.map view ~f:Command.row
           in
           Developer_trace_retention.snapshot_checked
             traces
             request
             ~checked
             ~current_actor:actor
             ~exact_membership:(fun association ->
               Operator_authorization.is_current actor
               && Option.value_map current ~default:false ~f:(fun row ->
                 Developer_trace_retention.Association.matches_row association row))))
  ;;

  let snapshot t ~actor ~lookup =
    let open Result.Let_syntax in
    match lookup with
    | Command { run_id; key } ->
      let%bind observed = Command.observe t ~actor ~run_id ~key in
      (match observed with
       | None -> Error (P.Error.invalid_request "developer command is absent")
       | Some view -> read_row t ~actor (Command.row view))
    | Repl_task { run_id; key } ->
      let%bind observed = Repl.Task.observe t ~actor ~run_id ~key in
      (match observed with
       | None -> Error (P.Error.invalid_request "developer REPL Task is absent")
       | Some view -> read_row t ~actor (Repl.Task.row view))
    | Repl_statement { instance; epoch; key } ->
      (match t.repl with
       | None -> Error (P.Error.invalid_request "developer REPL is not installed")
       | Some repl ->
         let%bind entry = Repl_epoch.historical_trace repl ~actor ~instance ~epoch ~key in
         (match t.trace, entry with
          | None, _ | _, None -> Ok Developer_trace_retention.Availability.Unavailable
          | Some traces, Some entry ->
            Developer_trace_retention.with_read traces ~actor entry ~f:(fun request ->
              let%bind checked =
                Developer_trace_retention.check_read_current
                  traces
                  request
                  ~current_actor:actor
                  ~checkpoint:t.checkpoint
              in
              let%bind current =
                Repl_epoch.historical_trace repl ~actor ~instance ~epoch ~key
              in
              Developer_trace_retention.snapshot_checked
                traces
                request
                ~checked
                ~current_actor:actor
                ~exact_membership:(fun _ ->
                  Option.exists current ~f:(phys_equal entry)
                  &&
                  match Repl_epoch.historical_trace repl ~actor ~instance ~epoch ~key with
                  | Ok (Some current) -> phys_equal current entry
                  | Ok None | Error _ -> false))))
  ;;
end
