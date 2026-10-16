open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Receipt = Store.Command.Receipt
module Row = Store.Command.Row
module Lifecycle = Developer_command_lifecycle
module Original = Developer_command_original
module Budgets = Developer_command_budgets
module Capture = Authoring_context_service.Developer
module Root = Developer_root_owner
module Jobs = Developer_script_job_service
module Native = Developer_native_executor
module Background = Developer_background_job_runner
module Model = Developer_model_job_runner
module Live_resources = Developer_live_resources
module Child_runner = Developer_child_agent_runner

type live_services =
  { jobs : Jobs.t
  ; native : Native.t
  ; background : Background.t
  ; model : Model.t option
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
      }
  | Root_owned of
      { root : Root.run
      ; resources : Developer_live_resources.live
      ; origin_row : Row.t
      ; services : live_services
      }
  | Root_provisional of
      { root : Root.run
      ; resources : Developer_live_resources.live
      ; origin_row : Row.t
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

type ready_run =
  { handle : Store.handle
  ; mutable actor : Operator_authorization.t
  ; capture_fingerprint : Chatml_source.Digest.t
  ; mutable live_selection : live_selection option
  ; mutable live_owner : live_owner option
  ; live_gate : Eio.Mutex.t
  }

type retirement_step =
  | Join_callbacks
  | Detach_scheduler
  | Close_root
  | Close_resources
  | Select_epoch

type retiring_run =
  { old : ready_run
  ; target_key : P.Idempotency_key.t
  ; target_request : Chatml_source.Digest.t
  ; target_capture : Chatml_source.Digest.t
  ; principal : P.Principal.t
  ; mutable step : retirement_step
  }

type appending_epoch =
  { owner : P.Execution_owner.developer
  ; key : P.Idempotency_key.t
  ; request_fingerprint : Chatml_source.Digest.t
  ; capture_fingerprint : Chatml_source.Digest.t
  ; previous : Store.Observation.t option
  ; principal : P.Principal.t
  }

type admitted_run =
  | Ready of ready_run
  | Retiring of retiring_run
  | Appending of appending_epoch

type t =
  { env : Eio_unix.Stdenv.base
  ; session_store : Agent_store.Session_store.t
  ; blobs : Agent_store.Blob_store.t
  ; capacity : Job_capacity.t
  ; store : Store.t
  ; roots : Root.Registry.t
  ; lifecycle : Lifecycle.t
  ; capture : Capture.t
  ; live_resources : Developer_live_resources.t
  ; disclosure_policy : Command_view.Disclosure_policy.t
  ; control_policy : Command_view.Control_policy.t
  ; provider : Provider_operator_port.t option
  ; model_target : Inference.Request.Target.t option
  ; job_policy : Chat_response.One_off_request.policy
  ; authorize_native :
      P.Host_invocation.t
      -> Chat_response.Tool_capability.binding
      -> (unit, P.Error.t) result
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

let create_live_services t ~root ~resources ~current_actor =
  let open Result.Let_syntax in
  let%bind jobs =
    Jobs.create
      ~root
      ~store:t.store
      ~disclosure:t.result_disclosure
      ~env:t.env
      ~current_actor
      ~prepare_source:(prepare_agent_job_source ~root ~resources)
  in
  let%bind native = Native.create ~root in
  let%bind native = Native.with_job_service native jobs in
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
      ~authorize:t.authorize_native
      ~prepare_output:t.prepare_output
  in
  let%bind model =
    match Root.selected_model root with
    | None -> Ok None
    | Some _ ->
      Model.create ~root ~results ~sources:(Live_resources.model_sources resources)
      |> Result.map ~f:Option.some
  in
  let%bind run_selected_target =
    Child_runner.create
      ~live:resources
      ~root
      ~native
      ~authorize:t.authorize_native
      ~prepare_output:t.prepare_output
  in
  let%map () =
    Live_resources.bind_agent_sources
      resources
      ~root
      ~models:model
      ~run_selected_target
      ~checkpoint:t.checkpoint
  in
  { jobs; native; background; model }
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
      ~approval_broker
      ~max_native_bindings
      ~child_sources
      ~content_limits
      ~remote_content
      ~agent_source_limits
      ~job_policy
      ~authorize_native
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
      ~now
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind capture =
    Authoring_installation.developer_capture
      authoring
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
      ~approval_broker
      ~max_native_bindings
      ~child_sources
      ~content_limits
      ~remote_content
      ~artifact_store
      ~agent_source_limits
  in
  let%bind store_owner =
    Store.open_or_create session_store ~sw ~limits:store_limits ~checkpoint |> store
  in
  let acquired_roots = ref None in
  let acquired_lifecycle = ref None in
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
    let%map lifecycle =
      Lifecycle.create
        ~sw
        ~store:store_owner
        ~max_active_commands:limits.max_active_commands
        ~disclosure
        ~control
        ~recovery
    in
    acquired_lifecycle := Some lifecycle;
    { env
    ; session_store
    ; blobs
    ; capacity
    ; store = store_owner
    ; roots
    ; lifecycle
    ; capture
    ; live_resources
    ; disclosure_policy = command_disclosure
    ; control_policy = command_control
    ; provider
    ; model_target
    ; job_policy
    ; authorize_native
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
  let cleanup () =
    Eio.Cancel.protect (fun () ->
      let open Result.Let_syntax in
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
      Receipt.Kind.equal (Row.kind row) Receipt.Kind.Live_run
      && P.Id.Principal.equal (Row.owner row).principal_id principal.id
      && P.Execution_owner.equal_developer (Row.owner row) (Store.owner ready.handle)
      && Result.is_ok (check_method t ~actor ~kind:Receipt.Kind.Live_run)
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
  { owner; key; request_fingerprint; capture_fingerprint; previous; principal }
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
      ; live_selection
      ; live_owner = None
      ; live_gate = Eio.Mutex.create ()
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
    in
    Hashtbl.set t.runs ~key:run_id ~data:(Appending selected);
    append_selected_epoch t ~run_id ~actor ~authorize_now ~live_selection selected
;;

let admit_fresh_run
      t
      slot
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
            ; live_selection
            ; live_owner = None
            ; live_gate = Eio.Mutex.create ()
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
    then
      Eio.Cancel.protect (fun () ->
        retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring)
    else
      Error (error P.Error.Conflict "developer run retirement is pending" ~retryable:true)
  | Some (Ready ready) ->
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
      && (match live_selection, ready.live_selection with
          | None, _ -> true
          | Some _, None -> true
          | Some requested, Some installed -> equal_live_selection requested installed)
      &&
      match live_selection with
      | None -> true
      | Some _ ->
        (match ready.live_owner with
         | None -> Option.is_none ready.live_selection || phys_equal ready.actor actor
         | Some (Root_owned _) -> phys_equal ready.actor actor
         | Some (Prepared _ | Root_provisional _) -> false)
    then (
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
        ; principal
        ; step = Join_callbacks
        }
      in
      Hashtbl.set t.runs ~key:run_id ~data:(Retiring retiring);
      Eio.Cancel.protect (fun () ->
        retire_admitted t slot ~run_id ~actor ~authorize_now ~live_selection retiring))
;;

let submit_fresh t ~actor ~run_id ~key ~original ~fresh_source ~kind ~run =
  let open Result.Let_syntax in
  let%bind () = check_method t ~actor ~kind in
  Lifecycle.with_slot t.lifecycle ~run_id ~f:(fun slot ->
    let open Result.Let_syntax in
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
    match found with
    | Some view -> Ok view
    | None ->
      let%bind () = Lifecycle.ensure_startup_recovered t.lifecycle slot in
      let%bind fresh =
        fresh_source
        |> Result.of_option
             ~error:(P.Error.invalid_request "developer source is unavailable")
      in
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
      let capture_fingerprint = Chatml_developer_capture.fingerprint capture in
      let%bind budgets = Budgets.for_original t.budgets original in
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
        let%bind () = check_method t ~actor ~kind in
        before_retirement ()
      in
      let%bind admitted =
        admit_fresh_run
          t
          slot
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
      let%bind reserved =
        Lifecycle.reserve
          t.lifecycle
          slot
          ~handle
          ~key
          ~kind
          ~request_fingerprint:(Original.fingerprint original)
          ~admit
      in
      let id = Lifecycle.View.row reserved |> Row.id in
      Lifecycle.launch t.lifecycle slot ~handle ~reserved ~f:(fun active ~sw ->
        run t ~actor ~candidate ~admitted ~capture ~budgets ~id ~active ~sw ~admit))
;;

let submit_pure t ~actor ~run_id ~key ~original ~fresh_source ~kind =
  submit_fresh
    t
    ~actor
    ~run_id
    ~key
    ~original
    ~fresh_source
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
  let%bind root, resources, services =
    match admitted.live_owner with
    | Some (Root_owned { root; resources; services; _ }) ->
      let%map () = Live_resources.check_current resources in
      root, resources, services
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
      let%bind resources =
        Live_resources.prepare
          t.live_resources
          ~handle:admitted.handle
          ~original_actor:actor
          ~prompt
          ~workspace
          ~checkpoint:t.checkpoint
          ~retain:(fun resources ->
            admitted.live_owner <- Some (Prepared { resources; origin_row }))
      in
      let%bind root =
        admit_live_root t ~actor ~capture ~candidate ~ready:admitted ~resources ~active
      in
      admitted.live_owner <- Some (Root_provisional { root; resources; origin_row });
      let%bind services =
        create_live_services t ~root ~resources ~current_actor:(fun () -> actor)
      in
      let%bind () =
        Job_scheduler.attach_developer scheduler ~root ~runner:services.background
      in
      let%map () =
        match services.model with
        | None -> Ok ()
        | Some model -> Job_scheduler.attach_developer_model scheduler ~root ~runner:model
      in
      root, resources, services
  in
  let%bind started = Lifecycle.Active.Preparing.bind_root preparing ~root in
  let%bind current = Root.current root in
  let%bind () = Root.check_effect_now root current in
  let execution_capture = Root.capture root in
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
      ~authorize:t.authorize_native
      ~prepare_output:t.prepare_output
      ~capture:execution_capture
      ~budgets
      ~env:t.env
  in
  admitted.live_owner <- Some (Root_owned { root; resources; services; origin_row });
  Developer_command_execution.Live.run execution ~started
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
  Eio.Mutex.use_rw ~protect:false admitted.live_gate (fun () ->
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
      ~admit)
;;

let submit_command t ~actor ~run_id ~key ~original ~fresh_source =
  let kind = Original.kind original in
  (match kind with
   | Receipt.Kind.Check | Fixture_run ->
     submit_pure t ~actor ~run_id ~key ~original ~fresh_source ~kind
   | Live_run ->
     submit_fresh
       t
       ~actor
       ~run_id
       ~key
       ~original
       ~fresh_source
       ~kind
       ~run:(run_live ~original))
  |> Result.map ~f:Command_view.of_lifecycle
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
    let%bind existing = store (Store.Result_root.find_existing t.store run_id) in
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
      let%bind row =
        store
          (Store.Result_root.find_command_snapshot existing ~principal:principal.id ~key)
      in
      (match row with
       | None ->
         if Operator_authorization.is_current actor
         then Ok None
         else
           Error
             (error
                P.Error.Permission_denied
                "developer command actor is no longer current"
                ~retryable:false)
       | Some row ->
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
       Lifecycle.with_slot t.lifecycle ~run_id ~f:(fun slot ->
         let%bind () = Lifecycle.ensure_startup_recovered t.lifecycle slot in
         observe_once t ~actor ~run_id ~key))
;;

let await_command t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  let%bind () =
    if t.closed
    then Error (error P.Error.Invalid_state "developer host is closed" ~retryable:false)
    else Ok ()
  in
  let%bind view = Lifecycle.await_terminal t.lifecycle ~actor ~run_id ~key in
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

let close_and_wait t =
  t.closed <- true;
  Eio.Cancel.protect (fun () ->
    let open Result.Let_syntax in
    let%bind () = Lifecycle.stop_and_join t.lifecycle in
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
    let%bind () = Root.Registry.close_and_wait t.roots in
    let%bind () = Lifecycle.close_and_wait t.lifecycle in
    let%bind () =
      List.fold_result live_runs ~init:() ~f:(fun () (_, live) ->
        Developer_live_resources.close_and_wait (live_resources live))
    in
    let%bind () = Developer_live_resources.drain_unresolved t.live_resources in
    store (Store.close_and_wait t.store))
;;

module Command = struct
  include Command_view

  let submit = submit_command
  let observe = observe_command
  let await_terminal = await_command
  let cancel_and_wait = cancel_command
end
