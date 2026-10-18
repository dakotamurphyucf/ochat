open! Core
module P = Agent_protocol

module Policy = struct
  type t =
    { wire_limits : P.Developer_common.Limits.t
    ; change_limits : Developer_change_hub.Limits.t
    ; repl_wire_limits : P.Developer_repl_wire.Limits.t option
    }

  let create
        ~wire_limits
        ~max_connections
        ~max_watches_per_connection
        ~max_pending_hints_per_connection
    =
    Developer_change_hub.Limits.create
      ~max_connections
      ~max_watches_per_connection
      ~max_pending_hints_per_connection
    |> Result.map ~f:(fun change_limits ->
      { wire_limits; change_limits; repl_wire_limits = None })
  ;;

  let wire_limits t = t.wire_limits
  let change_limits t = t.change_limits

  let with_repl_wire t repl_wire_limits =
    Ok { t with repl_wire_limits = Some repl_wire_limits }
  ;;

  let repl_wire_limits t = t.repl_wire_limits
end

module Common = P.Developer_common
module Wire = P.Developer_wire
module Repl_wire = P.Developer_repl_wire
module Approval = P.Developer_approval
module Original = Developer_command_original
module Store = Agent_store
module Task_row = Agent_store.Developer_command_row
module Task_receipt = Agent_store.Developer_command_receipt

type t =
  { host : Developer_host.t
  ; authoring : Authoring_installation.t
  ; changes : Developer_change_hub.t
  ; policy : Policy.t
  ; repl_registry : Developer_repl_registry.t option
  ; mutable closed : bool
  }

let error code message = P.Error.create code ~message ~retryable:false ()
let limits t = Policy.wire_limits t.policy

let validate_portable_capture capture =
  Developer_command_wire_receipt.validate_prepared
    (Chatml_developer_capture.prepared capture)
;;

let create ~host ~authoring ~changes ~policy =
  if not (Developer_host.uses_authoring host authoring)
  then Error (error Invalid_request "developer endpoint has a different authoring owner")
  else (
    let repl_registry =
      match Policy.repl_wire_limits policy with
      | None -> Ok None
      | Some limits ->
        Developer_repl_registry.create ~host ~limits |> Result.map ~f:Option.some
    in
    Result.map repl_registry ~f:(fun repl_registry ->
      { host; authoring; changes; policy; repl_registry; closed = false }))
;;

let available_features t =
  if t.closed
  then []
  else
    [ Common.Features.run; Common.Features.events ]
    @ (if Option.is_some t.repl_registry
       then [ Repl_wire.Features.repl; Repl_wire.Features.events ]
       else [])
    @
    if Developer_host.approval_installed t.host then [ Common.Features.approvals ] else []
;;

let methods_for_lease t lease =
  if t.closed
  then []
  else (
    let selected = Connection_context.Authoring_negotiation.selected lease in
    (if selected Common.Features.run
     then
       [ "developer.capabilities"
       ; "developer.submit"
       ; "developer.observe"
       ; "developer.await_terminal"
       ; "developer.cancel_and_wait"
       ]
     else [])
    @ (if Developer_host.approval_installed t.host && selected Common.Features.approvals
       then
         [ "developer.approval.list"
         ; "developer.approval.respond"
         ; "developer.approval.cancel"
         ]
       else [])
    @ (if selected Common.Features.run && selected Common.Features.events
       then [ "developer.watch"; "developer.unwatch" ]
       else [])
    @
    if Option.is_some t.repl_registry && selected Repl_wire.Features.repl
    then
      List.filter Repl_wire.Request.supported_methods ~f:(fun method_ ->
        if
          String.equal method_ "developer.repl.watch"
          || String.equal method_ "developer.repl.unwatch"
        then selected Repl_wire.Features.events
        else true)
    else [])
;;

let available_methods t ~connection =
  match Connection_context.authoring_negotiation connection with
  | Some lease when Connection_context.current_authoring_negotiation connection lease ->
    methods_for_lease t lease
  | Some _ | None -> []
;;

let kind = function
  | Common.Kind.Check -> Store.Developer_command_receipt.Kind.Check
  | Fixture_run -> Fixture_run
  | Live_run -> Live_run
;;

let target = function
  | Common.Target.One_off_v1 -> Chatml.Chatml_compiler_contract.Target.One_off_v1
  | Tool_v1 -> Tool_v1
  | Developer_tool_v1 -> Developer_tool_v1
  | Moderator_v1 -> Moderator_v1
  | Delegated_moderator_v1 -> Delegated_moderator_v1
;;

let route = function
  | Common.Route.Local_project value ->
    Original.Route.Local_project
      { root_index = value.root_index
      ; dependency_root_indices = value.dependency_root_indices
      ; project = value.project
      ; selection = value.selection
      }
  | Local_source value ->
    Original.Route.Local_source { root_index = value.root_index; member = value.member }
  | Authoring_project value ->
    Original.Route.Authoring_project { expected = value.expected }
  | Document_script value ->
    Original.Route.Document_script
      { expected = value.expected; selector = value.selector }
;;

let execution (value : Common.Execution_limits.t) : Chatml_execution.limits =
  { fuel = value.fuel
  ; max_tasks = value.max_tasks
  ; wall_seconds = value.wall_seconds
  ; max_value_bytes = value.max_value_bytes
  ; max_array_items = value.max_array_items
  ; max_depth = value.max_depth
  ; allocation_bytes = value.allocation_bytes
  ; max_calls = value.max_calls
  ; max_invocation_depth = value.max_invocation_depth
  }
;;

let capture_bounds limits =
  let request = Common.Limits.request limits in
  Chatml_developer_capture.Json_bounds.create
    ~max_bytes:(Int.min (Document_schema.Limits.max_bytes request) (1024 * 1024))
    ~max_depth:(Int.min (Document_schema.Limits.max_depth request) 128)
    ~max_nodes:(Int.min (Document_schema.Limits.max_nodes request) 100_000)
  |> Result.map_error ~f:(fun _ ->
    error Invalid_request "developer capture JSON limits are invalid")
;;

let fixture ~limits ~bounds fixture =
  let open Result.Let_syntax in
  let%bind steps =
    List.map (Common.Fixture.steps fixture) ~f:(fun step ->
      let response =
        match Common.Fixture.Step.response step with
        | Return value -> Chatml_developer_capture.Fixture.Response.Return value
        | Fail message -> Fail message
      in
      Chatml_developer_capture.Fixture.Step.create
        ~tool_name:(Common.Fixture.Step.tool_name step)
        ~input_schema:(Common.Fixture.Step.input_schema step)
        ~output_schema:(Common.Fixture.Step.output_schema step)
        ~expected_input:(Common.Fixture.Step.expected_input step)
        ~response
        ~json_bounds:bounds
        ~checkpoint:Eio.Fiber.yield
      |> Result.map_error ~f:(fun _ ->
        error Invalid_request "developer fixture step failed host validation"))
    |> Result.all
  in
  Chatml_developer_capture.Fixture.create
    ~steps
    ~max_steps:(Common.Limits.max_fixture_steps limits)
    ~max_bytes:(Common.Limits.max_fixture_bytes limits)
    ~checkpoint:Eio.Fiber.yield
  |> Result.map_error ~f:(fun _ ->
    error Invalid_request "developer fixture failed host validation")
;;

let original limits submit =
  let open Result.Let_syntax in
  let%bind bounds = capture_bounds limits in
  let%bind fixtures =
    match Common.Submit.fixtures submit with
    | None -> Ok None
    | Some requested -> fixture ~limits ~bounds requested |> Result.map ~f:Option.some
  in
  Original.create
    ~route:(route (Common.Submit.route submit))
    ~kind:(kind (Common.Submit.kind submit))
    ~target:(target (Common.Submit.target submit))
    ~prompt:(Common.Submit.prompt submit)
    ~workspace:(Common.Submit.workspace submit)
    ~entrypoint:(Common.Submit.entrypoint submit)
    ~input:(Common.Submit.input submit)
    ~fixtures
    ~requested_execution:
      (Option.map (Common.Submit.requested_execution submit) ~f:execution)
    ~requested_max_output_bytes:(Common.Submit.requested_max_output_bytes submit)
    ~bounds
    ~checkpoint:Eio.Fiber.yield
;;

let source_root roots index =
  List.find roots ~f:(fun root -> Int.equal (Authoring_source_host.Root.index root) index)
  |> Result.of_option
       ~error:(error Permission_denied "developer source root is not selected")
;;

let fresh_source_route t ~connection ~actor (route : Original.Route.t) =
  let open Result.Let_syntax in
  match route with
  | Local_project { root_index; dependency_root_indices; _ } ->
    let%bind roots =
      Authoring_installation.developer_roots
        t.authoring
        ~actor
        ~checkpoint:Eio.Fiber.yield
    in
    let%bind root = source_root roots root_index in
    let%map dependency_roots =
      List.map dependency_root_indices ~f:(source_root roots) |> Result.all
    in
    Original.Fresh_source.Local_project { root; dependency_roots }
  | Local_source { root_index; _ } ->
    let%bind roots =
      Authoring_installation.developer_roots
        t.authoring
        ~actor
        ~checkpoint:Eio.Fiber.yield
    in
    let%map root = source_root roots root_index in
    Original.Fresh_source.Local_source { root }
  | Authoring_project _ -> Ok (Original.Fresh_source.Authoring_project { connection })
  | Document_script _ -> Ok (Original.Fresh_source.Document_script { connection })
;;

let fresh_source t ~connection ~actor original =
  fresh_source_route t ~connection ~actor (Original.route original)
;;

let wire_kind = function
  | Store.Developer_command_receipt.Kind.Check -> Ok Common.Kind.Check
  | Fixture_run -> Ok Fixture_run
  | Live_run -> Ok Live_run
  | Repl_task_run ->
    Error (error Permission_denied "REPL Task row is not a Developer command")
;;

let wire_receipt ~limits receipt =
  Developer_command_wire_receipt.project receipt ~limits:(Common.Limits.receipt limits)
;;

let wire_view ~limits view =
  let open Result.Let_syntax in
  let row = Developer_host.Command.row view in
  let%bind kind = wire_kind (Developer_host.Command.kind view) in
  let%bind phase =
    match Developer_host.Command.phase view with
    | Store.Developer_command_row.Phase.Reserved -> Ok Common.View.Reserved
    | Waiting_permission -> Ok Waiting_permission
    | Execution_started -> Ok Execution_started
    | Terminal { execution_started; receipt } ->
      let%map receipt = wire_receipt ~limits receipt in
      Common.View.Terminal { execution_started; receipt }
  in
  Common.View.create
    ~limits
    ~command_id:(Developer_host.Command.id view)
    ~owner:(Developer_host.Command.owner view)
    ~key:(Store.Developer_command_row.key row)
    ~kind
    ~phase
    ~request_fingerprint:(Developer_host.Command.request_fingerprint view)
    ~capture_fingerprint:(Developer_host.Command.capture_fingerprint view)
;;

let wire_pending ~limits pending =
  let owner =
    match Developer_approval_owner.Pending.owner pending with
    | Command id -> Approval.Owner.Command id
    | Claimed_job { job_id; attempt } -> Claimed_job { job_id; attempt }
  in
  let execution =
    match Developer_approval_owner.Pending.execution pending with
    | Native_invocation id -> Approval.Execution.Native_invocation id
    | Claimed_model_job { job_id; attempt } -> Claimed_model_job { job_id; attempt }
  in
  Approval.Pending.create
    ~limits
    ~id:(Developer_approval_owner.Pending.id pending)
    ~owner
    ~execution
    ~runtime_id:(Developer_approval_owner.Pending.runtime_id pending)
    ~manifest_sha256:(Developer_approval_owner.Pending.manifest_sha256 pending)
    ~redacted_display:(Developer_approval_owner.Pending.redacted_display pending)
    ~expires_at:(Developer_approval_owner.Pending.expires_at pending)
;;

let wire_task_diagnostic ~limits (diagnostic : Task_receipt.Diagnostic.t) =
  let open Result.Let_syntax in
  let%bind location =
    Option.value_map diagnostic.location ~default:(Ok None) ~f:(fun location ->
      let%map location =
        Repl_wire.Submission.Location.create
          ~limits
          ~source:location.source
          ~digest:location.digest
          ~start_byte:location.start_byte
          ~end_byte:location.end_byte
      in
      Some location)
  in
  Repl_wire.Task_view.Diagnostic.create
    ~limits
    ~code:diagnostic.code
    ~message:diagnostic.message
    ~location
;;

let wire_task_outcome ~limits = function
  | Task_receipt.Task.Succeeded { value_preview } ->
    Ok (Repl_wire.Task_view.Succeeded value_preview)
  | Failed { error; diagnostic } ->
    Result.map
      (Option.value_map diagnostic ~default:(Ok None) ~f:(fun diagnostic ->
         Result.map (wire_task_diagnostic ~limits diagnostic) ~f:Option.some))
      ~f:(fun diagnostic -> Repl_wire.Task_view.Failed { error; diagnostic })
  | Limited { error; diagnostic } ->
    Result.map
      (Option.value_map diagnostic ~default:(Ok None) ~f:(fun diagnostic ->
         Result.map (wire_task_diagnostic ~limits diagnostic) ~f:Option.some))
      ~f:(fun diagnostic -> Repl_wire.Task_view.Limited { error; diagnostic })
  | Interrupted -> Ok Repl_wire.Task_view.Interrupted
  | Cancelled -> Ok Repl_wire.Task_view.Cancelled
;;

let wire_task_view ~limits view =
  let open Result.Let_syntax in
  let row = Developer_host.Repl.Task.row view in
  let%bind phase =
    match Task_row.phase row with
    | Reserved -> Ok Repl_wire.Task_view.Reserved
    | Waiting_permission -> Ok Waiting_permission
    | Execution_started -> Ok Execution_started
    | Terminal { execution_started; receipt } ->
      (match Task_receipt.value receipt with
       | Task outcome ->
         let%map outcome = wire_task_outcome ~limits outcome in
         Repl_wire.Task_view.Terminal { execution_started; outcome }
       | Check _ | Fixture _ | Live _ ->
         Error (error Invalid_state "REPL Task row has another receipt kind"))
  in
  Repl_wire.Task_view.create
    ~limits
    ~command_id:(Task_row.id row)
    ~owner:(Task_row.owner row)
    ~key:(Task_row.key row)
    ~phase
    ~request_fingerprint:(Task_row.request_fingerprint row)
    ~capture_fingerprint:(Task_row.capture_fingerprint row)
;;

let request_current_now t ~connection ~lease ~actor ~method_ =
  (not t.closed)
  && Connection_context.current_authoring_negotiation connection lease
  && Operator_authorization.is_current actor
  && List.mem (methods_for_lease t lease) method_ ~equal:String.equal
;;

let current_request t ~connection ~lease ~actor ~method_ =
  if
    (not (request_current_now t ~connection ~lease ~actor ~method_))
    || Result.is_error (Connection_context.request_actor connection (Some actor))
  then Error (error Permission_denied "developer request authority changed")
  else Ok ()
;;

let authorize_key t ~connection ~lease ~actor ~run_id ~key =
  let open Result.Let_syntax in
  let%bind () = current_request t ~connection ~lease ~actor ~method_:"developer.watch" in
  let%bind view = Developer_host.Command.observe t.host ~actor ~run_id ~key in
  let%bind () =
    match view with
    | Some view -> wire_kind (Developer_host.Command.kind view) |> Result.map ~f:ignore
    | None -> Error (error Permission_denied "developer watch key is not disclosed")
  in
  current_request t ~connection ~lease ~actor ~method_:"developer.watch"
;;

let authorize_change t ~connection ~lease ~actor change =
  let open Result.Let_syntax in
  let%bind () =
    authorize_key
      t
      ~connection
      ~lease
      ~actor
      ~run_id:change.Wire.Change.run_id
      ~key:change.key
  in
  let%bind () =
    match change.kind with
    | Wire.Change.Command_changed | Resync_required -> Ok ()
    | Approval_changed ->
      if
        (not (Developer_host.approval_installed t.host))
        || not
             (Connection_context.Authoring_negotiation.selected
                lease
                Common.Features.approvals)
      then Error (error Permission_denied "developer approval hint is not disclosed")
      else
        Developer_host.Approval.pending t.host ~actor ~run_id:change.run_id
        |> Result.map ~f:ignore
  in
  current_request t ~connection ~lease ~actor ~method_:"developer.watch"
;;

let handle t ?actor ~connection request =
  let open Result.Let_syntax in
  let%bind actor = Connection_context.request_actor connection actor in
  if not (Operator_authorization.is_current actor)
  then Error (error Permission_denied "developer actor is no longer current")
  else (
    let limits = limits t in
    let%bind request =
      Wire.Request.of_method_and_params
        ~limits
        ~method_:(Wire.Request.method_name request)
        ~params:(Wire.Request.params request)
    in
    let method_ = Wire.Request.method_name request in
    let%bind lease =
      Connection_context.authoring_negotiation connection
      |> Result.of_option
           ~error:(error Permission_denied "developer method is not negotiated")
    in
    let%bind () = current_request t ~connection ~lease ~actor ~method_ in
    let%bind response =
      match request with
      | Wire.Request.Capabilities ->
        Common.Capabilities.create
          ~format_version:1
          ~methods:(available_methods t ~connection)
          ~limits
        |> Result.map ~f:(fun value -> Wire.Response.Capabilities value)
      | Submit submit ->
        let%bind original = original limits submit in
        let%bind () = current_request t ~connection ~lease ~actor ~method_ in
        let%bind view =
          Developer_host.Command.submit_resolving
            t.host
            ~actor
            ~run_id:(Common.Submit.run_id submit)
            ~key:(Common.Submit.key submit)
            ~original
            ~request_current:(fun () ->
              request_current_now t ~connection ~lease ~actor ~method_)
            ~validate_capture:validate_portable_capture
            ~resolve_fresh:(fun () ->
              let%bind () = current_request t ~connection ~lease ~actor ~method_ in
              let%bind source = fresh_source t ~connection ~actor original in
              let%map () = current_request t ~connection ~lease ~actor ~method_ in
              source)
        in
        wire_view ~limits view |> Result.map ~f:(fun value -> Wire.Response.Submit value)
      | Observe lookup ->
        let%bind view =
          Developer_host.Command.observe
            t.host
            ~actor
            ~run_id:lookup.run_id
            ~key:lookup.key
        in
        let%map view =
          Option.value_map view ~default:(Ok None) ~f:(fun view ->
            Result.map (wire_view ~limits view) ~f:Option.some)
        in
        Wire.Response.Observe view
      | Await_terminal lookup ->
        let%bind view =
          Developer_host.Command.await_terminal
            t.host
            ~actor
            ~run_id:lookup.run_id
            ~key:lookup.key
        in
        wire_view ~limits view
        |> Result.map ~f:(fun value -> Wire.Response.Await_terminal value)
      | Cancel_and_wait lookup ->
        let%bind view =
          Developer_host.Command.cancel_and_wait
            t.host
            ~actor
            ~run_id:lookup.run_id
            ~key:lookup.key
        in
        wire_view ~limits view
        |> Result.map ~f:(fun value -> Wire.Response.Cancel_and_wait value)
      | Approval_list run_id ->
        let%bind pending = Developer_host.Approval.pending t.host ~actor ~run_id in
        let%bind pending = List.map pending ~f:(wire_pending ~limits) |> Result.all in
        Approval.Page.create ~limits pending
        |> Result.map ~f:(fun value -> Wire.Response.Approval_list value)
      | Approval_respond response ->
        let decision =
          match response.decision with
          | Approval.Decision.Approve_once -> Developer_host.Approval.Approve_once
          | Deny reason -> Deny reason
        in
        let%map () =
          Developer_host.Approval.respond
            t.host
            ~actor
            ~run_id:response.run_id
            ~id:response.id
            decision
        in
        Wire.Response.Approval_respond
      | Approval_cancel (run_id, id) ->
        let%map () = Developer_host.Approval.cancel t.host ~actor ~run_id ~id in
        Wire.Response.Approval_cancel
      | Watch lookup ->
        let%map () =
          Developer_change_hub.watch
            t.changes
            ~connection
            ~actor
            ~run_id:lookup.run_id
            ~key:lookup.key
            ~authorize_key:(fun () ->
              authorize_key
                t
                ~connection
                ~lease
                ~actor
                ~run_id:lookup.run_id
                ~key:lookup.key)
            ~authorize_change:(authorize_change t ~connection ~lease ~actor)
        in
        Wire.Response.Watch
      | Unwatch lookup ->
        let%map () =
          Developer_change_hub.unwatch
            t.changes
            ~connection
            ~run_id:lookup.run_id
            ~key:lookup.key
        in
        Wire.Response.Unwatch
    in
    let%bind () = current_request t ~connection ~lease ~actor ~method_ in
    if String.equal (Wire.Response.method_name response) method_
    then Ok response
    else Error (error Invalid_state "developer response method changed"))
;;

let encode_response t response = Wire.Response.to_json response ~limits:(limits t)

let repl_installation t =
  match t.repl_registry, Policy.repl_wire_limits t.policy with
  | Some registry, Some limits -> Ok (registry, limits)
  | None, None | Some _, None | None, Some _ ->
    Error (error Invalid_state "REPL wire is not installed")
;;

let repl_limits t = Result.map (repl_installation t) ~f:snd
let repl_instance registry wire = Developer_repl_registry.resolve_instance registry wire

let repl_current registry current =
  let open Result.Let_syntax in
  let%bind instance = repl_instance registry (Repl_wire.Current_epoch.instance current) in
  let%map epoch =
    Developer_repl_registry.resolve_epoch
      registry
      ~instance
      (Repl_wire.Current_epoch.epoch current)
  in
  instance, epoch
;;

let authorize_repl_watch t ~connection ~lease ~actor key =
  let open Result.Let_syntax in
  let%bind () =
    current_request t ~connection ~lease ~actor ~method_:"developer.repl.watch"
  in
  let%bind registry, _ = repl_installation t in
  let%bind () =
    match key with
    | Repl_wire.Watch_key.Repl lookup ->
      let%bind instance = repl_instance registry (Repl_wire.Lookup.instance lookup) in
      let%bind observed =
        Developer_host.Repl.observe
          t.host
          ~actor
          ~instance
          ~key:(Repl_wire.Lookup.key lookup)
      in
      if Option.is_some observed
      then Ok ()
      else Error (error Permission_denied "REPL watch key is not disclosed")
    | Task lookup ->
      let%bind observed =
        Developer_host.Repl.Task.observe
          t.host
          ~actor
          ~run_id:lookup.run_id
          ~key:lookup.key
      in
      if Option.is_some observed
      then Ok ()
      else Error (error Permission_denied "Task watch key is not disclosed")
  in
  current_request t ~connection ~lease ~actor ~method_:"developer.repl.watch"
;;

let handle_repl t ?actor ~connection request =
  let open Result.Let_syntax in
  let%bind registry, limits = repl_installation t in
  let%bind actor = Connection_context.request_actor connection actor in
  let%bind request =
    Repl_wire.Request.of_method_and_params
      ~limits
      ~method_:(Repl_wire.Request.method_name request)
      ~params:(Repl_wire.Request.params request)
  in
  let method_ = Repl_wire.Request.method_name request in
  let%bind lease =
    Connection_context.authoring_negotiation connection
    |> Result.of_option ~error:(error Permission_denied "REPL method is not negotiated")
  in
  let%bind () = current_request t ~connection ~lease ~actor ~method_ in
  let%bind response =
    match request with
    | Capabilities ->
      Repl_wire.Capabilities.create
        ~methods:
          (List.filter (available_methods t ~connection) ~f:(fun method_ ->
             String.is_prefix method_ ~prefix:"developer.repl."))
        ~limits
      |> Result.map ~f:(fun value -> Repl_wire.Response.Capabilities value)
    | Create create ->
      let selected_route = route (Repl_wire.Create.route create) in
      let%bind original =
        Developer_host.Repl.Original.create
          ~route:selected_route
          ~target:(target (Repl_wire.Create.target create))
          ~prompt:(Repl_wire.Create.prompt create)
          ~workspace:(Repl_wire.Create.workspace create)
          ~requested_execution:
            (Option.map (Repl_wire.Create.requested_execution create) ~f:execution)
          ~requested_max_output_bytes:(Repl_wire.Create.requested_max_output_bytes create)
          ~checkpoint:Eio.Fiber.yield
      in
      let%bind view =
        Developer_host.Repl.create_resolving
          t.host
          ~actor
          ~run_id:(Repl_wire.Create.run_id create)
          ~key:(Repl_wire.Create.key create)
          ~original
          ~request_current:(fun () ->
            request_current_now t ~connection ~lease ~actor ~method_)
          ~resolve_fresh:(fun () ->
            let%bind () = current_request t ~connection ~lease ~actor ~method_ in
            let%bind source = fresh_source_route t ~connection ~actor selected_route in
            let%map () = current_request t ~connection ~lease ~actor ~method_ in
            source)
      in
      Developer_repl_registry.project_view registry view
      |> Result.map ~f:(fun value -> Repl_wire.Response.Create value)
    | Submit submit ->
      let%bind instance = repl_instance registry (Repl_wire.Submit.instance submit) in
      let%bind epoch =
        Developer_repl_registry.resolve_epoch
          registry
          ~instance
          (Repl_wire.Submit.epoch submit)
      in
      let%bind expected_revision =
        Developer_repl_registry.resolve_revision
          registry
          ~instance
          ~epoch
          (Repl_wire.Submit.expected_revision submit)
      in
      let%bind submission =
        Developer_host.Repl.submit_with_request
          ~request_current:(fun () ->
            request_current_now t ~connection ~lease ~actor ~method_)
          t.host
          ~actor
          ~instance
          ~epoch
          ~expected_revision
          ~key:(Repl_wire.Submit.key submit)
          ~source:(Repl_wire.Submit.source submit)
      in
      Developer_repl_registry.project_submission registry ~instance submission
      |> Result.map ~f:(fun value -> Repl_wire.Response.Submit value)
    | Observe lookup ->
      let%bind instance = repl_instance registry (Repl_wire.Lookup.instance lookup) in
      let%bind submission =
        Developer_host.Repl.observe
          t.host
          ~actor
          ~instance
          ~key:(Repl_wire.Lookup.key lookup)
      in
      let%map submission =
        Option.value_map submission ~default:(Ok None) ~f:(fun submission ->
          Result.map
            (Developer_repl_registry.project_submission registry ~instance submission)
            ~f:Option.some)
      in
      Repl_wire.Response.Observe submission
    | Cancel_submission lookup ->
      let%bind instance = repl_instance registry (Repl_wire.Lookup.instance lookup) in
      let%bind submission =
        Developer_host.Repl.cancel_and_wait_with_request
          ~request_current:(fun () ->
            request_current_now t ~connection ~lease ~actor ~method_)
          t.host
          ~actor
          ~instance
          ~key:(Repl_wire.Lookup.key lookup)
      in
      Developer_repl_registry.project_submission registry ~instance submission
      |> Result.map ~f:(fun value -> Repl_wire.Response.Cancel_submission value)
    | Inspect_type inspect ->
      let current = Repl_wire.Inspect.current inspect in
      let%bind instance, epoch = repl_current registry current in
      let selector =
        match Repl_wire.Inspect.selector inspect with
        | Repl_wire.Type_selector.Expression ->
          Developer_host.Repl.Type_selector.Expression
        | Binding_index index -> Binding_index index
      in
      let%bind inspection =
        Developer_host.Repl.inspect_type
          t.host
          ~actor
          ~instance
          ~epoch
          ~key:(Repl_wire.Current_epoch.key current)
          ~statement_index:(Repl_wire.Inspect.statement_index inspect)
          ~selector
          ~max_bytes:(Repl_wire.Inspect.max_bytes inspect)
      in
      Developer_repl_registry.project_type_inspection registry ~instance ~epoch inspection
      |> Result.map ~f:(fun value -> Repl_wire.Response.Inspect_type value)
    | Task_references_page requested ->
      let current = Repl_wire.Reference_page_request.current requested in
      let%bind instance, epoch = repl_current registry current in
      let%bind after =
        Option.value_map
          (Repl_wire.Reference_page_request.after requested)
          ~default:(Ok None)
          ~f:(fun value ->
            Result.map Developer_host.Repl.Task_reference.(of_string value) ~f:Option.some)
      in
      let%bind page =
        Developer_host.Repl.task_references_page
          t.host
          ~actor
          ~instance
          ~epoch
          ~key:(Repl_wire.Current_epoch.key current)
          ~after
          ~limit:(Repl_wire.Reference_page_request.limit requested)
      in
      Developer_repl_registry.project_reference_page registry page
      |> Result.map ~f:(fun value -> Repl_wire.Response.Task_references_page value)
    | Task_submit requested ->
      let%bind reference =
        Developer_host.Repl.Task_reference.of_string
          (Repl_wire.Task_submit.reference requested)
      in
      let%bind original =
        Developer_host.Repl.Task.Original.create
          ~reference
          ~requested_execution:
            (Option.map
               (Repl_wire.Task_submit.requested_execution requested)
               ~f:execution)
          ~requested_max_output_bytes:
            (Repl_wire.Task_submit.requested_max_output_bytes requested)
          ~checkpoint:Eio.Fiber.yield
      in
      let%bind view =
        Developer_host.Repl.Task.submit_resolving
          t.host
          ~actor
          ~key:(Repl_wire.Task_submit.key requested)
          ~original
          ~request_current:(fun () ->
            request_current_now t ~connection ~lease ~actor ~method_)
          ~resolve_fresh_epoch:(fun () ->
            let%bind () = current_request t ~connection ~lease ~actor ~method_ in
            let%bind wire_instance, wire_epoch =
              Repl_wire.Task_submit.fresh_epoch requested
              |> Result.of_option
                   ~error:(error Lease_stale "fresh Task epoch is required")
            in
            let%bind instance = repl_instance registry wire_instance in
            let%map epoch =
              Developer_repl_registry.resolve_epoch registry ~instance wire_epoch
            in
            instance, epoch)
      in
      wire_task_view ~limits view
      |> Result.map ~f:(fun value -> Repl_wire.Response.Task_submit value)
    | Task_observe lookup ->
      let%bind view =
        Developer_host.Repl.Task.observe
          t.host
          ~actor
          ~run_id:lookup.run_id
          ~key:lookup.key
      in
      let%map view =
        Option.value_map view ~default:(Ok None) ~f:(fun view ->
          Result.map (wire_task_view ~limits view) ~f:Option.some)
      in
      Repl_wire.Response.Task_observe view
    | Task_await_terminal lookup ->
      let%bind view =
        Developer_host.Repl.Task.await_terminal
          t.host
          ~actor
          ~run_id:lookup.run_id
          ~key:lookup.key
      in
      wire_task_view ~limits view
      |> Result.map ~f:(fun value -> Repl_wire.Response.Task_await_terminal value)
    | Task_cancel_and_wait lookup ->
      let%bind view =
        Developer_host.Repl.Task.cancel_and_wait_with_request
          ~request_current:(fun () ->
            request_current_now t ~connection ~lease ~actor ~method_)
          t.host
          ~actor
          ~run_id:lookup.run_id
          ~key:lookup.key
      in
      wire_task_view ~limits view
      |> Result.map ~f:(fun value -> Repl_wire.Response.Task_cancel_and_wait value)
    | Reset_and_wait current ->
      let%bind instance, expected_epoch = repl_current registry current in
      let%bind view =
        Developer_host.Repl.reset_and_wait_with_request
          ~request_current:(fun () ->
            request_current_now t ~connection ~lease ~actor ~method_)
          t.host
          ~actor
          ~instance
          ~expected_epoch
          ~key:(Repl_wire.Current_epoch.key current)
      in
      Developer_repl_registry.project_view registry view
      |> Result.map ~f:(fun value -> Repl_wire.Response.Reset_and_wait value)
    | Close_and_wait lookup ->
      let%bind instance = repl_instance registry (Repl_wire.Lookup.instance lookup) in
      let%map () =
        Developer_host.Repl.close_and_wait_with_request
          ~request_current:(fun () ->
            request_current_now t ~connection ~lease ~actor ~method_)
          t.host
          ~actor
          ~instance
          ~key:(Repl_wire.Lookup.key lookup)
      in
      Repl_wire.Response.Close_and_wait
    | Watch watched ->
      let authorize_key () = authorize_repl_watch t ~connection ~lease ~actor watched in
      let authorize_change change =
        authorize_repl_watch t ~connection ~lease ~actor (Repl_wire.Change.key change)
      in
      let%map () =
        match watched with
        | Repl lookup ->
          let%bind instance = repl_instance registry (Repl_wire.Lookup.instance lookup) in
          Developer_change_hub.watch_repl
            t.changes
            ~connection
            ~actor
            ~instance
            ~wire_instance:(Repl_wire.Lookup.instance lookup)
            ~key:(Repl_wire.Lookup.key lookup)
            ~authorize_key
            ~authorize_change
        | Task lookup ->
          Developer_change_hub.watch_task
            t.changes
            ~connection
            ~actor
            ~run_id:lookup.run_id
            ~key:lookup.key
            ~authorize_key
            ~authorize_change
      in
      Repl_wire.Response.Watch
    | Unwatch watched ->
      let%map () =
        match watched with
        | Repl lookup ->
          Developer_change_hub.unwatch_repl
            t.changes
            ~connection
            ~wire_instance:(Repl_wire.Lookup.instance lookup)
            ~key:(Repl_wire.Lookup.key lookup)
        | Task lookup ->
          Developer_change_hub.unwatch_task
            t.changes
            ~connection
            ~run_id:lookup.run_id
            ~key:lookup.key
      in
      Repl_wire.Response.Unwatch
  in
  let%bind () = current_request t ~connection ~lease ~actor ~method_ in
  if String.equal (Repl_wire.Response.method_name response) method_
  then Ok response
  else Error (error Invalid_state "REPL response method changed")
;;

let encode_repl_response t response =
  let open Result.Let_syntax in
  let%bind _, limits = repl_installation t in
  Repl_wire.Response.to_json response ~limits
;;

let close_connection_and_wait t connection =
  Developer_change_hub.close_connection_and_wait t.changes connection
;;

let close_and_wait t =
  t.closed <- true;
  Developer_change_hub.close_and_wait t.changes
;;
