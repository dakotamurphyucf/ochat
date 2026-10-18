open! Core
module P = Agent_protocol

module Policy = struct
  type t =
    { wire_limits : P.Developer_common.Limits.t
    ; change_limits : Developer_change_hub.Limits.t
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
    |> Result.map ~f:(fun change_limits -> { wire_limits; change_limits })
  ;;

  let wire_limits t = t.wire_limits
  let change_limits t = t.change_limits
end

module Common = P.Developer_common
module Wire = P.Developer_wire
module Approval = P.Developer_approval
module Original = Developer_command_original
module Store = Agent_store

type t =
  { host : Developer_host.t
  ; authoring : Authoring_installation.t
  ; changes : Developer_change_hub.t
  ; policy : Policy.t
  ; mutable closed : bool
  }

let error code message = P.Error.create code ~message ~retryable:false ()
let limits t = Policy.wire_limits t.policy

let create ~host ~authoring ~changes ~policy =
  if not (Developer_host.uses_authoring host authoring)
  then Error (error Invalid_request "developer endpoint has a different authoring owner")
  else Ok { host; authoring; changes; policy; closed = false }
;;

let available_features t =
  if t.closed
  then []
  else
    [ Common.Features.run; Common.Features.events ]
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
    @
    if selected Common.Features.run && selected Common.Features.events
    then [ "developer.watch"; "developer.unwatch" ]
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

let fresh_source t ~connection ~actor original =
  let open Result.Let_syntax in
  match Original.route original with
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

let wire_kind = function
  | Store.Developer_command_receipt.Kind.Check -> Ok Common.Kind.Check
  | Fixture_run -> Ok Fixture_run
  | Live_run -> Ok Live_run
  | Repl_task_run ->
    Error (error Permission_denied "REPL Task row is not a Developer command")
;;

let wire_receipt ~limits receipt =
  P.Developer_receipt.of_json
    ~limits:(Common.Limits.receipt limits)
    (Store.Developer_command_receipt.to_json receipt)
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

let close_connection_and_wait t connection =
  Developer_change_hub.close_connection_and_wait t.changes connection
;;

let close_and_wait t =
  t.closed <- true;
  Developer_change_hub.close_and_wait t.changes
;;
