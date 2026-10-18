open! Core
module P = Agent_protocol
module Config = Developer_startup_configuration
module Service = Authoring_context_service.Developer
module Sources = Authoring_source_host
module Contracts = Authoring_standard_contracts
module Contract = Chatml.Chatml_compiler_contract
module Access = Authoring_project_access_configuration
module Upload = Authoring_upload_access_configuration
module Ledger = Chatml_authoring_context

let invalid message = P.Error.invalid_request message

let admitted result =
  Result.map_error result ~f:(fun _ -> invalid "invalid developer startup limits")
;;

type selected_row =
  { source : Config.Source.t
  ; principals : Config.Principal_selector.t list
  ; target : Config.Target.t
  ; selected : Service.Selected_contract.t
  ; contract_digest : Chatml_source.Digest.t
  }

let target_variant = function
  | Config.Target.One_off_v1 -> Contract.Target.One_off_v1
  | Developer_tool_v1 -> Developer_tool_v1
  | Tool_v1 _ -> Tool_v1
;;

let selected_row contracts (row : Config.Selection.t) =
  let open Result.Let_syntax in
  let variant = target_variant row.target in
  let%bind entry =
    Contracts.find contracts variant
    |> Result.of_option ~error:(invalid "missing shared developer Standard target")
  in
  let%bind selected =
    match row.target with
    | One_off_v1 -> Service.Selected_contract.of_standard_entry entry
    | Developer_tool_v1 ->
      Service.Selected_contract.of_standard_developer_tool_entry entry
    | Tool_v1 { tool_name; input_schema_json; output_schema_json } ->
      let%bind input_schema =
        Chatmd_shell_spec.Tool_schema.of_string input_schema_json
        |> Result.map_error ~f:(fun _ -> invalid "invalid selected Tool input schema")
      in
      let%bind output_schema =
        Chatmd_shell_spec.Tool_schema.of_string output_schema_json
        |> Result.map_error ~f:(fun _ -> invalid "invalid selected Tool output schema")
      in
      Service.Selected_contract.of_standard_tool_entry
        entry
        ~tool_name
        ~input_schema
        ~output_schema
  in
  let contract_digest =
    Contract.fingerprint
      (Service.Selected_contract.contract selected)
      ~checkpoint:Eio.Fiber.check
  in
  Ok
    { source = row.source
    ; principals = row.principals
    ; target = row.target
    ; selected
    ; contract_digest
    }
;;

let actor_matches
      (actor : Operator_authorization.t)
      (selector : Config.Principal_selector.t)
  =
  let principal = Operator_authorization.principal actor in
  Operator_authorization.is_current actor
  && P.Id.Principal.equal principal.id selector.principal_id
  && String.equal principal.authentication_kind selector.authentication_kind
;;

let root_name source_configuration root =
  List.nth
    source_configuration.Project_source_configuration.external_roots
    (Sources.Root.index root)
;;

let root_matches source_configuration root configured =
  Option.value_map
    (root_name source_configuration root)
    ~default:false
    ~f:(String.equal configured)
;;

let request_matches configured actual =
  match configured, actual with
  | ( Access.Selection.Library { environment = left }
    , Chatml_project.Selection.Library { environment = right } ) ->
    Option.equal String.equal left right
  | ( Entry { name = left_name; environment = left }
    , Chatml_project.Selection.Entry { name = right_name; environment = right } ) ->
    String.equal left_name right_name && Option.equal String.equal left right
  | Library _, Entry _ | Entry _, Library _ -> false
;;

let protocol_selection request =
  let target, environment =
    match request with
    | Access.Selection.Library { environment } ->
      P.Authoring.Context.Selection.Library, environment
    | Entry { name; environment } -> Entry name, environment
  in
  P.Authoring.Context.Selection.create ~target ~environment
  |> Result.map_error ~f:(fun _ -> invalid "invalid developer project request")
;;

let protocol_request_matches configured actual =
  match protocol_selection configured with
  | Ok expected -> P.Authoring.Context.Selection.equal expected actual
  | Error _ -> false
;;

let bound_read configured_project actor root =
  Option.value_map configured_project ~default:false ~f:(fun bound ->
    Authoring_configured_project_policy.read_admitted bound actor ~root)
;;

let document_matches source_configuration actual_origin context current configured =
  let source_path =
    Chatml_source.Id.relative_path
      (Chatml_source.id (Ledger.Document.Inputs.source current))
  in
  match configured, actual_origin, P.Authoring.Context.origin context with
  | ( Config.Source.Supplied_document
    , Service.Target_policy.Document_origin.Supplied_document
    , P.Authoring.Context.Supplied_document _ ) -> true
  | ( Host_document { root; member }
    , Host_document actual_root
    , P.Authoring.Context.Host_document _ ) ->
    root_matches source_configuration actual_root root && String.equal source_path member
  | Supplied_document, _, _ | Host_document _, _, _ -> false
;;

let capture_policy rows ~source_configuration ~configured_project =
  let select actor matches =
    List.find_map rows ~f:(fun row ->
      if matches row && List.exists row.principals ~f:(actor_matches actor)
      then Some row.selected
      else None)
  in
  Service.Target_policy.create
    ~local_project:(fun actor ~root ~project ~selection ->
      match configured_project with
      | None -> None
      | Some bound ->
        let project_name = Source.Portable_path.to_string project in
        select actor (fun row ->
          match row.source with
          | Config.Source.Local_project
              { root = configured_root; project = configured_project; request }
            when root_matches source_configuration root configured_root
                 && String.equal project_name configured_project
                 && request_matches request selection ->
            (match protocol_selection request with
             | Error _ -> false
             | Ok request ->
               Option.value_map
                 (Authoring_configured_project_policy.project_contract
                    bound
                    actor
                    ~root
                    ~project
                    ~selection:request)
                 ~default:false
                 ~f:(phys_equal (Service.Selected_contract.contract row.selected)))
          | _ -> false))
    ~local_source:(fun actor ~root ~member ->
      let member = Source.Portable_path.to_string member in
      select actor (fun row ->
        match row.source with
        | Config.Source.Local_source
            { root = configured_root; member = configured_member } ->
          root_matches source_configuration root configured_root
          && String.equal member configured_member
          && bound_read configured_project actor root
        | _ -> false))
    ~authoring_project:(fun actor ~origin ~context ~current ->
      match origin, P.Authoring.Context.origin context with
      | ( Service.Target_policy.Project_origin.Host_project root
        , P.Authoring.Context.Host_project actual ) ->
        (match configured_project with
         | None -> None
         | Some bound ->
           let project = Source.Portable_path.create actual.project_member in
           Option.bind (Result.ok project) ~f:(fun project ->
             select actor (fun row ->
               match row.source with
               | Config.Source.Authoring_project (Host_project configured) ->
                 root_matches source_configuration root configured.root
                 && String.equal actual.project_member configured.project
                 && protocol_request_matches configured.request actual.selection
                 && Option.value_map
                      (Authoring_configured_project_policy.project_contract
                         bound
                         actor
                         ~root
                         ~project
                         ~selection:actual.selection)
                      ~default:false
                      ~f:(phys_equal (Service.Selected_contract.contract row.selected))
               | _ -> false)))
      | Uploaded_project, Uploaded_project _ ->
        let prepared = Chatml_project_build.prepared (Ledger.Inputs.base current) in
        let environment =
          Chatml_prepared_program.Environment.name
            (Chatml_prepared_program.environment prepared)
        in
        select actor (fun row ->
          match row.source with
          | Config.Source.Authoring_project
              (Uploaded_project { environment = selected_environment }) ->
            String.equal selected_environment environment
            && Chatml_source.Digest.equal
                 row.contract_digest
                 (Chatml_prepared_program.compiler_contract prepared)
          | _ -> false)
      | Host_project _, Uploaded_project _
      | Uploaded_project, Host_project _
      | _, Host_document _
      | _, Supplied_document _ -> None)
    ~document_project:
      (fun
        actor ~origin ~context ~current ~parsed:_ ~declaration ~build:_ ~metadata:_ ->
      let script_id = Prompt.Chat_markdown.Declaration.script_id declaration in
      let authored = Prompt.Chat_markdown.Declaration.project_selection declaration in
      select actor (fun row ->
        match row.source with
        | Config.Source.Document_project
            { origin = configured_origin; script_id = selected_id; authored_project }
          when String.equal script_id selected_id
               && Option.value_map
                    authored
                    ~default:false
                    ~f:(Chatmd_shell_spec.Script_project.equal authored_project)
               && document_matches
                    source_configuration
                    origin
                    context
                    current
                    configured_origin ->
          (match configured_origin with
           | Config.Source.Supplied_document -> true
           | Host_document _ ->
             (match origin with
              | Service.Target_policy.Document_origin.Host_document root ->
                bound_read configured_project actor root
              | Supplied_document -> false))
        | _ -> false))
    ~document_source:
      (fun
        actor ~origin ~context ~current ~parsed:_ ~declaration ~script:_ ->
      let script_id = Prompt.Chat_markdown.Declaration.script_id declaration in
      select actor (fun row ->
        match row.source with
        | Config.Source.Document_source
            { origin = configured_origin; script_id = selected_id }
          when String.equal script_id selected_id
               && document_matches
                    source_configuration
                    origin
                    context
                    current
                    configured_origin ->
          (match configured_origin with
           | Config.Source.Supplied_document -> true
           | Host_document _ ->
             (match origin with
              | Service.Target_policy.Document_origin.Host_document root ->
                bound_read configured_project actor root
              | Supplied_document -> false))
        | _ -> false))
;;

let preflight_origins selections origins ~read_access ~upload_access ~configured_project =
  let host_source = Authoring_installation.Origins.host_source origins in
  let uploaded_project = Authoring_installation.Origins.uploaded_project origins in
  let supplied_document = Authoring_installation.Origins.supplied_document origins in
  List.fold_result selections ~init:() ~f:(fun () (selection : Config.Selection.t) ->
    let available =
      match selection.source with
      | Local_project _ | Authoring_project (Host_project _) ->
        host_source && Option.is_some read_access && Option.is_some configured_project
      | Local_source _ ->
        host_source && Option.is_some read_access && Option.is_some configured_project
      | Authoring_project (Uploaded_project _) ->
        uploaded_project && Option.is_some upload_access
      | Document_project { origin = Supplied_document; _ }
      | Document_source { origin = Supplied_document; _ } -> supplied_document
      | Document_project { origin = Host_document _; _ }
      | Document_source { origin = Host_document _; _ } ->
        host_source && Option.is_some read_access && Option.is_some configured_project
    in
    if available
    then Ok ()
    else Error (invalid "developer selected source origin is not installed"))
;;

let check_access_config ~source_configuration ~read_access ~upload_access =
  let open Result.Let_syntax in
  let%bind () =
    match read_access with
    | None -> Ok ()
    | Some access ->
      Access.validate access ~project_sources:source_configuration
      |> Result.map_error ~f:(fun _ -> invalid "invalid developer read access")
  in
  match upload_access with
  | None -> Ok ()
  | Some access ->
    Upload.validate access
    |> Result.map_error ~f:(fun _ -> invalid "invalid developer upload access")
;;

let current_owner actor owner =
  Operator_authorization.is_current actor
  && P.Id.Principal.equal
       (Operator_authorization.principal actor).id
       owner.P.Execution_owner.principal_id
;;

let command_read actor row =
  let principal = Operator_authorization.principal actor in
  let owner = Agent_store.Developer_command_row.owner row in
  current_owner actor owner
  && Result.is_ok
       (Developer_method_authorization.authorize_principal
          (Agent_store.Developer_command_row.kind row)
          ~principal)
;;

let command_control actor row = command_read actor row

let command_respond actor row =
  command_control actor row
  && P.Principal.has_scope
       (Operator_authorization.principal actor)
       P.Scope.Answer_approvals
;;

let%test_unit
    "configured approval response requires current owner, method and answer scope"
  =
  let ok = function
    | Ok value -> value
    | Error _ -> failwith "invalid configured approval fixture"
  in
  let principal_id = P.Id.Principal.of_string "pri_startup_approval" |> ok in
  let owner =
    P.Execution_owner.developer
      ~host_id:(P.Id.Server.of_string "srv_startup_approval" |> ok)
      ~run_id:(P.Id.Developer_run.of_string "dvr_startup_approval" |> ok)
      ~epoch:0
      ~principal_id
    |> ok
  in
  let owner =
    match owner with
    | P.Execution_owner.Developer owner -> owner
    | Conversation _ -> failwith "expected developer owner"
  in
  let digest =
    Chatml_source.Digest.of_contents "configured approval" ~checkpoint:ignore
  in
  let limits =
    Document_schema.Limits.create
      ~max_bytes:65_536
      ~max_depth:32
      ~max_fields:512
      ~max_nodes:1024
    |> ok
  in
  let row =
    Agent_store.Developer_command_row.create
      ~id:(Agent_store.Developer_command_receipt.Id.of_string "dcr_startup_approval" |> ok)
      ~key:(P.Idempotency_key.of_string "startup-approval" |> ok)
      ~owner
      ~capture_fingerprint:digest
      ~request_fingerprint:digest
      ~kind:Agent_store.Developer_command_receipt.Kind.Check
      ~limits
      ~poll:ignore
    |> ok
  in
  let actor ~id ~scopes =
    let principal =
      P.Principal.create
        ~id
        ~authentication_kind:"local.trusted"
        ~scopes:(P.Scope.Set.of_list scopes)
        ~attributes:[]
      |> ok
    in
    Operator_authorization.trusted_local principal
  in
  let no_answer = actor ~id:principal_id ~scopes:[ P.Scope.Developer_check ] in
  let answer =
    actor ~id:principal_id ~scopes:[ P.Scope.Developer_check; P.Scope.Answer_approvals ]
  in
  let wrong_method = actor ~id:principal_id ~scopes:[ P.Scope.Answer_approvals ] in
  let foreign =
    actor
      ~id:(P.Id.Principal.of_string "pri_startup_foreign" |> ok)
      ~scopes:[ P.Scope.Developer_check; P.Scope.Answer_approvals ]
  in
  let stale =
    Operator_authorization.guarded
      ~principal:(Operator_authorization.principal answer)
      ~is_current:(fun () -> false)
  in
  assert (command_control no_answer row);
  assert (not (command_respond no_answer row));
  assert (command_respond answer row);
  assert (not (command_respond wrong_method row));
  assert (not (command_respond foreign row));
  assert (not (command_respond stale row))
;;

let job_read actor _job =
  Operator_authorization.is_current actor
  && P.Principal.has_scope
       (Operator_authorization.principal actor)
       P.Scope.Developer_live_run
;;

let job_respond actor job =
  job_read actor job
  && P.Principal.has_scope
       (Operator_authorization.principal actor)
       P.Scope.Answer_approvals
;;

let model_target (policy : Session_factory.inference_policy) (model : Config.model) =
  match model with
  | Config.Disabled -> Ok None
  | Provider_default ->
    let preparation_error error =
      invalid
        ("developer model target unavailable: "
         ^ Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
    in
    let open Result.Let_syntax in
    let%bind target =
      policy.capture_developer_default_target () |> Result.map_error ~f:preparation_error
    in
    let%bind resolved =
      policy.resolve_inference_context target |> Result.map_error ~f:preparation_error
    in
    if Inference.Request.Target.equal (Inference_runtime.Context.target resolved) target
    then Ok (Some target)
    else Error (invalid "developer model target changed during startup")
;;

let prepare_output = function
  | Openai.Responses.Tool_output.Output.Text text -> Ok (`String text)
  | output -> Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output)
;;

let bind
      (config : Config.t)
      ~source_host
      ~authoring
      ~source_configuration
      ~read_access
      ~upload_access
      ~configured_project
      ~contracts
      ~inference_policy
      ~initialize_limits
  =
  let open Result.Let_syntax in
  let%bind () = Config.validate config in
  let%bind () =
    if Authoring_installation.issued_by_source_host authoring source_host
    then Ok ()
    else Error (invalid "developer Authoring source host mismatch")
  in
  let%bind origins =
    Authoring_installation.installed_origins authoring
    |> Result.of_option ~error:(invalid "developer Authoring owner is closed")
  in
  let%bind () =
    if
      Option.value_map configured_project ~default:true ~f:(fun bound ->
        Authoring_configured_project_policy.issued_by bound source_host)
    then Ok ()
    else Error (invalid "developer configured-project owner mismatch")
  in
  let%bind () = check_access_config ~source_configuration ~read_access ~upload_access in
  let%bind () =
    preflight_origins
      config.selections
      origins
      ~read_access
      ~upload_access
      ~configured_project
  in
  let%bind rows =
    List.fold_result config.selections ~init:[] ~f:(fun rows row ->
      selected_row contracts row |> Result.map ~f:(fun selected -> selected :: rows))
    |> Result.map ~f:List.rev
  in
  let capture_policy = capture_policy rows ~source_configuration ~configured_project in
  let%bind preset =
    Developer_startup_limits.create
      ~capacity:config.capacity
      ~approvals:config.approvals
      ~repl:config.repl
      ~portable:config.portable
      ~initialize_limits
  in
  let%bind model_target = model_target inference_policy config.model in
  let result_disclosure =
    Developer_result_disclosure.Policy.create ~authorize:(fun actor selection ->
      current_owner actor (Developer_result_disclosure.Selection.owner selection)
      && P.Principal.has_scope
           (Operator_authorization.principal actor)
           P.Scope.Developer_live_run)
  in
  let%bind developer =
    Developer_daemon_configuration.create_with_selected_prompt_profile
      ~capture_policy
      ~capture_limits:(Developer_startup_limits.capture preset)
      ~limits:(Developer_startup_limits.host preset)
      ~result_disclosure
      ~receipt_read:command_read
      ~command_control
      ~publisher_limits:(Developer_startup_limits.publisher preset)
      ~publisher_storage:Agent_store.Developer_result_publisher.Storage.Automatic
      ~model_target
      ~prepare_output
  in
  let developer =
    Option.value_map
      (Developer_startup_limits.approvals preset)
      ~default:developer
      ~f:(fun limits ->
        Developer_daemon_configuration.with_shell_approvals
          developer
          (Developer_daemon_configuration.Shell_approvals.create
             ~limits
             ~job_read
             ~job_respond
           |> Developer_daemon_configuration.Shell_approvals.with_command_respond
                ~authorize:command_respond))
  in
  let developer =
    Option.value_map
      (Developer_startup_limits.repl preset)
      ~default:developer
      ~f:(fun repl -> Developer_daemon_configuration.with_repl developer repl)
  in
  let developer =
    Option.value_map
      (Developer_startup_limits.portable preset)
      ~default:developer
      ~f:(fun portable ->
        Developer_daemon_configuration.with_portable_wire developer portable)
  in
  Ok developer
;;
