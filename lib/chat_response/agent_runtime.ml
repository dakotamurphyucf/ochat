open! Core
module CM = Prompt.Chat_markdown
module D = Chatmd_shell_spec.Diagnostic
module MC = Chatmd_shell_spec.Manifest_compiler
module S = Chatmd_shell_spec.Shell_spec

type diagnostic =
  { code : string
  ; message : string
  ; source : Chatmd_shell_spec.Source_ref.t option
  }
[@@deriving sexp, compare, equal]

type native_registration = Native_tool_pool.Registration.t =
  { implementation : Ochat_function.t
  ; implementation_revision : string
  ; result_contract : Tool_capability.result_contract
  ; authoring_metadata : Chatmd_shell_spec.Authoring_metadata.t option
  }

type t =
  { functions : Ochat_function.t list
  ; capabilities : (Tool_capability.t, Tool_capability.error) result Lazy.t
  ; classifications : (string * Tool_execution_event.agent_page_kind) list
  ; shell_tool_names : String.Set.t
  ; shell_registry : Shell_runtime.Registry.t option
  ; shell_manifest : Chatmd_shell_spec.Manifest.t option
  ; shell_admin_policy : Shell_runtime.Admin_policy.t option
  ; shell_security_status : Shell_runtime.Manifest_security.status option
  ; moderator_shell_runtime : string option
  ; legacy_moderator : Moderator_manager.Registry.artifact option
  }

type shell_inspection =
  { manifest : Chatmd_shell_spec.Manifest.t
  ; live_runtimes : S.t list
  ; administrative_policy : Shell_runtime.Admin_policy.t
  ; security_status : Shell_runtime.Manifest_security.status
  }

type declarations =
  { runtimes : S.t list
  ; shell_tools : Chatmd_shell_spec.Shell_tool_spec.t list
  ; scripts : Chatmd_shell_spec.Chatmd_script_spec.t list
  ; legacy_tools : MC.legacy_tool list
  ; moderator_runtime : MC.moderator_runtime option
  ; tools : CM.tool list
  }

let diagnostic code message = { code; message; source = None }

type resource_inspection =
  { manifest : Chatmd_shell_spec.Manifest.t
  ; live_runtimes : Chatmd_shell_spec.Shell_spec.t list
  ; administrative_policy : Shell_runtime.Admin_policy.t
  }

type prepared_shell_inspection =
  { manifest : Chatmd_shell_spec.Manifest.t
  ; live_runtimes : Chatmd_shell_spec.Shell_spec.t list
  ; administrative_policy : Shell_runtime.Admin_policy.t
  ; security_check :
      ( Shell_runtime.Manifest_security.status
        , Shell_runtime.Manifest_security.error list )
        Result.t
  }

let diagnostic_to_string diagnostic =
  let prefix =
    Option.value_map diagnostic.source ~default:"" ~f:(fun source -> source.file ^ ": ")
  in
  sprintf "%s[%s] %s" prefix diagnostic.code diagnostic.message
;;

let default_home env =
  match Sys.getenv "HOME" with
  | Some path -> Eio.Path.(Eio.Stdenv.fs env / path)
  | None -> Eio.Stdenv.cwd env
;;

let empty_declarations =
  { runtimes = []
  ; shell_tools = []
  ; scripts = []
  ; legacy_tools = []
  ; moderator_runtime = None
  ; tools = []
  }
;;

let legacy_tool (tool : CM.custom_tool) : MC.legacy_tool =
  { name = tool.name
  ; description = tool.description
  ; command = tool.command
  ; source = tool.source
  }
;;

let collect declarations = function
  | CM.Shell_runtime runtime ->
    { declarations with runtimes = runtime :: declarations.runtimes }
  | Moderator_runtime moderator ->
    { declarations with moderator_runtime = Some moderator }
  | Shell_script script -> { declarations with scripts = script :: declarations.scripts }
  | Tool (Shell tool as declaration) ->
    { declarations with
      shell_tools = tool :: declarations.shell_tools
    ; tools = declaration :: declarations.tools
    }
  | Tool (Custom tool as declaration) ->
    { declarations with
      legacy_tools = legacy_tool tool :: declarations.legacy_tools
    ; tools = declaration :: declarations.tools
    }
  | Tool declaration -> { declarations with tools = declaration :: declarations.tools }
  | Msg _
  | Developer _
  | System _
  | User _
  | Assistant _
  | Tool_call _
  | Tool_response _
  | Config _
  | Reasoning _
  | Script _
  | Extension_script _
  | Authoring_help _
  | Authoring_context _ -> declarations
;;

let declarations elements =
  List.fold elements ~init:empty_declarations ~f:collect
  |> fun declarations ->
  { runtimes = List.rev declarations.runtimes
  ; shell_tools = List.rev declarations.shell_tools
  ; scripts = List.rev declarations.scripts
  ; legacy_tools = List.rev declarations.legacy_tools
  ; moderator_runtime = declarations.moderator_runtime
  ; tools = List.rev declarations.tools
  }
;;

let validate_tool_paths host ~prompt_elements =
  let errors =
    (declarations prompt_elements).tools
    |> List.filter_map ~f:(fun declaration ->
      match Tool.validate_read_roots host declaration with
      | Ok () -> None
      | Error error -> Some (diagnostic error.code error.message))
  in
  if List.is_empty errors then Ok () else Error errors
;;

let platform () =
  match Core_unix.Utsname.sysname (Core_unix.uname ()) with
  | "Darwin" -> S.Macos
  | "Linux" -> S.Linux
  | _ -> S.Any
;;

let declaration_sources declarations =
  List.map declarations.runtimes ~f:(fun runtime -> runtime.S.source)
  @ List.map declarations.shell_tools ~f:(fun tool -> tool.source)
  @ List.map declarations.legacy_tools ~f:(fun tool -> tool.source)
  @ List.filter_map declarations.tools ~f:(function
    | CM.Read_file specification -> Some specification.source
    | Extension specification -> Some specification.source_ref
    | Builtin _ | Custom _ | Shell _ | Agent _ | Mcp _ | Inherited _ | Persistent_agent _
      -> None)
;;

let add_source_dir ~env (source_dirs, errors) source =
  let path =
    Eio.Path.(Eio.Stdenv.fs env / source.Chatmd_shell_spec.Source_ref.source_dir)
  in
  match Map.add source_dirs ~key:source.file ~data:path with
  | `Ok source_dirs -> source_dirs, errors
  | `Duplicate ->
    let existing = Map.find_exn source_dirs source.file in
    if String.equal (Eio.Path.native_exn existing) (Eio.Path.native_exn path)
    then source_dirs, errors
    else
      ( source_dirs
      , diagnostic
          "shell.ambiguous_source_directory"
          ("source file resolves to multiple directories: " ^ source.file)
        :: errors )
;;

let source_directories ~env declarations =
  declaration_sources declarations
  |> List.fold ~init:(String.Map.empty, []) ~f:(add_source_dir ~env)
  |> fun (source_dirs, errors) ->
  if List.is_empty errors then Ok source_dirs else Error (List.rev errors)
;;

let host
      ~env
      ~workspace
      ~tool_dir
      ~prompt_dir
      ~session_dir
      ~cache_dir
      ~home
      ~session_id
      ~prompt_elements
  =
  let declarations = declarations prompt_elements in
  source_directories ~env declarations
  |> Result.map ~f:(fun source_dirs ->
    Shell_runtime.Host.
      { env
      ; workspace
      ; tool_dir
      ; prompt_dir
      ; session_dir
      ; cache_dir
      ; home
      ; source_dirs
      ; process_environment = Core_unix.environment ()
      ; session_id
      })
;;

let diagnostic_of_manifest (diagnostic : D.t) =
  { code = diagnostic.code; message = diagnostic.message; source = diagnostic.source }
;;

let diagnostic_of_authorizer (error : Shell_runtime.Manifest_authorizer.error) =
  diagnostic error.code error.message
;;

let diagnostic_of_registry (error : Shell_runtime.Registry.error) =
  diagnostic error.code error.message
;;

let compile ~platform declarations =
  MC.compile_with_material
    { runtimes = declarations.runtimes
    ; tools = declarations.shell_tools
    ; scripts = declarations.scripts
    ; legacy_tools = declarations.legacy_tools
    ; moderator_runtime = declarations.moderator_runtime
    ; platform
    ; supported_features = Chatmd_shell_spec.Feature.phase4
    }
  |> Result.map_error ~f:(List.map ~f:diagnostic_of_manifest)
;;

let prepare_shell_manifest ~platform ~prompt_elements =
  compile ~platform (declarations prompt_elements)
;;

let inspect_material ~env ~platform ~prompt_elements =
  let open Result.Let_syntax in
  let%bind administrative_policy =
    Shell_runtime.Admin_policy_loader.load_from_environment ~env
    |> Result.map_error ~f:(fun error -> [ diagnostic error.code error.message ])
  in
  let%bind manifest, material = prepare_shell_manifest ~platform ~prompt_elements in
  let%bind live_runtimes =
    MC.effective_runtimes material ~manifest
    |> Result.map_error ~f:(List.map ~f:diagnostic_of_manifest)
  in
  let%bind () =
    Shell_runtime.Admin_policy.evaluate
      administrative_policy
      ~manifest
      ~runtimes:live_runtimes
    |> Result.map_error ~f:(fun violations ->
      List.map violations ~f:(fun violation ->
        diagnostic
          violation.Shell_runtime.Admin_policy.code
          (sprintf
             "requested=%s; ceiling=%s; policy=%s; remediation=%s"
             violation.requested
             violation.ceiling
             violation.policy_source
             violation.remediation)))
  in
  Ok (({ manifest; live_runtimes; administrative_policy } : resource_inspection), material)
;;

let inspect_resources ~env ~platform ~prompt_elements =
  inspect_material ~env ~platform ~prompt_elements |> Result.map ~f:fst
;;

let inspect_shell ~env ~platform ~prompt_elements =
  let open Result.Let_syntax in
  let%bind (inspection : resource_inspection) =
    inspect_resources ~env ~platform ~prompt_elements
  in
  if
    List.exists (declarations prompt_elements).scripts ~f:(fun script ->
      Option.is_some script.Chatmd_shell_spec.Chatmd_script_spec.project_selection)
  then
    Error
      [ diagnostic
          "shell.prepared_inspection_required"
          "Configured hooks require captured program inspection for executable \
           authorization"
      ]
  else (
    let%map security_status =
      Shell_runtime.Manifest_security.verify
        ~env
        ~admin_policy:inspection.administrative_policy
        ~manifest:inspection.manifest
      |> Result.map_error
           ~f:
             (List.map ~f:(fun (error : Shell_runtime.Manifest_security.error) ->
                diagnostic error.code error.message))
    in
    ({ manifest = inspection.manifest
     ; live_runtimes = inspection.live_runtimes
     ; administrative_policy = inspection.administrative_policy
     ; security_status
     }
     : shell_inspection))
;;

let inspect_prepared_shell
      ?startup
      ?programs
      ~env
      ~limits
      ~checkpoint
      ~platform
      ~prompt_elements
      ()
  =
  Startup_compilation.with_context
    ?startup
    ~limits
    ~env
    ~checkpoint
    ~map_error:(fun error -> [ diagnostic error.Chatml_compilation.code error.message ])
    ~f:(fun startup ->
      Startup_compilation.within
        startup
        ~map_error:(fun error ->
          [ diagnostic error.Chatml_compilation.code error.message ])
        ~f:(fun () ->
          let open Result.Let_syntax in
          let%bind (inspection : resource_inspection), material =
            inspect_material ~env ~platform ~prompt_elements
          in
          let manifest = inspection.manifest in
          let%map prepared =
            Shell_preparation.prepare
              ~startup
              ?programs
              ~env
              ~limits
              ~checkpoint
              ~admin_policy:inspection.administrative_policy
              ~manifest
              material
            |> Result.map_error ~f:(List.map ~f:diagnostic_of_registry)
          in
          let manifest =
            Shell_runtime.Registry.prepared_manifest (Shell_preparation.registry prepared)
          in
          let security_check =
            Shell_runtime.Manifest_security.verify
              ~env
              ~admin_policy:inspection.administrative_policy
              ~manifest
          in
          ({ manifest
           ; live_runtimes = inspection.live_runtimes
           ; administrative_policy = inspection.administrative_policy
           ; security_check
           }
           : prepared_shell_inspection)))
    ()
;;

let instantiate
      ~startup
      ~programs
      ~compilation_limits
      ~checkpoint
      ~sw
      ~host
      ~admin_policy
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~model_completion
      ~extension_snapshots
      ~persist_extension_snapshots
      ~preflight
      manifest
      material
  =
  let open Result.Let_syntax in
  let%bind prepared =
    Shell_preparation.prepare
      ~startup
      ?programs
      ~limits:compilation_limits
      ~checkpoint
      ~env:host.Shell_runtime.Host.env
      ~manifest
      ~admin_policy
      material
    |> Result.map_error ~f:(List.map ~f:diagnostic_of_registry)
  in
  let manifest =
    Shell_runtime.Registry.prepared_manifest (Shell_preparation.registry prepared)
  in
  let%bind security_status =
    Shell_runtime.Manifest_security.verify
      ~env:host.Shell_runtime.Host.env
      ~admin_policy
      ~manifest
    |> Result.map_error
         ~f:
           (List.map ~f:(fun (error : Shell_runtime.Manifest_security.error) ->
              diagnostic error.code error.message))
  in
  let%bind grant =
    Shell_runtime.Manifest_authorizer.authorize manifest_authorizer manifest
    |> Result.map_error ~f:(fun error -> [ diagnostic_of_authorizer error ])
  in
  (* The registry owns activation and calls preflight once. Retain its caller's
     source-bearing diagnostics without widening the native registry error type.
     Exceptions bypass this expected-error transport and propagate unchanged. *)
  let preflight_diagnostics = ref None in
  Shell_runtime.Registry.instantiate_prepared_after
    ~sw
    ~host
    ~grant
    ~approval_provider
    ~approval_store
    ~model_completion
    ~extension_snapshots
    ?persist_extension_snapshots
    ~preflight:(fun staged ->
      preflight
        (Native_tool_pool.Shell_view.Staged staged)
        ~shell_preparation:(Some prepared)
      |> Result.map_error ~f:(fun diagnostics ->
        preflight_diagnostics := Some diagnostics;
        List.map diagnostics ~f:(fun (diagnostic : diagnostic) ->
          Shell_runtime.Registry.
            { code = diagnostic.code; message = diagnostic.message; runtime_id = None })))
    (Shell_preparation.registry prepared)
  |> Result.map_error ~f:(fun errors ->
    match !preflight_diagnostics with
    | Some diagnostics -> diagnostics
    | None -> List.map errors ~f:diagnostic_of_registry)
  |> Result.map ~f:(fun (registry, admitted) -> registry, security_status, admitted)
;;

let shell_registry
      ~startup
      ~programs
      ~compilation_limits
      ~checkpoint
      ~sw
      ~host
      ~platform
      ~admin_policy
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~model_completion
      ~extension_snapshots
      ~persist_extension_snapshots
      ~preflight
      declarations
  =
  if
    List.is_empty declarations.runtimes
    && List.is_empty declarations.shell_tools
    && List.is_empty declarations.legacy_tools
    && Option.is_none declarations.moderator_runtime
  then
    preflight Native_tool_pool.Shell_view.Absent ~shell_preparation:None
    |> Result.map ~f:(fun admitted -> None, None, None, admitted)
  else
    Result.bind
      (Startup_compilation.within
         startup
         ~map_error:(fun e -> [ diagnostic e.Chatml_compilation.code e.message ])
         ~f:(fun () -> compile ~platform declarations))
      ~f:(fun (manifest, material) ->
        instantiate
          ~startup
          ~programs
          ~compilation_limits
          ~checkpoint
          ~sw
          ~host
          ~admin_policy
          ~manifest_authorizer
          ~approval_provider
          ~approval_store
          ~model_completion
          ~extension_snapshots
          ~persist_extension_snapshots
          ~preflight
          manifest
          material
        |> Result.map ~f:(fun (registry, security_status, admitted) ->
          ( Some registry
          , Some (Shell_runtime.Registry.manifest registry)
          , Some security_status
          , admitted )))
;;

let model_completion ~ctx ~agent:_ ~model =
  let inference = Ctx.inference_execution ctx in
  let selected_model =
    Option.value
      model
      ~default:
        (Inference.Request.Target.model
           (Inference_runtime.Context.target
              (Inference_client.Execution.context inference)))
  in
  let complete ~prompt =
    Eio.Switch.run (fun sw ->
      Inference_client.Execution.complete_text
        inference
        ~sw
        ?model
        ~settings:[]
        ~messages:
          [ ( System
            , "You are a shell-command approval reviewer. Return only the strict JSON \
               decision requested by the user message. You have no tools and cannot \
               execute commands." )
          ; User, prompt
          ]
        ()
      |> Result.map ~f:(fun text ->
        Shell_runtime.Model_reviewer.
          { text; model = selected_model; input_tokens = None; output_tokens = None })
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Inference_client.Execution.Completion_error.sexp_of_t error)))
  in
  Some complete
;;

let shell_tool_names declarations =
  List.map declarations.shell_tools ~f:(fun tool ->
    tool.Chatmd_shell_spec.Shell_tool_spec.name)
  @ List.map declarations.legacy_tools ~f:(fun tool -> tool.MC.name)
  |> String.Set.of_list
;;

let moderator_runtime = function
  | None -> None
  | Some manifest -> manifest.Chatmd_shell_spec.Manifest.payload.moderator_runtime
;;

let moderator_process_handler t =
  match t.shell_registry, t.moderator_shell_runtime with
  | Some registry, Some runtime_id ->
    Some (Shell_runtime.Moderator_process_adapter.handler ~registry ~runtime_id)
  | None, None | Some _, None | None, Some _ -> None
;;

let create_native
      ~startup
      ~extension_resources
      ~programs
      ~compilation_limits
      ~checkpoint
      ~preflight
      ?(native_registrations = [])
      ?native_service_revision
      ?agent_source_runner
      ~sw
      ~ctx
      ~host
      ~platform
      ~prompt_elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ?(extension_snapshots = [])
      ?persist_extension_snapshots
      ~run_agent
      ()
  =
  let open Result.Let_syntax in
  let%bind declarations, admin_policy =
    Startup_compilation.within
      startup
      ~map_error:(fun e -> [ diagnostic e.Chatml_compilation.code e.message ])
      ~f:(fun () ->
        let%bind _ =
          Native_tool_pool.validate_registrations native_registrations
          |> Result.map_error
               ~f:
                 (List.map ~f:(fun error ->
                    diagnostic error.Native_tool_pool.Error.code error.message))
        in
        if
          List.exists prompt_elements ~f:(function
            | CM.Extension_script _ | Tool (Extension _) -> not extension_resources
            | Tool (Inherited _) -> true
            | Authoring_context _ | Authoring_help _ -> not extension_resources
            | _ -> false)
        then
          Error
            [ diagnostic
                "chatml.extension_unavailable"
                "extension declarations are parsed but execution and authoring services \
                 are not yet enabled on this host"
            ]
        else (
          let declarations = declarations prompt_elements in
          let%map admin_policy =
            Shell_runtime.Admin_policy_loader.load_from_environment ~env:(Ctx.env ctx)
            |> Result.map_error ~f:(fun error -> [ diagnostic error.code error.message ])
          in
          declarations, admin_policy))
  in
  let prepare_pool shell ~shell_preparation =
    Startup_compilation.within
      startup
      ~map_error:(fun e -> [ diagnostic e.Chatml_compilation.code e.message ])
      ~f:(fun () ->
        let%bind pool =
          Native_tool_pool.create
            ~registrations:native_registrations
            ?native_service_revision
            ?agent_source_runner
            ~sw
            ~ctx
            ~host
            ~run_agent
            ~admin_policy
            ~shell
            prompt_elements
          |> Result.map_error
               ~f:
                 (List.map ~f:(fun error ->
                    diagnostic error.Native_tool_pool.Error.code error.message))
        in
        checkpoint ();
        let%map capabilities, legacy_moderator, admitted =
          preflight pool ~shell_preparation
        in
        checkpoint ();
        pool, capabilities, legacy_moderator, admitted)
  in
  let%map
      ( shell_registry
      , shell_manifest
      , shell_security_status
      , (pool, capabilities, legacy_moderator, admitted) )
    =
    shell_registry
      ~startup
      ~programs
      ~compilation_limits
      ~checkpoint
      ~sw
      ~host
      ~platform
      ~admin_policy
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~model_completion:(model_completion ~ctx)
      ~extension_snapshots
      ~persist_extension_snapshots
      ~preflight:prepare_pool
      declarations
  in
  let native =
    { functions = Native_tool_pool.functions pool
    ; capabilities
    ; classifications = Native_tool_pool.classifications pool
    ; shell_tool_names = shell_tool_names declarations
    ; shell_registry
    ; shell_manifest
    ; shell_admin_policy = Option.some_if (Option.is_some shell_manifest) admin_policy
    ; shell_security_status
    ; moderator_shell_runtime = moderator_runtime shell_manifest
    ; legacy_moderator
    }
  in
  native, admitted
;;

let prepare_legacy
      ~startup
      ?programs
      ~surface
      ~env
      ~capabilities
      ~checkpoint
      ~limits
      elements
  =
  Moderator_manager.Registry.prepare_elements
    Moderator_manager.Registry.empty
    ~startup
    ?programs
    ~surface
    ~capabilities
    ~env
    ~checkpoint
    ~limits
    ~max_dependency_depth:128
    elements
  |> Result.map ~f:snd
  |> Result.map_error
       ~f:
         (List.map ~f:(fun (error : D.t) ->
            { code = error.code; message = error.message; source = error.source }))
;;

let declares_legacy elements =
  List.exists elements ~f:(function
    | CM.Script _ -> true
    | _ -> false)
;;

let create
      ?native_service_revision
      ?startup
      ?agent_source_runner
      ?programs
      ?(moderator_surface = Chatml.Chatml_builtin_surface.moderator_surface)
      ?(compilation_limits = Chatml_compilation.default_limits)
      ?(checkpoint = Eio.Fiber.check)
      ~sw
      ~ctx
      ~host
      ~platform
      ~prompt_elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ?extension_snapshots
      ?persist_extension_snapshots
      ~run_agent
      ()
  =
  Startup_compilation.with_context
    ?startup
    ~limits:compilation_limits
    ~env:(Ctx.env ctx)
    ~checkpoint
    ~map_error:(fun e -> [ diagnostic e.Chatml_compilation.code e.message ])
    ~f:(fun startup ->
      let checkpoint () = Startup_compilation.checkpoint startup in
      create_native
        ~startup
        ~extension_resources:false
        ~programs
        ~compilation_limits
        ~checkpoint
        ~preflight:(fun pool ~shell_preparation:_ ->
          let capabilities = Native_tool_pool.capabilities pool in
          let open Result.Let_syntax in
          let%map legacy_moderator =
            if not (declares_legacy prompt_elements)
            then Ok None
            else (
              let%bind capabilities =
                Lazy.force capabilities
                |> Result.map_error ~f:(fun error ->
                  [ diagnostic error.Tool_capability.code error.message ])
              in
              prepare_legacy
                ~startup
                ?programs
                ~surface:moderator_surface
                ~env:(Ctx.env ctx)
                ~capabilities
                ~checkpoint
                ~limits:compilation_limits
                prompt_elements)
          in
          capabilities, legacy_moderator, ())
        ?native_service_revision
        ?agent_source_runner
        ~sw
        ~ctx
        ~host
        ~platform
        ~prompt_elements
        ~manifest_authorizer
        ~approval_provider
        ~approval_store
        ?extension_snapshots
        ?persist_extension_snapshots
        ~run_agent
        ()
      |> Result.map ~f:fst)
    ()
;;

type extension_resources =
  { native : t
  ; definition : Extension_compiler.definition
  ; managed : Managed_tool_registry.t
  }

let prepare_extensions
      ?native_service_revision
      ?startup
      ?agent_source_runner
      ?(moderator_surface = Chatml.Chatml_builtin_surface.moderator_surface)
      ?(compilation_limits = Chatml_compilation.default_limits)
      ?(checkpoint = Eio.Fiber.check)
      ?(native_registrations = [])
      ?(delegated_moderator = false)
      ?programs
      ~sw
      ~ctx
      ~host
      ~platform
      ~prompt_elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ?extension_snapshots
      ?persist_extension_snapshots
      ~run_agent
      ()
  =
  Startup_compilation.with_context
    ?startup
    ~limits:compilation_limits
    ~env:(Ctx.env ctx)
    ~checkpoint
    ~map_error:(fun e -> [ diagnostic e.Chatml_compilation.code e.message ])
    ~f:(fun startup ->
      let checkpoint () = Startup_compilation.checkpoint startup in
      let open Result.Let_syntax in
      let%bind () =
        Startup_compilation.within
          startup
          ~map_error:(fun e -> [ diagnostic e.Chatml_compilation.code e.message ])
          ~f:(fun () ->
            if List.length prompt_elements > 16_384
            then
              Error
                [ diagnostic "chatml.definition_limit" "too many definition elements" ]
            else (
              try
                ignore (CM.validate_declarations prompt_elements);
                Ok ()
              with
              | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
              | _ ->
                Error
                  [ diagnostic
                      "chatml.invalid_definition"
                      "invalid captured extension declarations"
                  ]))
      in
      let prepare_managed pool ~shell_preparation =
        let%bind capabilities =
          Lazy.force (Native_tool_pool.capabilities pool)
          |> Result.map_error ~f:(fun error ->
            [ diagnostic error.Tool_capability.code error.message ])
        in
        let%bind managed =
          Managed_tool_registry.prepare
            ~startup
            ~limits:compilation_limits
            ~delegated_moderator
            ?programs
            ~env:(Ctx.env ctx)
            ~owner:host.Shell_runtime.Host.session_id
            ~capabilities
            prompt_elements
          |> Result.map_error
               ~f:
                 (List.map ~f:(fun (error : D.t) ->
                    { code =
                        (match error.code with
                         | "capability.duplicate_name" -> "agent.duplicate_tool_name"
                         | code -> code)
                    ; message = error.message
                    ; source = error.source
                    }))
        in
        let capabilities = Managed_tool_registry.capabilities managed in
        checkpoint ();
        let%bind () =
          match shell_preparation with
          | None -> Ok ()
          | Some prepared ->
            Shell_preparation.admit_tools prepared ~capabilities ~checkpoint
            |> Result.map_error ~f:(fun error ->
              [ diagnostic error.Chatml_compilation.code error.message ])
        in
        let%map legacy_moderator =
          if not (declares_legacy prompt_elements)
          then Ok None
          else
            prepare_legacy
              ~startup
              ?programs
              ~surface:moderator_surface
              ~env:(Ctx.env ctx)
              ~capabilities
              ~checkpoint
              ~limits:compilation_limits
              prompt_elements
        in
        lazy (Ok capabilities), legacy_moderator, managed
      in
      let%map native, managed =
        create_native
          ~startup
          ~extension_resources:true
          ~programs
          ~compilation_limits
          ~checkpoint
          ~preflight:prepare_managed
          ~native_registrations
          ?native_service_revision
          ?agent_source_runner
          ~sw
          ~ctx
          ~host
          ~platform
          ~prompt_elements
          ~manifest_authorizer
          ~approval_provider
          ~approval_store
          ?extension_snapshots
          ?persist_extension_snapshots
          ~run_agent
          ()
      in
      { native; definition = Managed_tool_registry.definition managed; managed })
    ()
;;

let inherit_native ?managed ~(parent : t) ~capabilities () =
  let module C = Tool_capability in
  let open Result.Let_syntax in
  let binding_result result =
    Result.map_error result ~f:(fun error -> [ diagnostic error.C.code error.message ])
  in
  let%bind parent_capabilities = Lazy.force parent.capabilities |> binding_result in
  let%bind () =
    match managed with
    | None -> Ok ()
    | Some delegation
      when String.equal
             (C.fingerprint capabilities)
             (C.fingerprint (Managed_tool_registry.delegation_selection delegation)) ->
      Ok ()
    | Some _ ->
      Error
        [ diagnostic
            "delegation.selection_changed"
            "managed delegation does not match the selected tools"
        ]
  in
  let%bind functions =
    List.fold_result (C.references capabilities) ~init:[] ~f:(fun functions reference ->
      let%bind binding =
        C.resolve
          parent_capabilities
          ~id:reference.C.id
          ~fingerprint:reference.fingerprint
        |> binding_result
      in
      let%bind () = C.check_delegation binding |> binding_result in
      match C.implementation binding with
      | Native fn -> Ok (fn :: functions)
      | Managed _ when Option.is_some managed ->
        let registry =
          Managed_tool_registry.delegation_registry (Option.value_exn managed)
        in
        let%map _ = Managed_tool_registry.resolve registry binding |> binding_result in
        functions
      | Managed _ ->
        Error
          [ diagnostic
              "delegation.owner_dispatch_unavailable"
              "inherited managed tools require their owner's authorized dispatcher"
          ])
  in
  let names =
    C.references capabilities
    |> List.map ~f:(fun reference -> reference.C.name)
    |> String.Set.of_list
  in
  let execution_names =
    match managed with
    | None -> names
    | Some delegated ->
      Managed_tool_registry.delegation_registry delegated
      |> Managed_tool_registry.capabilities
      |> C.references
      |> List.map ~f:(fun reference -> reference.C.name)
      |> String.Set.of_list
  in
  Ok
    { parent with
      functions = List.rev functions
    ; capabilities = lazy (Ok capabilities)
    ; classifications =
        List.filter parent.classifications ~f:(fun (name, _) -> Set.mem names name)
    ; shell_tool_names = Set.inter parent.shell_tool_names execution_names
    ; moderator_shell_runtime = None
    ; legacy_moderator = None
    }
;;
