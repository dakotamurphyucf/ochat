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

type t =
  { functions : Ochat_function.t list
  ; classifications : (string * Tool_execution_event.agent_page_kind) list
  ; shell_registry : Shell_runtime.Registry.t option
  ; shell_manifest : Chatmd_shell_spec.Manifest.t option
  ; shell_admin_policy : Shell_runtime.Admin_policy.t option
  ; shell_security_status : Shell_runtime.Manifest_security.status option
  ; moderator_shell_runtime : string option
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
  | Script _ -> declarations
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
      ~resource_runner
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
      ; resource_runner
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

let inspect_shell ~env ~platform ~prompt_elements =
  let open Result.Let_syntax in
  let declarations = declarations prompt_elements in
  let%bind administrative_policy =
    Shell_runtime.Admin_policy_loader.load_from_environment ~env
    |> Result.map_error ~f:(fun error -> [ diagnostic error.code error.message ])
  in
  let%bind manifest, material = compile ~platform declarations in
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
  let%map security_status =
    Shell_runtime.Manifest_security.verify
      ~env
      ~admin_policy:administrative_policy
      ~manifest
    |> Result.map_error ~f:(fun errors ->
      List.map errors ~f:(fun error -> diagnostic error.code error.message))
  in
  { manifest; live_runtimes; administrative_policy; security_status }
;;

let instantiate
      ~sw
      ~host
      ~admin_policy
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~model_completion
      ~extension_snapshots
      ~persist_extension_snapshots
      manifest
      material
  =
  Result.bind
    (Shell_runtime.Registry.prepare_with_policy ~manifest ~material ~admin_policy
     |> Result.map_error ~f:(List.map ~f:diagnostic_of_registry))
    ~f:(fun prepared ->
      Result.bind
        (Shell_runtime.Manifest_security.verify
           ~env:host.Shell_runtime.Host.env
           ~admin_policy
           ~manifest
         |> Result.map_error
              ~f:
                (List.map ~f:(fun (error : Shell_runtime.Manifest_security.error) ->
                   diagnostic error.code error.message)))
        ~f:(fun security_status ->
          Result.bind
            (Shell_runtime.Manifest_authorizer.authorize manifest_authorizer manifest
             |> Result.map_error ~f:(fun error -> [ diagnostic_of_authorizer error ]))
            ~f:(fun grant ->
              Shell_runtime.Registry.instantiate_prepared
                ~sw
                ~host
                ~grant
                ~approval_provider
                ~approval_store
                ~model_completion
                ~extension_snapshots
                ?persist_extension_snapshots
                prepared
              |> Result.map_error ~f:(List.map ~f:diagnostic_of_registry)
              |> Result.map ~f:(fun registry -> registry, security_status))))
;;

let shell_registry
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
      declarations
  =
  if
    List.is_empty declarations.runtimes
    && List.is_empty declarations.shell_tools
    && List.is_empty declarations.legacy_tools
    && Option.is_none declarations.moderator_runtime
  then Ok (None, None, None)
  else
    Result.bind (compile ~platform declarations) ~f:(fun (manifest, material) ->
      instantiate
        ~sw
        ~host
        ~admin_policy
        ~manifest_authorizer
        ~approval_provider
        ~approval_store
        ~model_completion
        ~extension_snapshots
        ~persist_extension_snapshots
        manifest
        material
      |> Result.map ~f:(fun (registry, security_status) ->
        Some registry, Some manifest, Some security_status))
;;

let xml_escape value =
  value
  |> String.substr_replace_all ~pattern:"&" ~with_:"&amp;"
  |> String.substr_replace_all ~pattern:"\"" ~with_:"&quot;"
  |> String.substr_replace_all ~pattern:"<" ~with_:"&lt;"
  |> String.substr_replace_all ~pattern:">" ~with_:"&gt;"
;;

let reviewer_prompt model =
  let config =
    Option.value_map model ~default:"" ~f:(fun model ->
      sprintf "<config model=\"%s\"/>" (xml_escape model))
  in
  config
  ^ "<system>You are a shell-command approval reviewer. Return only the strict JSON "
  ^ "decision requested by the user message. You have no tools and cannot execute \
     commands.</system>"
;;

let reviewer_item prompt =
  CM.Basic
    { type_ = "input_text"
    ; text = Some prompt
    ; image_url = None
    ; document_url = None
    ; is_local = false
    ; cleanup_html = false
    ; markdown = false
    }
;;

let model_completion ~ctx ~run_agent ~agent ~model =
  let complete ~prompt =
    try
      let text =
        run_agent
          ?prompt_dir:None
          ?session_id:None
          ?observer:None
          ~source:("shell-model-reviewer:" ^ agent)
          ~ctx
          (reviewer_prompt model)
          [ reviewer_item prompt ]
      in
      Ok
        Shell_runtime.Model_reviewer.
          { text
          ; model = Option.value model ~default:"ochat-default"
          ; input_tokens = None
          ; output_tokens = None
          }
    with
    | exn -> Error (Exn.to_string exn)
  in
  Some complete
;;

let create_shell_function registry name =
  match Shell_runtime.Registry.tool registry name with
  | None ->
    Error (diagnostic "shell.tool_missing" ("compiled shell tool is missing: " ^ name))
  | Some specification ->
    Shell_tool.create registry specification
    |> Result.map_error ~f:(fun error -> diagnostic error.code error.message)
;;

let functions_of_tool ~sw ~ctx ~run_agent shell_registry = function
  | CM.Custom tool ->
    Option.value_exn shell_registry
    |> fun registry ->
    create_shell_function registry tool.name |> Result.map ~f:List.return
  | declaration ->
    (try Ok (Tool.of_declaration ?shell_registry ~sw ~ctx ~run_agent declaration) with
     | exn -> Error (diagnostic "agent.tool_construction_failed" (Exn.to_string exn)))
;;

let function_name function_ = function_.Ochat_function.info.function_.name

let duplicate_function functions =
  List.map functions ~f:function_name |> List.find_a_dup ~compare:String.compare
;;

let validate_functions functions =
  match duplicate_function functions with
  | None -> Ok functions
  | Some name ->
    Error
      [ diagnostic "agent.duplicate_tool_name" ("duplicate exposed tool name: " ^ name) ]
;;

let build_functions ~sw ~ctx ~run_agent shell_registry tools =
  List.map tools ~f:(functions_of_tool ~sw ~ctx ~run_agent shell_registry)
  |> Result.all
  |> Result.map ~f:List.concat
  |> Result.map_error ~f:List.return
  |> Result.bind ~f:validate_functions
;;

let classifications tools = List.filter_map tools ~f:Tool.agent_page_classification

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

let create
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
  let declarations = declarations prompt_elements in
  let model_completion = model_completion ~ctx ~run_agent in
  Result.bind
    (Shell_runtime.Admin_policy_loader.load_from_environment ~env:(Ctx.env ctx)
     |> Result.map_error ~f:(fun error -> [ diagnostic error.code error.message ]))
    ~f:(fun admin_policy ->
      Result.bind
        (shell_registry
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
           declarations)
        ~f:(fun (shell_registry, shell_manifest, shell_security_status) ->
          build_functions ~sw ~ctx ~run_agent shell_registry declarations.tools
          |> Result.map ~f:(fun functions ->
            { functions
            ; classifications = classifications declarations.tools
            ; shell_registry
            ; shell_manifest
            ; shell_admin_policy =
                Option.some_if (Option.is_some shell_manifest) admin_policy
            ; shell_security_status
            ; moderator_shell_runtime = moderator_runtime shell_manifest
            })))
;;
