open Core
module CM = Prompt.Chat_markdown

module Source_requirement = struct
  type t =
    | Ordinary
    | Source of Native_source_registration.t option
end

module Registration = struct
  type t =
    { implementation : Ochat_function.t
    ; implementation_revision : string
    ; result_contract : Tool_capability.result_contract
    ; authoring_metadata : Chatmd_shell_spec.Authoring_metadata.t option
    ; source_requirement : Source_requirement.t
    }
end

module Error = struct
  type t =
    { code : string
    ; message : string
    }
  [@@deriving sexp, compare, equal]
end

module Shell_view = struct
  type t =
    | Absent
    | Live of Shell_runtime.Registry.t
    | Staged of Shell_runtime.Registry.Staged.t
end

type t =
  { functions : Ochat_function.t list
  ; capabilities : (Tool_capability.t, Tool_capability.error) Result.t Lazy.t
  ; classifications : (string * Tool_execution_event.agent_page_kind) list
  ; registrations : Registration.t list
  }

let error code message = Error.{ code; message }

let shell_function shell specification =
  let result =
    match shell with
    | Shell_view.Absent ->
      Error
        Shell_tool.
          { code = "shell.tool_missing"
          ; message = "shell runtime registry is not available"
          }
    | Live registry -> Shell_tool.create registry specification
    | Staged registry -> Shell_tool.prepare registry specification
  in
  Result.map_error result ~f:(fun value -> error value.Shell_tool.code value.message)
;;

let custom_function shell name =
  let specification =
    match shell with
    | Shell_view.Absent -> None
    | Live registry -> Shell_runtime.Registry.tool registry name
    | Staged registry ->
      List.find (Shell_runtime.Registry.Staged.tools registry) ~f:(fun value ->
        String.equal value.Chatmd_shell_spec.Shell_tool_spec.name name)
  in
  match specification with
  | None -> Error (error "shell.tool_missing" ("compiled shell tool is missing: " ^ name))
  | Some specification -> shell_function shell specification
;;

let functions_of_tool ~construct shell = function
  | CM.Custom tool -> custom_function shell tool.name |> Result.map ~f:List.return
  | Shell specification -> shell_function shell specification |> Result.map ~f:List.return
  | declaration -> construct declaration
;;

let function_name function_ = function_.Ochat_function.info.function_.name

let has_installed_source ~functions ~registrations name =
  match
    List.find functions ~f:(fun implementation ->
      String.equal (function_name implementation) name)
  with
  | None -> false
  | Some implementation ->
    List.exists registrations ~f:(fun (registration : Registration.t) ->
      phys_equal implementation registration.implementation
      &&
      match registration.source_requirement with
      | Source (Some _) -> true
      | Source None | Ordinary -> false)
;;

let duplicate_function functions =
  List.map functions ~f:function_name |> List.find_a_dup ~compare:String.compare
;;

let validate_functions functions =
  match duplicate_function functions with
  | None -> Ok functions
  | Some name ->
    Error [ error "agent.duplicate_tool_name" ("duplicate exposed tool name: " ^ name) ]
;;

let build_functions
      ~agent_source_runner
      ~allow_supplied_agent
      ~native_registrations
      ~construct
      shell
      tools
  =
  (* Extension declarations are consumed by the prepared definition and owned
     dispatcher. They never receive a legacy/no-op runner. *)
  List.filter tools ~f:(function
    | CM.Extension _ -> false
    | _ -> true)
  |> List.map ~f:(fun declaration ->
    let supplied =
      match declaration with
      | CM.Builtin name | Persistent_agent ({ name; _ }, _) ->
        List.find native_registrations ~f:(fun (registration : Registration.t) ->
          String.equal name (function_name registration.implementation))
      | Agent { name; _ } when allow_supplied_agent ->
        List.find native_registrations ~f:(fun (registration : Registration.t) ->
          String.equal name (function_name registration.implementation))
      | _ -> None
    in
    match supplied with
    | Some registration ->
      (match declaration with
       | Persistent_agent (_, policy)
         when not
                (String.equal registration.implementation.info.type_ "function"
                 && (not registration.implementation.info.function_.strict)
                 && Jsonaf.exactly_equal
                      registration.implementation.info.function_.parameters
                      (Agent_tool_contract.parameters policy)
                 && Tool_capability.equal_result_contract
                      registration.result_contract
                      Invocation_v1) ->
         Error
           (error
              "agent.persistence_contract"
              "authored registration does not match its declaration's persistence \
               contract")
       | Persistent_agent _ ->
         let source_requirement =
           match registration.source_requirement with
           | Ordinary -> Source_requirement.Source None
           | Source _ as source -> source
         in
         Ok [ { registration with source_requirement } ]
       | Agent _ ->
         (match registration.source_requirement with
          | Source (Some source)
            when not
                   (Native_source_registration.matches_implementation
                      source
                      registration.implementation) ->
            Error
              (error
                 "agent.source_registration_mismatch"
                 "supplied Agent source belongs to a different implementation")
          | Ordinary ->
            Ok
              [ { registration with source_requirement = Source_requirement.Source None }
              ]
          | Source _ -> Ok [ registration ])
       | _ -> Ok [ registration ])
    | None ->
      functions_of_tool ~construct shell declaration
      |> Result.map ~f:(fun functions ->
        let implementation_revision =
          let declared = CM.sexp_of_tool declaration in
          let identity =
            match declaration, agent_source_runner with
            | CM.Agent _, Some runner ->
              [%sexp
                ("ochat.agent-source-runner.v1" : string)
              , (declared : Sexp.t)
              , (Chatml_source.Digest.to_hex runner.Tool.Agent_source_runner.revision
                 : string)]
            | _ -> declared
          in
          Sexp.to_string identity |> Chatmd_shell_spec.Source_ref.digest
        in
        List.map functions ~f:(fun implementation ->
          let source_requirement =
            match declaration with
            | CM.Agent specification ->
              let registration =
                Option.bind agent_source_runner ~f:(fun runner ->
                  Option.map runner.Tool.Agent_source_runner.prepare ~f:(fun prepare ->
                    Native_source_registration.create
                      ~implementation
                      ~prepare:(fun ~launch ~request:_ ~checkpoint ->
                        prepare ~declaration:specification ~launch ~checkpoint
                        |> Result.map ~f:Option.some)))
              in
              Source_requirement.Source registration
            | Persistent_agent _ -> Source None
            | _ -> Ordinary
          in
          Registration.
            { implementation
            ; implementation_revision
            ; result_contract = Native_output
            ; authoring_metadata = None
            ; source_requirement
            })))
  |> Result.all
  |> Result.map ~f:List.concat
  |> Result.map_error ~f:List.return
  |> Result.bind ~f:(fun registrations ->
    validate_functions (List.map registrations ~f:(fun value -> value.implementation))
    |> Result.map ~f:(fun functions -> functions, registrations))
;;

let validate_registrations registrations =
  validate_functions
    (List.map registrations ~f:(fun (value : Registration.t) -> value.implementation))
  |> Result.map ~f:(fun _ -> ())
;;

let create_with
      ~dir
      ~tool_dir
      ~construct
      ~allow_supplied_agent
      ~(host : Shell_runtime.Host.t)
      ~shell
      ~admin_policy
      ?native_service_revision
      ?agent_source_runner
      ~registrations
      prompt_elements
  =
  let open Result.Let_syntax in
  let native_registrations = registrations in
  let%bind () = validate_registrations registrations in
  let tools =
    List.filter_map prompt_elements ~f:(function
      | CM.Tool tool -> Some tool
      | _ -> None)
  in
  let shell_manifest =
    match shell with
    | Shell_view.Absent -> None
    | Live registry -> Some (Shell_runtime.Registry.manifest registry)
    | Staged registry -> Some (Shell_runtime.Registry.Staged.manifest registry)
  in
  let%map functions, registrations =
    build_functions
      ~agent_source_runner
      ~allow_supplied_agent
      ~native_registrations
      ~construct
      shell
      tools
  in
  let capabilities =
    lazy
      (try
         let resource_fingerprint =
           let path value = Eio.Path.native_exn value in
           [%sexp
             ("ochat.tool-resources.v2" : string)
           , ([ path dir
              ; path tool_dir
              ; path host.workspace
              ; path host.tool_dir
              ; path host.prompt_dir
              ; path host.session_dir
              ; path host.cache_dir
              ; path host.home
              ]
              : string list)
           , (Map.to_alist host.source_dirs
              |> List.map ~f:(fun (name, value) -> name, path value)
              : (string * string) list)
           , (host.process_environment : string array)
           , (Option.map shell_manifest ~f:(fun manifest ->
                manifest.Chatmd_shell_spec.Manifest.sha256)
              : string option)
           , (admin_policy : Shell_runtime.Admin_policy.t)]
           |> Sexp.to_string
           |> Chatmd_shell_spec.Source_ref.digest
         in
         let resource_fingerprint =
           match native_service_revision with
           | None -> resource_fingerprint
           | Some revision ->
             [%sexp
               ("ochat.tool-service-policy.v1" : string)
             , (resource_fingerprint : string)
             , (revision : string)]
             |> Sexp.to_string_mach
             |> Chatmd_shell_spec.Source_ref.digest
         in
         Authoring_registration.create
           ~delegation_restrictions:
             (List.filter_map tools ~f:(function
                | CM.Agent { name; _ }
                  when not (has_installed_source ~functions ~registrations name) ->
                  Some
                    ( name
                    , "legacy agent tools lack delegated actor policy and approval \
                       services" )
                | CM.Builtin "fork"
                  when not
                         (List.exists
                            native_registrations
                            ~f:(fun (registration : Registration.t) ->
                              String.equal
                                (function_name registration.implementation)
                                "fork")) ->
                  Some
                    ( "fork"
                    , "legacy fork requires its owning driver; use a delegated session \
                       service" )
                | _ -> None))
           ~host_metadata:
             (List.filter_map registrations ~f:(fun value ->
                Option.map value.authoring_metadata ~f:(fun metadata ->
                  function_name value.implementation, metadata)))
           ~result_contracts:
             (List.map registrations ~f:(fun value ->
                function_name value.implementation, value.result_contract))
           ~declarations:
             (List.filter_map prompt_elements ~f:(function
                | CM.Authoring_help help
                  when not
                         (List.exists tools ~f:(function
                            | CM.Extension tool -> String.equal tool.name help.tool
                            | _ -> false)) -> Some help
                | _ -> None))
           ~owner:host.session_id
           ~resource_fingerprint
           (List.map registrations ~f:(fun value ->
              value.implementation_revision, value.implementation))
         |> Result.map ~f:Authoring_registration.capabilities
       with
       | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
       | _ ->
         Error
           Tool_capability.
             { code = "capability.unavailable"
             ; message = "host cannot construct capability identities"
             })
  in
  { functions
  ; capabilities
  ; classifications = List.filter_map tools ~f:Tool.agent_page_classification
  ; registrations
  }
;;

let create
      ~sw
      ~ctx
      ~host
      ~shell
      ~admin_policy
      ?native_service_revision
      ?agent_source_runner
      ~registrations
      ~run_agent
      prompt_elements
  =
  create_with
    ~dir:(Ctx.dir ctx)
    ~tool_dir:(Ctx.tool_dir ctx)
    ~allow_supplied_agent:false
    ~construct:(fun declaration ->
      try
        Ok
          (Tool.of_declaration ?agent_source_runner ~host ~sw ~ctx ~run_agent declaration)
      with
      | exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        (match exn with
         | Eio.Cancel.Cancelled _ | Eio.Time.Timeout ->
           Stdlib.Printexc.raise_with_backtrace exn backtrace
         | _ -> Error (error "agent.tool_construction_failed" (Exn.to_string exn))))
    ~host
    ~shell
    ~admin_policy
    ?native_service_revision
    ?agent_source_runner
    ~registrations
    prompt_elements
;;

let create_without_inference
      ~sw
      ~(host : Shell_runtime.Host.t)
      ~shell
      ~admin_policy
      ?check_preparation_now
      ?native_service_revision
      ~registrations
      prompt_elements
  =
  create_with
    ~dir:host.Shell_runtime.Host.workspace
    ~tool_dir:host.tool_dir
    ~allow_supplied_agent:true
    ~construct:(fun declaration ->
      Tool.of_declaration_without_inference
        ~host
        ~sw
        ?check_preparation_now:
          (Option.map check_preparation_now ~f:(fun check () ->
             Result.map_error (check ()) ~f:(fun error ->
               Tool.Native_error.{ code = error.Error.code; message = error.message })))
        declaration
      |> Result.map_error ~f:(fun error ->
        Error.{ code = error.Tool.Native_error.code; message = error.message }))
    ~host
    ~shell
    ~admin_policy
    ?native_service_revision
    ~registrations
    prompt_elements
;;

let functions t = t.functions
let capabilities t = t.capabilities
let classifications t = t.classifications
let registrations t = t.registrations
