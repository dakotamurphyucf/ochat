open Core
module P = Agent_protocol
module CM = Prompt.Chat_markdown
module Runtime = Chat_response.Agent_runtime
module Managed = Chat_response.Managed_tool_registry
module EC = Chat_response.Extension_compiler
module C = Chat_response.Tool_capability
module Startup = Chat_response.Startup_compilation
module Shell = Chat_response.Shell_preparation
module Registry = Shell_runtime.Registry
module Store = Agent_store.Prompt_artifact_store

module Original = struct
  type prepare_persistent_source =
    binding:C.binding
    -> defining_revision:Prompt_revision.t
    -> reference:CM.Agent_reference.t
    -> owner:P.Job.launch_owner
    -> request:Chat_response.Background_request.t
    -> checkpoint:(unit -> unit)
    -> (Chat_response.Background_request.Agent_artifact.t option, P.Error.t) Result.t

  type t =
    { source : Authored_agent_source.t
    ; revision : Prompt_revision.t
    ; native : Runtime.t
    ; definition : EC.definition option
    ; managed : Managed.t option
    ; agent_source : Captured_agent_runner.Source_binding.t option
    ; prepare_persistent_source : prepare_persistent_source option
    }
end

type t =
  { original_native : Runtime.t
  ; selected : Prompt_revision.t
  ; native : Runtime.t
  ; definition : EC.definition option
  ; managed : Managed.t option
  ; shell_code_view : Registry.Code_view.t option
  ; agent_source_code_view : Agent_source_code_view.t
  ; agent_source_runner : Chat_response.Tool.Agent_source_runner.t option
  }

let revision t = t.selected
let native t = t.native
let definition t = t.definition
let managed t = t.managed
let shell_code_view t = t.shell_code_view
let agent_source_code_view t = t.agent_source_code_view
let agent_source_runner t = t.agent_source_runner
let invalid message = Error (P.Error.invalid_request message)

let compilation_error (error : Chatml_compilation.error) =
  P.Error.invalid_request error.message
;;

let diagnostics errors =
  P.Error.invalid_request
    (List.map errors ~f:(fun (error : Chatmd_shell_spec.Diagnostic.t) -> error.message)
     |> String.concat ~sep:"\n")
;;

let registry_errors errors =
  P.Error.invalid_request
    (List.map errors ~f:(fun (error : Registry.error) -> error.message)
     |> String.concat ~sep:"\n")
;;

let runtime_errors errors =
  P.Error.invalid_request
    (List.map errors ~f:(fun (error : Runtime.diagnostic) -> error.message)
     |> String.concat ~sep:"\n")
;;

let parsed revision =
  Result.of_option
    (Prompt_revision.parser_context revision)
    ~error:
      (P.Error.invalid_request "selected authored code requires its actual parser owner")
;;

let coherent_original (original : Original.t) ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let artifact = Prompt_revision.artifact original.revision in
  let%bind expected =
    Authored_agent_source.artifact
      original.source
      ~revision_id:artifact.revision_id
      ~created_at:artifact.created_at
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  checkpoint ();
  let%bind () =
    if String.equal expected.manifest_sha256 artifact.manifest_sha256
    then Ok ()
    else invalid "original specialist source and revision do not belong together"
  in
  let%bind capabilities =
    Lazy.force original.native.capabilities
    |> Result.map_error ~f:(fun (error : C.error) ->
      P.Error.invalid_request error.message)
  in
  let%bind () =
    match original.managed, original.definition with
    | Some managed, Some definition ->
      if
        phys_equal (Managed.capabilities managed) capabilities
        && phys_equal (Managed.definition managed) definition
      then Ok ()
      else invalid "original specialist managed/native owners are inconsistent"
    | None, None -> Ok ()
    | Some _, None | None, Some _ ->
      invalid "original specialist has a partial managed owner"
  in
  let%bind () =
    match original.native.shell_registry, original.native.shell_manifest with
    | Some registry, Some manifest ->
      if String.equal (Registry.manifest registry).sha256 manifest.sha256
      then Ok ()
      else invalid "original specialist shell manifest/resource owners are inconsistent"
    | None, None -> Ok ()
    | Some _, None | None, Some _ ->
      invalid "original specialist has a partial shell owner"
  in
  Ok capabilities
;;

let rebind_managed original ~selected ~startup ~limits ~programs ~env ~checkpoint =
  match original.Original.managed with
  | Some managed ->
    Managed.rebind_code
      managed
      ~startup
      ~limits
      ~delegated_moderator:true
      ~programs
      ~env
      ~checkpoint
      (Prompt_revision.elements selected)
    |> Result.map_error ~f:diagnostics
    |> Result.map ~f:(fun managed -> Some managed, Some (Managed.definition managed))
  | None ->
    let unsupported =
      List.exists (Prompt_revision.elements selected) ~f:(fun element ->
        checkpoint ();
        match element with
        | CM.Extension_script _ | Tool (Extension _) | Authoring_help _ -> true
        | Msg _
        | Developer _
        | System _
        | User _
        | Assistant _
        | Tool_call _
        | Tool_response _
        | Config _
        | Reasoning _
        | Shell_runtime _
        | Moderator_runtime _
        | Script _
        | Shell_script _
        | Authoring_context _
        | Tool
            ( Builtin _
            | Read_file _
            | Custom _
            | Shell _
            | Agent _
            | Mcp _
            | Inherited _
            | Persistent_agent _ ) -> false)
    in
    if unsupported
    then invalid "selected code requires an original managed authority owner"
    else Ok (None, None)
;;

let prepare_shell
      original
      ~selected
      ~startup
      ~limits
      ~programs
      ~env
      ~capabilities
      ~manifest_authorizer
      ~checkpoint
  =
  let open Result.Let_syntax in
  let platform =
    Option.value_map
      original.Original.native.shell_manifest
      ~default:(Chatmd_shell_spec.Shell_spec.Any : Chatmd_shell_spec.Shell_spec.platform)
      ~f:(fun manifest -> manifest.payload.platform)
  in
  let%bind manifest, material =
    Runtime.prepare_shell_manifest
      ~platform
      ~prompt_elements:(Prompt_revision.elements selected)
    |> Result.map_error ~f:runtime_errors
  in
  match original.native.shell_registry, original.native.shell_admin_policy with
  | None, _ ->
    if
      List.is_empty manifest.payload.runtimes
      && List.is_empty manifest.payload.tools
      && Option.is_none manifest.payload.moderator_runtime
    then Ok None
    else invalid "selected code cannot introduce native shell resources"
  | Some _, None -> invalid "original shell owner has no administrative policy"
  | Some _, Some admin_policy ->
    let%bind runtimes =
      Chatmd_shell_spec.Manifest_compiler.effective_runtimes material ~manifest
      |> Result.map_error ~f:diagnostics
    in
    let%bind () =
      Shell_runtime.Admin_policy.evaluate admin_policy ~manifest ~runtimes
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "selected shell exceeds original administrative policy")
    in
    let%bind prepared =
      Shell.prepare
        ~startup
        ?programs
        ~env
        ~limits
        ~checkpoint
        ~admin_policy
        ~manifest
        material
      |> Result.map_error ~f:registry_errors
    in
    let%bind () =
      Shell.admit_tools prepared ~capabilities ~checkpoint
      |> Result.map_error ~f:compilation_error
    in
    let prepared = Shell.registry prepared in
    let manifest = Registry.prepared_manifest prepared in
    checkpoint ();
    let%bind _ =
      Shell_runtime.Manifest_security.verify ~env ~admin_policy ~manifest
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "selected shell security verification rejected")
    in
    checkpoint ();
    let%bind grant =
      Shell_runtime.Manifest_authorizer.authorize manifest_authorizer manifest
      |> Result.map_error ~f:(fun (error : Shell_runtime.Manifest_authorizer.error) ->
        P.Error.invalid_request error.message)
    in
    checkpoint ();
    Ok (Some (prepared, grant))
;;

let bind_source
      original
      ~selected
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~env
      ~sw
      ~checkpoint
  =
  match original.Original.agent_source with
  | Some source ->
    Captured_agent_runner.Source_binding.bind
      source
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~env
      ~revision:selected
      ~sw
    |> Result.map ~f:Option.some
  | None ->
    let open Result.Let_syntax in
    let%bind parsed = parsed selected in
    checkpoint ();
    if List.is_empty (CM.Parsed.agent_references parsed)
    then Ok None
    else invalid "selected agent declarations require the original source binding"
;;

let registrations original ~capabilities ~view ~runner ~checkpoint =
  let open Result.Let_syntax in
  let rec loop count reversed = function
    | [] -> Ok (List.rev reversed)
    | registration :: rest ->
      checkpoint ();
      if count >= 16_384
      then invalid "too many original native registrations"
      else (
        let implementation =
          registration.Chat_response.Native_tool_pool.Registration.implementation
        in
        let%bind context =
          Agent_source_code_view.find_for_preparation view ~implementation
        in
        let%bind registration =
          match context with
          | None -> Ok registration
          | Some context ->
            let reference = Agent_source_code_view.Context.reference context in
            let defining_revision = Agent_source_code_view.Context.revision context in
            let%bind binding =
              C.find capabilities ~name:implementation.Ochat_function.info.function_.name
              |> Result.map_error ~f:(fun (error : C.error) ->
                P.Error.invalid_request error.message)
            in
            let%bind () =
              if
                Option.exists
                  (C.native_implementation binding)
                  ~f:(phys_equal implementation)
              then Ok ()
              else
                invalid "selected source registration is not the original native binding"
            in
            let%bind prepare =
              match CM.Agent_reference.invocation reference with
              | Tool declaration ->
                (match runner with
                 | Some
                     { Chat_response.Tool.Agent_source_runner.prepare = Some prepare; _ }
                   ->
                   Ok
                     (fun ~owner ~request:_ ~checkpoint ->
                       prepare ~declaration ~owner ~checkpoint
                       |> Result.map ~f:Option.some)
                 | None | Some { prepare = None; _ } ->
                   invalid "selected agent has no installed source preparer")
              | Persistent_tool _ ->
                (match original.Original.prepare_persistent_source with
                 | Some prepare -> Ok (prepare ~binding ~defining_revision ~reference)
                 | None ->
                   invalid "selected persistent agent has no original Factory preparer")
              | Content _ ->
                invalid "content occurrence cannot become a native registration"
            in
            let source =
              Chat_response.Native_source_registration.create ~implementation ~prepare
            in
            Ok { registration with source_requirement = Source (Some source) }
        in
        loop (count + 1) (registration :: reversed) rest)
  in
  loop 0 [] original.Original.native.native_registrations
;;

let prepare
      original
      ~selected
      ~script_preparation
      ~startup
      ~env
      ~sw
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~extension_snapshots
      ~persist_extension_snapshots
      ~check_authority
      ~checkpoint
  =
  let exception Work_limit in
  let work = ref 0 in
  let maximum = Runtime_script_preparation.max_custody_work_steps script_preparation in
  let poll () =
    if !work >= maximum then raise Work_limit;
    incr work;
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  let preflight () =
    poll ();
    Startup.checkpoint startup
  in
  let admit () =
    poll ();
    check_authority ();
    Eio.Fiber.check ()
  in
  let open Result.Let_syntax in
  try
    admit ();
    let%bind managed, definition, shell, view, runner, native =
      Startup.within startup ~map_error:compilation_error ~f:(fun () ->
        preflight ();
        Eio.Switch.check sw;
        let%bind capabilities = coherent_original original ~checkpoint:preflight in
        let%bind () =
          Store.verify_tree_with_checkpoint
            ~root:(Prompt_revision.materialized_tree original.revision)
            (Prompt_revision.artifact original.revision)
            ~checkpoint:preflight
          |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
        in
        let%bind () =
          Store.verify_tree_with_checkpoint
            ~root:(Prompt_revision.materialized_tree selected)
            (Prompt_revision.artifact selected)
            ~checkpoint:preflight
          |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
        in
        let%bind original_parsed = parsed original.revision in
        let%bind selected_parsed = parsed selected in
        let%bind () =
          Authored_native_contract.validate
            ~original:(CM.Parsed.elements original_parsed)
            ~selected:(CM.Parsed.elements selected_parsed)
            ~checkpoint:preflight
        in
        Runtime_script_preparation.with_revision
          (Some script_preparation)
          ~startup
          selected
          ~env
          ~invocation:Delegated_ui
          ~checkpoint:preflight
          ~f:(fun startup ~compilation_limits ~programs ->
            let%bind managed, definition =
              rebind_managed
                original
                ~selected
                ~startup
                ~limits:compilation_limits
                ~programs
                ~env
                ~checkpoint:preflight
            in
            let%bind shell =
              prepare_shell
                original
                ~selected
                ~startup
                ~limits:compilation_limits
                ~programs
                ~env
                ~capabilities
                ~manifest_authorizer
                ~checkpoint:preflight
            in
            let%bind view =
              Agent_source_code_view.create
                ~original:original.revision
                ~selected
                ~native:original.native
                ~sw
                ~max_work_steps:(maximum - !work)
                ~checkpoint:preflight
            in
            let%bind runner =
              bind_source
                original
                ~selected
                ~script_preparation
                ~manifest_authorizer
                ~approval_provider
                ~response_dir
                ~env
                ~sw
                ~checkpoint:preflight
            in
            let%map native_registrations =
              registrations original ~capabilities ~view ~runner ~checkpoint:preflight
            in
            ( managed
            , definition
            , shell
            , view
            , runner
            , { original.native with native_registrations } )))
    in
    admit ();
    let%bind shell_code_view =
      match shell, original.native.shell_registry with
      | None, _ -> Ok None
      | Some (prepared, grant), Some registry ->
        Registry.rebind_code
          registry
          ~prepared
          ~grant
          ~sw
          ~extension_snapshots
          ~persist_extension_snapshots
          ~max_work_steps:(maximum - !work)
          ~checkpoint:poll
          ~before_activation:admit
          ()
        |> Result.map_error ~f:registry_errors
        |> Result.map ~f:Option.some
      | Some _, None -> invalid "selected shell lost its original resource owner"
    in
    admit ();
    Ok
      { original_native = original.native
      ; selected
      ; native
      ; definition
      ; managed
      ; shell_code_view
      ; agent_source_code_view = view
      ; agent_source_runner = runner
      }
  with
  | Work_limit -> invalid "selected authored preparation work limit exceeded"
;;

let check t ~original ~selected =
  let artifact = Prompt_revision.artifact selected in
  let admitted = Prompt_revision.artifact t.selected in
  if
    phys_equal t.original_native original.Original.native
    && P.Id.Prompt_revision.equal artifact.revision_id admitted.revision_id
    && String.equal artifact.manifest_sha256 admitted.manifest_sha256
  then Ok ()
  else invalid "selected authored code has a foreign original or artifact"
;;
