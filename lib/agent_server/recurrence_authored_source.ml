open! Core

let protocol_of_store error =
  Agent_protocol.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
    ~retryable:false
    ()
;;

let unavailable code message =
  Agent_protocol.Error.create code ~message ~retryable:false ()
;;

let capabilities (prepared : Agent_session.Runtime_builder.authored_resources) =
  let native = prepared.resources.native in
  if Lazy.is_val native.capabilities
  then
    Lazy.force native.capabilities
    |> Result.map_error ~f:(fun error ->
      unavailable Permission_denied error.Chat_response.Tool_capability.message)
  else Error (unavailable Invalid_state "authored private resources are not realized")
;;

let check_artifact
      (prepared : Agent_session.Runtime_builder.authored_resources)
      ~selector
      ~(artifact : Agent_store.Prompt_artifact_store.Artifact.t)
  =
  let module A = Agent_session in
  let module S = A.Authored_agent_source in
  let open Result.Let_syntax in
  let%bind () =
    if
      String.equal
        selector.Agent_store.Captured_source_kind.Authored_tool.name
        (S.identity prepared.source).tool_name
      && String.equal selector.source_sha256 (S.fingerprint prepared.source)
    then Ok ()
    else Error (unavailable Permission_denied "captured authored source binding changed")
  in
  let%bind expected =
    S.artifact
      prepared.source
      ~revision_id:artifact.revision_id
      ~created_at:artifact.created_at
    |> Result.map_error ~f:protocol_of_store
  in
  if String.equal expected.manifest_sha256 artifact.manifest_sha256
  then Ok ()
  else
    Error
      (unavailable
         Permission_denied
         "captured authored publication differs from its live source")
;;

let capture
      resources
      ~(state : Agent_session.Session_state.t)
      ~public
      ~name
      ~mode
      ~checkpoint
  =
  let module A = Agent_session in
  let module S = A.Authored_agent_source in
  let module Spec = Chatmd_shell_spec.Extension_spec in
  let open Result.Let_syntax in
  checkpoint ();
  let%bind prepared = Authored_resources.find resources ~public ~name in
  let declared =
    A.Prompt_revision.elements prepared.revision
    |> List.filter_map ~f:(function
      | Prompt.Chat_markdown.Extension_script script
        when Spec.equal_script_kind script.kind Moderator_script -> Some script
      | _ -> None)
  in
  let%bind () =
    match prepared.resources.definition with
    | None ->
      Error
        (unavailable
           Invalid_request
           "autonomous authored target requires its versioned lifecycle moderator")
    | Some definition ->
      let compiled =
        Chat_response.Extension_compiler.compiled_scripts definition
        |> List.filter_map ~f:(fun (script, _) ->
          if Spec.equal_script_kind script.kind Moderator_script
          then Some script
          else None)
      in
      (match declared, compiled with
       | [ declared ], [ compiled ] when Spec.equal_script declared compiled -> Ok ()
       | _ ->
         Error
           (unavailable
              Invalid_request
              "autonomous authored target lacks one exact prepared lifecycle moderator"))
  in
  let source = prepared.source in
  let source_sha256 = S.fingerprint source in
  let%bind selector =
    Agent_store.Captured_source_kind.Authored_tool.create ~name ~source_sha256
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request
        (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let publication =
    [%sexp
      ("ochat.recurrence-authored-capture.v1" : string)
    , (name : string)
    , (source_sha256 : string)]
    |> Sexp.to_string_mach
    |> Chatmd_shell_spec.Source_ref.digest
  in
  let%bind revision_id =
    Agent_protocol.Id.Prompt_revision.of_string ("prv_recurrence_" ^ publication)
  in
  let defining_revision = S.defining_revision source in
  let defining = A.Prompt_revision.artifact defining_revision in
  let%bind artifact =
    S.artifact source ~revision_id ~created_at:defining.created_at
    |> Result.map_error ~f:protocol_of_store
  in
  checkpoint ();
  let%bind artifact =
    Agent_store.Captured_prompt_artifact.create artifact
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request
        (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%bind private_capabilities = capabilities prepared in
  let%bind capability_pins =
    Chat_response.Background_request.capability_pins private_capabilities
  in
  let%bind parent =
    A.Recurrence_target.Parent.create
      ~workspace:state.spec.workspace_instance
      ~permission_profile:state.spec.permission_profile
      ~permission_profile_digest:state.spec.permission_profile_digest
      ~runtime_policy:state.spec.runtime_policy
      ~configuration_revision:state.spec.configuration_revision
      ~inference_selection:state.spec.inference_target
      ~capability_pins
  in
  let%bind target =
    A.Recurrence_target.create_authored
      ~authored_tool:selector
      ~mode
      ~artifact
      ~definition:(A.Prompt_revision.definition defining_revision)
      ~parent
  in
  checkpoint ();
  let%bind current = Authored_resources.find resources ~public ~name in
  let%bind capabilities = capabilities current in
  if
    String.equal (S.fingerprint current.source) source_sha256
    && String.equal
         (Chat_response.Tool_capability.fingerprint capabilities)
         (Chat_response.Tool_capability.fingerprint private_capabilities)
  then Ok target
  else
    Error (unavailable Permission_denied "authored source changed during target capture")
;;
