open Core
module Store = Agent_store.Prompt_artifact_store
module G = Chat_response.Generated_admission
module C = Chat_response.Tool_capability
module B = Chat_response.Background_request
module D = Chatmd_shell_spec.Diagnostic

type t =
  { artifact : Store.Artifact.t
  ; admission : G.t
  ; capability_pins : (string * string) list
  }

let artifact t = t.artifact
let admission t = t.admission
let capability_pins t = t.capability_pins
let error code message = Error [ D.error ~code message ]

let store_result result =
  Result.map_error result ~f:(fun error ->
    [ D.error
        ~code:"delegation.artifact"
        (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
    ])
;;

let cap_result result =
  Result.map_error result ~f:(fun (error : C.error) ->
    [ D.error ~code:error.code error.message ])
;;

let protocol_result result =
  Result.map_error result ~f:(fun (error : Agent_protocol.Error.t) ->
    [ D.error ~code:"delegation.capability_changed" error.message ])
;;

let with_identity t ~revision_id ~created_at =
  let open Result.Let_syntax in
  let%map artifact =
    Store.Artifact.with_identity t.artifact ~revision_id ~created_at |> store_result
  in
  { t with artifact }
;;

let initial_history t ~session_id =
  Chat_response.Initial_prompt_history.create ~session_id (G.elements t.admission)
;;

let select ~capabilities references =
  let open Result.Let_syntax in
  let%bind names =
    List.map references ~f:(fun (reference : C.reference) ->
      C.resolve capabilities ~id:reference.id ~fingerprint:reference.fingerprint
      |> cap_result
      |> Result.map ~f:(fun binding -> (C.reference binding).name))
    |> Result.all
  in
  C.select capabilities ~names |> cap_result
;;

let check_current ~current_capabilities selected =
  let open Result.Let_syntax in
  let%map _ = select ~capabilities:(current_capabilities ()) (C.references selected) in
  ()
;;

let prepare
      ?limits
      ?catalog
      ~env
      ~dir
      ~revision_id
      ~created_at
      ~current_capabilities
      ~references
      bundle
  =
  let open Result.Let_syntax in
  let%bind selected = select ~capabilities:(current_capabilities ()) references in
  let%bind admission =
    G.prepare
      ?limits
      ?catalog
      ~env
      ~dir
      ~ceiling:selected
      ~requested_names:
        (List.map (C.references selected) ~f:(fun reference -> reference.name))
      bundle
  in
  let%bind () = check_current ~current_capabilities selected in
  let%bind capability_pins =
    B.capability_pins (G.capabilities admission) |> protocol_result
  in
  let root_file = Chatmd_source_bundle.root_file bundle in
  let sources = Chatmd_source_bundle.sources bundle in
  let root_chatmd = List.Assoc.find_exn sources ~equal:String.equal root_file in
  let%bind sources =
    List.filter_map sources ~f:(fun (relative_path, contents) ->
      if String.equal relative_path root_file
      then None
      else Some (Store.Source.create ~relative_path ~contents))
    |> Result.all
    |> store_result
  in
  let%map artifact =
    Store.Artifact.create
      ~revision_id
      ~root_relative_path:root_file
      ~root_chatmd
      ~sources
      ~parser_schema_version:4
      ~runtime_schema_version:2
      ~created_at
      ()
    |> store_result
  in
  { artifact; admission; capability_pins }
;;

let same_manifest expected actual =
  if
    String.equal
      expected.Store.Artifact.manifest_sha256
      actual.Store.Artifact.manifest_sha256
  then Ok ()
  else
    error
      "delegation.artifact_conflict"
      "generated revision already contains a different definition"
;;

let install ~artifact_store ~transaction_id t =
  let open Result.Let_syntax in
  let verify () =
    let%bind installed =
      Store.load artifact_store t.artifact.revision_id |> store_result
    in
    same_manifest t.artifact installed
  in
  match Store.exists artifact_store t.artifact.revision_id with
  | true -> verify ()
  | false ->
    (match Store.install artifact_store ~transaction_id t.artifact with
     | Ok () -> verify ()
     | Error failure ->
       (* Another admission may have installed this same reserved revision while
          file IO yielded. Only its exact verified manifest permits replay. *)
       (match Store.exists artifact_store t.artifact.revision_id with
        | true -> verify ()
        | false -> store_result (Error failure)))
;;

let install_reserved ~delegations ~reservation ~artifact_store t =
  let module D = Agent_store.Delegation_store in
  let open Result.Let_syntax in
  let%bind current = D.find delegations reservation.D.key |> store_result in
  let%bind current =
    match current with
    | Some current
      when D.Admission.equal current.admission reservation.admission
           && String.equal current.request_sha256 reservation.request_sha256 -> Ok current
    | _ ->
      error
        "delegation.reservation"
        "generated artifact has no matching durable reservation"
  in
  let%bind () =
    match current.revocation with
    | None -> Ok ()
    | Some _ ->
      error "delegation.revoked" "generated artifact reservation has been revoked"
  in
  let%bind source = D.Admission.authored_source current.admission |> store_result in
  let%bind () =
    match source with
    | None -> Ok ()
    | Some _ ->
      error
        "delegation.reservation"
        "authored artifacts require authored definition admission"
  in
  let%bind () =
    match
      Agent_protocol.Id.Prompt_revision.equal
        current.admission.revision_id
        t.artifact.revision_id
      && String.equal current.admission.manifest_sha256 t.artifact.manifest_sha256
      && List.equal
           (fun (left_name, left_pin) (right_name, right_pin) ->
              String.equal left_name right_name && String.equal left_pin right_pin)
           current.admission.capability_pins
           t.capability_pins
    with
    | true -> Ok ()
    | false ->
      error
        "delegation.reservation"
        "generated artifact differs from its reserved definition or capabilities"
  in
  let%bind () =
    install ~artifact_store ~transaction_id:current.admission.transaction_id t
  in
  D.advance delegations current Artifact_installed |> store_result
;;

let load_artifact ~artifact_store ~revision_id ~manifest_sha256 =
  let open Result.Let_syntax in
  let%bind artifact = Store.load artifact_store revision_id |> store_result in
  let%bind () =
    match String.equal artifact.manifest_sha256 manifest_sha256 with
    | true -> Ok ()
    | false ->
      error
        "delegation.artifact_identity"
        "generated artifact differs from its admitted manifest"
  in
  let%map () =
    match
      ( artifact.parser_schema_version
      , artifact.runtime_schema_version
      , artifact.prompt_definition_id
      , artifact.canonical_source
      , artifact.shell_manifest_sha256 )
    with
    | 4, 2, None, None, None -> Ok ()
    | _ ->
      error
        "delegation.artifact_contract"
        "artifact is not a supported scoped generated definition"
  in
  artifact
;;

let restore
      ?limits
      ?source_limits
      ?catalog
      ~env
      ~artifact_store
      ~revision_id
      ~manifest_sha256
      ~current_capabilities
      ~pins
      ()
  =
  let open Result.Let_syntax in
  let%bind selected =
    B.rebind_capabilities ~pins ~capabilities:(current_capabilities ()) |> protocol_result
  in
  let%bind artifact = load_artifact ~artifact_store ~revision_id ~manifest_sha256 in
  let sources =
    (artifact.root_relative_path, artifact.root_chatmd)
    :: List.map artifact.sources ~f:(fun source ->
      source.Store.Source.relative_path, source.contents)
  in
  let%bind bundle =
    Chatmd_source_bundle.create
      ?limits:source_limits
      ~root_file:artifact.root_relative_path
      ~sources
      ()
    |> Result.map_error ~f:(fun message ->
      [ D.error ~code:"delegation.source_bounds" message ])
  in
  let%bind admission =
    G.prepare
      ?limits
      ?catalog
      ~env
      ~dir:(Store.materialized_tree artifact_store revision_id)
      ~ceiling:selected
      ~requested_names:(List.map pins ~f:fst)
      bundle
  in
  let%bind () = check_current ~current_capabilities selected in
  let%bind capability_pins =
    B.capability_pins (G.capabilities admission) |> protocol_result
  in
  let%bind () =
    let ordered values =
      List.sort values ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    if
      List.equal
        (fun (name, pin) (other_name, other_pin) ->
           String.equal name other_name && String.equal pin other_pin)
        (ordered capability_pins)
        (ordered pins)
    then Ok ()
    else
      error
        "delegation.selection_changed"
        "generated manifest differs from its saved effective selection"
  in
  Ok { artifact; admission; capability_pins }
;;

module Source_data = struct
  type t =
    { artifact : Store.Artifact.t
    ; reference : Agent_store.Delegation_store.Reference.t
    ; capability_pins : (string * string) list
    ; moderator : Agent_protocol.Invocation.observer option
    }

  let artifact (t : t) = t.artifact
  let reference (t : t) = t.reference
  let capability_pins (t : t) = t.capability_pins
  let moderator (t : t) = t.moderator

  let load
        ?(limits = Chatml_compilation.default_limits)
        ?source_limits
        ~artifact_store
        ~(record : Agent_store.Delegation_store.record)
        ()
    =
    let open Result.Let_syntax in
    let%bind artifact =
      load_artifact
        ~artifact_store
        ~revision_id:record.admission.revision_id
        ~manifest_sha256:record.admission.manifest_sha256
    in
    let%bind bundle =
      Chatmd_source_bundle.create
        ?limits:source_limits
        ~root_file:artifact.root_relative_path
        ~sources:
          ((artifact.root_relative_path, artifact.root_chatmd)
           :: List.map artifact.sources ~f:(fun source ->
             source.Store.Source.relative_path, source.contents))
        ()
      |> Result.map_error ~f:(fun message ->
        [ D.error ~code:"delegation.source_bounds" message ])
    in
    let%bind parsed =
      G.Source.parse
        ~dir:(Store.materialized_tree artifact_store artifact.revision_id)
        bundle
    in
    let script = G.Source.moderator parsed in
    let%map () =
      Chat_response.Extension_compiler.validate_delegated_source
        ~limits
        (Option.to_list script)
    in
    { artifact
    ; reference = Agent_store.Delegation_store.reference record
    ; capability_pins = record.admission.capability_pins
    ; moderator =
        Option.map script ~f:(fun script ->
          Agent_protocol.Invocation.
            { script_id = script.id; source_sha256 = script.source_sha256 })
    }
  ;;
end

module Metadata_source = struct
  type t = Source_data.t

  let artifact = Source_data.artifact
  let reference = Source_data.reference
  let capability_pins = Source_data.capability_pins
  let moderator = Source_data.moderator

  let load
        ?limits
        ?source_limits
        ~artifact_store
        ~(record : Agent_store.Delegation_store.record)
        ()
    =
    let open Result.Let_syntax in
    let%bind () =
      match
        ( record.stage
        , record.revocation
        , record.admission.lifetime
        , record.admission.authored_tool
        , record.admission.captured_recurrence )
      with
      | Linked, None, (Owned | Independent _), None, None -> Ok ()
      | _ ->
        error
          "delegation.metadata_source_record"
          "generated metadata requires a linked unrevoked plain-generated record"
    in
    Source_data.load ?limits ?source_limits ~artifact_store ~record ()
  ;;
end

module Source = struct
  type definition = t
  type t = Source_data.t

  let artifact = Source_data.artifact
  let reference = Source_data.reference
  let capability_pins = Source_data.capability_pins
  let moderator = Source_data.moderator

  let load
        ?(limits = Chatml_compilation.default_limits)
        ?source_limits
        ~artifact_store
        ~(record : Agent_store.Delegation_store.record)
        ()
    =
    let open Result.Let_syntax in
    let%bind () =
      match
        ( record.stage
        , record.revocation
        , record.admission.lifetime
        , record.admission.authored_tool
        , record.admission.captured_recurrence )
      with
      | Linked, None, Independent _, None, None -> Ok ()
      | _ ->
        error
          "delegation.source_record"
          "inert generated source requires its linked unrevoked Independent record"
    in
    Source_data.load ~limits ?source_limits ~artifact_store ~record ()
  ;;

  let check_definition (t : t) ~(definition : definition) =
    let module Manager = Chat_response.Moderator_manager in
    let failure message =
      Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
    in
    let open Result.Let_syntax in
    let%bind _, selected =
      Manager.Registry.of_definition
        Manager.Registry.empty
        (G.definition definition.admission)
      |> Result.map_error ~f:failure
    in
    let observed =
      Option.map selected ~f:(fun selected ->
        Agent_protocol.Invocation.
          { script_id = Manager.Registry.script_id selected
          ; source_sha256 = Manager.Registry.source_hash selected
          })
    in
    let sorted pins = List.sort pins ~compare:(fun (a, _) (b, _) -> String.compare a b) in
    if
      Agent_protocol.Id.Prompt_revision.equal
        t.artifact.revision_id
        definition.artifact.revision_id
      && String.equal t.artifact.manifest_sha256 definition.artifact.manifest_sha256
      && Store.Artifact.same_content t.artifact definition.artifact
      && List.equal
           (fun (name, pin) (other_name, other_pin) ->
              String.equal name other_name && String.equal pin other_pin)
           (sorted t.capability_pins)
           (sorted definition.capability_pins)
      && Option.equal Agent_protocol.Invocation.equal_observer t.moderator observed
    then Ok ()
    else Error (failure "generated executable differs from its selected artifact source")
  ;;
end
