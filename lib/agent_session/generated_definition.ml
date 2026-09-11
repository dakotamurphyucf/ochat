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
    Store.Artifact.create
      ~revision_id
      ~root_relative_path:t.artifact.root_relative_path
      ~root_chatmd:t.artifact.root_chatmd
      ~sources:t.artifact.sources
      ~parser_schema_version:t.artifact.parser_schema_version
      ~runtime_schema_version:t.artifact.runtime_schema_version
      ~created_at
      ()
    |> store_result
  in
  { t with artifact }
;;

let initial_history t ~session_id =
  let module CM = Prompt.Chat_markdown in
  let module R = Openai.Responses in
  let open Result.Let_syntax in
  let%bind allocator =
    History_entry.Allocator.create
      ~namespace:(Agent_protocol.Id.Session.to_string session_id)
      ~next_sequence:0
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  let items =
    List.filter_map (G.elements t.admission) ~f:(function
      | CM.Msg message
      | System message
      | Developer message
      | User message
      | Assistant message ->
        let role =
          match message.role with
          | "system" -> R.Input_message.System
          | "developer" -> Developer
          | "user" -> User
          | "assistant" -> Assistant
          | _ -> assert false
        in
        let texts =
          match message.content with
          | None -> []
          | Some (CM.Text text) -> [ text ]
          | Some (Items items) ->
            List.map items ~f:(function
              | Basic item -> Option.value item.text ~default:""
              | Agent _ -> assert false)
        in
        Some
          (R.Item.Input_message
             { role
             ; content =
                 List.map texts ~f:(fun text ->
                   R.Input_message.Text { text; _type = "input_text" })
             ; _type = "message"
             })
      | _ -> None)
  in
  let%map history =
    List.map items ~f:(History_entry.create ~allocator)
    |> Result.all
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  history, History_entry.Allocator.next_sequence allocator
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
  let%bind () =
    match current.admission.authored_tool with
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
