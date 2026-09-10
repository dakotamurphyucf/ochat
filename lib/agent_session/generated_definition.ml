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
  let%bind artifact = Store.load artifact_store revision_id |> store_result in
  let%bind () =
    match String.equal artifact.manifest_sha256 manifest_sha256 with
    | true -> Ok ()
    | false ->
      error
        "delegation.artifact_identity"
        "generated artifact differs from its admitted manifest"
  in
  let%bind () =
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
