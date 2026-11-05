(* Proposed sibling test/recurrence_creation/captured_source_tests.ml.
   Local domain/data law only: no Actor/Store/resource authority is minted. *)
open! Core
open Creation_support
module Due = Due_support
module Target = A.Recurrence_target
module Kind = Agent_store.Captured_source_kind
module Raw = Agent_store.Delegation_captured_origin
module Origin = A.Recurrence_child_origin
module Delegate = Agent_store.Delegation_store

let replace_payload json f = Due.set json "payload" (f (Due.field json "payload"))

let definition_with_target template target =
  F.create
    ~key:(F.key template)
    ~creator:(F.creator template)
    ~source:(F.source template)
    ~birth_lifecycle_revision:(F.birth_lifecycle_revision template)
    ~schedule:(F.schedule template)
    ~target
    ~policy:(F.policy template)
    ~created_at:(F.created_at template)
  |> protocol_ok
;;

let decode_target json =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits:Target.limits json in
  let%bind value = Target.of_document document in
  let%map document = Target.to_document value in
  D.Document.json document
;;

let decode_origin json =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits:Raw.limits json in
  let%map value = Raw.of_document document in
  Raw.to_json value
;;

let assert_source_codec ~decode ~json ~marker =
  assert (D.Json.equal json (decode json |> document_ok));
  let markers =
    D.Document.inspect ~limits:Target.limits json
    |> document_ok
    |> D.Document.required_semantics
  in
  List.iter
    [ Due.set
        json
        "required_semantics"
        (`Array
            (List.filter_map markers ~f:(fun value ->
               if String.equal value marker then None else Some (`String value))))
    ; Due.set
        json
        "required_semantics"
        (`Array
            (List.map (markers @ [ "unsupported.future-source.v1" ]) ~f:(fun value ->
               `String value)))
    ; replace_payload json (fun payload -> Due.set payload "authored_source" `Null)
    ; replace_payload json (fun payload ->
        Due.set
          payload
          "authored_source"
          (Due.set
             (Due.field payload "authored_source")
             "source_sha256"
             (`String "INVALID")))
    ; replace_payload json (function
        | `Object fields ->
          `Object
            (List.filter fields ~f:(fun (name, _) ->
               not (String.equal name "authored_source")))
        | _ -> assert false)
    ]
    ~f:(fun changed -> assert (Result.is_error (decode changed)));
  let enriched =
    replace_payload json (fun payload ->
      Due.set
        payload
        "authored_source"
        (Due.append
           (Due.field payload "authored_source")
           "future_selector"
           (`Number "1.00e+2")))
    |> fun json -> Due.append json "future_envelope" (`String "retained")
  in
  [%test_eq: string]
    (Jsonaf.to_string enriched)
    (Jsonaf.to_string (decode enriched |> document_ok))
;;

let reservation definition =
  let nominal = N.create_interval definition ~slot:0L |> protocol_ok in
  Reservation.create
    ~definition
    ~control_revision:1L
    ~principal_id
    ~child_session_id:(P.Id.Session.of_string "ses_source_codec_child" |> protocol_ok)
    ~transaction_id
    ~nominal_json:(N.to_json nominal)
  |> protocol_ok
;;

let%expect_test "captured authored source is explicit data with exact legacy custody" =
  with_actor_workspace (fun _ workspace ->
    let legacy = fresh_target workspace in
    let artifact, definition, parent =
      match Target.view legacy with
      | Fresh_child { artifact; definition; parent; mode = _; source_kind = Artifact } ->
        artifact, definition, parent
      | Existing_session _ | Fresh_child _ -> failwith "expected local Artifact fixture"
    in
    (* Independent old wire oracle, not a copied legacy decoder/type family. *)
    let old_payload =
      `Object
        [ "kind", `String "fresh_child"
        ; ( "artifact"
          , Agent_store.Captured_prompt_artifact.to_document artifact
            |> document_ok
            |> D.Document.json )
        ; "definition", A.Prompt_definition.to_jsonaf definition |> protocol_ok
        ; ( "parent"
          , `Object
              [ "workspace", A.Workspace_instance.to_jsonaf workspace
              ; "permission_profile", `String "interactive"
              ; "permission_profile_digest", `String (String.make 64 'a')
              ; "runtime_policy", `Null
              ; "configuration_revision", `String "0"
              ; ( "inference_selection"
                , Inference.Selection.to_json (inference_selection ()) )
              ; "capability_pins", `Array []
              ] )
        ; "mode", P.Run.Mode.to_json Single_turn
        ]
    in
    let old_json =
      D.Document.create
        ~limits:Target.limits
        ~kind:"ochat.recurrence_target"
        ~version:1
        ~payload:old_payload
      |> document_ok
      |> D.Document.json
      |> fun json ->
      Due.append
        json
        "required_semantics"
        (`Array [ `String "recurrence.explicit-run-mode.v1" ])
    in
    let legacy_json = Target.to_document legacy |> document_ok |> D.Document.json in
    [%test_eq: string] (Jsonaf.to_string old_json) (Jsonaf.to_string legacy_json);
    let old_digest =
      P.Json_codec.canonical_string old_json
      |> protocol_ok
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
    in
    [%test_eq: string] old_digest (Target.digest legacy |> protocol_ok);
    assert (D.Json.equal old_json (decode_target old_json |> document_ok));
    let authored_tool =
      Kind.Authored_tool.create ~name:"worker" ~source_sha256:(String.make 64 'd')
      |> document_ok
    in
    let alias : Delegate.Admission.authored_tool = authored_tool in
    assert (Kind.Authored_tool.equal alias authored_tool);
    [%test_eq: string]
      (Sexp.to_string_mach
         (Sexp.List
            [ Sexp.List [ Sexp.Atom "name"; Sexp.Atom "worker" ]
            ; Sexp.List [ Sexp.Atom "source_sha256"; Sexp.Atom (String.make 64 'd') ]
            ]))
      (Sexp.to_string_mach (Delegate.Admission.sexp_of_authored_tool alias));
    assert (
      Result.is_error
        (Kind.Authored_tool.create ~name:"bad\nname" ~source_sha256:(String.make 64 'd')));
    assert (
      Result.is_error
        (Kind.Authored_tool.create ~name:"worker" ~source_sha256:(String.make 64 'D')));
    let authored =
      Target.create_authored
        ~authored_tool
        ~mode:Single_turn
        ~artifact
        ~definition
        ~parent
      |> protocol_ok
    in
    let authored_json = Target.to_document authored |> document_ok |> D.Document.json in
    assert_source_codec
      ~decode:decode_target
      ~json:authored_json
      ~marker:"recurrence.authored-tool-source.v1";
    assert (not (String.equal old_digest (Target.digest authored |> protocol_ok)));
    let altered_source = { authored_tool with name = "different-worker" } in
    let altered =
      Target.create_authored
        ~authored_tool:altered_source
        ~mode:Single_turn
        ~artifact
        ~definition
        ~parent
      |> protocol_ok
    in
    assert (
      not
        (String.equal
           (Target.digest authored |> protocol_ok)
           (Target.digest altered |> protocol_ok)));
    let with_pin =
      Target.Parent.create
        ~workspace:(Target.Parent.workspace parent)
        ~permission_profile:(Target.Parent.permission_profile parent)
        ~permission_profile_digest:(Target.Parent.permission_profile_digest parent)
        ~runtime_policy:(Target.Parent.runtime_policy parent)
        ~configuration_revision:(Target.Parent.configuration_revision parent)
        ~inference_selection:(Target.Parent.inference_selection parent)
        ~capability_pins:[ "read_file", String.make 64 'e' ]
      |> protocol_ok
    in
    let pinned =
      Target.create_authored
        ~authored_tool
        ~mode:Single_turn
        ~artifact
        ~definition
        ~parent:with_pin
      |> protocol_ok
    in
    assert (
      not
        (String.equal
           (Target.digest authored |> protocol_ok)
           (Target.digest pinned |> protocol_ok)));
    let full_definition =
      definition_with_target (Due.definition "source_codec") authored
    in
    let row = reservation full_definition in
    let initial =
      actor_state
        ~workspace_instance:workspace
        ~liveness:Detached
        ~start_immediately:false
    in
    let raw =
      Origin.create full_definition ~reservation:row ~initial_spec:initial.spec
      |> protocol_ok
    in
    assert_source_codec
      ~decode:decode_origin
      ~json:(Raw.to_json raw)
      ~marker:"delegation.captured-authored-source.v1";
    (* Independent old-origin encoding oracle using the same immutable operands. *)
    let old_definition = definition_with_target full_definition legacy in
    let old_row = reservation old_definition in
    let old_origin =
      Origin.create old_definition ~reservation:old_row ~initial_spec:initial.spec
      |> protocol_ok
    in
    let expected_old_origin =
      D.Document.create
        ~limits:Raw.limits
        ~kind:"delegation.captured_origin"
        ~version:1
        ~payload:
          (`Object
              [ "reservation", Reservation.to_json old_row
              ; "definition", Raw.definition raw
              ; "initial_spec_sha256", `String (Raw.initial_spec_sha256 raw)
              ])
      |> document_ok
      |> D.Document.json
    in
    [%test_eq: string]
      (Jsonaf.to_string expected_old_origin)
      (Jsonaf.to_string (Raw.to_json old_origin));
    assert (Kind.equal (Raw.source_kind old_origin) Artifact);
    let target =
      match Inference.Selection.view initial.spec.inference_target with
      | Captured target -> target
      | Unresolved -> failwith "fixture has no inference target"
    in
    (* This is a local immutable record, not a reserved ledger entry/permission. *)
    let record : Agent_store.Delegation_record.t =
      { key = Reservation.delegation_key row |> protocol_ok
      ; request_sha256 = Reservation.request_sha256 row
      ; admission =
          { child_session_id = Reservation.child_session_id row
          ; revision_id = Reservation.artifact_revision_id row
          ; transaction_id = Reservation.transaction_id row
          ; manifest_sha256 = Reservation.manifest_sha256 row
          ; parent_revision_id = prompt_revision_id
          ; parent_stop_epoch = Some 0L
          ; authority_sha256 = String.make 64 'f'
          ; authored_tool = None
          ; captured_recurrence = Some raw
          ; capability_pins = []
          ; lifetime = Owned
          ; created_at = timestamp
          ; inference_target = Some target
          }
      ; stage = Reserved
      ; revocation = None
      ; artifact_collection = None
      ; preservation = None
      }
    in
    let origin = Origin.of_record record |> protocol_ok in
    Origin.validate_definition origin full_definition |> protocol_ok;
    assert (
      Option.equal
        Kind.Authored_tool.equal
        (Delegate.Admission.authored_source record.admission |> store_ok)
        (Some authored_tool));
    assert (
      Result.is_error
        (Origin.validate_definition
           origin
           (definition_with_target full_definition altered)));
    let substituted_raw =
      Raw.create_authored
        ~authored_tool:altered_source
        ~reservation:(Reservation.to_json row)
        ~definition:(Raw.definition raw)
        ~initial_spec_sha256:(Raw.initial_spec_sha256 raw)
      |> document_ok
    in
    let substituted_record : Agent_store.Delegation_record.t =
      { record with
        admission = { record.admission with captured_recurrence = Some substituted_raw }
      }
    in
    let substituted = Origin.of_record substituted_record |> protocol_ok in
    assert (Result.is_error (Origin.validate_definition substituted full_definition));
    let conflicting : Agent_store.Delegation_record.t =
      { record with admission = { record.admission with authored_tool = Some alias } }
    in
    assert (Result.is_error (Delegate.Admission.authored_source conflicting.admission));
    assert (Result.is_error (Agent_store.Delegation_document.create conflicting)));
  print_endline
    "legacy bytes/hash, alias, semantic coupling, custody and definition association";
  [%expect
    {| legacy bytes/hash, alias, semantic coupling, custody and definition association |}]
;;
