open Core
module S = Agent_store
module D = Document_schema
module C = S.Delegation_document

let checked result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let fixture =
  {|{"format":"ochat.document","schema_version":6,"kind":"delegation.intent","future_envelope":{"nullable":null},"payload":{"key":{"parent_session_id":"ses_parent_named","parent_generation":"3","principal_id":"pri_named_parent","idempotency_key":"named-fixture","future_key":"kept"},"request_sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","admission":{"child_session_id":"ses_child_named","revision_id":"prv_child_named","transaction_id":"txn_named_admission","manifest_sha256":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","parent_revision_id":"prv_parent_named","authority_sha256":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc","capability_pins":[{"name":"read_file","pin":"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd","future_pin":1e+00}],"lifetime":{"kind":"owned","future_lifetime":null},"created_at":"2026-08-15T12:00:00Z","inference_target":{"adapter":"fixture.responses","profile":"selected","endpoint":"fixture://responses","model":"fixture-model","settings":[],"unknown_target":1e+00},"future_admission":{"preserve":null}},"stage":"reserved","future_disposition":"kept"}}|}
;;

let field json name =
  match D.Json.field json ~name with
  | Value json -> json
  | Null -> `Null
  | Absent -> assert false
;;

let edit json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal name key then value else old))
  | _ -> assert false
;;

let%expect_test "named delegation disposition preserves immutable admission and presence" =
  let original = D.Document.decode ~limits:C.limits fixture |> checked in
  let carrier = C.of_document original |> checked in
  let digest = C.admission_sha256 carrier in
  assert (
    String.equal
      digest
      Digestif.SHA256.(
        digest_string (Jsonaf.to_string (field (D.Document.payload original) "admission"))
        |> to_hex));
  let updated =
    C.with_disposition
      carrier
      ~stage:Artifact_installed
      ~revocation:(Some Parent_stopped)
      ~artifact_collection:(Some Prepared)
    |> checked
  in
  let document = C.to_document updated |> checked in
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.payload original) "admission")
      (field (D.Document.payload document) "admission"));
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.json original) "future_envelope")
      (field (D.Document.json document) "future_envelope"));
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.payload original) "key")
      (field (D.Document.payload document) "key"));
  assert (String.equal digest (C.admission_sha256 (C.of_document document |> checked)));
  let admission = field (D.Document.payload document) "admission" in
  assert (
    match D.Json.field admission ~name:"parent_stop_epoch" with
    | Absent -> true
    | Null | Value _ -> false);
  assert (
    match D.Json.field admission ~name:"authored_tool" with
    | Absent -> true
    | Null | Value _ -> false);
  let altered = { (C.to_record updated) with request_sha256 = String.make 64 'e' } in
  assert (Result.is_error (C.of_record altered));
  print_endline
    "original named hash, nested extensions and absent fields survive; immutable \
     substitution rejects";
  [%expect
    {|original named hash, nested extensions and absent fields survive; immutable substitution rejects|}]
;;

let%expect_test "delegation current admission requires target and supported semantics" =
  let document = D.Document.decode ~limits:C.limits fixture |> checked in
  let payload = D.Document.payload document in
  List.iter
    [ `Null; `Object [] ]
    ~f:(fun invalid ->
      let admission = edit (field payload "admission") "inference_target" invalid in
      let json =
        edit (D.Document.json document) "payload" (edit payload "admission" admission)
      in
      assert (
        Result.is_error
          (C.of_document (D.Document.inspect ~limits:C.limits json |> checked))));
  let json =
    match D.Document.json document with
    | `Object fields ->
      `Object (fields @ [ "required_semantics", `Array [ `String "unknown-authority" ] ])
    | _ -> assert false
  in
  assert (
    Result.is_error (C.of_document (D.Document.inspect ~limits:C.limits json |> checked)));
  let missing_target =
    { (C.to_record (C.of_document document |> checked)).admission with
      inference_target = None
    }
  in
  let known =
    { (C.to_record (C.of_document document |> checked)) with
      admission = missing_target
    ; preservation = None
    }
  in
  assert (Result.is_error (C.create known));
  print_endline "missing/corrupt capture and unsupported authority semantics rejected";
  [%expect {|missing/corrupt capture and unsupported authority semantics rejected|}]
;;

module P = Agent_protocol
module Origin = S.Delegation_goal_origin

let protocol result =
  Result.map_error result ~f:(fun (error : P.Error.t) -> error.message)
  |> Result.ok_or_failwith
;;

let append json name value =
  match json with
  | `Object fields -> `Object (fields @ [ name, value ])
  | _ -> assert false
;;

let remove json name =
  match json with
  | `Object fields ->
    `Object (List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
  | _ -> assert false
;;

let scope () =
  let record =
    D.Document.decode ~limits:C.limits fixture
    |> checked
    |> C.of_document
    |> checked
    |> C.value
  in
  let owner =
    P.Session_ref.create
      ~server_id:(P.Id.Server.of_string "srv_goal_origin" |> protocol)
      ~session_id:record.key.parent_session_id
  in
  let source =
    P.Run_source.create
      ~observer:{ script_id = "goal-origin"; source_sha256 = String.make 64 'd' }
      ~generation:record.key.parent_generation
      ~installation_epoch:1L
    |> protocol
  in
  let receipt =
    P.Run_receipt.create
      ~run_id:(P.Id.Run.of_string "run_goal_origin" |> protocol)
      ~principal_id:record.key.principal_id
      ~source
      ~key:record.key.idempotency_key
      ~request_sha256:(String.make 64 'd')
      ~kind:Admission
      ~run_revision:0L
      ~session_revision:1L
      ~committed_at:record.admission.created_at
    |> protocol
  in
  let creator_episode =
    P.Goal_episode.of_json
      (`Object
          [ "session", P.Session_ref.to_json owner
          ; "admission", P.Run_receipt.to_json receipt
          ])
    |> protocol
  in
  let branch =
    Origin.Anchor.create
      ~parent_session_id:record.key.parent_session_id
      ~parent_generation:record.key.parent_generation
      ~principal_id:record.key.principal_id
      ~key:record.key.idempotency_key
      ~child_session_id:record.admission.child_session_id
      ~transaction_id:record.admission.transaction_id
    |> protocol
  in
  Origin.Scope.create
    ~owner
    ~goal_id:(P.Id.Goal.of_string "goal_origin" |> protocol)
    ~source
    ~creator_episode
    ~branch
  |> protocol
;;

let%expect_test "Goal origin validates frozen roots, bounds and sexp admission" =
  let scope = scope () in
  let origin = Origin.associated ~max_scopes:1 [ scope ] |> protocol in
  assert (Origin.equal origin (Origin.t_of_sexp (Origin.sexp_of_t origin)));
  assert (
    Origin.equal
      origin
      (Origin.of_json ~max_scopes:1 ~limits:C.limits (Origin.to_json origin) |> protocol));
  List.iter
    [ Origin.associated ~max_scopes:1 []
    ; Origin.associated ~max_scopes:0 [ scope ]
    ; Origin.associated ~max_scopes:2 [ scope; scope ]
    ]
    ~f:(fun result -> assert (Result.is_error result));
  let different_source =
    P.Run_source.create
      ~observer:{ script_id = "other-source"; source_sha256 = String.make 64 'e' }
      ~generation:3
      ~installation_epoch:1L
    |> protocol
  in
  assert (
    Result.is_error
      (Origin.Scope.create
         ~owner:(Origin.Scope.owner scope)
         ~goal_id:(Origin.Scope.goal_id scope)
         ~source:different_source
         ~creator_episode:(Origin.Scope.creator_episode scope)
         ~branch:(Origin.Scope.branch scope)));
  assert (
    Result.is_error
      (Origin.Scope.of_json
         (edit (Origin.Scope.to_json scope) "root_id" (`String (String.make 64 '0')))));
  let invalid_branch =
    edit
      (Origin.Anchor.to_json (Origin.Scope.branch scope))
      "parent_generation"
      (`Number "-1")
  in
  assert (Result.is_error (Origin.Anchor.of_json invalid_branch));
  assert (
    Exn.does_raise (fun () ->
      Origin.Anchor.t_of_sexp (Sexp.Atom (Jsonaf.to_string invalid_branch)) |> ignore));
  let forged =
    edit
      (Origin.to_json origin)
      "scopes"
      (`Array [ Origin.Scope.to_json scope; Origin.Scope.to_json scope ])
  in
  assert (Result.is_error (Origin.of_json ~max_scopes:1 ~limits:C.limits forged));
  print_endline
    "frozen creator/root identity, bounded unique roots and validating sexp reject \
     substitution";
  [%expect
    {|frozen creator/root identity, bounded unique roots and validating sexp reject substitution|}]
;;

let%expect_test
    "v6 raw conversion preserves Unknown and v7 Goal origins require semantics"
  =
  let original = D.Document.decode ~limits:C.limits fixture |> checked in
  let historical = C.of_document original |> checked in
  assert (Option.is_none (C.value historical).admission.goal_origin);
  let converted = C.to_document historical |> checked in
  [%test_eq: int] 7 (D.Document.version converted);
  assert (
    Jsonaf.exactly_equal
      (field (D.Document.payload original) "admission")
      (field (D.Document.payload converted) "admission"));
  assert (List.is_empty (D.Document.required_semantics converted));
  let origin = Origin.associated ~max_scopes:1 [ scope () ] |> protocol in
  List.iter [ Origin.unrelated; origin ] ~f:(fun origin ->
    let record = C.to_record historical in
    let record =
      { record with
        admission = { record.admission with goal_origin = Some origin }
      ; preservation = None
      }
    in
    let authored = C.create record |> checked |> C.to_document |> checked in
    assert (
      List.mem
        (D.Document.required_semantics authored)
        "delegation.goal-origin.v1"
        ~equal:String.equal);
    let unmarked = remove (D.Document.json authored) "required_semantics" in
    assert (
      Result.is_error
        (C.of_document (D.Document.inspect ~limits:C.limits unmarked |> checked)));
    let admission = field (D.Document.payload authored) "admission" in
    let raw_origin =
      append (field admission "goal_origin") "future_origin" (`Number "1e+00")
    in
    let admission = edit admission "goal_origin" raw_origin in
    let raw =
      edit
        (D.Document.json authored)
        "payload"
        (edit (D.Document.payload authored) "admission" admission)
      |> D.Document.inspect ~limits:C.limits
      |> checked
    in
    let known = C.of_document raw |> checked in
    let sha256 = C.admission_sha256 known in
    let changed =
      C.with_disposition
        known
        ~stage:Artifact_installed
        ~revocation:None
        ~artifact_collection:None
      |> checked
    in
    let updated = C.to_document changed |> checked in
    assert (
      Jsonaf.exactly_equal admission (field (D.Document.payload updated) "admission"));
    assert (String.equal sha256 (C.admission_sha256 changed));
    let substitute =
      { (C.to_record known) with
        admission = { record.admission with goal_origin = None }
      }
    in
    assert (Result.is_error (C.of_record substitute)));
  let admission =
    append (field (D.Document.payload original) "admission") "goal_origin" `Null
  in
  let null_origin =
    edit
      (D.Document.json original)
      "payload"
      (edit (D.Document.payload original) "admission" admission)
    |> D.Document.inspect ~limits:C.limits
    |> checked
  in
  assert (Result.is_error (C.of_document null_origin));
  print_endline
    "raw v6 omission/hash retained; explicit v7 origins require marker and preserve \
     unknown lexemes";
  [%expect
    {|raw v6 omission/hash retained; explicit v7 origins require marker and preserve unknown lexemes|}]
;;
