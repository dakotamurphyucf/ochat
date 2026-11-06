open! Core
open Fixtures
module P = Agent_protocol
module A = Agent_session
module D = Document_schema
module Store = Agent_store

let fields = function
  | `Object fields -> fields
  | _ -> failwith "expected object fixture"
;;

let member raw name = List.Assoc.find_exn (fields raw) name ~equal:String.equal

let set raw name value =
  `Object (List.Assoc.add (fields raw) ~equal:String.equal name value)
;;

let without raw name = `Object (List.Assoc.remove (fields raw) name ~equal:String.equal)

let record_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Store.Document_record.Error.t)]
;;

let future = `Object [ "opaque", `Array [ `Null; `String "retained" ] ]

(* Storage-only Resume DATA supplies no runtime custody or actual Run. The old
   request is made through its current public codec and remains byte-for-byte
   identical inside the converted action envelope. *)
let resume_fixture () =
  let source =
    P.Run_source.create
      ~observer:{ script_id = "goal"; source_sha256 = String.make 64 'a' }
      ~generation:0
      ~installation_epoch:1L
    |> protocol_ok
  in
  let request =
    P.Goal_resume_request.create
      ~session_id
      ~generation:0
      ~source
      ~goal_id:(P.Id.Goal.of_string "goal_saved_resume" |> protocol_ok)
      ~run_id:(P.Id.Run.of_string "run_saved_resume" |> protocol_ok)
      ~key:(P.Idempotency_key.of_string "saved-resume" |> protocol_ok)
      ~expected_goal_revision:0L
      ~expected_catalog_revision:0L
      ~expected_run_revision:0L
    |> protocol_ok
  in
  let intent = P.Goal_native_resume_intent.create ~request ~principal_id |> protocol_ok in
  let plain =
    P.Invocation.create { (invocation_fixture ()).context with tool_name = "goal_resume" }
    |> protocol_ok
  in
  let resolved =
    P.Invocation.dispatch plain
    |> protocol_ok
    |> fun dispatched ->
    P.Invocation.resolve dispatched ~session_id ~generation:0 (Complete `Null)
    |> protocol_ok
  in
  let invocation =
    P.Invocation.record_native_goal_action resolved (Resume intent) |> protocol_ok
  in
  request, intent, invocation
;;

let old_row intent invocation =
  let intent =
    P.Goal_native_resume_intent.to_json intent
    |> fun raw -> set raw "future_intent" future
  in
  P.Invocation.Storage.to_json invocation
  |> fun raw -> set (without raw "native_goal_action") "native_goal_resume" intent
;;

let old_document ~kind ~version ~payload =
  let document =
    D.Document.create ~limits:document_limits ~kind ~version ~payload |> document_ok
  in
  D.Document.json document
  |> fun raw ->
  set
    raw
    "required_semantics"
    (`Array [ `String P.Goal_native_resume_intent.persistence_semantics ])
  |> D.Document.inspect ~limits:document_limits
  |> document_ok
;;

let assert_preserved_row row request =
  assert (not (List.Assoc.mem (fields row) "native_goal_resume" ~equal:String.equal));
  let action = member row "native_goal_action" in
  assert (D.Json.equal (member action "kind") (`String "resume"));
  let intent = member action "intent" in
  assert (D.Json.equal (member intent "future_intent") future);
  assert (D.Json.equal (member intent "request") (P.Goal_resume_request.to_json request))
;;

let%expect_test "old raw State becomes one canonical preservation basis" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let request, intent, invocation = resume_fixture () in
    let payload = state_document initial |> D.Document.payload in
    let payload = set payload "invocations" (`Array [ old_row intent invocation ]) in
    let old = old_document ~kind:"session.state" ~version:10 ~payload in
    let original = D.Document.to_string old in
    let unmarked =
      without (D.Document.json old) "required_semantics"
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    assert (
      Result.is_error (A.Session_state_document.decode unmarked ~limits:document_limits));
    let decoded =
      A.Session_state_document.decode old ~limits:document_limits |> document_ok
    in
    let value = A.Session_state_document.value decoded in
    let values =
      P.Session_metadata.Values.create ~display_name:(Some "updated") ~labels:[]
      |> protocol_ok
    in
    let edited =
      A.Session_delta.apply value (Metadata_changed (values, 1L)) |> protocol_ok
    in
    let encoded =
      A.Session_state_document.with_value decoded edited
      |> A.Session_state_document.encode ~limits:document_limits
      |> document_ok
    in
    let row =
      match member (D.Document.payload encoded) "invocations" with
      | `Array [ row ] -> row
      | _ -> failwith "expected saved Invocation"
    in
    assert_preserved_row row request;
    let encoded_twice =
      A.Session_state_document.decode encoded ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.encode ~limits:document_limits
      |> document_ok
    in
    assert (D.Json.equal (D.Document.json encoded) (D.Document.json encoded_twice));
    assert (String.equal original (D.Document.to_string old));
    let invalid_version =
      D.Document.json old
      |> fun raw ->
      set raw "schema_version" (`Number "11")
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    assert (
      Result.is_error
        (A.Session_state_document.decode invalid_version ~limits:document_limits));
    print_s
      [%sexp
        (D.Document.version encoded : int)
      , (D.Document.required_semantics encoded : string list)]);
  [%expect {| (11 (goal.native-resume.v1)) |}]
;;

let%expect_test "old raw delta changed/reconciled/Created keeps original physical digest" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let request, intent, invocation = resume_fixture () in
    let row = old_row intent invocation in
    let state =
      old_document
        ~kind:"session.state"
        ~version:10
        ~payload:
          (set
             (state_document initial |> D.Document.payload)
             "invocations"
             (`Array [ row ]))
    in
    List.iter [ "invocation_changed"; "invocation_reconciled"; "created" ] ~f:(fun kind ->
      let change =
        if String.equal kind "created"
        then `Object [ "kind", `String kind; "state", D.Document.json state ]
        else `Object [ "kind", `String kind; "value", row ]
      in
      let old =
        old_document
          ~kind:"session.delta"
          ~version:6
          ~payload:(`Object [ "changes", `Array [ change ] ])
      in
      let bytes = D.Document.to_string old in
      let digest = Store.Document_record.digest bytes in
      let framed =
        Store.Document_record.encode old ~limits:document_limits ~flags:0 |> record_ok
      in
      let record =
        Store.Document_record.decode_file
          ~limits:document_limits
          ~expected_digest:(Some digest)
          framed
        |> record_ok
      in
      let converted =
        A.Session_delta_document.decode
          (Store.Document_record.document record)
          ~limits:document_limits
        |> document_ok
        |> A.Session_delta_document.document
      in
      let converted_change =
        match member (D.Document.payload converted) "changes" with
        | `Array [ change ] -> change
        | _ -> failwith "expected saved change"
      in
      let row =
        if String.equal kind "created"
        then (
          match
            member (member (member converted_change "state") "payload") "invocations"
          with
          | `Array [ row ] -> row
          | _ -> failwith "expected Created Invocation")
        else member converted_change "value"
      in
      assert_preserved_row row request;
      assert (String.equal (Store.Document_record.stored_bytes record) bytes);
      assert (String.equal (Store.Document_record.stored_digest record) digest);
      assert (not (String.equal (D.Document.to_string converted) bytes));
      assert (
        D.Json.equal
          (D.Document.json converted)
          (A.Session_delta_document.decode converted ~limits:document_limits
           |> document_ok
           |> A.Session_delta_document.document
           |> D.Document.json));
      print_s [%sexp (kind : string), (D.Document.version converted : int)]));
  [%expect
    {|
    (invocation_changed 7)
    (invocation_reconciled 7)
    (created 7)
    |}]
;;

let%expect_test "conversion rejects aliases, duplicate sources and explicit null" =
  let _, intent, invocation = resume_fixture () in
  let old_public =
    P.Invocation.to_json invocation
    |> fun raw ->
    set
      (without raw "native_goal_action")
      "native_goal_resume"
      (P.Goal_native_resume_intent.to_json intent)
    |> fun raw -> set raw "schema_version" (`Number "14")
  in
  assert (P.Invocation.equal invocation (P.Invocation.of_json old_public |> protocol_ok));
  assert (
    Result.is_error (P.Invocation.of_json (set old_public "native_goal_resume" `Null)));
  let row = old_row intent invocation in
  let intent_raw = member row "native_goal_resume" in
  let reject raw =
    assert (Result.is_error (P.Native_goal_action_conversion.invocation_record raw))
  in
  reject (`Object (fields row @ [ "native_goal_resume", `Null ]));
  reject (set row "native_goal_action" `Null);
  reject
    (`Object (fields row @ [ "native_goal_action", `Null; "native_goal_action", `Null ]));
  let converted = P.Native_goal_action_conversion.invocation_record row |> document_ok in
  assert (
    D.Json.equal (member (member converted "native_goal_action") "intent") intent_raw);
  let null =
    set row "native_goal_resume" `Null
    |> P.Native_goal_action_conversion.invocation_record
    |> document_ok
  in
  assert (D.Json.equal (member null "native_goal_action") `Null);
  assert (Result.is_error (P.Invocation.Storage.of_json null));
  assert (Result.is_error (P.Invocation.Storage.of_json row));
  print_endline "no collision, duplicate normalization, null omission or current alias";
  [%expect {| no collision, duplicate normalization, null omission or current alias |}]
;;

let%expect_test "live actions reserve later growth while terminal actions release it" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let _, intent, invocation = resume_fixture () in
    let limits_for_current document =
      D.Limits.create
        ~max_bytes:(String.length (D.Document.to_string document) + 1)
        ~max_depth:(D.Limits.max_depth document_limits)
        ~max_fields:(D.Limits.max_fields document_limits)
        ~max_nodes:(D.Limits.max_nodes document_limits)
      |> document_ok
    in
    let state = { initial with invocations = [ invocation ] } in
    let current = state_document state in
    assert (
      Result.is_error
        (A.Session_state_document.encode
           (A.Session_state_document.authored state)
           ~limits:(limits_for_current current)));
    let terminal =
      P.Goal_native_resume_intent.accept intent ~now:timestamp
      |> protocol_ok
      |> fun accepted ->
      P.Goal_native_resume_intent.claim accepted ~now:timestamp
      |> protocol_ok
      |> fun claimed ->
      P.Goal_native_resume_intent.reject claimed (P.Error.invalid_request "retired")
      |> protocol_ok
    in
    let accepted =
      P.Goal_native_resume_intent.accept intent ~now:timestamp |> protocol_ok
    in
    let claimed =
      P.Goal_native_resume_intent.claim accepted ~now:timestamp |> protocol_ok
    in
    let invocation =
      P.Invocation.record_native_goal_action invocation (Resume accepted) |> protocol_ok
    in
    let invocation =
      P.Invocation.record_native_goal_action invocation (Resume claimed) |> protocol_ok
    in
    let invocation =
      P.Invocation.record_native_goal_action invocation (Resume terminal) |> protocol_ok
    in
    let state = { initial with invocations = [ invocation ] } in
    let current = state_document state in
    assert (
      Result.is_ok
        (A.Session_state_document.encode
           (A.Session_state_document.authored state)
           ~limits:(limits_for_current current)));
    print_endline "current bytes fit; live future does not; terminal current fits");
  [%expect {| current bytes fit; live future does not; terminal current fits |}]
;;

let%expect_test "component depth agrees with complete actual State boundary" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let _, intent, invocation = resume_fixture () in
    let state = { initial with invocations = [ invocation ] } in
    let current = state_document state in
    let current_row = P.Invocation.Storage.to_json invocation in
    let full_probes =
      P.Goal_native_action_intent.capacity_projections (Resume intent)
      |> List.map ~f:(fun action ->
        let row = set current_row "native_goal_action" action in
        set
          (D.Document.json current)
          "payload"
          (set (D.Document.payload current) "invocations" (`Array [ row ])))
    in
    let with_depth max_depth =
      D.Limits.create
        ~max_bytes:(D.Limits.max_bytes document_limits)
        ~max_depth
        ~max_fields:(D.Limits.max_fields document_limits)
        ~max_nodes:(D.Limits.max_nodes document_limits)
      |> document_ok
    in
    (* Independent placement oracle: validate the complete projected State tree
       at each possible depth, rather than repeating the component's offset. *)
    let boundary =
      List.find_exn (List.range 1 129) ~f:(fun depth ->
        List.for_all full_probes ~f:(fun raw ->
          Result.is_ok (D.Json.validate ~limits:(with_depth depth) raw)))
    in
    let encode depth =
      A.Session_state_document.encode
        (A.Session_state_document.authored state)
        ~limits:(with_depth depth)
    in
    assert (Result.is_ok (encode boundary));
    assert (Result.is_error (encode (boundary - 1)));
    print_endline "actual full State accepts boundary and rejects one below");
  [%expect {| actual full State accepts boundary and rejects one below |}]
;;
