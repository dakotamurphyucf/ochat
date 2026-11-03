open! Core
open Support

let map_first_entry json ~f =
  match field json "entries" with
  | `Array (first :: rest) -> set json "entries" (`Array (f first :: rest))
  | _ -> failwith "expected retained entry"
;;

let map_payload json ~f = set json "payload" (f (field json "payload"))

let map_entry json ~f =
  map_first_entry json ~f:(fun row ->
    set row "document" (map_payload (field row "document") ~f))
;;

let map_first_definition payload ~f =
  match field payload "definitions" with
  | `Array (first :: rest) ->
    set
      payload
      "definitions"
      (`Array (set first "value" (f (field first "value")) :: rest))
  | _ -> failwith "expected definition"
;;

let check_fresh_custody () =
  with_actor_workspace (fun _env workspace_instance ->
    let module Artifact = Agent_store.Prompt_artifact_store.Artifact in
    let module Captured = Agent_store.Captured_prompt_artifact in
    let module Bundle = Agent_store.Prompt_program_bundle in
    let definition =
      A.Prompt_definition.create
        ~id:(P.Id.Prompt_definition.of_string "prd_recurrence_fresh" |> protocol_ok)
        ~config_name:"captured-headless"
        ~root_file:"/unavailable/provenance-only.chatmd"
        ~root_invocation:Headless
        ~allowed_workspaces:(Option.to_list workspace_instance.definition_id)
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:(Some "original captured definition")
      |> store_ok
      |> fun value -> A.Prompt_definition.with_tool_contract value Developer_model_spawn
    in
    let bundle kind contents =
      let metadata =
        D.Document.create
          ~limits:D.Limits.default
          ~kind
          ~version:1
          ~payload:(`Object [ "future_metadata", `Number "2.00e1" ])
        |> document_ok
      in
      Bundle.create ~metadata ~payloads:[ Bundle.Payload.create contents |> store_ok ]
      |> store_ok
    in
    let artifact =
      Artifact.create
        ~revision_id:prompt_revision_id
        ~prompt_definition_id:definition.id
        ~root_chatmd:"<developer>captured</developer>"
        ~sources:[]
        ~program_bundle:(bundle "fixture.programs" "let captured = 42")
        ~content_bundle:(bundle "fixture.content" "\000\255captured content")
        ~parser_schema_version:1
        ~runtime_schema_version:1
        ~created_at:timestamp
        ()
      |> store_ok
    in
    let artifact_document =
      Captured.create artifact
      |> document_ok
      |> Captured.to_document
      |> document_ok
      |> D.Document.json
      |> fun json ->
      append json "future_artifact" (`Number "1.00e+2")
      |> D.Document.inspect ~limits:Captured.limits
      |> document_ok
    in
    let captured = Captured.of_document artifact_document |> document_ok in
    let parent =
      A.Recurrence_target.Parent.create
        ~workspace:workspace_instance
        ~permission_profile:"interactive"
        ~permission_profile_digest:(String.make 64 'a')
        ~runtime_policy:None
        ~configuration_revision:0L
        ~inference_selection:(inference_selection ())
        ~capability_pins:[]
      |> protocol_ok
    in
    let target =
      A.Recurrence_target.create_fresh ~artifact:captured ~definition ~parent
      |> protocol_ok
    in
    let capture = { (capture ()) with target } in
    let catalog, _ = fresh C.empty "fresh-custody" (Create (id "fresh", capture)) in
    let restored = roundtrip catalog in
    let restored_target = F.target (E.current (entry restored "fresh")) in
    match A.Recurrence_target.view restored_target with
    | Existing_session _ -> assert false
    | Fresh_child { artifact = actual; definition = actual_definition; parent = _ } ->
      assert (
        D.Json.equal
          (D.Document.json artifact_document)
          (Captured.to_document actual |> document_ok |> D.Document.json));
      assert (
        D.Json.equal
          (A.Prompt_definition.to_jsonaf definition |> protocol_ok)
          (A.Prompt_definition.to_jsonaf actual_definition |> protocol_ok));
      assert (
        String.equal (Captured.value actual).manifest_sha256 artifact.manifest_sha256))
;;

let%expect_test "nested carrier custody and one bounded malformed-update matrix" =
  check_fresh_custody ();
  let captured = capture () in
  let target = A.Recurrence_target.to_document captured.target |> document_ok in
  let target_document =
    D.Document.json target
    |> fun json ->
    map_payload json ~f:(fun payload ->
      append payload "future_capture" (`Number "1.00e+2"))
    |> D.Document.inspect ~limits:A.Recurrence_target.limits
    |> document_ok
  in
  let target = A.Recurrence_target.of_document target_document |> document_ok in
  let captured = { captured with target } in
  let catalog, _ = fresh C.empty "nested" (Create (id "nested", captured)) in
  let raw =
    C.to_jsonaf catalog
    |> protocol_ok
    |> fun raw ->
    map_entry raw ~f:(fun payload ->
      append payload "future_entry" (`Object [ "exact", `Number "2.00e1" ]))
  in
  let catalog = C.of_jsonaf raw |> protocol_ok in
  let enabled, _ = fresh catalog "enable-nested" (Enable (id "nested")) in
  let paused = C.pause_for_stop enabled |> protocol_ok |> Option.value_exn in
  let retained_target =
    F.target (E.current (entry paused "nested"))
    |> A.Recurrence_target.to_document
    |> document_ok
    |> D.Document.json
  in
  assert (D.Json.equal retained_target (D.Document.json target_document));
  let paused_raw = C.to_jsonaf paused |> protocol_ok in
  let child =
    field paused_raw "entries"
    |> function
    | `Array (row :: _) -> field row "document"
    | _ -> assert false
  in
  assert (
    D.Json.equal
      (field (field child "payload") "future_entry")
      (`Object [ "exact", `Number "2.00e1" ]));
  let invalid =
    [ set paused_raw "entries" (`Array [])
    ; set paused_raw "receipts" (`Array [])
    ; map_entry paused_raw ~f:(fun payload ->
        set payload "control_revision" (`String "0"))
    ; map_entry paused_raw ~f:(fun payload ->
        set
          payload
          "control"
          (`Object [ "kind", `String "future_execution"; "reason", `Null ]))
    ; map_entry paused_raw ~f:(fun payload ->
        map_first_definition payload ~f:(fun document ->
          map_payload document ~f:(fun payload ->
            set
              payload
              "creator"
              (P.Id.Principal.to_json
                 (P.Id.Principal.of_string "pri_other" |> protocol_ok)))))
    ; map_entry paused_raw ~f:(fun payload -> set payload "definitions" (`Array []))
    ]
  in
  List.iter invalid ~f:(fun raw ->
    match C.of_jsonaf raw with
    | Error _ -> ()
    | Ok incoming -> assert (Result.is_error (C.validate_transition paused ~incoming)));
  let forged = P.Id.Recurrence.t_of_sexp (Sexp.Atom "not-an-id") in
  assert (
    Result.is_error
      (Command.create
         ~session_id
         ~generation:0
         ~source:None
         ~expected_lifecycle_revision:None
         ~expected_catalog_revision:0L
         ~expected_control_revision:(Some 0L)
         ~key:(P.Idempotency_key.of_string "forged" |> protocol_ok)
         ~mutation:(Pause forged)));
  let rec deep n = if n = 0 then `Null else `Array [ deep (n - 1) ] in
  assert (Result.is_error (C.of_jsonaf (append paused_raw "deep" (deep 300))));
  print_endline
    "opaque target/entry unknowns retained; erasure, rewrites, future variants and depth \
     rejected";
  [%expect
    {| opaque target/entry unknowns retained; erasure, rewrites, future variants and depth rejected |}]
;;

let%expect_test "bounded receipts and additive actual control capacity" =
  let catalog, _ = create C.empty "capacity" in
  let catalog, _ = fresh catalog "first-enable" (Enable (id "capacity")) in
  let full =
    List.fold (List.init 254 ~f:Fn.id) ~init:catalog ~f:(fun catalog n ->
      fst (fresh catalog ("saturate-" ^ Int.to_string n) (Enable (id "capacity"))))
  in
  assert (Int.equal (List.length (C.receipts full)) 256);
  assert (Result.is_error (apply full (request full "overflow" (Pause (id "capacity")))));
  let paused = C.pause_for_stop full |> protocol_ok |> Option.value_exn in
  let retired =
    C.retire_stale_births paused ~current_revision:Lifecycle.one ~archived:false
    |> protocol_ok
    |> Option.value_exn
  in
  assert (Int.equal (List.length (C.receipts retired)) 256);
  assert (
    Int.equal
      (C.reservation full |> protocol_ok |> C.Reservation.required_transition_steps)
      2);
  assert (
    Int.equal
      (C.reservation paused |> protocol_ok |> C.Reservation.required_transition_steps)
      1);
  assert (
    Int.equal
      (C.reservation retired |> protocol_ok |> C.Reservation.required_transition_steps)
      0);
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let state = { initial with recurrence_catalog = Some catalog } in
    let delta =
      A.Session_delta.Recurrence_catalog_changed
        (C.pause_for_stop catalog |> protocol_ok |> Option.value_exn)
    in
    let transition =
      A.Session_transition.apply state ~delta ~payloads:[] ~now:timestamp |> protocol_ok
    in
    assert (List.is_empty transition.events);
    let original = A.Session_state_document.authored state in
    let document =
      A.Session_state_document.encode original ~limits:document_limits |> document_ok
    in
    let capacity =
      A.Session_state_document.recurrence_capacity
        original
        ~document
        ~limits:document_limits
      |> document_ok
    in
    let next =
      A.Session_state_document.with_value original transition.state
      |> A.Session_state_document.encode ~limits:document_limits
      |> document_ok
    in
    let actual =
      A.Capacity_growth.measure (D.Document.json next) ~limits:document_limits
      |> protocol_ok
    in
    let before =
      A.Capacity_growth.measure (D.Document.json document) ~limits:document_limits
      |> protocol_ok
    in
    let reserved =
      A.Capacity_growth.add before (A.Recurrence_control_capacity.growth capacity)
      |> protocol_ok
    in
    assert (
      A.Capacity_growth.encoded_bytes actual <= A.Capacity_growth.encoded_bytes reserved);
    assert (A.Capacity_growth.fields actual <= A.Capacity_growth.fields reserved);
    assert (A.Capacity_growth.nodes actual <= A.Capacity_growth.nodes reserved);
    let terminal =
      A.Session_state_document.terminal_capacity
        original
        ~document
        ~limits:document_limits
      |> document_ok
    in
    assert (
      Result.is_error
        (A.Session_terminal_journal.validate
           state
           ~terminal
           ~recurrence:None
           ~limits:document_limits
           ~max_payload_length:1048576));
    assert (
      Result.is_error
        (A.Session_terminal_journal.validate
           state
           ~terminal
           ~recurrence:(Some capacity)
           ~limits:document_limits
           ~max_payload_length:512));
    A.Session_terminal_journal.validate
      state
      ~terminal
      ~recurrence:(Some capacity)
      ~limits:document_limits
      ~max_payload_length:1048576
    |> protocol_ok;
    let exhausted =
      { state with
        counters = { state.counters with revision = Int64.pred Int64.max_value }
      }
    in
    assert (Result.is_error (A.Session_state.validate exhausted)));
  print_endline
    "256 receipts retained; pause/retire still available; no control event; host/journal \
     reserve checked";
  [%expect
    {| 256 receipts retained; pause/retire still available; no control event; host/journal reserve checked |}]
;;
