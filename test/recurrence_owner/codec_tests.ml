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

let check_produced_run_evidence ~initial ~snapshot ~intent =
  let module Attempt = A.Recurrence_execution_attempt in
  let module Origin = A.Recurrence_run_origin in
  let module Reference = A.Recurrence_run_reference in
  let attempt =
    Attempt.create
      ~intent
      ~number:0
      ~principal_id
      ~expected_revision:initial.A.Session_state.counters.revision
      ~key:(P.Idempotency_key.of_string "produced-origin" |> protocol_ok)
    |> protocol_ok
  in
  let request = Attempt.request attempt ~intent |> protocol_ok in
  (* Actual pure producers, not host authorization/actor observation fixtures. *)
  let scope =
    A.Run_admission.Scope.create
      ~principal_id
      ~observer:(A.Recurrence_execution_intent.source intent).observer
      ~startup_pending:(fun () -> true)
      ~authorize:(fun _ -> Ok ())
    |> protocol_ok
  in
  let admitted =
    A.Run_admission.prepare_request
      initial
      ~scope
      ~request:(A.Run_admission_request.of_headless request)
      ~session:(A.Recurrence_execution_intent.target intent)
      ~run_id:(P.Id.Run.of_string "run_produced_origin" |> protocol_ok)
      ~operation:None
      ~request_sha256:(Attempt.request_sha256 attempt)
      ~now:timestamp
    |> protocol_ok
  in
  let run = A.Run_admission.run admitted in
  let origin =
    Origin.create ~intent ~attempt ~run ~admission:(A.Run_admission.receipt admitted)
    |> protocol_ok
  in
  Run_origin_tests.check ~initial ~admitted ~origin;
  let check_terminal index =
    let actual = A.Run_state.find index run.id |> Option.value_exn in
    let receipt =
      List.find_exn (A.Run_state.receipts index) ~f:(fun receipt ->
        P.Run_receipt.Kind.equal receipt.kind Terminal)
    in
    Reference.create ~origin ~run:actual ~receipt |> protocol_ok |> ignore
  in
  (* Independent alternative branches from the same retained admission. *)
  A.Run_retirement.recover
    (A.Run_admission.index admitted)
    ~session_revision:2L
    ~now:timestamp
  |> protocol_ok
  |> check_terminal;
  let state =
    A.Session_delta.apply initial (Run_state_changed (A.Run_admission.index admitted))
    |> protocol_ok
  in
  let executing, _ =
    A.Queued_moderator_event.claim_ordinary
      ~state
      ~id:(P.Id.Moderator_execution.of_string "mex_produced_origin" |> protocol_ok)
      ~snapshot
      ~operation_id:None
      ~event:Session_start
      ~now:timestamp
    |> protocol_ok
  in
  let completed =
    P.Moderator_execution.complete
      executing
      ~checkpoint_sha256:executing.context.checkpoint_sha256
      ~requests:{ request_turn = false; request_compaction = false; end_session = None }
    |> protocol_ok
  in
  let work_delta =
    A.Session_delta.Batch
      [ Moderator_execution_changed executing; Moderator_execution_changed completed ]
  in
  let transition =
    A.Run_transition.prepare
      state
      ~scope:(A.Run_scope.create ~run ~execution_id:executing.context.id |> protocol_ok)
      ~executing
      ~work_delta
      ~owned_work:[]
      ~action:None
      ~now:timestamp
    |> protocol_ok
  in
  let final =
    A.Session_delta.apply state (A.Run_transition.delta transition) |> protocol_ok
  in
  check_terminal (Option.value_exn final.run_state)
;;

let check_fresh_custody () =
  with_actor_workspace (fun env workspace_instance ->
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
      A.Recurrence_target.create_fresh
        ~mode:Single_turn
        ~artifact:captured
        ~definition
        ~parent
      |> protocol_ok
    in
    List.iter [ P.Run.Mode.Single_turn; Workflow ] ~f:(fun mode ->
      let target =
        A.Recurrence_target.create_fresh ~mode ~artifact:captured ~definition ~parent
        |> protocol_ok
        |> A.Recurrence_target.to_document
        |> document_ok
        |> A.Recurrence_target.of_document
        |> document_ok
      in
      assert (P.Run.Mode.equal (A.Recurrence_target.run_mode target |> protocol_ok) mode));
    let current =
      A.Recurrence_target.to_document target |> document_ok |> D.Document.json
    in
    let remove json name =
      match json with
      | `Object fields ->
        `Object (List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
      | _ -> assert false
    in
    let decode json =
      D.Document.inspect ~limits:A.Recurrence_target.limits json
      |> Result.bind ~f:A.Recurrence_target.of_document
    in
    let historical =
      map_payload current ~f:(fun payload -> remove payload "mode")
      |> fun json -> remove json "required_semantics"
    in
    let legacy = decode historical |> document_ok in
    assert (Result.is_error (A.Recurrence_target.run_mode legacy));
    assert (
      D.Json.equal
        historical
        (A.Recurrence_target.to_document legacy |> document_ok |> D.Document.json));
    List.iter
      [ map_payload current ~f:(fun payload -> set payload "mode" `Null)
      ; map_payload current ~f:(fun payload -> set payload "mode" (`String "implicit"))
      ; remove current "required_semantics"
      ]
      ~f:(fun json -> assert (Result.is_error (decode json)));
    let capture = { (capture ()) with target } in
    let catalog, _ = fresh C.empty "fresh-custody" (Create (id "fresh", capture)) in
    let restored = roundtrip catalog in
    let retained_definition = E.current (entry restored "fresh") in
    let restored_target = F.target retained_definition in
    let module Reservation = A.Recurrence_child_reservation in
    let module Association = A.Recurrence_child_association in
    let module Intent = A.Recurrence_execution_intent in
    let module Store = Agent_store.Delegation_store in
    let nominal =
      A.Recurrence_nominal.create_interval retained_definition ~slot:0L |> protocol_ok
    in
    let reserve suffix =
      Reservation.create
        ~definition:retained_definition
        ~control_revision:0L
        ~principal_id
        ~child_session_id:(P.Id.Session.of_string ("ses_intent_" ^ suffix) |> protocol_ok)
        ~transaction_id:
          (P.Id.Transaction.of_string ("txn_intent_" ^ suffix) |> protocol_ok)
        ~nominal_json:(A.Recurrence_nominal.to_json nominal)
      |> protocol_ok
    in
    let reservation = reserve "original" in
    let key = Reservation.delegation_key reservation |> protocol_ok in
    (* Pure validated reference data, not proof of child installation/linkage. *)
    let reference =
      Store.reference_of_jsonaf
        (`Object
            [ ( "key"
              , `Object
                  [ "parent_session_id", P.Id.Session.to_json key.parent_session_id
                  ; "parent_generation", `Number (Int.to_string key.parent_generation)
                  ; "principal_id", P.Id.Principal.to_json key.principal_id
                  ; "idempotency_key", P.Idempotency_key.to_json key.idempotency_key
                  ] )
            ; ( "child_session_id"
              , P.Id.Session.to_json (Reservation.child_session_id reservation) )
            ; ( "revision_id"
              , P.Id.Prompt_revision.to_json
                  (Reservation.artifact_revision_id reservation) )
            ; "request_sha256", `String (Reservation.request_sha256 reservation)
            ; "admission_sha256", `String (String.make 64 'b')
            ])
      |> store_ok
    in
    let association = Association.create ~reservation ~reference |> protocol_ok in
    let child = Intent.Child.create ~reservation ~association |> protocol_ok in
    assert (
      Result.is_error (Intent.Child.create ~reservation:(reserve "mixed") ~association));
    let manager, _, _ = handoff_definition env in
    let snapshot =
      Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
    in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let initial =
      A.Session_state.create
        ~identity:
          { initial.identity with session_id = Reservation.child_session_id reservation }
        ~spec:initial.spec
        ~initial_history:[]
    in
    let initial =
      { initial with moderator = Some (A.Moderator_checkpoint.encode snapshot) }
    in
    let observer =
      A.Moderator_checkpoint.observer initial.moderator |> protocol_ok |> Option.value_exn
    in
    let child_source =
      A.Run_source_installation.apply
        A.Run_source_installation.initial
        ~change:(Replace observer)
      |> protocol_ok
      |> A.Run_source_installation.captured ~generation:initial.identity.generation
      |> protocol_ok
    in
    let intent =
      Intent.create
        ~definition:retained_definition
        ~control_revision:0L
        ~nominal_json:(Reservation.nominal_json reservation)
        ~target:
          (P.Session_ref.create
             ~server_id
             ~session_id:(Reservation.child_session_id reservation))
        ~source:child_source
        ~child:(Some child)
      |> protocol_ok
      |> Intent.to_json
      |> Intent.of_json
      |> protocol_ok
    in
    Intent.validate intent ~definition:retained_definition ~reservation:(Some reservation)
    |> protocol_ok;
    assert (
      Result.is_error
        (Intent.validate
           intent
           ~definition:retained_definition
           ~reservation:(Some (reserve "mixed"))));
    (match Intent.input intent with
     | Authored_start -> ()
     | User_submission _ -> assert false);
    check_produced_run_evidence ~initial ~snapshot ~intent;
    match A.Recurrence_target.view restored_target with
    | Existing_session _ -> assert false
    | Fresh_child
        { artifact = actual; definition = actual_definition; parent = _; mode = _ } ->
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
