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

let check_goal_definition_storage ~initial catalog =
  let module State = A.Session_state_document in
  let module Delta = A.Session_delta_document in
  let module Store = Agent_store in
  let marker = P.Session_storage_semantics.recurrence_controller_goal in
  let state = { initial with A.Session_state.recurrence_catalog = Some catalog } in
  let state_document =
    State.encode (State.authored state) ~limits:document_limits |> document_ok
  in
  let check_marker document ~decode =
    let markers = D.Document.required_semantics document in
    assert (List.mem markers marker ~equal:String.equal);
    let without_marker =
      List.filter markers ~f:(fun name -> not (String.equal name marker))
    in
    let unmarked =
      set
        (D.Document.json document)
        "required_semantics"
        (`Array (List.map without_marker ~f:(fun name -> `String name)))
      |> D.Document.inspect ~limits:document_limits
      |> document_ok
    in
    assert (Result.is_error (decode unmarked));
    let old_codec =
      D.Domain_codec.create
        ~limits:document_limits
        ~kind:(D.Document.kind document)
        ~version:(D.Document.version document)
        ~shape:D.Shape.value
        ~supported_semantics:without_marker
        ~decode:Result.return
        ~encode:Result.return
      |> document_ok
    in
    match D.Domain_codec.decode old_codec document with
    | Error (D.Error.Required_semantics_unknown name) -> assert (String.equal name marker)
    | Error _ | Ok _ ->
      failwith "old decoder accepted or misclassified a Goal-bound child"
  in
  check_marker state_document ~decode:(State.decode ~limits:document_limits);
  let snapshot =
    Store.Snapshot.create
      ~limits:document_limits
      ~session_id:state.identity.session_id
      ~transaction_sequence:0L
      ~transaction_hash:None
      ~event_sequence:0L
      ~created_at:state.identity.updated_at
      ~prompt_artifact:(P.Id.Prompt_revision.to_string state.spec.prompt_revision_id)
      ~workspace_identity:state.spec.workspace_instance.conflict_domain
      ~payload:state_document
    |> store_ok
  in
  let frame =
    Store.Snapshot.stored snapshot
    |> Store.Snapshot.Stored.record
    |> Store.Document_record.document
    |> Store.Document_record.encode ~limits:document_limits ~flags:0
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Store.Document_record.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let restored =
    Store.Snapshot.decode_file
      ~max_payload_length:(D.Limits.max_bytes document_limits)
      frame
    |> store_ok
  in
  assert (D.Json.equal (D.Document.json restored.payload) (D.Document.json state_document));
  let initial_document =
    State.encode (State.authored initial) ~limits:document_limits
    |> document_ok
    |> State.decode ~limits:document_limits
    |> document_ok
  in
  List.iter
    [ A.Session_delta.Recurrence_catalog_changed catalog; Created state ]
    ~f:(fun value ->
      let delta =
        Delta.create value ~limits:document_limits ~state_document:State.authored
        |> document_ok
        |> Delta.document
      in
      check_marker delta ~decode:(Delta.decode ~limits:document_limits);
      let transaction =
        Store.Transaction.create
          ~limits:document_limits
          ~session_id:state.identity.session_id
          ~generation:state.identity.generation
          ~transaction_sequence:1L
          ~previous_transaction_hash:None
          ~session_revision:1L
          ~first_event_sequence:None
          ~last_event_sequence:None
          ~accepted_at_ns:0L
          ~command_audit:None
          ~delta
          ~durable_events:[]
        |> store_ok
      in
      let decoded =
        Store.Transaction.decode (Store.Transaction.encode transaction) |> store_ok
      in
      let replayed = Delta.decode ~limits:document_limits decoded.delta |> document_ok in
      assert (
        D.Json.equal (D.Document.json (Delta.document replayed)) (D.Document.json delta));
      let replay =
        Delta.apply replayed ~limits:document_limits initial_document
        |> document_ok
        |> State.value
      in
      let restored_catalog = Option.value_exn replay.recurrence_catalog in
      assert (
        D.Json.equal
          (C.to_jsonaf catalog |> protocol_ok)
          (C.to_jsonaf restored_catalog |> protocol_ok)))
;;

let check_goal_definition ~initial capture historical old_receipt =
  let module Binding = F.Goal_binding in
  let module Request = Command.Goal_owner_request in
  let module Receipt = A.Recurrence_command_receipt in
  let marker = P.Session_storage_semantics.recurrence_controller_goal in
  let source = F.source historical in
  let key = F.key historical in
  let goal_id = P.Id.Goal.of_string "goal_recurrence_codec" |> protocol_ok in
  (* Pure persisted evidence. The actual Actor helper has its own checks. *)
  let run =
    P.Run.create
      ~id:(P.Id.Run.of_string "run_recurrence_goal_codec" |> protocol_ok)
      ~session
      ~principal_id
      ~source
      ~mode:Workflow
      ~lifecycle:Admitted
      ~revision:0L
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:timestamp
      ~updated_at:timestamp
    |> protocol_ok
  in
  let admission =
    P.Run_receipt.create
      ~run_id:run.id
      ~principal_id
      ~source
      ~key:(P.Idempotency_key.of_string "admit-recurrence-codec" |> protocol_ok)
      ~request_sha256:(String.make 64 'a')
      ~kind:Admission
      ~run_revision:0L
      ~session_revision:1L
      ~committed_at:timestamp
    |> protocol_ok
  in
  let creator_episode = P.Goal_episode.create ~run ~admission |> protocol_ok in
  let capture_binding revision =
    Binding.Capture.create
      ~owner:session
      ~goal_id
      ~creator_episode
      ~goal_revision_at_capture:revision
    |> protocol_ok
  in
  let binding = capture_binding 1L in
  let create ~key ~source ~target constructor =
    constructor
      ~key
      ~creator:principal_id
      ~source
      ~birth_lifecycle_revision:Lifecycle.zero
      ~schedule:(F.schedule historical)
      ~target
      ~policy:(F.policy historical)
      ~created_at:timestamp
  in
  let target = F.target historical in
  let positive = create ~key ~source ~target (F.create_bound ~binding) |> protocol_ok in
  let document = F.to_document positive in
  assert (List.equal String.equal (D.Document.required_semantics document) [ marker ]);
  let old_document = F.to_document historical in
  let target_document = A.Recurrence_target.to_document target |> document_ok in
  let legacy_document =
    D.Document.create
      ~kind:"ochat.recurrence_definition"
      ~version:1
      ~limits:document_limits
      ~payload:
        (`Object
            [ "key", A.Recurrence_identity.Definition.to_json key
            ; "creator", P.Id.Principal.to_json principal_id
            ; "source", P.Run_source.to_json source
            ; "birth_lifecycle_revision", `String "0"
            ; "schedule", F.Schedule.to_json (F.schedule historical)
            ; "target", D.Document.json target_document
            ; "policy", A.Recurrence_policy.to_json (F.policy historical)
            ; "created_at", P.Timestamp.to_json timestamp
            ])
    |> document_ok
  in
  let canonical_sha256 json =
    P.Json_codec.canonical_string json
    |> protocol_ok
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  assert (
    String.equal
      (D.Document.to_string old_document)
      (D.Document.to_string legacy_document));
  let restored_old = F.of_document legacy_document |> document_ok in
  assert (
    String.equal
      (F.digest restored_old)
      (canonical_sha256 (D.Document.json legacy_document)));
  let remove json name =
    match json with
    | `Object fields -> `Object (List.Assoc.remove fields ~equal:String.equal name)
    | _ -> assert false
  in
  let without_binding =
    map_payload (D.Document.json document) ~f:(fun payload ->
      remove payload "goal_binding")
    |> fun json -> remove json "required_semantics"
  in
  assert (String.equal (F.digest historical) (canonical_sha256 without_binding));
  assert (Binding.equal (F.goal_binding historical) Unbound);
  let decoded = F.of_document document |> document_ok in
  assert (Binding.equal (F.goal_binding decoded) (Controller_goal binding));
  assert (String.equal (F.digest decoded) (F.digest positive));
  assert (not (String.equal (F.digest historical) (F.digest positive)));
  let decode raw =
    D.Document.inspect ~limits:document_limits raw |> Result.bind ~f:F.of_document
  in
  let raw = D.Document.json document in
  let annotated =
    map_payload raw ~f:(fun payload ->
      set
        payload
        "goal_binding"
        (append (field payload "goal_binding") "future" (`Number "1.00e+2")))
  in
  let retained = decode annotated |> document_ok in
  assert (
    String.equal
      (D.Document.to_string (F.to_document retained))
      (Jsonaf.to_string annotated));
  assert (Binding.equal (F.goal_binding retained) (Controller_goal binding));
  assert (not (String.equal (F.digest retained) (F.digest positive)));
  List.iter
    [ remove raw "required_semantics"
    ; map_payload raw ~f:(fun payload -> remove payload "goal_binding")
    ; map_payload raw ~f:(fun payload -> set payload "goal_binding" `Null)
    ]
    ~f:(fun json -> assert (Result.is_error (decode json)));
  let another_owner =
    P.Session_ref.create
      ~server_id:(P.Id.Server.of_string "srv_other" |> protocol_ok)
      ~session_id
  in
  let key_for ~owner ~revision =
    A.Recurrence_identity.Definition.create
      ~controller:owner
      ~recurrence_id:(E.id (E.create historical |> protocol_ok))
      ~revision
    |> protocol_ok
  in
  assert (
    Result.is_error
      (create
         ~key:(key_for ~owner:another_owner ~revision:0L)
         ~source
         ~target
         (F.create_bound ~binding)));
  assert (
    Result.is_error
      (create ~key ~source:(Support.source 2L) ~target (F.create_bound ~binding)));
  assert (
    Result.is_error
      (create ~key ~source ~target:(Support.capture ()).target (F.create_bound ~binding)));
  let next_key = key_for ~owner:session ~revision:1L in
  let edit entry constructor =
    let next = create ~key:next_key ~source ~target constructor |> protocol_ok in
    E.edit entry next
  in
  let entry = E.create positive |> protocol_ok in
  edit entry (F.create_bound ~binding) |> protocol_ok |> ignore;
  assert (Result.is_error (edit entry F.create));
  assert (Result.is_error (edit entry (F.create_bound ~binding:(capture_binding 2L))));
  assert (
    Result.is_error (edit (E.create historical |> protocol_ok) (F.create_bound ~binding)));
  let ordinary = request C.empty "fresh-custody" (Create (E.id entry, capture)) in
  let legacy_command =
    `Object
      [ "kind", `String "recurrence_command_v1"
      ; "session_id", P.Id.Session.to_json session_id
      ; "generation", `String "0"
      ; "source", P.Run_source.to_json source
      ; "expected_lifecycle_revision", `String "0"
      ; "expected_catalog_revision", `String "0"
      ; "expected_control_revision", `Null
      ; "key", P.Idempotency_key.to_json ordinary.key
      ; ( "mutation"
        , `Object
            [ "kind", `String "create"
            ; "recurrence_id", P.Id.Recurrence.to_json (E.id entry)
            ; ( "capture"
              , `Object
                  [ "schedule", F.Schedule.to_json (F.schedule historical)
                  ; ( "target_sha256"
                    , `String (A.Recurrence_target.digest target |> protocol_ok) )
                  ; "policy", A.Recurrence_policy.to_json (F.policy historical)
                  ] )
            ] )
      ]
  in
  let legacy_digest = canonical_sha256 legacy_command in
  assert (String.equal (Command.digest ordinary |> protocol_ok) legacy_digest);
  assert (String.equal legacy_digest old_receipt.Receipt.request_sha256);
  let with_owner command expected_goal_revision =
    let goal = Request.create ~goal_id ~expected_goal_revision |> protocol_ok in
    Command.with_goal_owner command goal
  in
  let bound_request = with_owner ordinary 1L |> protocol_ok in
  let digest = Command.digest bound_request |> protocol_ok in
  assert (not (String.equal digest old_receipt.request_sha256));
  assert (
    not
      (String.equal
         digest
         (Command.digest (with_owner ordinary 2L |> protocol_ok) |> protocol_ok)));
  let existing =
    request C.empty "existing-goal-control" (Create (id "existing", Support.capture ()))
  in
  assert (Result.is_error (with_owner existing 1L));
  assert (Result.is_error (apply C.empty bound_request));
  let ordinary_catalog, retained = apply C.empty ordinary |> committed in
  (match apply ordinary_catalog ordinary |> protocol_ok with
   | Retained receipt -> assert (Receipt.equal receipt retained)
   | Commit _ -> failwith "exact ordinary replay unexpectedly published");
  assert (Result.is_error (apply ordinary_catalog bound_request));
  let receipt =
    Receipt.of_json (set (Receipt.to_json old_receipt) "request_sha256" (`String digest))
    |> protocol_ok
  in
  let catalog = C.commit C.empty ~entry ~receipt |> protocol_ok in
  let restored = roundtrip catalog in
  assert (
    String.equal
      (F.digest (E.current (C.find restored (E.id entry) |> Option.value_exn)))
      (F.digest positive));
  check_goal_definition_storage ~initial catalog
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
    let catalog, receipt = fresh C.empty "fresh-custody" (Create (id "fresh", capture)) in
    let restored = roundtrip catalog in
    let retained_definition = E.current (entry restored "fresh") in
    check_goal_definition
      ~initial:
        (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false)
      capture
      retained_definition
      receipt;
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
