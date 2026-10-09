open Core
open Fixtures

let%test_unit "subscription storage preserves absent, null and object raw schemas" =
  with_actor_workspace (fun _env workspace_instance ->
    let module S = Agent_session.Session_state in
    let module D = Agent_session.Session_state_document in
    let module U = Agent_protocol.Subscription in
    let _, staged, _, _, _ = extension_fixture workspace_instance in
    assert (not (List.is_empty staged.subscriptions));
    List.iter
      [ None; Some `Null; Some (`Object [ "type", `String "object" ]) ]
      ~f:(fun completion_schema ->
        let subscriptions =
          List.map staged.subscriptions ~f:(fun (subscription : U.t) ->
            let fresh =
              U.create { subscription.context with completion_schema } |> protocol_ok
            in
            let updated =
              U.finish
                fresh
                ~expected_epoch:0
                ~now:timestamp
                (Option.value_exn subscription.result)
              |> protocol_ok
              |> fst
            in
            let json = U.Storage.to_json updated in
            let fields = Agent_protocol.Json_codec.fields json |> protocol_ok in
            assert (
              Option.equal
                Jsonaf.exactly_equal
                (Agent_protocol.Json_codec.optional fields "completion_schema")
                completion_schema);
            assert (U.equal updated (U.Storage.of_json json |> protocol_ok));
            let delta =
              Agent_session.Session_delta_document.create
                (Subscription_changed updated)
                ~limits:document_limits
                ~state_document:D.authored
              |> document_ok
              |> Agent_session.Session_delta_document.value
            in
            (match delta with
             | Batch [ Subscription_changed restored ] ->
               assert (U.equal updated restored)
             | _ -> assert false);
            updated)
        in
        let state = { staged with subscriptions } in
        S.validate state |> protocol_ok;
        let document =
          D.encode (D.authored state) ~limits:document_limits |> document_ok
        in
        let restored =
          D.decode ~limits:document_limits document |> document_ok |> D.value
        in
        assert_same_session_snapshot state restored))
;;

let%test_unit "authored moderator deltas reject malformed present native records" =
  let module D = Agent_session.Session_delta_document in
  let capture value =
    D.create
      value
      ~limits:document_limits
      ~state_document:Agent_session.Session_state_document.authored
  in
  let valid =
    Agent_session.Runtime_builder.encode_moderator_snapshot (handoff_snapshot 7)
  in
  List.iter
    [ Some `Null; Some (`Object []) ]
    ~f:(fun moderator ->
      assert (Result.is_error (capture (Moderator_changed moderator)));
      assert (
        Result.is_error
          (capture
             (Batch [ Moderator_changed (Some valid); Moderator_changed moderator ]))));
  List.iter [ None; Some valid ] ~f:(fun moderator ->
    match capture (Moderator_changed moderator) |> document_ok |> D.value with
    | Batch [ Moderator_changed restored ] ->
      assert (Option.equal Jsonaf.exactly_equal moderator restored)
    | _ -> assert false)
;;

let%test_unit "named state codec preserves complete values with present optional records" =
  with_actor_workspace (fun _env workspace_instance ->
    let module S = Agent_session.Session_state in
    let module D = Agent_session.Session_state_document in
    let initial, staged, _, _, _ = extension_fixture workspace_instance in
    let scope =
      Chat_response.Authoring_materialization.session_scope ~session_id ~generation:0
    in
    let references =
      Chat_response.Authoring_reference_index.empty ~scope ()
      |> protocol_ok
      |> Chat_response.Authoring_reference_index.to_json
    in
    let populated =
      { staged with
        identity = { staged.identity with labels = [ "", "empty key"; "owner", "test" ] }
      ; spec =
          { staged.spec with
            protocol =
              { staged.spec.protocol with labels = [ "", "empty key"; "owner", "test" ] }
          ; prompt_definition_id = Some prompt_id
          ; runtime_policy = Some "retained policy"
          ; quota_key = Some { conflict_domain = "fixture"; prompt_id }
          }
      ; automatic_turn_budget =
          Some
            (Agent_session.Automatic_turn_budget.create
               Chat_response.Runtime_semantics.default_policy)
      ; conversation =
          { staged.conversation with
            kv_store = [ "", "empty key" ]
          ; tasks = [ `Null; `Object [ "text", `String "retained task" ] ]
          ; authoring_reference_index = Some references
          ; authoring_publication =
              Some
                (Chat_response.Authoring_publication.context_of_jsonaf
                   (`Object
                       [ "version", `Number "1"
                       ; "scope", `String scope
                       ; "identity", `String (String.make 64 'a')
                       ; "policy", `String (String.make 64 'b')
                       ])
                 |> protocol_ok)
          }
      ; invocations =
          List.map
            staged.invocations
            ~f:(fun (invocation : Agent_protocol.Invocation.t) ->
              let module I = Agent_protocol.Invocation in
              I.create { invocation.context with deadline = Some timestamp }
              |> protocol_ok
              |> I.dispatch
              |> protocol_ok
              |> fun updated ->
              I.resolve
                updated
                ~session_id
                ~generation:0
                (match invocation.status with
                 | Resolved outcome | Published outcome -> outcome
                 | Admitted | Dispatching -> assert false)
              |> protocol_ok)
      ; subscriptions =
          List.map
            staged.subscriptions
            ~f:(fun (subscription : Agent_protocol.Subscription.t) ->
              let module U = Agent_protocol.Subscription in
              let fresh =
                U.create { subscription.context with completion_schema = Some `True }
                |> protocol_ok
              in
              U.finish
                fresh
                ~expected_epoch:0
                ~now:timestamp
                (Option.value_exn subscription.result)
              |> protocol_ok
              |> fst)
      ; attachments =
          [ { id = Agent_protocol.Id.Attachment.create ()
            ; session_id
            ; mode = Owner_read_write
            ; owner_lease =
                Some
                  { generation = 1L
                  ; expires_at = timestamp
                  ; disconnect_grace_until = Some timestamp
                  ; principal_id = Some principal_id
                  ; reclaim_token_sha256 = Some (String.make 64 'c')
                  }
            }
          ]
      ; moderator =
          Some
            (Agent_session.Runtime_builder.encode_moderator_snapshot (handoff_snapshot 7))
      ; halted = true
      ; halt_reason = Some "retained halt"
      ; failure = Some (Agent_protocol.Error.invalid_request "retained failure")
      }
    in
    List.iter [ initial; populated ] ~f:(fun state ->
      S.validate state |> protocol_ok;
      let document = D.encode (D.authored state) ~limits:document_limits |> document_ok in
      let restored =
        D.decode ~limits:document_limits document |> document_ok |> D.value
      in
      assert_same_session_snapshot state restored))
;;

let%test_unit
    "authored null state cannot erase invalid optional records before validation"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let module S = Agent_session.Session_state in
    let module D = Agent_session.Session_state_document in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    List.iter
      [ { initial with moderator = Some `Null }
      ; { initial with
          conversation =
            { initial.conversation with authoring_reference_index = Some `Null }
        }
      ]
      ~f:(fun invalid ->
        assert (Result.is_error (S.validate invalid));
        assert (Result.is_error (D.encode (D.authored invalid) ~limits:document_limits))))
;;

let%expect_test
    "event execution journal recovery preserves outcomes and prevents intent replay"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let module E = Agent_protocol.Moderator_execution in
    let module S = Agent_session.Session_state in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let make name =
      E.create
        { id =
            Agent_protocol.Id.Moderator_execution.of_string ("mex_" ^ name) |> protocol_ok
        ; session_id
        ; generation = 0
        ; source = { script_id = "private-script"; source_sha256 = String.make 64 'a' }
        ; operation_id = None
        ; job = None
        ; phase = Internal_event
        ; event = `String "private event payload"
        ; checkpoint_sha256 = String.make 64 'b'
        ; created_at = timestamp
        }
      |> protocol_ok
    in
    let waiting_start = make "waiting" in
    let completed =
      E.complete
        waiting_start
        ~checkpoint_sha256:(String.make 64 'c')
        ~requests:{ request_turn = true; request_compaction = true; end_session = None }
      |> protocol_ok
    in
    let waiting = E.accept_compaction completed ~operation_id |> protocol_ok in
    let applied = E.apply_intent waiting |> protocol_ok in
    assert (
      Option.equal
        Agent_protocol.Id.Operation.equal
        applied.compaction_operation_id
        (Some operation_id));
    assert (Result.is_error (E.apply_intent applied));
    let pending_start = make "pending" in
    let pending =
      E.complete
        pending_start
        ~checkpoint_sha256:(String.make 64 'c')
        ~requests:{ request_turn = true; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let failed_start = make "failed" in
    let failed =
      E.fail
        failed_start
        { code = "event.failed"
        ; message = "private failure"
        ; retryable = false
        ; details = `Null
        }
      |> protocol_ok
    in
    let running = make "running" in
    List.iter
      [ waiting_start; completed; waiting; pending; failed; running ]
      ~f:(fun receipt ->
        assert (E.equal receipt (E.of_json (E.to_json receipt) |> protocol_ok)));
    let deltas =
      List.map
        [ waiting_start
        ; completed
        ; waiting
        ; pending_start
        ; pending
        ; failed_start
        ; failed
        ; running
        ]
        ~f:(fun receipt ->
          Agent_session.Session_delta.Moderator_execution_changed receipt)
    in
    let transition =
      Agent_session.Session_transition.apply
        ~now:timestamp
        initial
        ~delta:(Batch deltas)
        ~payloads:[]
      |> protocol_ok
    in
    let journal =
      Agent_store.Transaction.create
        ~limits:document_limits
        ~session_id
        ~generation:0
        ~transaction_sequence:transition.state.counters.transaction_sequence
        ~previous_transaction_hash:None
        ~session_revision:transition.state.counters.revision
        ~first_event_sequence:
          (Some (List.hd_exn transition.events).Agent_protocol.Event.Durable.sequence)
        ~last_event_sequence:
          (Some (List.last_exn transition.events).Agent_protocol.Event.Durable.sequence)
        ~accepted_at_ns:
          (Agent_protocol.Timestamp.to_time_ns timestamp
           |> Time_ns.to_int_ns_since_epoch
           |> Int64.of_int)
        ~command_audit:None
        ~delta:(delta_document transition.delta)
        ~durable_events:
          (List.map transition.events ~f:(fun event -> event_document event))
      |> store_ok
      |> Agent_store.Transaction.encode
      |> Agent_store.Transaction.decode
      |> store_ok
    in
    let replayed = replay_transaction initial journal |> store_ok in
    assert_same_session_snapshot transition.state replayed;
    let restored = restore_state replayed |> store_ok in
    assert_same_session_snapshot replayed restored;
    let projected =
      S.extension_status restored |> List.map ~f:Agent_protocol.Extension_status.to_json
    in
    let encoded_projection = Jsonaf.to_string (`Array projected) in
    assert (not (String.is_substring encoded_projection ~substring:"private"));
    assert (
      List.length
        (Agent_protocol.Extension_status.list_of_json (`Array projected) |> protocol_ok)
      = 4);
    ignore
      (Agent_session.Session_persistence.durable_events ~limits:document_limits journal
       |> store_ok
       : Agent_protocol.Event.Durable.t list);
    let plan state =
      Agent_session.Invocation_recovery.plan
        ~state
        ~namespace:"event-recovery"
        ~first_sequence:0
        ~reason:"owner interrupted"
      |> protocol_ok
    in
    let recovery = plan restored in
    assert (List.is_empty recovery.appended && recovery.next_sequence = 0);
    let recovered =
      List.fold_result recovery.deltas ~init:restored ~f:Agent_session.Session_delta.apply
      |> protocol_ok
    in
    S.validate recovered |> protocol_ok;
    assert (List.is_empty (plan recovered).deltas);
    let lookup name =
      List.find_exn recovered.moderator_executions ~f:(fun receipt ->
        String.equal
          (Agent_protocol.Id.Moderator_execution.to_string receipt.E.context.id)
          ("mex_" ^ name))
    in
    let interrupted = lookup "running" in
    let retained = lookup "waiting" in
    assert (E.equal failed (lookup "failed"));
    assert (E.equal pending (lookup "pending"));
    assert (
      Option.equal
        Agent_protocol.Id.Operation.equal
        retained.compaction_operation_id
        (Some operation_id));
    assert (E.equal waiting retained);
    assert (
      Result.is_error
        (E.complete
           interrupted
           ~checkpoint_sha256:(String.make 64 'c')
           ~requests:
             { request_turn = false; request_compaction = false; end_session = None }));
    assert (
      Result.is_error
        (E.accept_compaction
           waiting
           ~operation_id:
             (Agent_protocol.Id.Operation.of_string "op_rebound" |> protocol_ok)));
    let changed_source =
      E.create
        { running.context with
          source = { running.context.source with source_sha256 = String.make 64 'd' }
        }
      |> protocol_ok
    in
    assert (
      Result.is_error (E.validate_transition ~previous:(Some running) changed_source));
    assert (
      Result.is_error
        (S.validate { initial with moderator_executions = [ make "one"; make "two" ] }));
    assert (Result.is_error (S.upgrade_schema { restored with schema_version = 4 }));
    let migrated = restore_state initial |> store_ok in
    let older =
      { recovered with
        identity = { recovered.identity with generation = 1 }
      ; inference_ledger =
          Agent_session.Inference_ledger.with_generation
            recovered.inference_ledger
            ~generation:1
          |> Result.map_error ~f:(fun error ->
            Sexp.to_string_hum (Agent_session.Inference_ledger.Error.sexp_of_t error))
          |> Result.ok_or_failwith
      }
    in
    let retired =
      List.fold_result
        (plan older).deltas
        ~init:older
        ~f:Agent_session.Session_delta.apply
      |> protocol_ok
    in
    let states state =
      S.extension_status state
      |> List.map ~f:(fun s -> s.Agent_protocol.Extension_status.id, s.state)
    in
    print_s
      [%sexp
        { schema = (migrated.schema_version : int)
        ; recovered = (states recovered : (string * string) list)
        ; after_reset = (states retired : (string * string) list)
        ; provider_entries = (List.length recovered.conversation.canonical_history : int)
        }]);
  [%expect
    {|
    ((schema 24)
     (recovered
      ((mex_failed failed) (mex_pending completed.pending)
       (mex_running interrupted) (mex_waiting completed.waiting_compaction)))
     (after_reset
      ((mex_failed failed) (mex_pending completed.discarded)
       (mex_running interrupted) (mex_waiting completed.discarded)))
     (provider_entries 0))
    |}]
;;

let%expect_test "invocation deltas replay through durable transactions and snapshots" =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let admitted = invocation_fixture () in
    let dispatched = Agent_protocol.Invocation.dispatch admitted |> protocol_ok in
    let resolved =
      Agent_protocol.Invocation.resolve
        dispatched
        ~session_id
        ~generation:0
        (Complete (`String "done"))
      |> protocol_ok
    in
    let delta =
      Agent_session.Session_delta.Batch
        [ Invocation_changed admitted
        ; Invocation_changed dispatched
        ; Invocation_changed resolved
        ]
    in
    let transaction =
      Agent_store.Transaction.create
        ~limits:document_limits
        ~session_id
        ~generation:0
        ~transaction_sequence:1L
        ~previous_transaction_hash:None
        ~session_revision:1L
        ~first_event_sequence:None
        ~last_event_sequence:None
        ~accepted_at_ns:
          (Agent_protocol.Timestamp.to_time_ns timestamp
           |> Time_ns.to_int_ns_since_epoch
           |> Int64.of_int)
        ~command_audit:None
        ~delta:(delta_document delta)
        ~durable_events:[]
      |> store_ok
    in
    let transaction =
      Agent_store.Transaction.decode (Agent_store.Transaction.encode transaction)
      |> store_ok
    in
    let replayed = replay_transaction initial transaction |> store_ok in
    let restored = restore_state replayed |> store_ok in
    let invocation = List.hd_exn restored.invocations in
    print_s [%sexp (invocation.status : Agent_protocol.Invocation.status)];
    let published = Agent_protocol.Invocation.publish invocation |> protocol_ok in
    let final =
      Agent_session.Session_delta.apply restored (Invocation_changed published)
      |> protocol_ok
    in
    print_s
      [%sexp ((List.hd_exn final.invocations).status : Agent_protocol.Invocation.status)];
    let repeated_resolution =
      Agent_session.Session_delta.apply final (Invocation_changed resolved)
    in
    print_s [%sexp (Result.is_error repeated_resolution : bool)]);
  [%expect
    {|
    (Resolved (Complete (String done)))
    (Published (Complete (String done)))
    true |}]
;;

let%test_unit "bound publication journal replay validates the retained call and output" =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let id sequence =
      History_entry.Id.create ~namespace:"publication" ~sequence |> Result.ok_or_failwith
    in
    let call =
      Openai.Responses_history.create_with_id_exn
        ~id:(id 0)
        (Openai.Responses.Item.Function_call
           { name = "read_file"
           ; arguments = "{}"
           ; call_id = "call"
           ; _type = "function_call"
           ; id = None
           ; status = None
           })
    in
    let admitted =
      Agent_protocol.Invocation.create
        ~routing:
          (let fingerprint payload =
             Agent_protocol.Invocation.
               { sha256 = Chatmd_shell_spec.Source_ref.digest payload
               ; byte_length = String.length payload
               }
           in
           { kind = Function
           ; original_name = "alias"
           ; original_payload = fingerprint "original private input"
           ; final_payload = fingerprint "private execution input"
           ; canonical_payload = Some (fingerprint "{}")
           ; preparation = Passed
           })
        { (invocation_fixture ()).context with
          origin = Model
        ; provider_call_id = Some "call"
        ; call_entry_id = Some (id 0)
        }
      |> protocol_ok
    in
    let routing = Option.value_exn admitted.routing in
    List.iter
      [ { routing with kind = Custom }
      ; { routing with
          canonical_payload = Some { sha256 = String.make 64 'a'; byte_length = 2 }
        }
      ]
      ~f:(fun routing ->
        let wrong =
          Agent_protocol.Invocation.create ~routing admitted.context |> protocol_ok
        in
        assert (
          Result.is_error
            (Agent_session.Session_delta.apply
               initial
               (Batch
                  [ Canonical_entries_appended
                      [ Agent_session.History_codec.to_protocol call ]
                  ; Invocation_changed wrong
                  ]))));
    let dispatched = Agent_protocol.Invocation.dispatch admitted |> protocol_ok in
    let resolved =
      Agent_protocol.Invocation.resolve
        dispatched
        ~session_id
        ~generation:0
        (Complete (`String "done"))
      |> protocol_ok
    in
    let output =
      Openai.Responses_history.create_with_id_exn
        ~id:(id 1)
        (Openai.Responses.Item.Function_call_output
           { output =
               Text
                 (Jsonaf.to_string
                    (Agent_protocol.Invocation.outcome_to_json
                       (Complete (`String "done"))))
           ; call_id = "call"
           ; _type = "function_call_output"
           ; id = None
           ; status = None
           })
    in
    let published =
      Agent_protocol.Invocation.publish_with_history resolved ~output_entry_id:(id 1)
      |> protocol_ok
    in
    let prefix =
      Agent_session.Session_delta.
        [ Canonical_entries_appended [ Agent_session.History_codec.to_protocol call ]
        ; Invocation_changed admitted
        ; Invocation_changed dispatched
        ; Invocation_changed resolved
        ]
    in
    assert (
      Result.is_error
        (Agent_session.Session_delta.apply
           initial
           (Batch (prefix @ [ Invocation_changed published ]))));
    let delta =
      Agent_session.Session_delta.Batch
        (prefix
         @ [ Canonical_entries_appended [ Agent_session.History_codec.to_protocol output ]
           ; Invocation_changed published
           ])
    in
    let transaction =
      Agent_store.Transaction.create
        ~limits:document_limits
        ~session_id
        ~generation:0
        ~transaction_sequence:1L
        ~previous_transaction_hash:None
        ~session_revision:1L
        ~first_event_sequence:None
        ~last_event_sequence:None
        ~accepted_at_ns:
          (Agent_protocol.Timestamp.to_time_ns timestamp
           |> Time_ns.to_int_ns_since_epoch
           |> Int64.of_int)
        ~command_audit:None
        ~delta:(delta_document delta)
        ~durable_events:[]
      |> store_ok
    in
    let transaction =
      Agent_store.Transaction.decode (Agent_store.Transaction.encode transaction)
      |> store_ok
    in
    let replayed = replay_transaction initial transaction |> store_ok in
    let restored = restore_state replayed |> store_ok in
    assert (Poly.equal restored.invocations [ published ]);
    let changed_call =
      match Openai.Responses_history.item_exn call with
      | Function_call value ->
        Openai.Responses_history.create_with_id_exn
          ~id:(id 0)
          (Function_call { value with arguments = "changed" })
      | _ -> assert false
    in
    let changed =
      { restored with
        conversation =
          { restored.conversation with
            canonical_history =
              List.map [ changed_call; output ] ~f:Agent_session.History_codec.to_protocol
          }
      }
    in
    assert (Result.is_error (restore_state changed));
    let compacted =
      { restored with
        conversation = { restored.conversation with canonical_history = [] }
      }
    in
    let compacted = restore_state compacted |> store_ok in
    assert (Poly.equal compacted.invocations [ published ]);
    let wrong =
      Agent_session.History_codec.user_text ~id:(id 1) "forged result"
      |> Agent_session.History_codec.to_protocol
    in
    let corrupted =
      { restored with
        conversation =
          { restored.conversation with
            canonical_history = [ Agent_session.History_codec.to_protocol call; wrong ]
          }
      }
    in
    assert (Result.is_error (restore_state corrupted)))
;;

let%expect_test
    "named state documents preserve data and reject invalid invocation ownership"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    assert (Result.is_error (restore_state { initial with schema_version = 2 }));
    let migrated = restore_state initial |> store_ok in
    print_s
      [%sexp
        { version = (migrated.schema_version : int)
        ; records = (List.length migrated.invocations : int)
        }];
    let invocation = invocation_fixture () in
    let foreign =
      Agent_protocol.Invocation.create
        { invocation.context with session_id = second_session_id }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_delta.apply initial (Invocation_changed foreign))
         : bool)];
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate
              { initial with invocations = [ foreign ] })
         : bool)];
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate
              { initial with invocations = [ invocation; invocation ] })
         : bool)];
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.upgrade_schema
              { initial with
                schema_version = Agent_session.Session_state.current_schema_version + 1
              })
         : bool)]);
  [%expect
    {|
    ((version 24) (records 0))
    true
    true
    true
    true
    |}]
;;

let%expect_test "named compaction archives admit typed state and preserve captured bytes" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let state =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let archived = state in
      let store =
        Agent_store.Session_store.create
          ~env
          ~sw
          ~root:
            (Filename.concat
               workspace_instance.canonical_root.native_path
               "archive-store")
          ~server_id:(Agent_protocol.Id.Server.of_string "srv_archive_test" |> protocol_ok)
          ~process_start_identity:None
          ~lock_nonce:"archive-store-lock"
        |> store_ok
      in
      let metadata =
        Agent_store.Session_store.Metadata.
          { schema_version = Agent_store.Session_store.current_metadata_schema_version
          ; session = Agent_session.Session_state.summary archived
          ; prompt_artifact =
              Agent_protocol.Id.Prompt_revision.to_string prompt_revision_id
          ; workspace_identity = workspace_instance.conflict_domain
          ; data_schema_version = Agent_session.Session_state.current_schema_version
          }
      in
      let handle =
        Agent_store.Session_store.create_session
          store
          ~sw
          ~transaction_id
          ~actor_lock_nonce:"archive-actor-lock"
          metadata
        |> store_ok
      in
      let reference =
        Agent_session.Compaction_archive.reference
          (Agent_session.Session_state_document.authored archived)
          ~limits:Document_schema.Limits.default
          operation_id
        |> protocol_ok
      in
      Agent_session.Compaction_archive.write
        ~env
        ~handle
        ~max_payload_length:1048576
        reference
        (Agent_session.Session_state_document.authored archived)
      |> protocol_ok;
      let restored =
        Agent_session.Compaction_archive.read
          ~env
          ~handle
          ~max_payload_length:1048576
          reference
        |> protocol_ok
      in
      let reader =
        Agent_store.Retention_reader.create
          ~env
          ~root:(Agent_store.Session_store.Handle.archive_directory handle)
          ~max_entries:4
          ~max_bytes:1048576
        |> store_ok
      in
      let contents =
        Agent_store.Retention_reader.read
          reader
          ~path:(Agent_session.Compaction_archive.filename reference)
          ~max_bytes:1048576
        |> store_ok
      in
      let module A = Agent_session.Compaction_archive in
      let module S = Agent_session.Session_state in
      let module State_document = Agent_session.Session_state_document in
      let module D = Document_schema in
      let unchanged () =
        let actual =
          Eio.Path.load
            Eio.Path.(
              Eio.Stdenv.fs env
              / Agent_store.Session_store.Handle.archive_directory handle
              / A.filename reference)
        in
        assert (String.equal contents actual)
      in
      let legacy_job =
        Agent_protocol.Job.
          { id = Agent_protocol.Id.Job.of_string "job_archive_admission" |> protocol_ok
          ; session_id = archived.identity.session_id
          ; generation = archived.identity.generation
          ; kind = Model_call
          ; payload = `Null
          ; status = Queued
          ; retry_policy = Never
          ; attempt = -1
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Not_required
          ; launch = None
          ; progress = None
          }
      in
      let invalid_states =
        [ { archived with
            conversation = { archived.conversation with initial_prompt_entry_count = -1 }
          }
        ; { archived with
            jobs = [ legacy_job ]
          ; model_job_targets = [ model_job_binding archived legacy_job ]
          }
        ]
      in
      let raw_archive child =
        D.Document.create
          ~limits:document_limits
          ~kind:"session.compaction_archive"
          ~version:1
          ~payload:(`Object [ "state", D.Document.json child ])
        |> document_ok
      in
      let reference_for_document document =
        { reference with
          sha256 = Agent_store.Document_record.digest (D.Document.to_string document)
        }
      in
      List.iter invalid_states ~f:(fun bad ->
        (* These values demonstrate the distinction between native validation
           and the complete typed decoder, rather than a generic JSON failure. *)
        S.validate bad |> protocol_ok;
        let valid =
          { bad with
            conversation =
              { bad.conversation with
                initial_prompt_entry_count =
                  Int.max 0 bad.conversation.initial_prompt_entry_count
              }
          ; jobs =
              List.map bad.jobs ~f:(fun job ->
                { job with Agent_protocol.Job.attempt = Int.max 0 job.attempt })
          }
        in
        let valid_child =
          State_document.encode (State_document.authored valid) ~limits:document_limits
          |> document_ok
        in
        let set json name value =
          match json with
          | `Object fields ->
            `Object
              (List.map fields ~f:(fun (key, old) ->
                 key, if String.equal key name then value else old))
          | _ -> failwith "expected admitted fixture object"
        in
        let payload = D.Document.payload valid_child in
        let payload =
          if bad.conversation.initial_prompt_entry_count < 0
          then (
            let conversation =
              Agent_store.Document_fields.required payload "conversation" Result.return
              |> document_ok
            in
            set
              payload
              "conversation"
              (set conversation "initial_prompt_entry_count" (`Number "-1")))
          else (
            let jobs =
              Agent_store.Document_fields.required
                payload
                "jobs"
                Agent_store.Document_fields.array
              |> document_ok
            in
            set
              payload
              "jobs"
              (`Array (List.map jobs ~f:(fun job -> set job "attempt" (`Number "-1")))))
        in
        let child =
          D.Document.create
            ~limits:document_limits
            ~kind:"session.state"
            ~version:(D.Document.version valid_child)
            ~payload
          |> document_ok
        in
        let bad = State_document.authored bad in
        assert (Result.is_error (State_document.encode bad ~limits:document_limits));
        assert (Result.is_error (State_document.decode ~limits:document_limits child));
        assert (Result.is_error (A.archive_document bad ~limits:document_limits));
        assert (Result.is_error (A.reference bad ~limits:document_limits operation_id));
        assert (
          Result.is_error (A.write ~env ~handle ~max_payload_length:1048576 reference bad));
        unchanged ();
        (* Raw callers cannot bypass child admission with a correctly captured
           digest and individually valid complete universal envelopes. *)
        let document = raw_archive child in
        assert (
          Result.is_error
            (A.write_document
               ~env
               ~handle
               ~max_payload_length:1048576
               (reference_for_document document)
               document));
        unchanged ());
      let valid_document =
        A.archive_document (State_document.authored archived) ~limits:document_limits
        |> document_ok
      in
      let wrong_owner =
        { archived with
          identity =
            { archived.identity with
              session_id =
                Agent_protocol.Id.Session.of_string "ses_other_archive" |> protocol_ok
            }
        ; inference_ledger =
            fresh_inference_ledger
              ~session_id:
                (Agent_protocol.Id.Session.of_string "ses_other_archive" |> protocol_ok)
              ~generation:archived.identity.generation
        }
      in
      let wrong_owner_document =
        A.archive_document (State_document.authored wrong_owner) ~limits:document_limits
        |> document_ok
      in
      let missing_invocation =
        Agent_session.Session_state.Compaction_archive.
          { invocation_id =
              Agent_protocol.Id.Invocation.of_string "inv_missing_archive" |> protocol_ok
          ; output_entry_id = None
          ; publication_discarded = None
          ; interruption_reason = Some "daemon restarted"
          }
      in
      List.iter
        [ reference_for_document wrong_owner_document, wrong_owner_document
        ; { reference with revision = Int64.succ reference.revision }, valid_document
        ; ( { reference with invocation_dispositions = [ missing_invocation ] }
          , valid_document )
        ; { reference with sha256 = String.make 64 '0' }, valid_document
        ]
        ~f:(fun (reference, document) ->
          assert (
            Result.is_error
              (A.write_document
                 ~env
                 ~handle
                 ~max_payload_length:1048576
                 reference
                 document));
          unchanged ());
      (* Unknown fields and raw numeric spelling remain in the supplied bytes;
         typed admission must not substitute a reencoded value. *)
      let captured =
        let add fields name value = `Object ((name, value) :: fields) in
        let child =
          State_document.encode (State_document.authored archived) ~limits:document_limits
          |> document_ok
        in
        let child_json =
          match D.Document.json child, D.Document.payload child with
          | `Object envelope, `Object payload ->
            `Object
              (List.map envelope ~f:(fun (key, value) ->
                 ( key
                 , if String.equal key "payload"
                   then add payload "future_state" (`Number "1e+00")
                   else value )))
          | _ -> assert false
        in
        D.Document.inspect
          ~limits:document_limits
          (`Object
              [ "future_envelope", `String "opaque"
              ; "format", `String "ochat.document"
              ; "kind", `String "session.compaction_archive"
              ; "schema_version", `Number "1"
              ; "payload", `Object [ "future_archive", `True; "state", child_json ]
              ])
        |> document_ok
      in
      let captured_reference = reference_for_document captured in
      A.write_document
        ~env
        ~handle
        ~max_payload_length:1048576
        captured_reference
        captured
      |> protocol_ok;
      let captured_contents =
        Agent_store.Retention_reader.read
          reader
          ~path:(A.filename reference)
          ~max_bytes:1048576
        |> store_ok
      in
      let record =
        match
          Agent_store.Document_record.decode_file
            ~limits:document_limits
            ~expected_digest:(Some captured_reference.sha256)
            captured_contents
        with
        | Ok record -> record
        | Error error -> raise_s [%sexp (error : Agent_store.Document_record.Error.t)]
      in
      assert (
        String.equal
          (Agent_store.Document_record.stored_bytes record)
          (D.Document.to_string captured));
      A.decode_file
        ~handle
        ~max_payload_length:1048576
        captured_reference
        captured_contents
      |> protocol_ok
      |> ignore;
      let bounded =
        Agent_session.Compaction_archive.decode_file
          ~handle
          ~max_payload_length:1048576
          reference
          contents
        |> protocol_ok
      in
      assert (
        Sexp.equal
          (Agent_session.Session_state.sexp_of_t bounded)
          (Agent_session.Session_state.sexp_of_t restored));
      assert (
        Result.is_error
          (Agent_session.Compaction_archive.decode_file
             ~handle
             ~max_payload_length:1048576
             { reference with sha256 = String.make 64 '0' }
             contents));
      print_s
        [%sexp
          { version = (restored.schema_version : int)
          ; records = (List.length restored.invocations : int)
          }];
      Agent_store.Session_store.close_session store handle |> store_ok;
      Agent_store.Session_store.close store |> store_ok));
  [%expect {| ((version 24) (records 0)) |}]
;;

let%expect_test
    "fast completion stays pending until acknowledgement then commits exactly one \
     history entry"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, resolved, _, delivery = extension_fixture workspace_instance in
    let restored = restore_state staged |> store_ok in
    let entry = notification_entry delivery in
    let committed =
      Agent_protocol.Delivery.commit delivery ~history_id:entry.id ~now:timestamp
      |> protocol_ok
    in
    let transition state delta =
      Agent_session.Session_transition.apply ~now:timestamp state ~delta ~payloads:[]
    in
    print_s
      [%sexp
        (Result.is_error (transition restored (Delivery_committed (committed, entry)))
         : bool)];
    print_s [%sexp (List.length restored.conversation.canonical_history : int)];
    let published = Agent_protocol.Invocation.publish resolved |> protocol_ok in
    let ready = transition restored (Invocation_changed published) |> protocol_ok in
    let delivered =
      transition ready.state (Delivery_committed (committed, entry)) |> protocol_ok
    in
    let repeated =
      transition delivered.state (Delivery_committed (committed, entry)) |> protocol_ok
    in
    let status_updates transition =
      List.filter_map transition.Agent_session.Session_transition.events ~f:(fun event ->
        Agent_protocol.Event.Durable.extension_status event |> protocol_ok)
    in
    assert (
      Poly.equal
        (status_updates ready)
        [ Agent_session.Session_state.extension_status ready.state ]);
    assert (
      Poly.equal
        (status_updates delivered)
        [ Agent_session.Session_state.extension_status delivered.state ]);
    assert (List.is_empty (status_updates repeated));
    print_s [%sexp (List.length repeated.state.conversation.canonical_history : int)];
    let restored = restore_state repeated.state |> store_ok in
    print_s
      [%sexp
        ((List.hd_exn restored.conversation.canonical_history).provenance
         : Agent_protocol.History.provenance)];
    print_s
      [%sexp (Result.is_error (transition restored (Delivery_changed committed)) : bool)]);
  [%expect
    {|
    true
    0
    1
    (Runtime_notification dlv_atomic)
    true |}]
;;

let%expect_test
    "invalid delivery transaction cannot publish its acknowledgement or forge human \
     provenance"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, resolved, _, delivery = extension_fixture workspace_instance in
    let published = Agent_protocol.Invocation.publish resolved |> protocol_ok in
    let entry = { (notification_entry delivery) with provenance = Canonical } in
    let committed =
      Agent_protocol.Delivery.commit delivery ~history_id:entry.id ~now:timestamp
      |> protocol_ok
    in
    let delta =
      Agent_session.Session_delta.Batch
        [ Invocation_changed published; Delivery_committed (committed, entry) ]
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_transition.apply
              ~now:timestamp
              staged
              ~delta
              ~payloads:[])
         : bool)];
    print_s
      [%sexp ((List.hd_exn staged.invocations).status : Agent_protocol.Invocation.status)];
    print_s [%sexp (List.length staged.conversation.canonical_history : int)]);
  [%expect
    {|
    true
    (Resolved (Pending (Subscription sub_atomic) (String accepted)))
    0 |}]
;;

let%expect_test "extension references reject missing work and competing delivery owners" =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, _, subscription, delivery = extension_fixture workspace_instance in
    let original = List.hd_exn staged.invocations in
    let wrong_ack =
      Agent_protocol.Invocation.create original.context
      |> protocol_ok
      |> Agent_protocol.Invocation.dispatch
      |> protocol_ok
    in
    let wrong_ack =
      Agent_protocol.Invocation.resolve
        wrong_ack
        ~session_id
        ~generation:0
        (Complete `Null)
      |> protocol_ok
    in
    assert (
      Result.is_error
        (Agent_session.Session_state.validate { staged with invocations = [ wrong_ack ] }));
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate { staged with subscriptions = [] })
         : bool)];
    let duplicate =
      Agent_protocol.Delivery.create
        { delivery.context with
          id = Agent_protocol.Id.Delivery.of_string "dlv_competing" |> protocol_ok
        }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate
              { staged with deliveries = duplicate :: staged.deliveries })
         : bool)];
    let wrong =
      Agent_protocol.Delivery.create
        { delivery.context with completion = Succeeded (`String "forged") }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate { staged with deliveries = [ wrong ] })
         : bool)];
    let foreign =
      Agent_protocol.Subscription.create
        { subscription.context with session_id = second_session_id }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_delta.apply staged (Subscription_changed foreign))
         : bool)];
    let stale =
      Agent_protocol.Subscription.create { subscription.context with generation = 1 }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_delta.apply staged (Subscription_changed stale))
         : bool)]);
  [%expect
    {|
    true
    true
    true
    true
    true |}]
;;

let%expect_test "named invocation snapshots preserve pending publication" =
  with_actor_workspace (fun _env workspace_instance ->
    let state =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let invocation =
      invocation_fixture () |> Agent_protocol.Invocation.dispatch |> protocol_ok
    in
    let invocation =
      Agent_protocol.Invocation.resolve
        invocation
        ~session_id
        ~generation:0
        (Complete `Null)
      |> protocol_ok
    in
    let pending = { state with invocations = [ invocation ] } in
    let restored = restore_state pending |> store_ok in
    print_s
      [%sexp
        { version = (restored.schema_version : int)
        ; invocations = (List.length restored.invocations : int)
        ; subscriptions = (List.length restored.subscriptions : int)
        ; deliveries = (List.length restored.deliveries : int)
        }];
    print_s
      [%sexp
        ((List.hd_exn restored.invocations).status : Agent_protocol.Invocation.status)]);
  [%expect
    {|
    ((version 24) (invocations 1) (subscriptions 0) (deliveries 0))
    (Resolved (Complete Null))
    |}]
;;

let%expect_test
    "actor extension commit exposes neither queued work nor notification before \
     persistence succeeds"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let _, staged, resolved, _, delivery = extension_fixture workspace_instance in
      let staged = { staged with lifecycle = { desired = Running; observed = Idle } } in
      let fail_commit = ref true in
      let callbacks = ref 0 in
      let history_events = ref 0 in
      let actor =
        Agent_session.Session_actor.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:staged
          ~operation_worker:None
          ~persistence:
            { archive_reference
            ; commit =
                (fun ~command_audit:_ ~previous:_ _ ->
                  if !fail_commit
                  then
                    Error
                      (Agent_protocol.Error.create
                         Persistence_error
                         ~message:"injected disk failure"
                         ~retryable:true
                         ())
                  else Ok ())
            }
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_extension" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "fixture")
            ; job_results = None
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; notification_limits = Agent_session.Staged_notifications.default_limits
            ; ingress_limits = Agent_session.Staged_ingress.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed =
                (fun _ events ->
                  Int.incr callbacks;
                  List.iter events ~f:(fun event ->
                    if Agent_protocol.Event.Durable.equal_kind event.kind History_appended
                    then Int.incr history_events))
            }
      in
      let published = Agent_protocol.Invocation.publish resolved |> protocol_ok in
      let entry = notification_entry delivery in
      let committed =
        Agent_protocol.Delivery.commit delivery ~history_id:entry.id ~now:timestamp
        |> protocol_ok
      in
      let job =
        Agent_protocol.Job.
          { id = Agent_protocol.Id.Job.of_string "job_atomic" |> protocol_ok
          ; session_id
          ; generation = 0
          ; kind = Async_tool
          ; payload = `Null
          ; status = Queued
          ; retry_policy = Never
          ; attempt = 0
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Not_required
          ; launch = None
          ; progress = None
          }
      in
      let changes =
        Agent_session.Session_actor.Extension_change.
          [ Start_job job; Invocation published; Publish (committed, entry) ]
      in
      let commit expected_revision changes =
        Agent_session.Session_actor.commit_extensions
          actor
          ~generation:0
          ~expected_revision
          changes
      in
      print_s [%sexp (Result.is_error (commit staged.counters.revision changes) : bool)];
      let failed = Agent_session.Session_actor.state actor |> protocol_ok in
      print_s
        [%sexp
          { jobs = (List.length failed.jobs : int)
          ; history = (List.length failed.conversation.canonical_history : int)
          ; callbacks = (!callbacks : int)
          }];
      fail_commit := false;
      let after = commit staged.counters.revision changes |> protocol_ok in
      print_s [%sexp (Result.is_error (commit staged.counters.revision changes) : bool)];
      commit
        after.revision
        Agent_session.Session_actor.Extension_change.
          [ Invocation published; Publish (committed, entry) ]
      |> protocol_ok
      |> ignore;
      let final = Agent_session.Session_actor.state actor |> protocol_ok in
      print_s
        [%sexp
          { jobs = (List.length final.jobs : int)
          ; history = (List.length final.conversation.canonical_history : int)
          ; history_events = (!history_events : int)
          }];
      Agent_session.Session_actor.shutdown actor));
  [%expect
    {|
    true
    ((jobs 0) (history 0) (callbacks 0))
    true
    ((jobs 1) (history 1) (history_events 1)) |}]
;;

let%expect_test
    "extension status journal replay preserves projection and rejects future codecs"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, resolved, _, _ = extension_fixture workspace_instance in
    let published = Agent_protocol.Invocation.publish resolved |> protocol_ok in
    let transition =
      Agent_session.Session_transition.apply
        ~now:timestamp
        staged
        ~delta:(Invocation_changed published)
        ~payloads:[]
      |> protocol_ok
    in
    let transaction events =
      Agent_store.Transaction.create
        ~limits:document_limits
        ~session_id
        ~generation:0
        ~transaction_sequence:transition.state.counters.transaction_sequence
        ~previous_transaction_hash:None
        ~session_revision:transition.state.counters.revision
        ~first_event_sequence:
          (Some (List.hd_exn events).Agent_protocol.Event.Durable.sequence)
        ~last_event_sequence:
          (Some (List.last_exn events).Agent_protocol.Event.Durable.sequence)
        ~accepted_at_ns:
          (Agent_protocol.Timestamp.to_time_ns timestamp
           |> Time_ns.to_int_ns_since_epoch
           |> Int64.of_int)
        ~command_audit:None
        ~delta:(delta_document transition.delta)
        ~durable_events:(List.map events ~f:(fun event -> event_document event))
      |> store_ok
      |> Agent_store.Transaction.encode
      |> Agent_store.Transaction.decode
      |> store_ok
    in
    let journal = transaction transition.events in
    let replayed = replay_transaction staged journal |> store_ok in
    let statuses = Agent_session.Session_state.extension_status replayed in
    let events =
      Agent_session.Session_persistence.durable_events ~limits:document_limits journal
      |> store_ok
    in
    let updates =
      List.filter_map events ~f:(fun event ->
        Agent_protocol.Event.Durable.extension_status event |> protocol_ok)
    in
    assert (Poly.equal updates [ statuses ]);
    let corrupt =
      List.map events ~f:(fun event ->
        match event.kind, event.payload with
        | Session_updated, `Object fields ->
          { event with
            payload =
              `Object
                (("extension_status", `Array [ `Object [ "version", `Number "99" ] ])
                 :: List.filter fields ~f:(fun (name, _) ->
                   not (String.equal name "extension_status")))
          }
        | _ -> event)
    in
    List.iter corrupt ~f:(fun event ->
      let document =
        Document_schema.Document.create
          ~limits:document_limits
          ~kind:"session.event"
          ~version:1
          ~payload:(Agent_session.Durable_event_document.to_jsonaf event)
        |> document_ok
      in
      if Agent_protocol.Event.Durable.equal_kind event.kind Session_updated
      then
        assert (
          Result.is_error
            (Agent_session.Durable_event_document.decode ~limits:document_limits document))));
  print_endline "journal state/status agree; future status codec rejected during replay";
  [%expect {| journal state/status agree; future status codec rejected during replay |}]
;;

let%test_unit
    "canonical restore retains opaque captures independently of runtime lowering"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let module H = History_entry.Payload in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let semantic =
      H.Semantic.create
        (Unknown { provider_kind = "future.reasoning" })
        ~metadata:H.Metadata.empty
      |> Result.ok_or_failwith
    in
    let raw = `Object [ "type", `String "future.reasoning"; "opaque", `String "exact" ] in
    let payload =
      H.captured semantic ~origin:H.Origin.unavailable ~raw |> Result.ok_or_failwith
    in
    let entry = History_entry.create_with_id ~id:history_id payload in
    let canonical = Agent_session.History_codec.to_canonical entry in
    let state =
      { initial with
        conversation = { initial.conversation with canonical_history = [ canonical ] }
      }
    in
    assert (
      Result.is_error
        (Agent_session.Session_state.validate
           { state with
             conversation =
               { state.conversation with canonical_history = [ canonical; canonical ] }
           }));
    assert (
      Result.is_error
        (Agent_session.Session_state.validate
           { state with
             conversation =
               { state.conversation with
                 deferred_user_entries =
                   [ pending_document ~generation:state.identity.generation canonical ]
               }
           }));
    let restored = restore_state state |> store_ok in
    let retained = List.hd_exn restored.conversation.canonical_history in
    assert (Agent_protocol.History.equal_entry canonical retained);
    let neutral = Agent_session.History_codec.of_canonical retained |> protocol_ok in
    assert (
      Jsonaf.exactly_equal (H.to_json payload) (H.to_json (History_entry.payload neutral)));
    assert (
      Result.is_error (Openai.Responses_history.to_item (History_entry.payload neutral)));
    assert (
      Result.is_error
        (Agent_session.History_codec.of_canonical { retained with redacted = true }));
    assert (
      Result.is_error
        (Agent_session.History_codec.of_canonical { retained with kind = Tool_call })))
;;

let%test_unit
    "opaque neutral moderator overlays restore and project without provider decoding"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let module H = History_entry.Payload in
    let payload =
      Jsonaf.of_string
        {|{"format":"ochat.document","kind":"history.payload","schema_version":1,"future_envelope":true,"payload":{"semantic":{"type":"unknown","provider_kind":"future.overlay","metadata":{}},"representation":{"type":"captured","origin":{"type":"unavailable"},"raw":{"type":"future.overlay","opaque":"exact","future_raw":[null,1e+00]}},"future_payload":null}}|}
      |> H.of_json
      |> Result.ok_or_failwith
    in
    let inserted_id =
      History_entry.Id.create ~namespace:"overlay" ~sequence:0 |> Result.ok_or_failwith
    in
    let snapshot =
      { (handoff_snapshot 0) with
        revision = 1
      ; next_change_id = 2
      ; prepended_items =
          [ Session.Moderator_state.Identity_snapshot.Inserted.
              { entry_id = inserted_id
              ; change_id = 0
              ; script_label = None
              ; value = payload
              }
          ]
      ; replacements =
          [ Session.Moderator_state.Identity_snapshot.Replacement.
              { target_id = history_id
              ; change_id = 1
              ; script_label = None
              ; value = payload
              }
          ]
      }
    in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let state =
      { initial with
        conversation = { initial.conversation with canonical_history = [ actor_entry ] }
      ; moderator =
          Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)
      }
    in
    let colliding_snapshot =
      { snapshot with
        prepended_items =
          [ { (List.hd_exn snapshot.prepended_items) with entry_id = history_id } ]
      }
    in
    assert (
      Result.is_error
        (Agent_session.Session_state.validate
           { state with
             moderator =
               Some
                 (Agent_session.Runtime_builder.encode_moderator_snapshot
                    colliding_snapshot)
           }));
    let deferred =
      Agent_session.History_codec.user_text ~id:inserted_id "queued"
      |> Agent_session.History_codec.to_canonical
    in
    assert (
      Result.is_error
        (Agent_session.Session_state.validate
           { state with
             conversation =
               { state.conversation with
                 deferred_user_entries =
                   [ pending_document ~generation:state.identity.generation deferred ]
               }
           }));
    let restored = restore_state state |> store_ok in
    let projected = Agent_session.Session_state.snapshot ~now:timestamp restored in
    let effective = Option.value_exn projected.effective_history in
    [%test_eq: int] 2 (List.length effective.entries);
    let inserted = List.nth_exn effective.entries 0
    and replacement = List.nth_exn effective.entries 1 in
    assert (History_entry.Id.equal inserted.id inserted_id);
    assert (History_entry.Id.equal replacement.id history_id);
    assert (Jsonaf.exactly_equal inserted.payload (H.to_json payload));
    assert (Jsonaf.exactly_equal replacement.payload (H.to_json payload));
    assert (Agent_protocol.History.equal_provenance inserted.provenance Moderator_inserted);
    assert (
      Agent_protocol.History.equal_provenance
        replacement.provenance
        (Moderator_replaced history_id)))
;;
