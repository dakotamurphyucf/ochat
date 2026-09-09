open Core
open Fixtures

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
        ~delta:
          (Sexp.to_string_mach (Agent_session.Session_delta.sexp_of_t transition.delta))
        ~durable_events:
          (List.map transition.events ~f:(fun event ->
             Sexp.to_string_mach (Agent_protocol.Event.Durable.sexp_of_t event)))
      |> store_ok
      |> Agent_store.Transaction.encode
      |> Agent_store.Transaction.decode
      |> store_ok
    in
    let replayed =
      Agent_session.Session_persistence.apply_transaction initial journal |> store_ok
    in
    assert_same_session_snapshot transition.state replayed;
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (S.sexp_of_t replayed))
      |> store_ok
    in
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
      (Agent_session.Session_persistence.durable_events journal |> store_ok
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
    let migrated = S.upgrade_schema { initial with schema_version = 4 } |> protocol_ok in
    let older =
      { recovered with identity = { recovered.identity with generation = 1 } }
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
    ((schema 8)
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
        ~delta:(Sexp.to_string_mach (Agent_session.Session_delta.sexp_of_t delta))
        ~durable_events:[]
      |> store_ok
    in
    let transaction =
      Agent_store.Transaction.decode (Agent_store.Transaction.encode transaction)
      |> store_ok
    in
    let replayed =
      Agent_session.Session_persistence.apply_transaction initial transaction |> store_ok
    in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t replayed))
      |> store_ok
    in
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
      History_entry.create_with_id
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
      History_entry.create_with_id
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
        ~delta:(Sexp.to_string_mach (Agent_session.Session_delta.sexp_of_t delta))
        ~durable_events:[]
      |> store_ok
    in
    let transaction =
      Agent_store.Transaction.decode (Agent_store.Transaction.encode transaction)
      |> store_ok
    in
    let replayed =
      Agent_session.Session_persistence.apply_transaction initial transaction |> store_ok
    in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t replayed))
      |> store_ok
    in
    assert (Poly.equal restored.invocations [ published ]);
    let changed_call =
      match History_entry.item call with
      | Function_call value ->
        History_entry.create_with_id
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
    assert (
      Result.is_error
        (Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t changed))));
    let compacted =
      { restored with
        conversation = { restored.conversation with canonical_history = [] }
      }
    in
    let compacted =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t compacted))
      |> store_ok
    in
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
    assert (
      Result.is_error
        (Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t corrupted)))))
;;

let%expect_test
    "legacy state migration preserves data and rejects invalid invocation ownership"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let legacy =
      match Agent_session.Session_state.sexp_of_t { initial with schema_version = 2 } with
      | Sexp.List fields ->
        Sexp.List
          (List.filter fields ~f:(function
             | Sexp.List (Sexp.Atom "invocations" :: _) -> false
             | _ -> true))
      | _ -> assert false
    in
    let migrated =
      Agent_session.Session_persistence.restore_snapshot (Sexp.to_string_mach legacy)
      |> store_ok
    in
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
              { initial with schema_version = 9 })
         : bool)]);
  [%expect
    {|
    ((version 8) (records 0))
    true
    true
    true
    true
    |}]
;;

let%expect_test "pre-extension compaction archives remain readable after state migration" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let state =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let legacy = { state with schema_version = 2 } in
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
          { schema_version = Agent_store.Session_store.current_schema_version
          ; session = Agent_session.Session_state.summary legacy
          ; prompt_artifact =
              Agent_protocol.Id.Prompt_revision.to_string prompt_revision_id
          ; workspace_identity = workspace_instance.conflict_domain
          ; data_schema_version = 2
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
      let reference = Agent_session.Compaction_archive.reference legacy operation_id in
      Agent_session.Compaction_archive.write
        ~env
        ~handle
        ~max_payload_length:1048576
        reference
        legacy
      |> protocol_ok;
      let restored =
        Agent_session.Compaction_archive.read
          ~env
          ~handle
          ~max_payload_length:1048576
          reference
        |> protocol_ok
      in
      print_s
        [%sexp
          { version = (restored.schema_version : int)
          ; records = (List.length restored.invocations : int)
          }];
      Agent_store.Session_store.close_session store handle |> store_ok;
      Agent_store.Session_store.close store |> store_ok));
  [%expect {| ((version 8) (records 0)) |}]
;;

let%expect_test
    "fast completion stays pending until acknowledgement then commits exactly one \
     history entry"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, resolved, _, delivery = extension_fixture workspace_instance in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t staged))
      |> store_ok
    in
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
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t repeated.state))
      |> store_ok
    in
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

let%expect_test "schema-3 invocation snapshots migrate without losing pending publication"
  =
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
    let legacy = { state with schema_version = 3; invocations = [ invocation ] } in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t legacy))
      |> store_ok
    in
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
    ((version 8) (invocations 1) (subscriptions 0) (deliveries 0))
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
            { commit =
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
        ~delta:
          (Sexp.to_string_mach (Agent_session.Session_delta.sexp_of_t transition.delta))
        ~durable_events:
          (List.map events ~f:(fun event ->
             Sexp.to_string_mach (Agent_protocol.Event.Durable.sexp_of_t event)))
      |> store_ok
      |> Agent_store.Transaction.encode
      |> Agent_store.Transaction.decode
      |> store_ok
    in
    let journal = transaction transition.events in
    let replayed =
      Agent_session.Session_persistence.apply_transaction staged journal |> store_ok
    in
    let statuses = Agent_session.Session_state.extension_status replayed in
    let events = Agent_session.Session_persistence.durable_events journal |> store_ok in
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
    assert (
      Result.is_error
        (Agent_session.Session_persistence.durable_events (transaction corrupt))));
  print_endline "journal state/status agree; future status codec rejected during replay";
  [%expect {| journal state/status agree; future status codec rejected during replay |}]
;;
