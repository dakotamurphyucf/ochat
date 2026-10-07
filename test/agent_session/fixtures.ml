open Core

let () = Mirage_crypto_rng_unix.use_default ()

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected protocol error", (error : Agent_protocol.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected store error", (error : Agent_store.Store_error.t)]
;;

let document_limits = Document_schema.Limits.default

let document_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected document error", (error : Document_schema.Error.t)]
;;

let state_document state =
  Agent_session.Session_state_document.encode
    (Agent_session.Session_state_document.authored state)
    ~limits:document_limits
  |> document_ok
;;

let delta_document delta =
  Agent_session.Session_delta_document.create
    delta
    ~limits:document_limits
    ~state_document:Agent_session.Session_state_document.authored
  |> document_ok
  |> Agent_session.Session_delta_document.document
;;

let restore_delta delta =
  Agent_session.Session_delta_document.decode
    ~limits:document_limits
    (delta_document delta)
  |> document_ok
  |> Agent_session.Session_delta_document.value
;;

let event_document event =
  Agent_session.Durable_event_document.create event ~limits:document_limits
  |> document_ok
  |> Agent_session.Durable_event_document.document
;;

let snapshot_of_state ?transaction_hash state =
  let open Result.Let_syntax in
  let%bind payload =
    Agent_session.Session_state_document.encode
      (Agent_session.Session_state_document.authored state)
      ~limits:document_limits
    |> Result.map_error ~f:(fun error ->
      Agent_store.Store_error.Corrupt
        (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  (* Standalone authored test snapshots have no journal. Supply an explicit
     fixture anchor for progressed states; callers exercising a physical chain
     pass its actual persisted transaction digest. *)
  let transaction_hash =
    Option.value
      transaction_hash
      ~default:
        (if Int64.equal state.counters.transaction_sequence 0L
         then None
         else Some (String.make 64 'a'))
  in
  Agent_store.Snapshot.create
    ~limits:document_limits
    ~session_id:state.Agent_session.Session_state.identity.session_id
    ~transaction_sequence:state.counters.transaction_sequence
    ~transaction_hash
    ~event_sequence:state.counters.event_sequence
    ~created_at:state.identity.updated_at
    ~prompt_artifact:
      (Agent_protocol.Id.Prompt_revision.to_string state.spec.prompt_revision_id)
    ~workspace_identity:state.spec.workspace_instance.conflict_domain
    ~payload
;;

let snapshot_record state ~transaction_hash =
  snapshot_of_state ~transaction_hash state |> store_ok
;;

let restore_state state =
  let open Result.Let_syntax in
  let%bind snapshot = snapshot_of_state state in
  let%map restored =
    Agent_session.Session_persistence.restore_snapshot ~limits:document_limits snapshot
  in
  Agent_session.Session_persistence.Restored.state restored
;;

let replay_transaction state transaction =
  Agent_session.Session_persistence.apply_transaction
    ~limits:document_limits
    (Agent_session.Session_persistence.Restored.authored state)
    transaction
  |> Result.map ~f:Agent_session.Session_persistence.Restored.state
;;

let runtime_history_payload entry =
  Agent_session.History_codec.of_canonical entry
  |> protocol_ok
  |> Openai.Responses_history.item_exn
  |> Openai.Responses.Item.jsonaf_of_t
;;

let command_audit ~principal_id ~session_id request_digest =
  Agent_store.Idempotency_store.Command_audit.encode
    { key =
        { principal_id
        ; session_id = Some session_id
        ; method_name = "session.start"
        ; idempotency_key =
            Agent_protocol.Idempotency_key.of_string "fixture-start" |> protocol_ok
        }
    ; request_digest
    ; protected_record = false
    }
  |> store_ok
;;

(* Compare the complete snapshots, including counters and recovery metadata. *)
let assert_same_session_snapshot expected actual =
  [%test_eq: Sexp.t]
    (Agent_session.Session_state.sexp_of_t expected)
    (Agent_session.Session_state.sexp_of_t actual)
;;

let workspace_id =
  Agent_protocol.Id.Workspace_definition.of_string "wsd_agent_session_test" |> protocol_ok
;;

let instance_id =
  Agent_protocol.Id.Workspace_instance.of_string "wsi_agent_session_test" |> protocol_ok
;;

let second_instance_id =
  Agent_protocol.Id.Workspace_instance.of_string "wsi_agent_session_second" |> protocol_ok
;;

let session_id =
  Agent_protocol.Id.Session.of_string "ses_agent_session_test" |> protocol_ok
;;

let second_session_id =
  Agent_protocol.Id.Session.of_string "ses_agent_session_second" |> protocol_ok
;;

let third_session_id =
  Agent_protocol.Id.Session.of_string "ses_agent_session_third" |> protocol_ok
;;

let prompt_id =
  Agent_protocol.Id.Prompt_definition.of_string "prd_agent_session_test" |> protocol_ok
;;

let transaction_id =
  Agent_protocol.Id.Transaction.of_string "txn_agent_session_test" |> protocol_ok
;;

let prompt_revision_id =
  Agent_protocol.Id.Prompt_revision.of_string "prv_agent_session_test" |> protocol_ok
;;

let permission_id =
  Agent_protocol.Id.Permission.of_string "per_agent_session_test" |> protocol_ok
;;

let operation_id =
  Agent_protocol.Id.Operation.of_string "op_agent_session_test" |> protocol_ok
;;

let archive_reference ~previous ~kind operation_id =
  Agent_session.Compaction_archive.reference_for
    (Agent_session.Session_state_document.authored previous)
    ~limits:Document_schema.Limits.default
    ~kind
    operation_id
;;

let history_id =
  History_entry.Id.create ~namespace:"actor" ~sequence:0
  |> function
  | Ok value -> value
  | Error error -> failwith error
;;

let second_prompt_id =
  Agent_protocol.Id.Prompt_definition.of_string "prd_agent_session_second" |> protocol_ok
;;

let principal_id =
  Agent_protocol.Id.Principal.of_string "pri_agent_session_test" |> protocol_ok
;;

let second_principal_id =
  Agent_protocol.Id.Principal.of_string "pri_agent_session_second" |> protocol_ok
;;

let timestamp = Agent_protocol.Timestamp.of_string "2026-08-15T12:00:00Z" |> protocol_ok

let with_temp_directory f =
  Eio_main.run (fun env ->
    let temporary =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        ("ochat-agent-session."
         ^ (Agent_protocol.Id.Transaction.create ()
            |> Agent_protocol.Id.Transaction.to_string))
    in
    let root = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () -> f env temporary)
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root))
;;

let inference_selection () =
  let target =
    Inference.Request.Target.create
      ~adapter:"fixture"
      ~profile:"explicit-fixture"
      ~profile_revision:None
      ~account:None
      ~endpoint:"local-fixture"
      ~model:"fixture-model"
      ~settings:[]
      ~limits:document_limits
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  Inference.Selection.captured target ~limits:document_limits
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* Bind manually authored Model_call fixtures to their actual selected source. *)
let model_job_binding (state : Agent_session.Session_state.t) job =
  match Inference.Selection.view state.spec.inference_target with
  | Unresolved -> failwith "model job fixture requires a captured source selection"
  | Captured target ->
    Agent_session.Model_job_target.create job ~target ~limits:document_limits
    |> protocol_ok
;;

let actor_state ~workspace_instance ~liveness ~start_immediately =
  let execution_host, persistence =
    match liveness with
    | Agent_protocol.Session.Process_bound ->
      Agent_protocol.Session.Embedded, Agent_protocol.Session.Transient
    | Detached | Owner_bound _ -> Daemon, Durable
  in
  let protocol =
    Agent_protocol.Session.Spec.create
      ~execution_host
      ~prompt:(Local_path "/prompt.chatmd")
      ~workspace:Current
      ~liveness
      ~persistence
      ~start_immediately
      ~labels:[]
      ()
    |> protocol_ok
  in
  let identity =
    Agent_session.Session_state.Identity.
      { session_id
      ; display_name = Some "actor"
      ; creating_principal = Some principal_id
      ; created_at = timestamp
      ; updated_at = timestamp
      ; labels = []
      ; generation = 0
      }
  in
  let spec =
    Agent_session.Session_state.Spec.
      { protocol
      ; prompt_definition_id = None
      ; delegation = None
      ; prompt_revision_id
      ; inference_target = inference_selection ()
      ; workspace_instance
      ; permission_profile = "interactive"
      ; permission_profile_digest = "profile-digest"
      ; runtime_policy = None
      ; quota_key = None
      }
  in
  Agent_session.Session_state.create ~identity ~spec ~initial_history:[]
;;

let actor_entry =
  Agent_session.History_codec.user_text ~id:history_id "hello"
  |> Agent_session.History_codec.to_protocol
;;

let with_actor_workspace f =
  with_temp_directory (fun env temporary ->
    let workspace_instance =
      Agent_session.Workspace_resolver.resolve_current
        ~env
        ~instance_id
        ~path:temporary
        ~access:Shared_write
        ~created_at:timestamp
      |> store_ok
    in
    f env workspace_instance)
;;

let invocation_fixture () =
  Agent_protocol.Invocation.create
    { id = Agent_protocol.Id.Invocation.of_string "inv_session_test" |> protocol_ok
    ; session_id
    ; generation = 0
    ; origin = Script
    ; provider_call_id = None
    ; call_entry_id = None
    ; parent_invocation = None
    ; parent_job = None
    ; tool_name = "read_file"
    ; implementation_revision = "revision-1"
    ; capability_fingerprint = "capability-1"
    ; input = `Null
    ; created_at = timestamp
    ; deadline = None
    }
  |> protocol_ok
;;

let extension_fixture workspace_instance =
  let initial =
    actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
  in
  let admitted = invocation_fixture () in
  let dispatched = Agent_protocol.Invocation.dispatch admitted |> protocol_ok in
  let subscription =
    Agent_protocol.Subscription.create
      { id = Agent_protocol.Id.Subscription.of_string "sub_atomic" |> protocol_ok
      ; session_id
      ; generation = 0
      ; invocation_id = admitted.context.id
      ; source = None
      ; parent_job = None
      ; kind = "fixture"
      ; created_at = timestamp
      ; deadline =
          Agent_protocol.Timestamp.of_string "2026-08-15T13:00:00Z" |> protocol_ok
      ; completion_schema = None
      ; wake = Request_turn
      ; ingress_capability = None
      }
    |> protocol_ok
  in
  let finished, _ =
    Agent_protocol.Subscription.finish
      subscription
      ~expected_epoch:0
      ~now:timestamp
      (Succeeded (`String "ready"))
    |> protocol_ok
  in
  let resolved =
    Agent_protocol.Invocation.resolve
      dispatched
      ~session_id
      ~generation:0
      (Pending (Subscription subscription.context.id, `String "accepted"))
    |> protocol_ok
  in
  let delivery =
    Agent_protocol.Delivery.create
      { id = Agent_protocol.Id.Delivery.of_string "dlv_atomic" |> protocol_ok
      ; session_id
      ; generation = 0
      ; invocation_id = Some admitted.context.id
      ; work = Some (Subscription subscription.context.id)
      ; correlation = "fixture"
      ; source = Moderator
      ; completion = Succeeded (`String "ready")
      ; wake = Request_turn
      ; created_at = timestamp
      ; ownership = None
      }
    |> protocol_ok
  in
  let delta =
    Agent_session.Session_delta.Batch
      [ Invocation_changed admitted
      ; Invocation_changed dispatched
      ; Subscription_changed subscription
      ; Subscription_changed finished
      ; Invocation_changed resolved
      ; Delivery_changed delivery
      ]
  in
  let staged =
    Agent_session.Session_transition.apply ~now:timestamp initial ~delta ~payloads:[]
    |> protocol_ok
  in
  initial, staged.state, resolved, finished, delivery
;;

let notification_entry delivery =
  let id =
    History_entry.Id.create ~namespace:"notification" ~sequence:0 |> Result.ok_or_failwith
  in
  Agent_session.Notification_history.create ~id delivery |> protocol_ok
;;

let worker_output_item =
  Openai.Responses.Item.Output_message
    { role = Assistant
    ; id = "worker-output"
    ; content = [ { annotations = []; text = "done"; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
;;

let completed_worker_result
      (input : Agent_session.Operation_worker.Input.t)
      (capabilities : Agent_session.Operation_worker.Capabilities.t)
  =
  let open Result.Let_syntax in
  let%bind id =
    History_entry.Id_source.allocate capabilities.id_source
    |> Result.map_error ~f:(fun message ->
      Agent_protocol.Error.create Internal_error ~message ~retryable:false ())
  in
  let entry = Openai.Responses_history.create_with_id_exn ~id worker_output_item in
  let%map () = capabilities.commit_entry entry in
  Agent_session.Operation_worker.Summary.
    { final_history = input.history @ [ entry ]
    ; runtime_requests = []
    ; moderator_snapshot = None
    }
;;

let completed_worker ~started ~release =
  Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
    Eio.Promise.resolve started ();
    Eio.Promise.await release;
    match completed_worker_result input capabilities with
    | Ok result -> Agent_session.Operation_worker.Completed result
    | Error failure -> Agent_session.Operation_worker.Failed failure)
;;

let rec await_idle actor =
  let state = Agent_session.Session_actor.state actor |> protocol_ok in
  match state.active_operation, state.lifecycle.observed with
  | None, Idle -> state
  | _ ->
    Eio.Fiber.yield ();
    await_idle actor
;;

let handoff_error message =
  Agent_protocol.Error.create Internal_error ~message ~retryable:false ()
;;

let handoff_snapshot count =
  Session.Moderator_state.Identity_snapshot.
    { script_id = "handoff"
    ; script_source_hash = "fixture"
    ; current_state = Session.Snapshot.Int count
    ; queued_internal_events = []
    ; halted = false
    ; revision = 0
    ; next_change_id = 0
    ; prepended_items = []
    ; appended_items = []
    ; replacements = []
    ; tombstones = []
    ; halted_reason = None
    }
;;

let with_handoff_actor ?(reject = fun _ -> false) ?inference ~make_worker f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor_ready, actor_ready_u = Eio.Promise.create () in
      let initial =
        actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
      in
      let backend =
        Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
      in
      let persistence = Agent_session.Memory_backend.persistence backend in
      let worker = make_worker env actor_ready in
      let actor =
        Agent_session.Session_actor.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:initial
          ~operation_worker:(Some worker)
          ~persistence:
            { archive_reference
            ; commit =
                (fun ~command_audit ~previous next ->
                  if reject next
                  then Error (handoff_error "injected invocation save failure")
                  else persistence.commit ~command_audit ~previous next)
            }
          ~services:
            { now = Agent_protocol.Timestamp.now
            ; create_attachment_id = Agent_protocol.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "handoff-test")
            ; job_results = None
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; notification_limits = Agent_session.Staged_notifications.default_limits
            ; ingress_limits = Agent_session.Staged_ingress.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Option.iter inference ~f:(fun inference ->
        Agent_session.Session_actor.set_runtime_worker
          actor
          ~worker:(Some worker)
          ~inference:(Some inference)
        |> protocol_ok);
      Eio.Promise.resolve actor_ready_u actor;
      Exn.protect
        ~finally:(fun () -> Agent_session.Session_actor.shutdown actor)
        ~f:(fun () ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
            let writer, _ =
              Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
              |> protocol_ok
            in
            Agent_session.Session_actor.start actor ~attachment_id:writer.id
            |> protocol_ok
            |> ignore;
            let entry =
              Agent_session.History_codec.user_text ~id:history_id "handoff"
              |> Agent_session.History_codec.to_protocol
            in
            Agent_session.Session_actor.submit_message
              actor
              ~attachment_id:writer.id
              entry
            |> protocol_ok
            |> ignore;
            f env actor writer backend))))
;;

let publication_call caps ?(custom = false) () =
  let id =
    History_entry.Id_source.allocate
      caps.Agent_session.Operation_worker.Capabilities.id_source
    |> Result.ok_or_failwith
  in
  let item =
    if custom
    then
      Openai.Responses.Item.Custom_tool_call
        { name = "read_file"
        ; input = "{}"
        ; call_id = "reused"
        ; _type = "custom_tool_call"
        ; id = None
        }
    else
      Openai.Responses.Item.Function_call
        { name = "read_file"
        ; arguments = "{}"
        ; call_id = "reused"
        ; _type = "function_call"
        ; id = None
        ; status = None
        }
  in
  let call = Openai.Responses_history.create_with_id_exn ~id item in
  let invocation =
    Agent_protocol.Invocation.create
      { (invocation_fixture ()).context with
        id = Agent_protocol.Id.Invocation.create ()
      ; origin = Model
      ; provider_call_id = Some "reused"
      ; call_entry_id = Some id
      }
    |> protocol_ok
  in
  call, invocation
;;

let publication_output
      caps
      ?(custom = false)
      ?(text = "{\"type\":\"complete\",\"value\":\"done\"}")
      ()
  =
  let id =
    History_entry.Id_source.allocate
      caps.Agent_session.Operation_worker.Capabilities.id_source
    |> Result.ok_or_failwith
  in
  let item =
    if custom
    then
      Openai.Responses.Item.Custom_tool_call_output
        { output = Text text
        ; call_id = "reused"
        ; _type = "custom_tool_call_output"
        ; id = None
        }
    else
      Openai.Responses.Item.Function_call_output
        { output = Text text
        ; call_id = "reused"
        ; _type = "function_call_output"
        ; id = None
        ; status = None
        }
  in
  Openai.Responses_history.create_with_id_exn ~id item
;;

let resolve_publication caps invocation =
  caps.Agent_session.Operation_worker.Capabilities.with_moderator_invocation
    ~invocation
    (fun ~dispatched ~commit ->
       let resolved =
         Agent_protocol.Invocation.resolve
           dispatched
           ~session_id:dispatched.context.session_id
           ~generation:dispatched.context.generation
           (Complete (`String "done"))
         |> protocol_ok
       in
       commit ~resolved ~snapshot:(handoff_snapshot 1))
;;

let native_registry ?(custom = false) ?(on_call = fun () -> ()) calls ~raises =
  let module Definition = struct
    type input = string

    let name = "read_file"
    let description = Some "native invocation fixture"
    let type_ = if custom then "custom" else "function"
    let parameters = `Object [ "type", `String (if custom then "string" else "object") ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun input ->
         Int.incr calls;
         on_call ();
         assert (String.equal input "{}");
         if raises then failwith "private runner diagnostic";
         Openai.Responses.Tool_output.Output.Text "private output")
  in
  Chat_response.Tool_capability.create
    ~owner:"fixture"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
    [ Chatmd_shell_spec.Source_ref.digest "native v1", implementation ]
  |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
  |> Result.ok_or_failwith
;;

let native_context ?(input = `Object []) registry invocation =
  let module C = Chat_response.Tool_capability in
  let reference = List.hd_exn (C.references registry) in
  let invocation =
    Agent_protocol.Invocation.create
      { invocation.Agent_protocol.Invocation.context with
        input
      ; implementation_revision = reference.implementation_revision
      ; capability_fingerprint = C.fingerprint registry
      }
    |> protocol_ok
  in
  reference, invocation
;;

let handoff_definition
      ?capability_registry
      ?execution_policy
      ?snapshot
      ?(declare_tool = true)
      ?(events = "| _ -> Task.pure(state)")
      ?(script_limits = "")
      ?(schema = "true")
      ?(finish = "Task.pure(state)")
      ?(resolve = "Invocation.resolve(p.context.invocation_id, `Complete(`Null))")
      ?(moderator_capabilities = Chat_response.Moderation.Capabilities.default)
      env
  =
  let module EC = Chat_response.Extension_compiler in
  let module M = Chat_response.Moderator_manager in
  let module C = Chat_response.Tool_capability in
  let dir = Eio.Stdenv.cwd env in
  let source =
    {|<script id="handoff" language="chatml" kind="moderator" api="extensibility-v1" |}
    ^ script_limits
    ^ {|>
    let initial_state = [0]
    let on_event = fun ctx state event -> match event with
    | `Tool_invoked(p) ->
      let ignored = state[0] <- state[0] + 1 in
      Task.bind(Runtime.emit(`String("committed")), fun ignored ->
      Task.bind(|}
    ^ resolve
    ^ {|, fun ignored -> |}
    ^ finish
    ^ {|))
    |}
    ^ events
    ^ "\n</script>"
    ^
    match declare_tool with
    | true ->
      {|<tool name="counter" type="moderator" moderator="handoff"
      input_schema="schema.json" output_schema="schema.json"/>|}
    | false -> ""
  in
  let loader =
    Source_loader.captured_filesystem ~root:dir ~sources:[ "schema.json", schema ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs ~dir ~source_loader:loader source
  in
  let capabilities =
    match capability_registry with
    | Some registry -> registry
    | None ->
      C.create
        ~owner:"handoff"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "fixture")
        []
      |> Result.map_error ~f:(fun e -> e.C.message)
      |> Result.ok_or_failwith
  in
  let definition =
    EC.prepare_definition_in_domain ~env ~capabilities elements
    |> Result.map_error ~f:(fun ds ->
      String.concat ~sep:"; " (List.map ds ~f:Chatmd_shell_spec.Diagnostic.to_string))
    |> Result.ok_or_failwith
  in
  let _, artifact =
    M.Registry.of_definition M.Registry.empty definition |> Result.ok_or_failwith
  in
  let allocator =
    History_entry.Allocator.create ~namespace:"handoff-overlay" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let manager =
    M.create_entries
      ~env
      ?execution_policy
      ?snapshot
      ~artifact:(Option.value_exn artifact)
      ~capabilities:moderator_capabilities
      ~allocator
      ()
    |> Result.ok_or_failwith
  in
  let invocation () =
    let tool = List.hd_exn (EC.prepared_tools definition) in
    Agent_protocol.Invocation.create
      { (invocation_fixture ()).context with
        id = Agent_protocol.Id.Invocation.create ()
      ; tool_name = "counter"
      ; implementation_revision = EC.fingerprint tool
      ; capability_fingerprint = C.fingerprint (EC.capabilities tool)
      }
    |> protocol_ok
  in
  manager, invocation, definition
;;

let handoff_manager env =
  let manager, invocation, _ = handoff_definition env in
  manager, invocation
;;

let permission_policy ~tool_default ~fallback ~evaluator ~reviewer =
  Agent_session.Permission_policy.create
    ~id:"test.permission"
    ~tool_default
    ~approval_timeout_ms:None
    ~fallback
    ~manifest_authorization:Require_grant
    ~evaluator
    ~evaluator_revision:(Option.some_if (Option.is_some evaluator) "test-v1")
    ~reviewer
  |> protocol_ok
;;

let audit_actor ?(with_invocation = false) ~sw ~env ~workspace_instance ~reject_archive ()
  =
  let initial =
    actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
  in
  let entry =
    Agent_session.History_codec.user_text ~id:history_id "original history"
    |> Agent_session.History_codec.to_protocol
  in
  let initial =
    { initial with
      conversation = { initial.conversation with canonical_history = [ entry ] }
    }
  in
  let initial =
    if not with_invocation
    then initial
    else (
      let call =
        Openai.Responses_history.create_with_id_exn
          ~id:history_id
          (Openai.Responses.Item.Function_call
             { name = "read_file"
             ; arguments = "null"
             ; call_id = "audit-call"
             ; id = None
             ; status = None
             ; _type = "function_call"
             })
        |> Agent_session.History_codec.to_protocol
      in
      let inv =
        Agent_protocol.Invocation.create
          { (invocation_fixture ()).context with
            origin = Model
          ; provider_call_id = Some "audit-call"
          ; call_entry_id = Some history_id
          }
        |> protocol_ok
        |> Agent_protocol.Invocation.dispatch
        |> protocol_ok
      in
      let inv =
        Agent_protocol.Invocation.resolve inv ~session_id ~generation:0 (Complete `Null)
        |> protocol_ok
      in
      { initial with
        invocations = [ inv ]
      ; conversation = { initial.conversation with canonical_history = [ call ] }
      })
  in
  let backend =
    Agent_session.Memory_backend.create ~event_capacity:64 ~initial_state:initial
  in
  let persistence = Agent_session.Memory_backend.persistence backend in
  let persistence =
    Agent_session.Session_actor.
      { archive_reference
      ; commit =
          (fun ~command_audit ~previous transition ->
            if
              reject_archive
              && List.length
                   transition.Agent_session.Session_transition.state.conversation
                     .compaction_archives
                 > List.length previous.conversation.compaction_archives
            then
              Error
                (Agent_protocol.Error.create
                   Persistence_error
                   ~message:"injected archive failure"
                   ~retryable:false
                   ())
            else persistence.commit ~command_audit ~previous transition)
      }
  in
  let actor =
    Agent_session.Session_actor.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~mailbox_capacity:32
      ~compaction_env:None
      ~initial_state:initial
      ~persistence
      ~operation_worker:None
      ~services:
        { now = Agent_protocol.Timestamp.now
        ; create_attachment_id = Agent_protocol.Id.Attachment.create
        ; create_reclaim_token = (fun () -> "audit-token")
        ; job_results = None
        ; monotonic_now = (fun () -> Mtime.min_stamp)
        ; schedule_limits = Agent_session.Staged_schedules.default_limits
        ; notification_limits = Agent_session.Staged_notifications.default_limits
        ; ingress_limits = Agent_session.Staged_ingress.default_limits
        ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
        ; state_committed = (fun _ _ -> ())
        }
  in
  actor, backend
;;
