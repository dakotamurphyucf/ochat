open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module B = Agent_session.Runtime_builder
module R = Agent_session.Prompt_revision
module Source = Agent_session.Authored_agent_source
module Binding = Agent_session.Authored_agent_binding
module Authority = Agent_session.Delegation_authority
module C = Chat_response.Tool_capability
module D = Agent_store.Delegation_store
module Store = Agent_store.Session_store
module Artifacts = Agent_store.Prompt_artifact_store

let%expect_test
    "authored child dispatch owns private tools and independent moderator state"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let root =
        Eio.Path.(Eio.Stdenv.fs env / workspace_instance.canonical_root.native_path)
      in
      List.iter [ "agents"; "data"; "session"; "cache" ] ~f:(fun directory ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(root / directory));
      let save name contents =
        Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / name) contents
      in
      save
        "root.chatmd"
        {|<developer>Parent instructions.</developer>
<tool name="apply_patch"/>
<tool name="researcher" agent="agents/researcher.chatmd" local persistence="optional"/>|};
      save "data/value.txt" "approved-private-value";
      save
        "agents/input.json"
        {|{"type":"object","properties":{},"additionalProperties":false}|};
      save "agents/output.json" {|{"type":"string"}|};
      save
        "agents/researcher.chatmd"
        {|<config model="authored-specialist" reasoning_effort="high"/>
<developer>Captured specialist instructions.</developer>
<tool name="read_file"><read id="data" path="${workspace}/data"/></tool>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = [0]
let read () = Tool.call("read_file", `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("value.txt")}]))
let on_event ctx state event = match event with
| `Session_start ->
    let* result = read() in
    (match result with
     | `Ok(_) -> let ignored = state[0] <- 10 in Task.pure(state)
     | `Error(code) -> Task.fail(code))
| `Tool_invoked(p) ->
    let* result = read() in
    (match result with
     | `Ok(_) ->
         let ignored = state[0] <- state[0] + 1 in
         let* ignored = Invocation.resolve(p.context.invocation_id, `Complete(`String(String.concat("specialist-count-", to_string(state[0]))))) in
         Task.pure(state)
     | `Error(code) -> Task.fail(code))
| _ -> Task.pure(state)
</script>
<tool name="counter" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|};
      let catalog =
        Agent_session.Prompt_definition.create
          ~id:prompt_id
          ~config_name:"authored-runtime"
          ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
          ~allowed_workspaces:[ workspace_id ]
          ~permission_profile:"interactive"
          ~runtime_policy:None
          ~enabled:true
          ~description:None
        |> store_ok
      in
      let store =
        Store.create
          ~env
          ~sw
          ~root:(Eio.Path.native_exn Eio.Path.(root / "store"))
          ~server_id:(P.Id.Server.create ())
          ~process_start_identity:None
          ~lock_nonce:"authored-runtime"
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () -> Store.close store |> store_ok)
        ~f:(fun () ->
          let artifacts =
            Artifacts.create
              ~env
              ~root:(Agent_store.Data_root.prompt_artifacts_path (Store.data_root store))
            |> store_ok
          in
          let parent_revision =
            Agent_session.Prompt_revision_builder.build
              ~env
              ~artifact_store:artifacts
              ~transaction_id:(P.Id.Transaction.create ())
              ~created_at:timestamp
              catalog
            |> Authored_agent_source_tests.built
          in
          let paths : Agent_session.Runtime_paths.t =
            { workspace = root
            ; tool_dir = root
            ; prompt_dir = R.materialized_tree parent_revision
            ; session_dir = Eio.Path.(root / "session")
            ; cache_dir = Eio.Path.(root / "cache")
            ; home = root
            }
          in
          let prepared =
            B.prepare_authored_resources
              ~parent_revision
              ~tool_name:"researcher"
              ~native_service_revision:None
              ~env
              ~sw
              ~paths
              ~storage_paths:paths
              ~session_id
              ~one_off_policy:Chat_response.One_off_request.default_policy
              ~authoring_validation_host:None
              ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
              ~approval_provider:Shell_runtime.Approval_broker.None_available
              ~approval_store:(Shell_access.Approval.create_store ())
            |> protocol_ok
          in
          let private_caps = Runtime_resource_tests.capabilities prepared.resources in
          let registration =
            Agent_session.Authored_agent_call.registration
              ~source:prepared.source
              ~capabilities:private_caps
              ~services:(fun _ -> failwith "unexpected nested specialist creation")
              ()
            |> protocol_ok
          in
          let public =
            C.create
              ~result_contracts:[ "researcher", registration.result_contract ]
              ~owner:"authored-parent"
              ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "parent")
              [ registration.implementation_revision, registration.implementation ]
            |> Authored_agent_authority_tests.caps_ok
          in
          let binding =
            Binding.bind
              ~source:prepared.source
              ~public
              ~reference:(List.hd_exn (C.references public))
              ~capabilities:private_caps
            |> protocol_ok
          in
          let profile =
            permission_policy
              ~tool_default:Allow
              ~fallback:Fallback_deny
              ~evaluator:None
              ~reviewer:None
          in
          let parent =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          in
          let parent =
            { parent with
              identity = { parent.identity with session_id = third_session_id }
            ; spec =
                { parent.spec with
                  prompt_revision_id = R.id parent_revision
                ; permission_profile = profile.id
                ; permission_profile_digest = profile.revision_digest
                }
            ; lifecycle = { desired = Running; observed = Idle }
            }
          in
          let artifact =
            Source.artifact
              prepared.source
              ~revision_id:(P.Id.Prompt_revision.create ())
              ~created_at:timestamp
            |> store_ok
          in
          let ledger = Store.delegations store in
          let admission : D.Admission.t =
            { child_session_id = session_id
            ; revision_id = artifact.revision_id
            ; transaction_id = P.Id.Transaction.create ()
            ; manifest_sha256 = artifact.manifest_sha256
            ; parent_revision_id = parent.spec.prompt_revision_id
            ; parent_stop_epoch = Some parent.stop_epoch
            ; authored_tool =
                Some
                  { name = "researcher"
                  ; source_sha256 = Source.fingerprint prepared.source
                  }
            ; authority_sha256 = Authority.fingerprint parent |> protocol_ok
            ; capability_pins =
                Chat_response.Background_request.capability_pins private_caps
                |> protocol_ok
            ; lifetime = Owned
            ; created_at = timestamp
            }
          in
          let reserved =
            D.reserve
              ledger
              ~key:
                { parent_session_id = third_session_id
                ; parent_generation = 0
                ; principal_id
                ; idempotency_key =
                    P.Idempotency_key.of_string "authored-runtime" |> protocol_ok
                }
              ~request_sha256:(Source.fingerprint prepared.source)
              ~admission
              ~max_records:8
              ~max_bytes:1048576
            |> store_ok
            |> function
            | D.New record -> record
            | _ -> failwith "expected new reservation"
          in
          Source.install_reserved
            ~delegations:ledger
            ~reservation:reserved
            ~artifact_store:artifacts
            ~capability_pins:admission.capability_pins
            prepared.source
          |> protocol_ok
          |> ignore;
          let revision =
            Agent_session.Prompt_revision_builder.reparse
              ~definition:catalog
              ~artifact
              ~materialized_tree:
                (Artifacts.materialized_tree artifacts artifact.revision_id)
            |> Authored_agent_source_tests.built
          in
          let authority =
            Authority.create
              ~host:
                { state =
                    (fun id ->
                      assert (P.Id.Session.equal id third_session_id);
                      Ok parent)
                ; resolve =
                    (fun reference ->
                      D.resolve ledger reference
                      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
                ; capabilities =
                    (fun id ->
                      assert (P.Id.Session.equal id third_session_id);
                      Ok public)
                }
              ~authored_capabilities:(fun record ~public ->
                Binding.resolve binding ~record ~public ~current:private_caps)
              ~reference:(D.reference reserved)
              ~capabilities:private_caps
              ()
          in
          let actor_ref = ref None in
          let actor () = Option.value_exn !actor_ref in
          let admitted = ref 0
          and requests = ref 0
          and revoke_during_read = ref false in
          let services : B.extension_services =
            { runtime_policy = Chat_response.Runtime_semantics.default_policy
            ; script_tools =
                (fun native ->
                  assert (
                    String.equal
                      (C.fingerprint private_caps)
                      (C.fingerprint (Generated_runtime_tests.capabilities native)));
                  Agent_session.Script_tool_calls.create
                    ~registry:(fun () -> private_caps)
                    ~moderator_names:(String.Set.singleton "counter")
                    ~now:P.Timestamp.now
                    ~is_halted:(fun () -> (A.state (actor ()) |> protocol_ok).halted)
                    ~requires_active_moderator:(fun _ -> false)
                    ~authorize:(fun invocation _ ->
                      assert (
                        P.Id.Session.equal
                          invocation.P.Invocation.context.session_id
                          session_id);
                      Int.incr admitted;
                      Eio.Fiber.yield ();
                      (match !revoke_during_read, invocation.context.tool_name with
                       | true, "read_file" ->
                         D.revoke ledger reserved Authority_changed |> store_ok |> ignore
                       | _ -> ());
                      Ok ())
                    ~prepare_output:(function
                      | Openai.Responses.Tool_output.Output.Text text -> Ok (`String text)
                      | output ->
                        Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output))
                    ~defer_observation:(fun _ -> Ok ()))
            ; standalone_execution_limits =
                Agent_session.Standalone_tool_dispatch.declared_execution_limits
            ; one_off_policy = Chat_response.One_off_request.default_policy
            ; native_service_revision = None
            ; authoring_validation_host = None
            ; claim_lifecycle =
                (fun ~event ->
                  A.with_current_moderator_event (actor ()) ~operation_id:None ~event)
            ; lifecycle_started = (fun _ -> false)
            ; history =
                (fun () ->
                  (A.state (actor ()) |> protocol_ok).conversation.canonical_history
                  |> Agent_session.History_codec.all_of_protocol
                  |> protocol_ok)
            ; standalone_completion =
                (fun ~tools:_ _ -> failwith "unexpected standalone completion")
            ; idle_notifications = (fun ~source:_ ~tools:_ () -> Ok false)
            ; notification_input =
                (fun ~source:_ ~tools:_ ~operation_id:_ () ->
                  Ok Chat_response.In_memory_stream.Safe_point_input.empty)
            ; initial_notification_input =
                (fun ~source:_ ~tools:_ ~operation_id:_ () ->
                  Ok Chat_response.In_memory_stream.Safe_point_input.empty)
            }
          in
          let post_stream ~sw:_ ~inputs:_ =
            Int.incr requests;
            match !requests mod 2 with
            | 1 ->
              let open Openai.Responses.Response_stream in
              let id = "counter-" ^ Int.to_string !requests in
              [ Output_item_added
                  { item =
                      Function_call
                        { name = "counter"
                        ; arguments = ""
                        ; call_id = id
                        ; _type = "function_call"
                        ; id = Some id
                        ; status = None
                        }
                  ; output_index = 0
                  ; type_ = "response.output_item.added"
                  }
              ; Function_call_arguments_done
                  { arguments = "{}"
                  ; item_id = id
                  ; output_index = 0
                  ; type_ = "response.function_call_arguments.done"
                  }
              ]
              |> Stdlib.List.to_seq
            | _ -> Stdlib.Seq.empty
          in
          let build ?(snapshot = None) () =
            B.build_authored_child
              ~services
              ~revision
              ~prepared
              ~authority
              ~history:[]
              ~sw
              ~env
              ~paths
              ~storage_paths:paths
              ~session_id
              ~history_namespace:(P.Id.Session.to_string session_id)
              ~next_history_sequence:1
              ~existing_moderator_snapshot:snapshot
              ~moderator_reservation_size:100
              ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
              ~approval_provider:Shell_runtime.Approval_broker.None_available
              ~approval_store:(Shell_access.Approval.create_store ())
              ~permission_profile:profile
              ~model_post_stream:(Some post_stream)
              ~review_permission:(fun _ -> failwith "unexpected reviewer")
              ~schedule_services:
                { after_ms = (fun ~delay_ms:_ ~payload:_ -> failwith "unexpected timer")
                ; cancel = (fun ~id:_ -> failwith "unexpected timer")
                }
              ~job_services:
                { spawn_model =
                    (fun ~recipe:_ ~payload:_ -> failwith "unexpected model recipe")
                ; call_model =
                    (fun ~recipe:_ ~payload:_ ~execute:_ ->
                      failwith "unexpected model recipe")
                }
          in
          let runtime = build () |> protocol_ok in
          assert (Int.equal 0 !requests && Int.equal 0 !admitted);
          let count runtime =
            match
              (Chat_response.Moderator_manager.identity_snapshot
                 (Option.value_exn runtime.B.moderator_manager)
               |> Result.ok_or_failwith)
                .current_state
            with
            | Session.Snapshot.Array [ Int count ] -> count
            | _ -> failwith "unexpected specialist state"
          in
          let initial =
            actor_state
              ~workspace_instance
              ~liveness:Process_bound
              ~start_immediately:false
          in
          let initial =
            { initial with
              spec =
                { initial.spec with
                  prompt_revision_id = artifact.revision_id
                ; delegation = Some (D.reference reserved)
                ; permission_profile = profile.id
                ; permission_profile_digest = profile.revision_digest
                ; protocol =
                    { initial.spec.protocol with
                      prompt = Generated artifact.revision_id
                    ; persistence = Durable
                    }
                }
            ; moderator = runtime.start_moderator () |> protocol_ok
            ; conversation =
                { initial.conversation with
                  canonical_history =
                    Agent_session.History_codec.all_to_protocol runtime.initial_history
                ; initial_prompt_entry_count = runtime.initial_prompt_entry_count
                ; next_history_sequence = Int64.of_int runtime.reserved_history_through
                ; reserved_history_through = Int64.of_int runtime.reserved_history_through
                }
            }
          in
          let handle =
            Store.create_session
              store
              ~sw
              ~transaction_id:admission.transaction_id
              ~actor_lock_nonce:"authored-runtime"
              { schema_version = Store.current_schema_version
              ; session = Agent_session.Session_state.summary initial
              ; prompt_artifact = P.Id.Prompt_revision.to_string artifact.revision_id
              ; workspace_identity = workspace_instance.conflict_domain
              ; data_schema_version = Agent_session.Session_state.current_schema_version
              }
            |> store_ok
          in
          Agent_session.Session_persistence.install_snapshot
            ~env
            ~handle
            ~max_payload_length:1048576
            ~transaction_hash:None
            initial
          |> store_ok
          |> ignore;
          List.iter [ D.Child_installed; Linked ] ~f:(fun stage ->
            D.advance ledger reserved stage |> store_ok |> ignore);
          let backend =
            Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
          in
          let actor_value =
            A.create
              ~sw
              ~clock:(Eio.Stdenv.clock env)
              ~mailbox_capacity:32
              ~compaction_env:None
              ~initial_state:initial
              ~operation_worker:None
              ~persistence:(Agent_session.Memory_backend.persistence backend)
              ~services:
                { now = P.Timestamp.now
                ; create_attachment_id = P.Id.Attachment.create
                ; create_reclaim_token = (fun () -> "authored-runtime")
                ; job_results = None
                ; monotonic_now = (fun () -> Mtime.min_stamp)
                ; schedule_limits = Agent_session.Staged_schedules.default_limits
                ; notification_limits = Agent_session.Staged_notifications.default_limits
                ; ingress_limits = Agent_session.Staged_ingress.default_limits
                ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
                ; state_committed = (fun _ _ -> ())
                }
          in
          actor_ref := Some actor_value;
          let owner =
            Agent_server.Runtime_owner.create
              ~actor:actor_value
              ~initial:(Some runtime)
              ~build:(fun () -> failwith "unexpected rebuild")
          in
          Exn.protect
            ~finally:(fun () ->
              Agent_server.Runtime_owner.close owner;
              A.shutdown actor_value;
              Store.close_session store handle |> store_ok)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                [%test_eq: string list]
                  [ "counter"; "read_file" ]
                  (List.filter_map runtime.moderator_tools ~f:(function
                     | Openai.Responses.Request.Tool.Function tool -> Some tool.name
                     | _ -> None)
                   |> List.sort ~compare:String.compare);
                A.set_operation_worker actor_value (Some runtime.worker) |> protocol_ok;
                let writer, _ =
                  A.attach actor_value ~mode:Read_write ~subscribe:false |> protocol_ok
                in
                A.start actor_value ~attachment_id:writer.id |> protocol_ok |> ignore;
                let submit text =
                  let reservation =
                    A.reserve_history_block actor_value ~count:1 |> protocol_ok
                  in
                  let id =
                    History_entry.Id.create
                      ~namespace:(P.Id.Session.to_string session_id)
                      ~sequence:(Int64.to_int_exn reservation.first_sequence)
                    |> Result.ok_or_failwith
                  in
                  let message =
                    runtime.parse_user_content
                      ~id
                      { kind = Plain_text; text; attachments = [] }
                    |> protocol_ok
                    |> Agent_session.History_codec.to_protocol
                  in
                  A.submit_message actor_value ~attachment_id:writer.id message
                  |> protocol_ok
                  |> ignore;
                  let rec wait () =
                    let state = A.state actor_value |> protocol_ok in
                    match state.active_operation with
                    | None -> state
                    | Some _ ->
                      Eio.Fiber.yield ();
                      wait ()
                  in
                  wait ()
                in
                let first = submit "Count once." in
                [%test_eq: int] 11 (count runtime);
                let second = submit "Count again." in
                [%test_eq: int] 12 (count runtime);
                assert_same_session_snapshot
                  second
                  (Agent_session.Memory_backend.state backend);
                let private_reads state =
                  List.filter_map
                    state.Agent_session.Session_state.invocations
                    ~f:(fun invocation ->
                      match
                        invocation.P.Invocation.context.tool_name, invocation.status
                      with
                      | ( "read_file"
                        , (Published (Complete value) | Resolved (Complete value)) ) ->
                        Some (Jsonaf.to_string value)
                      | _ -> None)
                in
                [%test_eq: int] 3 (List.length (private_reads second));
                List.iter (private_reads second) ~f:(fun output ->
                  assert (String.is_substring output ~substring:"approved-private-value"));
                let rendered state =
                  state.Agent_session.Session_state.conversation.canonical_history
                  |> List.map ~f:(fun entry -> Jsonaf.to_string entry.P.History.payload)
                  |> String.concat
                in
                assert (
                  String.is_substring (rendered first) ~substring:"specialist-count-11");
                assert (
                  String.is_substring (rendered second) ~substring:"specialist-count-12");
                let saved = runtime.start_moderator () |> protocol_ok in
                let fresh = build () |> protocol_ok in
                Exn.protect ~finally:fresh.close ~f:(fun () ->
                  [%test_eq: int] 0 (count fresh));
                let restored = build ~snapshot:saved () |> protocol_ok in
                Exn.protect ~finally:restored.close ~f:(fun () ->
                  [%test_eq: int] 12 (count restored));
                [%test_eq: int] 4 !requests;
                save "data/value.txt" "must-not-be-disclosed-after-revocation";
                revoke_during_read := true;
                let revoked = submit "Revoke while the private read is admitted." in
                [%test_eq: string list] (private_reads second) (private_reads revoked);
                assert (
                  not
                    (String.is_substring
                       (rendered revoked)
                       ~substring:"specialist-count-13"));
                assert (Result.is_error ((Option.value_exn runtime.check_execution) ()));
                assert (Result.is_error (build ()));
                [%test_eq: int] 5 !requests;
                print_endline
                  "private native + own ghost dispatch; repeated state 11/12; fresh \
                   state 0; restored state 12; revocation blocks nested read, next model \
                   request and rebuild PASS")))));
  [%expect
    {| private native + own ghost dispatch; repeated state 11/12; fresh state 0; restored state 12; revocation blocks nested read, next model request and rebuild PASS |}]
;;
