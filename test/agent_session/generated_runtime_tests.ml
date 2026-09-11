open Core
open Fixtures
module P = Agent_protocol
module A = Agent_session.Session_actor
module B = Agent_session.Runtime_builder
module G = Agent_session.Generated_definition
module C = Chat_response.Tool_capability
module AR = Chat_response.Agent_runtime
module Store = Agent_store.Prompt_artifact_store
module Session_store = Agent_store.Session_store
module D = Agent_store.Delegation_store
module Authority = Agent_session.Delegation_authority

let capabilities runtime =
  Lazy.force runtime.AR.capabilities
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let source =
  {|<config model="generated-child-model" reasoning_effort="high"/>
<developer>Child instructions only.</developer>
<tool type="inherited" name="read_file"/>
<script id="child-moderator" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = [0]
let on_event ctx state event = match event with
| `Session_start ->
    let* result = Tool.call("read_file", `Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("value.txt")}])) in
    (match result with
     | `Ok(_) -> let ignored = state[0] <- state[0] + 1 in Task.pure(state)
     | `Error(_) -> Task.pure(state))
| `Session_resume -> let ignored = state[0] <- state[0] + 10 in Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let prepare ~env ~root ~revision_id ~created_at ~parent source =
  let bundle =
    Chatmd_source_bundle.create
      ~root_file:"child.chatmd"
      ~sources:[ "child.chatmd", source ]
      ()
    |> Result.ok_or_failwith
  in
  G.prepare
    ~env
    ~dir:root
    ~revision_id
    ~created_at
    ~current_capabilities:(fun () -> capabilities parent)
    ~references:(C.references (capabilities parent))
    bundle
  |> Result.map_error ~f:(fun errors ->
    String.concat ~sep:"; " (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
  |> Result.ok_or_failwith
;;

let parent_runtime ~env ~sw ~root =
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir:root
      {|<tool name="read_file"><read id="data" path="${workspace}/data"/></tool><tool name="append_to_file"/>|}
  in
  let ctx =
    Chat_response.Ctx.create
      ~env
      ~dir:root
      ~tool_dir:root
      ~cache:(Chat_response.Cache.create ~max_size:16 ())
  in
  let host =
    AR.host
      ~env
      ~workspace:root
      ~tool_dir:root
      ~prompt_dir:root
      ~session_dir:root
      ~cache_dir:root
      ~home:root
      ~session_id:(P.Id.Session.to_string third_session_id)
      ~resource_runner:None
      ~prompt_elements:elements
    |> Result.map_error ~f:(fun errors ->
      String.concat ~sep:"; " (List.map errors ~f:AR.diagnostic_to_string))
    |> Result.ok_or_failwith
  in
  AR.create
    ~sw
    ~ctx
    ~host
    ~platform:(AR.platform ())
    ~prompt_elements:elements
    ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
    ~approval_provider:Shell_runtime.Approval_broker.None_available
    ~approval_store:(Shell_access.Approval.create_store ())
    ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
      failwith "unexpected implicit parent agent")
    ()
  |> Result.map_error ~f:(fun errors ->
    String.concat ~sep:"; " (List.map errors ~f:AR.diagnostic_to_string))
  |> Result.ok_or_failwith
;;

let%expect_test
    "generated actor shares dispatch but preserves inherited roots, admission and own \
     moderator state"
  =
  List.iter
    [ `Allowed
    ; `Plain
    ; `Denied
    ; `Revoked
    ; `Parent_stopped
    ; `Parent_policy_changed
    ; `Admission_revoked
    ; `Disclosure_revoked
    ; `Input_revoked
    ; `Before_model
    ; `Not_linked
    ]
    ~f:(fun mode ->
      with_actor_workspace (fun env workspace_instance ->
        Eio.Switch.run (fun sw ->
          let root =
            Eio.Path.(Eio.Stdenv.fs env / workspace_instance.canonical_root.native_path)
          in
          List.iter
            [ "parent/data"; "child/data"; "child/cache"; "child/session" ]
            ~f:(fun path ->
              Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(root / path));
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(root / "parent/data/value.txt")
            "original-parent-root";
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(root / "child/data/value.txt")
            "child-root-must-not-rebind";
          let parent = parent_runtime ~env ~sw ~root:Eio.Path.(root / "parent") in
          let child_source =
            match mode with
            | `Plain | `Before_model | `Input_revoked ->
              {|<config model="generated-child-model" reasoning_effort="high"/><developer>Child instructions only.</developer><tool type="inherited" name="read_file"/>|}
            | _ -> source
          in
          let definition =
            prepare
              ~env
              ~root
              ~revision_id:(P.Id.Prompt_revision.create ())
              ~created_at:timestamp
              ~parent
              child_source
          in
          let session_store =
            Session_store.create
              ~env
              ~sw
              ~root:(Eio.Path.native_exn Eio.Path.(root / "store"))
              ~server_id:(P.Id.Server.create ())
              ~process_start_identity:None
              ~lock_nonce:"generated-runtime"
            |> store_ok
          in
          let ledger = Session_store.delegations session_store in
          let artifact_store =
            Store.create
              ~env
              ~root:
                (Agent_store.Data_root.prompt_artifacts_path
                   (Session_store.data_root session_store))
            |> store_ok
          in
          let profile =
            permission_policy
              ~tool_default:Allow
              ~fallback:Fallback_deny
              ~evaluator:None
              ~reviewer:None
          in
          let parent_workspace =
            Agent_session.Workspace_resolver.resolve_current
              ~env
              ~instance_id:(P.Id.Workspace_instance.create ())
              ~path:(Eio.Path.native_exn Eio.Path.(root / "parent"))
              ~access:Shared_write
              ~created_at:timestamp
            |> store_ok
          in
          let parent_state =
            actor_state
              ~workspace_instance:parent_workspace
              ~liveness:Detached
              ~start_immediately:false
          in
          let parent_state =
            ref
              { parent_state with
                identity = { parent_state.identity with session_id = third_session_id }
              ; spec =
                  { parent_state.spec with
                    permission_profile = profile.id
                  ; permission_profile_digest = profile.revision_digest
                  }
              ; lifecycle = { desired = Running; observed = Idle }
              }
          in
          let admission =
            D.Admission.
              { child_session_id = session_id
              ; revision_id = (G.artifact definition).revision_id
              ; transaction_id = P.Id.Transaction.create ()
              ; manifest_sha256 = (G.artifact definition).manifest_sha256
              ; parent_revision_id = !parent_state.spec.prompt_revision_id
              ; parent_stop_epoch = Some !parent_state.stop_epoch
              ; authority_sha256 = Authority.fingerprint !parent_state |> protocol_ok
              ; capability_pins = G.capability_pins definition
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
                    P.Idempotency_key.of_string "generated-runtime-child" |> protocol_ok
                }
              ~request_sha256:(Chatmd_shell_spec.Source_ref.digest child_source)
              ~admission
              ~max_records:8
              ~max_bytes:1048576
            |> store_ok
            |> function
            | D.New record -> record
            | _ -> failwith "expected new child admission"
          in
          let _ =
            G.install_reserved
              ~delegations:ledger
              ~reservation:reserved
              ~artifact_store
              definition
            |> Result.map_error ~f:(fun errors ->
              String.concat
                ~sep:"; "
                (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
            |> Result.ok_or_failwith
          in
          let parent_runtime_ref = ref parent in
          let authority_for definition =
            Authority.create
              ~host:
                { state =
                    (fun id ->
                      assert (P.Id.Session.equal id third_session_id);
                      Ok !parent_state)
                ; resolve =
                    (fun reference ->
                      D.resolve ledger reference
                      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
                ; capabilities =
                    (fun id ->
                      assert (P.Id.Session.equal id third_session_id);
                      Ok (capabilities !parent_runtime_ref))
                }
              ~reference:(D.reference reserved)
              ~capabilities:
                (Chat_response.Generated_admission.capabilities (G.admission definition))
              ()
          in
          let actor_ref = ref None in
          let actor () = Option.value_exn !actor_ref in
          let registry =
            ref (Chat_response.Generated_admission.capabilities (G.admission definition))
          in
          let admitted = ref 0
          and requests = ref 0 in
          let services : B.extension_services =
            { runtime_policy = Chat_response.Runtime_semantics.default_policy
            ; script_tools =
                (fun native ->
                  assert (
                    String.equal
                      (C.fingerprint !registry)
                      (C.fingerprint (capabilities native)));
                  Agent_session.Script_tool_calls.create
                    ~registry:(fun () -> !registry)
                    ~moderator_names:String.Set.empty
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
                      match mode with
                      | `Allowed
                      | `Plain
                      | `Before_model
                      | `Not_linked
                      | `Disclosure_revoked
                      | `Input_revoked -> Ok ()
                      | `Parent_stopped ->
                        parent_state
                        := { !parent_state with
                             lifecycle = { desired = Stopped; observed = Stopped }
                           };
                        Ok ()
                      | `Parent_policy_changed ->
                        parent_state
                        := { !parent_state with
                             spec =
                               { !parent_state.spec with
                                 permission_profile_digest =
                                   Chatmd_shell_spec.Source_ref.digest
                                     "changed parent policy"
                               }
                           };
                        Ok ()
                      | `Admission_revoked ->
                        let _ = D.revoke ledger reserved Authority_changed |> store_ok in
                        Ok ()
                      | `Denied ->
                        Error
                          (P.Error.create
                             Permission_denied
                             ~message:"inherited host policy denied"
                             ~retryable:false
                             ())
                      | `Revoked ->
                        registry
                        := C.select !registry ~names:[]
                           |> Result.map_error ~f:(fun e -> e.C.message)
                           |> Result.ok_or_failwith;
                        Ok ())
                    ~prepare_output:(fun output ->
                      (match mode with
                       | `Disclosure_revoked ->
                         Eio.Fiber.yield ();
                         ignore
                           (D.revoke ledger reserved Authority_changed |> store_ok
                            : D.record)
                       | _ -> ());
                      match output with
                      | Openai.Responses.Tool_output.Output.Text text -> Ok (`String text)
                      | output ->
                        Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output))
                    ~defer_observation:(fun _ -> Ok ()))
            ; standalone_execution_limits =
                Agent_session.Standalone_tool_dispatch.declared_execution_limits
            ; one_off_policy = Chat_response.One_off_request.default_policy
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
                  (match mode with
                   | `Input_revoked ->
                     Eio.Fiber.yield ();
                     ignore
                       (D.revoke ledger reserved Authority_changed |> store_ok : D.record)
                   | _ -> ());
                  Ok Chat_response.In_memory_stream.Safe_point_input.empty)
            }
          in
          let paths : Agent_session.Runtime_paths.t =
            { workspace = Eio.Path.(root / "child")
            ; tool_dir = Eio.Path.(root / "child")
            ; prompt_dir =
                Store.materialized_tree artifact_store (G.artifact definition).revision_id
            ; session_dir = Eio.Path.(root / "child/session")
            ; cache_dir = Eio.Path.(root / "child/cache")
            ; home = Eio.Path.(root / "child")
            }
          in
          let post_stream ~sw:_ ~inputs:_ =
            Int.incr requests;
            match !requests with
            | 1 ->
              let open Openai.Responses.Response_stream in
              [ Output_item_added
                  { item =
                      Function_call
                        { name = "read_file"
                        ; arguments = ""
                        ; call_id = "child-read"
                        ; _type = "function_call"
                        ; id = Some "child-read-item"
                        ; status = None
                        }
                  ; output_index = 0
                  ; type_ = "response.output_item.added"
                  }
              ; Function_call_arguments_done
                  { arguments = {|{"root":"data","file":"value.txt"}|}
                  ; item_id = "child-read-item"
                  ; output_index = 0
                  ; type_ = "response.function_call_arguments.done"
                  }
              ]
              |> Stdlib.List.to_seq
            | _ -> Stdlib.Seq.empty
          in
          let build
                ?(parent_runtime = parent)
                ?(existing_moderator_snapshot = None)
                definition
            =
            B.build_generated
              ~services
              ~definition
              ~artifact_store
              ~parent_runtime
              ~authority:(authority_for definition)
              ~sw
              ~env
              ~paths
              ~storage_paths:paths
              ~session_id
              ~history_namespace:(P.Id.Session.to_string session_id)
              ~next_history_sequence:1
              ~existing_history:None
              ~existing_moderator_snapshot
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
          let runtime = build definition |> protocol_ok in
          assert (Int.equal 0 !requests && Int.equal 0 !admitted);
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
                  prompt_revision_id = (G.artifact definition).revision_id
                ; delegation = Some (D.reference reserved)
                ; permission_profile = profile.id
                ; permission_profile_digest = profile.revision_digest
                ; protocol =
                    { initial.spec.protocol with
                      prompt = Generated admission.revision_id
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
          let child_handle =
            Session_store.create_session
              session_store
              ~sw
              ~transaction_id:admission.transaction_id
              ~actor_lock_nonce:"generated-runtime-child"
              { schema_version = Session_store.current_schema_version
              ; session = Agent_session.Session_state.summary initial
              ; prompt_artifact = P.Id.Prompt_revision.to_string admission.revision_id
              ; workspace_identity = workspace_instance.conflict_domain
              ; data_schema_version = Agent_session.Session_state.current_schema_version
              }
            |> store_ok
          in
          let _ =
            Agent_session.Session_persistence.install_snapshot
              ~env
              ~handle:child_handle
              ~max_payload_length:1048576
              ~transaction_hash:None
              initial
            |> store_ok
          in
          let _ = D.advance ledger reserved Child_installed |> store_ok in
          (match mode with
           | `Not_linked -> ()
           | _ -> ignore (D.advance ledger reserved Linked |> store_ok : D.record));
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
                ; create_reclaim_token = (fun () -> "generated-child")
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
              Session_store.close_session session_store child_handle |> store_ok;
              Session_store.close session_store |> store_ok)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                let names =
                  List.filter_map runtime.moderator_tools ~f:(function
                    | Openai.Responses.Request.Tool.Function tool -> Some tool.name
                    | _ -> None)
                in
                [%test_eq: string list] [ "read_file" ] names;
                assert (
                  Result.is_error
                    (runtime.parse_user_content
                       ~id:history_id
                       { kind = Chatmd
                       ; text = {|<user><doc src="../parent/data/value.txt"/></user>|}
                       ; attachments = []
                       }));
                assert (
                  Result.is_error
                    (runtime.execute_model_job
                       ~recipe:Chat_response.Model_executor.agent_prompt_v1_name
                       ~payload:(`Object [])));
                A.set_operation_worker actor_value (Some runtime.worker) |> protocol_ok;
                let writer, _ =
                  A.attach actor_value ~mode:Read_write ~subscribe:false |> protocol_ok
                in
                A.start actor_value ~attachment_id:writer.id |> protocol_ok |> ignore;
                (match mode with
                 | `Before_model ->
                   parent_state
                   := { !parent_state with
                        lifecycle = { desired = Stopped; observed = Stopped }
                      }
                 | _ -> ());
                let message =
                  runtime.parse_user_content
                    ~id:history_id
                    { kind = Plain_text; text = "Inspect the file."; attachments = [] }
                  |> protocol_ok
                  |> Agent_session.History_codec.to_protocol
                in
                A.submit_message actor_value ~attachment_id:writer.id message
                |> protocol_ok
                |> ignore;
                let rec completed () =
                  let state = A.state actor_value |> protocol_ok in
                  match state.active_operation with
                  | None -> state
                  | Some _ ->
                    Eio.Fiber.yield ();
                    completed ()
                in
                let state = completed () in
                assert_same_session_snapshot
                  state
                  (Agent_session.Memory_backend.state backend);
                let rendered =
                  state.conversation.canonical_history
                  |> List.map ~f:(fun entry -> Jsonaf.to_string entry.P.History.payload)
                  |> String.concat
                in
                assert (
                  not
                    (String.is_substring rendered ~substring:"child-root-must-not-rebind"));
                let successes =
                  List.count state.invocations ~f:(fun invocation ->
                    match invocation.P.Invocation.status with
                    | Published (Complete _) | Resolved (Complete _) -> true
                    | _ -> false)
                in
                let snapshot =
                  Option.map runtime.moderator_manager ~f:(fun manager ->
                    Chat_response.Moderator_manager.identity_snapshot manager
                    |> Result.ok_or_failwith)
                in
                let count =
                  match
                    Option.map snapshot ~f:(fun snapshot ->
                      snapshot.Session.Moderator_state.Identity_snapshot.current_state)
                  with
                  | None -> 0
                  | Some (Session.Snapshot.Array [ Int n ]) -> n
                  | _ -> failwith "unexpected child state"
                in
                (match mode with
                 | `Plain ->
                   assert (String.is_substring rendered ~substring:"original-parent-root");
                   [%test_eq: int] 1 successes;
                   [%test_eq: int] 0 count
                 | `Allowed ->
                   assert (String.is_substring rendered ~substring:"original-parent-root");
                   [%test_eq: int] 2 successes;
                   [%test_eq: int] 1 count
                 | `Denied
                 | `Revoked
                 | `Parent_stopped
                 | `Parent_policy_changed
                 | `Admission_revoked
                 | `Disclosure_revoked
                 | `Input_revoked
                 | `Before_model
                 | `Not_linked ->
                   assert (
                     not (String.is_substring rendered ~substring:"original-parent-root"));
                   [%test_eq: int] 0 successes;
                   [%test_eq: int] 0 count);
                let expects_blocked_model =
                  match mode with
                  | `Parent_stopped
                  | `Parent_policy_changed
                  | `Admission_revoked
                  | `Disclosure_revoked
                  | `Input_revoked
                  | `Before_model
                  | `Not_linked -> true
                  | _ -> false
                in
                [%test_eq: int] (if expects_blocked_model then 0 else 2) !requests;
                (match expects_blocked_model with
                 | false -> ()
                 | true ->
                   let failure =
                     Agent_session.Memory_backend.events_after backend 0L
                     |> protocol_ok
                     |> List.exists ~f:(fun event ->
                       match
                         P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                         |> protocol_ok
                       with
                       | Operation_failed { state = Failed error; _ } ->
                         String.is_substring error.message ~substring:"delegation."
                       | _ -> false)
                   in
                   assert failure);
                (match mode with
                 | `Allowed ->
                   let saved = runtime.start_moderator () |> protocol_ok in
                   let fresh_parent =
                     parent_runtime ~env ~sw ~root:Eio.Path.(root / "parent")
                   in
                   assert (Result.is_error (build ~parent_runtime:fresh_parent definition));
                   let restored =
                     G.restore
                       ~env
                       ~artifact_store
                       ~revision_id:(G.artifact definition).revision_id
                       ~manifest_sha256:(G.artifact definition).manifest_sha256
                       ~current_capabilities:(fun () -> capabilities fresh_parent)
                       ~pins:(G.capability_pins definition)
                       ()
                     |> Result.map_error ~f:(fun errors ->
                       String.concat
                         ~sep:"; "
                         (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
                     |> Result.ok_or_failwith
                   in
                   registry
                   := Chat_response.Generated_admission.capabilities
                        (G.admission restored);
                   parent_runtime_ref := fresh_parent;
                   let rebuilt =
                     build
                       ~parent_runtime:fresh_parent
                       ~existing_moderator_snapshot:saved
                       restored
                     |> protocol_ok
                   in
                   Exn.protect ~finally:rebuilt.close ~f:(fun () ->
                     let restored_snapshot =
                       Chat_response.Moderator_manager.identity_snapshot
                         (Option.value_exn rebuilt.moderator_manager)
                       |> Result.ok_or_failwith
                     in
                     [%test_eq: Sexp.t]
                       (Session.Snapshot.sexp_of_t
                          (Option.value_exn snapshot).current_state)
                       (Session.Snapshot.sexp_of_t restored_snapshot.current_state);
                     [%test_eq: int] 2 !requests)
                 | `Plain
                 | `Denied
                 | `Revoked
                 | `Parent_stopped
                 | `Parent_policy_changed
                 | `Admission_revoked
                 | `Disclosure_revoked
                 | `Input_revoked
                 | `Before_model
                 | `Not_linked -> ());
                print_s
                  [%sexp
                    { mode : [ `Allowed
                             | `Plain
                             | `Denied
                             | `Revoked
                             | `Parent_stopped
                             | `Parent_policy_changed
                             | `Admission_revoked
                             | `Disclosure_revoked
                             | `Input_revoked
                             | `Before_model
                             | `Not_linked
                             ]
                    ; successes : int
                    ; moderator_state = (count : int)
                    ; fake_provider_requests = (!requests : int)
                    }])))));
  [%expect
    {|
    ((mode Allowed) (successes 2) (moderator_state 1) (fake_provider_requests 2))
    ((mode Plain) (successes 1) (moderator_state 0) (fake_provider_requests 2))
    ((mode Denied) (successes 0) (moderator_state 0) (fake_provider_requests 2))
    ((mode Revoked) (successes 0) (moderator_state 0) (fake_provider_requests 2))
    ((mode Parent_stopped) (successes 0) (moderator_state 0)
     (fake_provider_requests 0))
    ((mode Parent_policy_changed) (successes 0) (moderator_state 0)
     (fake_provider_requests 0))
    ((mode Admission_revoked) (successes 0) (moderator_state 0)
     (fake_provider_requests 0))
    ((mode Disclosure_revoked) (successes 0) (moderator_state 0)
     (fake_provider_requests 0))
    ((mode Input_revoked) (successes 0) (moderator_state 0)
     (fake_provider_requests 0))
    ((mode Before_model) (successes 0) (moderator_state 0)
     (fake_provider_requests 0))
    ((mode Not_linked) (successes 0) (moderator_state 0)
     (fake_provider_requests 0))
    |}]
;;
