open Core
open Agent_server_test_support
module P = Agent_protocol
module S = Agent_session
module D = Agent_store.Delegation_store
module E = P.Moderator_execution
module Cause = S.Policy_cause_data
module Policy_source = S.Policy_canonical_usage.Source
module Goal_origin = Agent_store.Delegation_goal_origin
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Owner = Agent_server.Runtime_owner
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let state (entry : Registry.entry) = A.state entry.actor |> protocol_ok

let create_child_result
      ?(lifetime = Agent_server.Session_factory.Owned)
      env
      root
      daemon
      (parent : Registry.entry)
  =
  let module G = Agent_session.Generated_definition in
  let module C = Chat_response.Tool_capability in
  let definition =
    Owner.with_background_runtime parent.runtime (fun runtime ->
      let native =
        Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
      in
      let capabilities =
        Lazy.force native.capabilities
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let bundle =
        Chatmd_source_bundle.create
          ~root_file:"child.chatmd"
          ~sources:
            [ ( "child.chatmd"
              , {|<developer>Child.</developer><tool type="inherited" name="read_file"/>|}
              )
            ]
          ()
        |> Result.ok_or_failwith
      in
      G.prepare
        ~env
        ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
        ~revision_id:(P.Id.Prompt_revision.create ())
        ~created_at:(P.Timestamp.now ())
        ~current_capabilities:(fun () -> capabilities)
        ~references:(C.references capabilities)
        bundle
      |> Result.map_error ~f:(fun errors ->
        P.Error.invalid_request
          (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
           |> String.concat ~sep:"\n")))
    |> protocol_ok
  in
  (* Serialize this policy-persistence example with the parent's idle policy
     ACKs. Concurrent birth/freshness races are a separate contract. *)
  A.with_moderator_checkpoint parent.actor (fun () ->
    Agent_server.Session_factory.create_generated_session
      ~start_immediately:true
      ~lifetime
      (Daemon.factory daemon)
      ~parent_session_id:(state parent).identity.session_id
      ~idempotency_key:(P.Idempotency_key.of_string "policy-child" |> protocol_ok)
      ~display_name:None
      definition)
;;

let create_child env root daemon parent =
  create_child_result env root daemon parent |> protocol_ok
;;

type independent_recovery =
  { parent_id : P.Id.Session.t
  ; child_id : P.Id.Session.t
  ; delegation : D.record
  ; startup : E.t
  ; policies : E.t list
  }

let settled (entry : Registry.entry) =
  A.with_moderator_checkpoint entry.actor (fun () -> A.state entry.actor) |> protocol_ok
;;

let lifecycle_receipts (current : S.Session_state.t) phase =
  List.filter current.moderator_executions ~f:(fun receipt ->
    E.equal_phase phase receipt.context.phase)
;;

let assert_current_lifecycle (current : S.Session_state.t) (receipt : E.t) =
  let index = Option.value_exn current.run_state in
  let pointer = S.Run_state.moderator_lifecycle index |> Option.value_exn in
  let _, source = S.Moderator_source_installation.capture current |> protocol_ok in
  assert (
    P.Id.Moderator_execution.equal
      receipt.context.id
      (S.Moderator_lifecycle.execution_id pointer));
  assert (P.Run_source.equal source (S.Moderator_lifecycle.source pointer));
  assert (P.Invocation.equal_observer source.observer receipt.context.source)
;;

let%expect_test "generated descendants use persisted parent policy across restart" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let save name contents =
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / name)
            contents
        in
        save "allowed.txt" "parent-approved sentinel";
        save "private.txt" "must never be disclosed";
        save "policy-proof.txt" "private synchronous policy result";
        save
          "parent.chatmd"
          {|<developer>Parent.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let rewrite = fun () -> Tool.rewrite_args(`Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("allowed.txt")}]))
let is_recovered_request = fun args ->
  match Json.get_field(args, "file") with
  | `Some(`String("recovered-request.txt")) -> true
  | _ -> false
let read_before_rewrite = fun () ->
  let* result = Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("policy-proof.txt")}
  ])) in
  match result with
  | `Ok(`String(_)) -> rewrite()
  | `Ok(_) -> Task.fail("expected actual policy read text")
  | `Error(code) -> Task.fail(code)
let on_event = fun ctx state event -> match event with
  | `Pre_tool_call(call) ->
    let* ignored = (match state with
      | 0 -> rewrite()
      | 2 -> if is_recovered_request(call.args) then read_before_rewrite() else rewrite()
      | 3 -> Runtime.end_session("parent policy finished")
      | _ -> Tool.reject("private parent reason")) in
    Task.pure(state + 1)
  | `Session_resume ->
    if state == 3 then Task.pure(state + 100) else Task.pure(state)
  | _ -> Task.pure(state)
</script>|};
        save
          "legacy.chatmd"
          {|<developer>Legacy parent.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="legacy-policy" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start -> Task.pure(state + 1)
| _ -> Task.pure(state)
</script>|};
        let configuration = config root root (Filename.concat root "parent.chatmd") in
        let legacy_configuration =
          let base = config root root (Filename.concat root "legacy.chatmd") in
          { base with
            server = { base.server with data_dir = Filename.concat root "legacy-data" }
          }
        in
        let requests = ref 0 in
        let with_daemon ?(configuration = configuration) f =
          Eio.Switch.run (fun sw ->
            let daemon =
              Daemon.start
                ~sw
                ~env
                ~config:configuration
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Daemon.default_options with
                    qualify_chatml_extensions = true
                  ; independent_lifetime_policy = Some "moderation-fixture-v1"
                  ; inference_policy =
                      Agent_server_test_support.inference_policy
                        ~default_model:"fixture-model"
                        ~post_stream:(fun ~sw:_ ~inputs ->
                          Int.incr requests;
                          match List.last inputs with
                          | Some (Openai.Responses.Item.Function_call_output _) ->
                            Stdlib.Seq.empty
                          | last ->
                            let file =
                              if
                                Option.exists last ~f:(fun item ->
                                  Openai.Responses.Item.jsonaf_of_t item
                                  |> Jsonaf.to_string
                                  |> String.is_substring
                                       ~substring:"Read recovered-request.txt")
                              then "recovered-request.txt"
                              else "private.txt"
                            in
                            let arguments =
                              Jsonaf.to_string
                                (`Object [ "root", `String "data"; "file", `String file ])
                            in
                            let open Openai.Responses.Response_stream in
                            [ Output_item_added
                                { item =
                                    Function_call
                                      { name = "read_file"
                                      ; arguments = ""
                                      ; call_id = sprintf "read-%d" !requests
                                      ; _type = "function_call"
                                      ; id = Some "read-item"
                                      ; status = None
                                      }
                                ; output_index = 0
                                ; type_ = "response.output_item.added"
                                }
                            ; Function_call_arguments_done
                                { arguments
                                ; item_id = "read-item"
                                ; output_index = 0
                                ; type_ = "response.function_call_arguments.done"
                                }
                            ; Output_item_done
                                { item =
                                    Function_call
                                      { name = "read_file"
                                      ; arguments
                                      ; call_id = sprintf "read-%d" !requests
                                      ; _type = "function_call"
                                      ; id = Some "read-item"
                                      ; status = Some "completed"
                                      }
                                ; output_index = 0
                                ; type_ = "response.output_item.done"
                                }
                            ]
                            |> Stdlib.List.to_seq)
                  }
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      f sw daemon client))))
        in
        let attach sw client id =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:id
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        let rec idle entry =
          let current = state entry in
          match current.active_operation with
          | None -> current
          | Some _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            idle entry
        in
        let send ?(text = "Read private.txt") handle =
          H.send_message handle { kind = Plain_text; text; attachments = [] }
          |> protocol_ok
          |> ignore
        in
        let check_parent ?delegated_count ?(policy_reads = 0) parent count =
          let current = settled parent in
          let snapshot =
            Agent_session.Moderator_checkpoint.decode current.moderator
            |> protocol_ok
            |> Option.value_exn
          in
          (match snapshot.current_state with
           | Session.Snapshot.Int value -> [%test_eq: int] count value
           | _ -> failwith "policy counter missing");
          [%test_eq: int] policy_reads (List.length current.invocations);
          let receipts =
            List.filter current.moderator_executions ~f:(fun receipt ->
              Option.is_some receipt.P.Moderator_execution.delegation)
          in
          [%test_eq: int]
            (Option.value delegated_count ~default:count)
            (List.length receipts);
          List.iter receipts ~f:(fun receipt -> assert (Option.is_some receipt.decision));
          receipts
        in
        let check_read_result (invocation : P.Invocation.t) =
          match invocation.status with
          | Published (Complete (`String text)) ->
            assert (String.is_substring text ~substring:"parent-approved sentinel");
            assert (not (String.is_substring text ~substring:"must never"))
          | status ->
            raise_s [%sexp "unexpected child result", (status : P.Invocation.status)]
        in
        let check_rejected_result (invocation : P.Invocation.t) =
          match invocation.status with
          | Published (Fail error) ->
            [%test_eq: string] "invocation.pre_tool_rejected" error.code;
            assert (
              not (String.is_substring error.message ~substring:"private parent reason"))
          | status ->
            raise_s [%sexp "unexpected rejection", (status : P.Invocation.status)]
        in
        let check_read child =
          let current = idle child in
          [%test_eq: int] 1 (List.length current.invocations);
          let invocation = List.hd_exn current.invocations in
          check_read_result invocation;
          invocation
        in
        let resolve_independent daemon ~(parent : Registry.entry) (child : Registry.entry)
          =
          let parent_state = state parent in
          let child_state = state child in
          let reference = Option.value_exn child_state.spec.delegation in
          let record =
            D.resolve
              (Agent_store.Session_store.delegations (Daemon.store daemon))
              reference
            |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            |> protocol_ok
          in
          assert (D.equal_stage Linked record.stage);
          assert (Option.is_none record.revocation);
          (match record.admission.lifetime with
           | Independent _ -> ()
           | Owned | Invocation_owned _ -> failwith "expected linked Independent child");
          assert (
            P.Id.Session.equal
              parent_state.identity.session_id
              record.key.parent_session_id);
          assert (
            P.Id.Session.equal
              child_state.identity.session_id
              record.admission.child_session_id);
          assert (
            Option.equal
              Goal_origin.equal
              record.admission.goal_origin
              (Some Goal_origin.unrelated));
          record
        in
        let rec await_lifecycle parent phase =
          let current = settled parent in
          match lifecycle_receipts current phase with
          | [ ({ status = Completed _; _ } as receipt) ] -> current, receipt
          | [] | [ { status = Running; _ } ] ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            await_lifecycle parent phase
          | receipts ->
            raise_s [%sexp "unexpected parent lifecycle", (receipts : E.t list)]
        in
        let check_stopped_parent (parent : Registry.entry) (startup : E.t) =
          let current = settled parent in
          (match current.lifecycle.desired, current.lifecycle.observed with
           | Stopped, Stopped -> ()
           | _ -> failwith "Independent policy left its ordinary parent running");
          assert (Option.is_none current.active_operation);
          [%test_eq: int64]
            0L
            (P.Inference_query.Summary.retained_attempts
               (S.Inference_ledger.summary current.inference_ledger));
          assert (not (Owner.is_loaded parent.runtime));
          assert (not (Owner.has_activation_handoff parent.runtime));
          assert (
            List.equal E.equal [ startup ] (lifecycle_receipts current Session_start));
          assert (List.is_empty (lifecycle_receipts current Session_resume));
          assert_current_lifecycle current startup
        in
        let call_independent
              ?text
              ?(policy_reads = 0)
              ~parent
              ~startup
              ~child
              handle
              ~count
              ~check_result
          =
          let before = (state child).invocations in
          send ?text handle;
          let current = idle child in
          let fresh =
            List.filter current.invocations ~f:(fun (invocation : P.Invocation.t) ->
              not
                (List.exists before ~f:(fun previous ->
                   P.Id.Invocation.equal previous.context.id invocation.context.id)))
          in
          let invocation =
            match fresh with
            | [ invocation ] -> invocation
            | _ ->
              raise_s
                [%sexp "expected one actual child call", (fresh : P.Invocation.t list)]
          in
          check_result invocation;
          let receipts = check_parent ~policy_reads parent count in
          let receipt =
            List.find_exn receipts ~f:(fun receipt ->
              let child = Option.value_exn receipt.delegation in
              P.Id.Invocation.equal child.child_invocation_id invocation.context.id)
          in
          let delegation = Option.value_exn receipt.delegation in
          assert (
            P.Id.Session.equal current.identity.session_id delegation.child_session_id);
          [%test_eq: int] current.identity.generation delegation.child_generation;
          let reference = Option.value_exn current.spec.delegation in
          [%test_eq: string] reference.admission_sha256 delegation.admission_sha256;
          assert (
            P.Invocation.equal_observer receipt.context.source startup.E.context.source);
          assert (E.equal_phase Pre_tool_call receipt.context.phase);
          (match receipt.status with
           | Completed _ -> ()
           | Running | Failed _ | Interrupted _ ->
             raise_s [%sexp "Independent policy did not complete", (receipt : E.t)]);
          check_stopped_parent parent startup;
          receipt, invocation
        in
        let check_policy_read
              daemon
              ~(parent : Registry.entry)
              ~(child : Registry.entry)
              (delegation : D.record)
              (receipt : E.t)
              (invocation : P.Invocation.t)
          =
          let current = settled parent in
          let caller = state child in
          let service =
            match current.invocations with
            | [ service ] -> service
            | _ -> failwith "expected one actual stopped policy service"
          in
          assert (P.Invocation.equal_origin Moderator service.context.origin);
          [%test_eq: string] "read_file" service.context.tool_name;
          assert (Option.is_none service.context.call_entry_id);
          assert (
            Option.equal
              P.Id.Moderator_execution.equal
              service.parent_event
              (Some receipt.context.id));
          assert (
            Jsonaf.exactly_equal
              service.context.input
              (`Object [ "root", `String "data"; "file", `String "policy-proof.txt" ]));
          (match service.status, invocation.status with
           | Resolved (Complete (`String read)), Published (Complete (`String result)) ->
             assert (
               String.is_substring read ~substring:"private synchronous policy result");
             assert (
               not
                 (String.is_substring
                    result
                    ~substring:"private synchronous policy result"))
           | _ -> failwith "expected separate completed policy and child reads");
          assert (List.is_empty current.jobs);
          let source =
            S.Run_state.installation (Option.value_exn current.run_state)
            |> S.Run_source_installation.captured ~generation:current.identity.generation
            |> protocol_ok
          in
          let prefix =
            Policy_source.Native_prefix.create
              ~session_id:current.identity.session_id
              ~generation:current.identity.generation
              ~current_source:(Some source)
              ~events:current.moderator_executions
              ~invocations:current.invocations
              ~jobs:current.jobs
              ~runs:current.run_state
            |> protocol_ok
          in
          let member =
            Cause.Member.invocation
              service.context.id
              ~generation:service.context.generation
            |> protocol_ok
          in
          let cause =
            Policy_source.cause current.policy_canonical_usage ~prefix ~member
            |> protocol_ok
            |> Option.value_exn
          in
          assert (Cause.matches_event cause receipt);
          assert (P.Run_source.equal source (Cause.callee_source cause));
          let session_ref (session : S.Session_state.t) =
            P.Session_ref.create
              ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
              ~session_id:session.identity.session_id
          in
          assert (P.Session_ref.equal (session_ref current) (Cause.callee cause));
          assert (P.Session_ref.equal (session_ref caller) (Cause.caller cause));
          assert (
            P.Id.Invocation.equal invocation.context.id (Cause.child_invocation cause));
          assert (
            Option.equal
              P.Inference_origin.equal
              invocation.inference_origin
              (Cause.caller_origin cause));
          assert (List.is_empty (Cause.targets cause));
          let reference = Option.value_exn caller.spec.delegation in
          assert (List.equal D.Reference.equal [ reference ] (Cause.path cause));
          match Cause.Caller_source.view (Cause.caller_source cause) with
          | Unmoderated actual ->
            [%test_eq: int] caller.identity.generation actual.generation;
            assert (
              P.Id.Prompt_revision.equal
                caller.spec.prompt_revision_id
                actual.prompt_revision_id);
            [%test_eq: int64]
              caller.spec.configuration_revision
              actual.configuration_revision;
            [%test_eq: string]
              delegation.admission.manifest_sha256
              actual.artifact_manifest_sha256
          | Installed _ -> failwith "ordinary child lost its verified unmoderated source"
        in
        with_daemon ~configuration:legacy_configuration (fun _sw daemon client ->
          let parent, _ =
            create_session ~key:"legacy-independent-parent" ~start_immediately:true client
          in
          let parent =
            Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
          in
          let before, startup = await_lifecycle parent Session_start in
          assert (P.Session.equal_desired_state before.lifecycle.desired Running);
          assert (not before.halted);
          assert (Option.is_none before.failure);
          Owner.with_loaded_runtime parent.runtime (fun runtime ->
            let manager = Option.value_exn runtime.S.Runtime_builder.moderator_manager in
            assert (
              Option.is_none
                (Chat_response.Moderator_manager.extension_definition manager));
            Ok ())
          |> protocol_ok;
          ignore (check_parent ~delegated_count:0 parent 1 : E.t list);
          let providers = !requests in
          (match create_child_result ~lifetime:Independent env root daemon parent with
           | Error { code = Invalid_state; message; retryable = false; _ }
             when String.equal
                    message
                    "delegation.owner_mediation_unavailable: parent requires an \
                     extensibility moderator" -> ()
           | Error error ->
             raise_s [%sexp "unexpected Legacy birth denial", (error : P.Error.t)]
           | Ok _ -> failwith "Legacy parent admitted an Independent child");
          let after = settled parent in
          assert (Option.equal Jsonaf.exactly_equal before.moderator after.moderator);
          assert (List.equal E.equal [ startup ] (lifecycle_receipts after Session_start));
          [%test_eq: int] providers !requests;
          let store = Daemon.store daemon in
          [%test_eq: int] 1 (List.length (Agent_store.Session_store.list_sessions store));
          let records =
            D.with_records
              (Agent_store.Session_store.delegations store)
              ~max_records:8
              ~max_bytes:1048576
              ~f:(fun records -> Ok records)
            |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            |> protocol_ok
          in
          assert (List.is_empty records));
        let independent, parent_id, child_id, grandchild_id =
          with_daemon (fun sw daemon client ->
            let independent_parent, _ =
              create_session
                ~key:"ordinary-independent-parent"
                ~start_immediately:true
                client
            in
            let parent =
              Registry.find (Daemon.registry daemon) independent_parent.id
              |> Option.value_exn
            in
            let started, startup = await_lifecycle parent Session_start in
            assert_current_lifecycle started startup;
            let child =
              create_child_result ~lifetime:Independent env root daemon parent
              |> protocol_ok
            in
            let child_id = (state child).identity.session_id in
            let delegation = resolve_independent daemon ~parent child in
            let parent_handle = attach sw client independent_parent.id in
            let providers = !requests in
            H.stop parent_handle ~mode:Graceful |> protocol_ok |> ignore;
            H.detach parent_handle |> protocol_ok;
            [%test_eq: int] providers !requests;
            check_stopped_parent parent startup;
            assert (P.Session.equal_desired_state (state child).lifecycle.desired Running);
            let handle = attach sw client child_id in
            let rewrite, _ =
              call_independent
                ~parent
                ~startup
                ~child
                handle
                ~count:1
                ~check_result:check_read_result
            in
            let rejection, _ =
              call_independent
                ~parent
                ~startup
                ~child
                handle
                ~count:2
                ~check_result:check_rejected_result
            in
            (match rewrite.decision, rejection.decision with
             | Some (Rewrite_args _), Some (Reject _) -> ()
             | _ ->
               failwith "expected the stopped policy's committed rewrite and rejection");
            H.detach handle |> protocol_ok;
            let independent =
              { parent_id = independent_parent.id
              ; child_id
              ; delegation
              ; startup
              ; policies = [ rewrite; rejection ]
              }
            in
            let parent, _ = create_session ~start_immediately:true client in
            let parent_entry =
              Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
            in
            let child = create_child env root daemon parent_entry in
            let child_id = (state child).identity.session_id in
            let handle = attach sw client child_id in
            send handle;
            let invocation = check_read child in
            let receipt = check_parent parent_entry 1 |> List.hd_exn in
            let delegation = Option.value_exn receipt.delegation in
            assert (
              P.Id.Invocation.equal delegation.child_invocation_id invocation.context.id);
            assert (P.Id.Session.equal delegation.child_session_id child_id);
            let reference = Option.value_exn (state child).spec.delegation in
            [%test_eq: string] reference.admission_sha256 delegation.admission_sha256;
            send handle;
            let rejected = idle child in
            (* The rejected call is journaled but never executes the native reader. *)
            [%test_eq: int] 2 (List.length rejected.invocations);
            let denied =
              List.find_exn rejected.invocations ~f:(fun candidate ->
                not (P.Id.Invocation.equal candidate.context.id invocation.context.id))
            in
            check_rejected_result denied;
            let receipts = check_parent parent_entry 2 in
            [%test_eq: int]
              1
              (List.count receipts ~f:(fun receipt ->
                 match receipt.decision with
                 | Some (Reject _) -> true
                 | _ -> false));
            let grandchild = create_child env root daemon child in
            H.detach handle |> protocol_ok;
            independent, parent.id, child_id, (state grandchild).identity.session_id)
        in
        with_daemon (fun sw daemon client ->
          let parent = Registry.load (Daemon.registry daemon) parent_id |> protocol_ok in
          let child = Registry.load (Daemon.registry daemon) child_id |> protocol_ok in
          let grandchild =
            Registry.load (Daemon.registry daemon) grandchild_id |> protocol_ok
          in
          ignore (check_parent parent 2);
          let handle = attach sw client grandchild_id in
          send handle;
          let invocation = check_read grandchild in
          let receipts = check_parent parent 3 in
          assert (
            List.exists receipts ~f:(fun receipt ->
              let delegated = Option.value_exn receipt.delegation in
              P.Id.Session.equal delegated.child_session_id grandchild_id
              && P.Id.Invocation.equal delegated.child_invocation_id invocation.context.id));
          assert (List.is_empty (state child).moderator_executions);
          send handle;
          let rec stopped entry =
            let current = state entry in
            match current.lifecycle.desired, current.active_operation with
            | Stopped, None -> current
            | _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              stopped entry
          in
          let ended = stopped parent in
          assert ended.halted;
          ignore (stopped child);
          ignore (stopped grandchild);
          ignore (check_parent parent 4);
          H.detach handle |> protocol_ok;
          let registry = Daemon.registry daemon in
          let parent = Registry.load registry independent.parent_id |> protocol_ok in
          let child = Registry.load registry independent.child_id |> protocol_ok in
          check_stopped_parent parent independent.startup;
          assert (
            D.equal_record
              independent.delegation
              (resolve_independent daemon ~parent child));
          let check_recovered_policies receipts =
            List.iter independent.policies ~f:(fun before ->
              assert (List.exists receipts ~f:(E.equal before)))
          in
          check_recovered_policies (check_parent parent 2);
          assert (P.Session.equal_desired_state (state child).lifecycle.desired Running);
          let handle = attach sw client independent.child_id in
          let rewritten, invocation =
            call_independent
              ~text:"Read recovered-request.txt"
              ~policy_reads:1
              ~parent
              ~startup:independent.startup
              ~child
              handle
              ~count:3
              ~check_result:check_read_result
          in
          (match rewritten.decision with
           | Some (Rewrite_args _) -> ()
           | _ -> failwith "expected recovered policy's committed rewrite");
          check_policy_read
            daemon
            ~parent
            ~child
            independent.delegation
            rewritten
            invocation;
          H.detach handle |> protocol_ok;
          let parent_handle = attach sw client independent.parent_id in
          let providers = !requests in
          H.start parent_handle ~queue_if_limited:false |> protocol_ok |> ignore;
          let resumed, resume = await_lifecycle parent Session_resume in
          assert (P.Session.equal_desired_state resumed.lifecycle.desired Running);
          assert (
            List.equal
              E.equal
              [ independent.startup ]
              (lifecycle_receipts resumed Session_start));
          assert_current_lifecycle resumed resume;
          assert (
            P.Invocation.equal_observer
              independent.startup.context.source
              resume.context.source);
          let policies = check_parent ~delegated_count:3 ~policy_reads:1 parent 103 in
          check_recovered_policies policies;
          assert (List.exists policies ~f:(E.equal rewritten));
          [%test_eq: int] providers !requests;
          H.detach parent_handle |> protocol_ok);
        print_endline
          "parent rewrites and rejects; checkpoint and decisions persist; grandchild \
           inherits through restart; parent end stops descendants";
        print_endline
          "ordinary Independent child uses stopped parent policy before and after \
           physical recovery; explicit parent start alone consumes Resume";
        print_endline
          "Legacy parent rejects Independent birth before child, ledger or provider \
           effects"));
  [%expect
    {|
    parent rewrites and rejects; checkpoint and decisions persist; grandchild inherits through restart; parent end stops descendants
    ordinary Independent child uses stopped parent policy before and after physical recovery; explicit parent start alone consumes Resume
    Legacy parent rejects Independent birth before child, ledger or provider effects
    |}]
;;
