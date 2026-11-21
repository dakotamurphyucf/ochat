open! Core
open Fixtures
module A = Agent_session
module Actor = A.Session_actor
module P = Agent_protocol
module R = A.Recovery_rollover
module Scope = P.Recovery_window.Scope
module Capacity = A.Session_context_capacity
module Counter = Inference_runtime.Context_capacity
module Identity = Recovery_note_actor_tests
module Storage = Recovery_note_share_tests
module C = Configuration_tests

let allow _ = Ok ()

let enable actor ~attachment_id =
  Actor.enable_recovery_windows
    actor
    ~actor:(Identity.proof [ Send_messages ])
    ~attachment_id
    ~authorize:allow
  |> protocol_ok
;;

let host ~mode ~targets =
  Capacity.Host.create
    ~mode_for_session:(fun owner ->
      Option.some_if (P.Session_ref.equal owner Identity.owner) mode)
    ~targets
  |> Result.map_error ~f:(fun _ -> "invalid context host fixture")
  |> Result.ok_or_failwith
;;

let set_manual actor =
  Actor.set_context_capacity_host actor (host ~mode:Manual ~targets:[]) |> protocol_ok
;;

let require_pending (receipt : R.Receipt.t) =
  match receipt.status with
  | Pending -> ()
  | Terminal _ -> failwith "admission returned a terminal receipt"
;;

let field json name = Identity.get json name
let assert_json expected actual = assert (Document_schema.Json.equal expected actual)

let%expect_test "operator admission compares the exact head and survives reopen" =
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let request actor attachment_id ?authorize_force ~name expected =
        Actor.request_context_rollover
          actor
          ~actor:(Identity.proof [ Send_messages ])
          ~attachment_id
          ~authorize:allow
          ?authorize_force
          ~key:(Identity.key name)
          expected
      in
      let expected, receipt =
        Storage.new_session
          env
          sw
          workspace
          Identity.owner
          ~name:"rollover-operator"
          (fun actor persistence journal ->
             let attachment_id = Identity.attach actor Owner_read_write in
             let scope = enable actor ~attachment_id in
             let state = Actor.state actor |> protocol_ok in
             let expected =
               R.Expected.create scope ~revision:state.counters.revision |> protocol_ok
             in
             let initial_count = Identity.count journal in
             let request = request actor attachment_id in
             Identity.expect_error Invalid_state (request ~name:"off" expected);
             set_manual actor;
             let stale = R.Expected.create scope ~revision:0L |> protocol_ok in
             Identity.expect_error Conflict (request ~name:"stale" stale);
             let deny _ =
               Error
                 (P.Error.create
                    Permission_denied
                    ~message:"force denied"
                    ~retryable:false
                    ())
             in
             Identity.expect_error
               Permission_denied
               (request ~authorize_force:deny ~name:"force" expected);
             [%test_eq: int] (Identity.count journal) initial_count;
             let receipt = request ~name:"operator-secret-key" expected |> protocol_ok in
             require_pending receipt;
             let same = request ~name:"operator-secret-key" expected |> protocol_ok in
             assert (R.Receipt.equal same receipt);
             Identity.expect_error
               Idempotency_conflict
               (request ~name:"operator-secret-key" stale);
             Identity.expect_error Conflict (request ~name:"second" expected);
             [%test_eq: int] (Identity.count journal) (initial_count + 1);
             let saved = A.Session_persistence.restored persistence in
             let snapshot =
               A.Session_state_document.encode
                 ~limits:document_limits
                 (A.Session_persistence.Restored.state_document saved)
               |> document_ok
             in
             assert (
               List.mem
                 (Document_schema.Document.required_semantics snapshot)
                 P.Session_storage_semantics.recovery_rollover
                 ~equal:String.equal);
             let state = Actor.state actor |> protocol_ok in
             let windows = Option.value_exn state.recovery_windows in
             assert (
               Scope.equal
                 scope
                 (A.Recovery_window_ledger.active windows
                  |> A.Recovery_window_record.scope));
             assert (List.is_empty state.conversation.canonical_history);
             let ledger = Option.value_exn state.recovery_rollover in
             let id = receipt.intent.id in
             let terminal = R.Receipt.Superseded Checkpoint_required in
             let change = R.Change.(Settle { id; terminal }) in
             let settled =
               R.apply
                 (Some ledger)
                 ~active:scope
                 ~revision:state.counters.revision
                 ~history:state.conversation.canonical_history
                 ~catalog:state.recovery_notes
                 change
               |> protocol_ok
             in
             let reopened = R.of_json (R.to_json settled) |> protocol_ok in
             let settled = R.find reopened ~id |> Option.value_exn in
             assert (R.Receipt.equal_status settled.status (Terminal terminal));
             let successor =
               Scope.create
                 ~session:Identity.owner
                 ~generation:(Scope.generation scope)
                 ~window:(P.Id.Context_window.create ())
               |> protocol_ok
             in
             let archive =
               Chatml_source.Digest.of_contents "test-only archive" ~checkpoint:ignore
             in
             let committed =
               R.Receipt.Committed
                 { successor
                 ; cut_revision = state.counters.revision
                 ; archive
                 ; checkpoint = Archive_only Checkpoint_missing
                 ; continuation = Awaiting_after_commit id
                 }
             in
             let change = R.Change.(Settle { id; terminal = committed }) in
             Identity.expect_error
               Invalid_request
               (A.Session_delta.apply state (Recovery_rollover_changed change));
             let public = R.Receipt.to_public_json receipt |> Jsonaf.to_string in
             assert (not (String.is_substring public ~substring:"operator-secret-key"));
             expected, receipt)
      in
      Storage.reopen_session
        env
        sw
        workspace
        Identity.owner
        ~name:"rollover-operator"
        (fun actor _ ->
           let attachment_id = Identity.attach actor Owner_read_write in
           let replayed =
             request actor attachment_id ~name:"operator-secret-key" expected
             |> protocol_ok
           in
           assert (R.Receipt.equal replayed receipt);
           require_pending replayed)));
  print_endline "Off and stale CAS rejected; one Manual admission replayed after reopen";
  [%expect {| Off and stale CAS rejected; one Manual admission replayed after reopen |}]
;;

let selected_tools env names =
  let source =
    List.map names ~f:(sprintf {|<tool name="%s"/>|}) |> String.concat ~sep:"\n"
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs ~dir:(Eio.Stdenv.cwd env) source
  in
  A.Extensibility_native_tools.registrations
    ~env
    ~elements
    ~one_off_policy:None
    ~authoring_validation_host:None
  |> protocol_ok
;;

let call_event ~name ~arguments index =
  let open Openai.Responses.Response_stream in
  let item =
    Item.Function_call
      { name
      ; arguments
      ; call_id = sprintf "rollover-call-%d" index
      ; id = Some (sprintf "rollover-item-%d" index)
      ; _type = "function_call"
      ; status = Some "completed"
      }
  in
  Stdlib.List.to_seq
    [ Output_item_done { item; output_index = 0; type_ = "response.output_item.done" } ]
;;

let exact_target_policy target =
  Counter.Policy.create
    ~target
    ~encoder:Openai_responses_v1
    ~model_limit:(Bounded { total = 500L; input = Some 450L; output = None })
    ~allowance:
      { host_total = None
      ; session_total = Some 480L
      ; omitted_output_default = Some 50L
      ; checkpoint_reserve = 20L
      ; warning_lead = 10L
      }
    ~estimate:None
  |> Result.map_error ~f:(fun _ -> "invalid target fixture")
  |> Result.ok_or_failwith
;;

let%expect_test "selected foreground tools use the real preparation and original actor" =
  let module Graph = Agent_server.Graph_tracking in
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let remaining = "context_remaining" in
      let new_window = "context_new_window" in
      let registration : Chat_response.Agent_runtime.native_registration list =
        selected_tools env [ remaining; new_window ]
      in
      [%test_eq: int] (List.length (selected_tools env [])) 0;
      [%test_eq: int] (List.length registration) 2;
      let contracts =
        List.map registration ~f:(fun registration ->
          registration.implementation.info.function_.name, registration.result_contract)
      in
      let implementations =
        List.map registration ~f:(fun registration ->
          registration.implementation_revision, registration.implementation)
      in
      let declared =
        Chat_response.Tool_capability.create
          ~result_contracts:contracts
          ~owner:"context-selection-fixture"
          ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "context-fixture")
          implementations
        |> Result.map_error ~f:(fun error -> error.message)
        |> Result.ok_or_failwith
      in
      let tools, tool_tbl = Ochat_function.functions (List.map implementations ~f:snd) in
      let attempt = ref 0 in
      let expected_window = ref None in
      let arguments window =
        Jsonaf.to_string
          (`Object [ "expected_window", P.Id.Context_window.to_json window ])
      in
      let fixture =
        Inference_fixture.create_with_capacity
          ~namespace:"selected-context-fixture"
          ~default_model:"context-fixture"
          ~binding_is_current:(fun () -> true)
          ~capacity_source:(fun _ ->
            Counter.Source.create ~encoder:Openai_responses_v1 ~read:(fun () ->
              Covered
                (Inference.Observation.Count.create (Actual 100L)
                 |> Result.map_error ~f:(fun _ -> "invalid fixture count")
                 |> Result.ok_or_failwith)))
          ~post_stream:(fun ~sw:_ ~inputs:_ ->
            incr attempt;
            match !attempt with
            | 1 | 3 -> call_event ~name:remaining ~arguments:"{}" !attempt
            | 2 | 6 -> Seq.empty
            | 4 ->
              call_event
                ~name:new_window
                ~arguments:(arguments (P.Id.Context_window.create ()))
                !attempt
            | 5 ->
              call_event
                ~name:new_window
                ~arguments:(arguments (Option.value_exn !expected_window))
                !attempt
            | _ -> failwith "unexpected context model request")
      in
      let target =
        Inference_fixture.capture_config fixture Chat_response.Config.default
        |> C.runtime_ok
      in
      let initial = Storage.source_state workspace Identity.owner in
      let inference_target =
        Inference.Selection.captured target ~limits:document_limits |> C.inference_ok
      in
      let initial = { initial with spec = { initial.spec with inference_target } } in
      let journal =
        Identity.journal
          Agent_store.Journal.create
          env
          (Filename.concat workspace.canonical_root.native_path "rollover-selected")
      in
      let terminal = ref None in
      let state_committed _ events =
        List.iter events ~f:(fun (event : P.Event.Durable.t) ->
          match
            P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
          with
          | Operation_completed operation
          | Operation_failed operation
          | Operation_cancelled operation
          | Operation_interrupted operation -> terminal := Some operation
          | _ -> ())
      in
      Identity.with_actor
        env
        sw
        journal
        (A.Session_persistence.Restored.authored initial)
        ~owner:Identity.owner
        ~latest_sequence:0L
        ~previous_transaction_hash:None
        ~snapshot_payload_limit:1048576
        ~before_commit:None
        ~now:P.Timestamp.now
        ~state_committed
        (fun actor _ ->
           let attachment_id = Identity.attach actor Owner_read_write in
           let scope = enable actor ~attachment_id in
           expected_window := Some (Scope.window scope);
           let capacity =
             host ~mode:Automatic ~targets:[ exact_target_policy target, `Automatic ]
           in
           Actor.set_context_capacity_host actor capacity |> protocol_ok;
           let policy =
             A.Configuration_policy.
               { select_profile =
                   (fun ~current:_ ~profile:_ ->
                     Error (P.Error.invalid_request "unavailable"))
               ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
               ; resolve =
                   (fun selected ->
                     Inference_fixture.resolve fixture selected
                     |> Result.map_error ~f:(fun _ ->
                       P.Error.invalid_request "unavailable"))
               }
           in
           Actor.set_configuration_policy actor policy |> protocol_ok;
           let services =
             A.Script_tool_calls.create
               ~registry:(fun () -> declared)
               ~moderator_names:String.Set.empty
               ~now:P.Timestamp.now
               ~is_halted:(fun () -> false)
               ~requires_active_moderator:(fun _ -> false)
               ~authorize:(fun _ _ -> Ok ())
               ~prepare_output:(function
                 | Openai.Responses.Tool_output.Output.Text text -> Ok (`String text)
                 | output -> Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output))
               ~defer_observation:(fun _ -> Ok ())
             |> fun tools ->
             A.Script_tool_calls.with_recovery_context_service
               tools
               { remaining = (fun access -> Actor.context_remaining_native actor ~access)
               ; new_window =
                   (fun access window ->
                     Actor.context_new_window_native actor ~access ~window)
               ; checkpoint = (fun _ _ -> failwith "unexpected authored checkpoint")
               }
           in
           let config =
             Authoring_input_tests.worker_config env (fun ~sw:_ ~inputs:_ -> Seq.empty)
           in
           let graph =
             Inference_tracking_tests.create_fixture_graph
               actor
               ~source_name:"selected-context-fixture"
               fixture
           in
           let worker =
             A.Turn_worker.create
               ~root_binding:
                 (Chat_response.Root_binding.create
                    (Inference_runtime.Session.create ~sw))
               ~dispatch_tool:(fun ~input ~capabilities ->
                 A.Script_tool_calls.native_dispatch
                   services
                   ~declared
                   ~input
                   ~capabilities)
               { config with
                 inference_context =
                   Inference_fixture.resolve fixture target |> C.runtime_ok
               ; inference_identity =
                   (fun allocate -> Graph.bind_identity graph ~allocate)
               ; on_inference_attempt = Graph.on_attempt graph
               ; on_inference_observation = Graph.on_observation graph
               ; on_inference_completion = Graph.on_completion graph
               ; tools = Chat_response.Tool.convert_tools tools
               ; tool_tbl
               ; permission_profile =
                   permission_policy
                     ~tool_default:Allow
                     ~fallback:Fallback_deny
                     ~evaluator:None
                     ~reviewer:None
               }
           in
           Actor.set_operation_worker actor (Some worker) |> protocol_ok;
           Actor.start actor ~attachment_id |> protocol_ok |> ignore;
           let submit scopes =
             terminal := None;
             let reserved = Actor.reserve_history_block actor ~count:1 |> protocol_ok in
             let input =
               Pending_input_tests.occurrence
                 (Int64.to_int_exn reserved.first_sequence)
                 "inspect context"
             in
             let admitted =
               Actor.submit_message_authenticated
                 actor
                 ~actor:(Identity.proof scopes)
                 ~attachment_id
                 input
               |> protocol_ok
             in
             let operation_id = Option.value_exn admitted.operation_id in
             let state = Authoring_input_tests.finished actor in
             (match !terminal with
              | Some { id; state = Completed; _ }
                when P.Id.Operation.equal id operation_id -> ()
              | terminal ->
                raise_s
                  [%sexp
                    "selected foreground did not complete"
                  , { operation_id : P.Id.Operation.t
                    ; terminal : P.Operation.t option
                    ; provider_calls = (!attempt : int)
                    }]);
             state
           in
           let outcomes state tool =
             List.filter_map state.A.Session_state.invocations ~f:(fun invocation ->
               if String.equal invocation.context.tool_name tool
               then (
                 match invocation.status with
                 | Published outcome | Resolved outcome -> Some outcome
                 | _ -> None)
               else None)
           in
           let expect_failure code = function
             | P.Invocation.Fail failure ->
               String.equal failure.code (P.Error.code_to_string code)
             | _ -> false
           in
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
             let denied = submit [ Send_messages ] in
             assert (
               List.exists
                 (outcomes denied remaining)
                 ~f:(expect_failure Permission_denied));
             assert (Option.is_none denied.recovery_rollover);
             let state = submit [ Send_messages; View_session_transcript ] in
             let completed tool =
               List.find_map_exn (outcomes state tool) ~f:(function
                 | Complete value -> Some value
                 | _ -> None)
             in
             let capacity = field (completed remaining) "capacity" in
             assert_json (`String "automatic") (field capacity "requested_mode");
             assert_json (`String "automatic") (field capacity "effective_mode");
             assert_json (`String "measured") (field capacity "evidence");
             assert_json (`Number "330") (field capacity "remaining");
             assert (List.exists (outcomes state new_window) ~f:(expect_failure Conflict));
             let admitted = completed new_window in
             assert_json (`String "pending") (field (field admitted "status") "kind");
             let ledger = Option.value_exn state.recovery_rollover in
             let id = P.Id.Operation.of_json (field admitted "id") |> protocol_ok in
             let receipt = R.find ledger ~id |> Option.value_exn in
             (match receipt.status with
              | Terminal (Superseded Checkpoint_required) -> ()
              | status ->
                raise_s
                  [%sexp
                    "native rollover without a checkpoint", (status : R.Receipt.status)]);
             assert (Scope.equal receipt.intent.expected.scope scope);
             (match receipt.intent.requester with
              | Native _ -> ()
              | Operator _ | Automatic _ -> failwith "wrong requester");
             let windows = Option.value_exn state.recovery_windows in
             assert (
               Scope.equal
                 scope
                 (A.Recovery_window_ledger.active windows
                  |> A.Recovery_window_record.scope));
             [%test_eq: int] !attempt 6);
           Graph.seal graph |> protocol_ok;
           Graph.finish graph |> protocol_ok)));
  print_endline
    "Selected tools enforced original scope, measured the actual preparation and settled \
     the native intent without a checkpoint";
  [%expect
    {| Selected tools enforced original scope, measured the actual preparation and settled the native intent without a checkpoint |}]
;;

let%expect_test "shared operator receipts preserve a pending cut across canonical replay" =
  let module Wire = P.Recovery_rollover_control in
  let actor_proof () =
    Identity.proof [ View_session_transcript; Send_messages; Administer_configuration ]
  in
  let inspect actor key =
    let observation = Actor.observe_resource_state actor |> protocol_ok in
    let status =
      Actor.inspect_context_rollover
        observation
        ~actor:(actor_proof ())
        ~authorize:allow
        ~authorize_writer:allow
        ~authorize_force:allow
        { session = Identity.owner; attachment_id = None; key = Some key }
      |> protocol_ok
    in
    match status.view with
    | Available { current; previous = None; receipt = Some receipt; actions; _ }
      when not (actions.rollover || actions.force) -> current, receipt
    | _ -> failwith "expected one redacted receipt under transcript authority"
  in
  let retry actor request =
    Actor.publish_context_rollover
      actor
      ~actor:(actor_proof ())
      ~authorize:allow
      ~authorize_force:allow
      request
    |> protocol_ok
  in
  with_actor_workspace (fun env workspace ->
    Eio.Switch.run (fun sw ->
      let key = Identity.key "pending-operator-control" in
      let seed actor attachment_id =
        let scope = enable actor ~attachment_id in
        set_manual actor;
        let state = Actor.state actor |> protocol_ok in
        let revision = state.counters.revision in
        let expected = Wire.Basis.create scope ~revision |> protocol_ok in
        let admitted = R.Expected.create scope ~revision |> protocol_ok in
        Actor.request_context_rollover
          actor
          ~actor:(actor_proof ())
          ~attachment_id
          ~authorize:allow
          ~key
          admitted
        |> protocol_ok
        |> require_pending;
        let current, receipt = inspect actor key in
        assert (Wire.Basis.equal receipt.expected expected);
        assert (Wire.Receipt.equal_outcome receipt.outcome Pending);
        [%test_eq: int64] current.revision (Int64.succ expected.revision);
        Wire.Request.{ expected; attachment_id; key; force = false }
      in
      Storage.new_session
        env
        sw
        workspace
        Identity.owner
        ~name:"control-exact-admitted"
        (fun actor _ _ ->
           let attachment_id = Identity.attach actor Owner_read_write in
           let request = seed actor attachment_id in
           let result = retry actor request in
           assert (Wire.Receipt.equal_outcome result.outcome (Failed Checkpoint_required));
           let current, _ = inspect actor key in
           [%test_eq: int64] current.revision Int64.(request.expected.revision + 2L);
           assert (Wire.Receipt.equal result (retry actor request)));
      let abandoned =
        Storage.new_session
          env
          sw
          workspace
          Identity.owner
          ~name:"control-reopened-pending"
          (fun actor _ _ -> seed actor (Identity.attach actor Owner_read_write))
      in
      Storage.reopen_session
        env
        sw
        workspace
        Identity.owner
        ~name:"control-reopened-pending"
        (fun actor _ ->
           let _, receipt = inspect actor key in
           assert (Wire.Basis.equal receipt.expected abandoned.expected);
           assert (Wire.Receipt.equal_outcome receipt.outcome Pending);
           let attachment_id = Identity.attach actor Owner_read_write in
           let request = { abandoned with attachment_id } in
           let resolved = retry actor request in
           assert (Wire.Receipt.equal_outcome resolved.outcome (Failed Basis_changed));
           assert (Wire.Receipt.equal resolved (retry actor request));
           let current, reported = inspect actor key in
           assert (Scope.equal current.scope abandoned.expected.scope);
           assert (Wire.Receipt.equal resolved reported))));
  print_endline
    "Exact pending cut settled once; reopened changed cut failed without replacing \
     history";
  [%expect
    {| Exact pending cut settled once; reopened changed cut failed without replacing history |}]
;;

let%expect_test "shared rollover wire rejects unbound receipts and open extension fields" =
  let module W = P.Recovery_rollover_control in
  let scope session =
    Scope.create ~session ~generation:4 ~window:(P.Id.Context_window.create ())
    |> protocol_ok
  in
  let current = W.Basis.create (scope Identity.owner) ~revision:6L |> protocol_ok in
  let request =
    W.Request.
      { expected = current
      ; key = Identity.key "wire-receipt"
      ; attachment_id = P.Id.Attachment.create ()
      ; force = false
      }
  in
  assert (
    W.Basis.equal
      current
      (W.Request.of_json (W.Request.to_json request) |> protocol_ok).expected);
  let foreign =
    P.Session_ref.create
      ~server_id:(P.Session_ref.server_id Identity.owner)
      ~session_id:(P.Id.Session.create ())
  in
  let committed successor =
    W.Receipt.Committed
      { successor; cut_revision = 7L; quality = Archive_only_missing_checkpoint }
  in
  Identity.expect_error
    Invalid_request
    (W.Receipt.create current (committed (scope foreign)));
  let forced =
    W.Receipt.create current (committed (scope Identity.owner)) |> protocol_ok
  in
  Identity.expect_error Invalid_request (W.Receipt.validate forced ~request);
  let pending = W.Receipt.create current Pending |> protocol_ok in
  let fields json = P.Json_codec.fields json |> protocol_ok |> P.Json_codec.to_alist in
  let underlying = W.Receipt.to_json pending |> fields in
  Identity.expect_error
    Invalid_request
    (W.Receipt.of_json (`Object (("principal", `String "opaque") :: underlying)));
  let status =
    W.Status.create
      Identity.owner
      (Available
         { current
         ; previous = None
         ; capacity = Unknown { reason = Disabled; target = None }
         ; receipt = Some pending
         ; actions = { rollover = true; force = true }
         })
    |> protocol_ok
  in
  assert (W.Status.equal status (W.Status.of_json (W.Status.to_json status) |> protocol_ok));
  Identity.expect_error
    Invalid_request
    (W.Status.validate
       status
       ~request:{ session = Identity.owner; attachment_id = None; key = None });
  Identity.expect_error
    Invalid_request
    (W.Inspect.of_json
       (`Object
           [ ( "session"
             , `Object
                 (("credentials", `String "hidden")
                  :: (P.Session_ref.to_json Identity.owner |> fields)) )
           ]));
  print_endline
    "Typed wire closed private fields, foreign scope, force mismatch and observer actions";
  [%expect
    {| Typed wire closed private fields, foreign scope, force mismatch and observer actions |}]
;;
