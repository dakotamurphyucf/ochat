open! Core
open Fixtures
module A = Agent_session.Session_actor
module C = Agent_protocol.Session_configuration
module T = Agent_session.Configuration_transition
module R = Inference.Request
module Runtime = Inference_runtime

let inference_ok value =
  Result.map_error value ~f:(fun e -> Sexp.to_string_hum (R.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let runtime_ok value =
  Result.map_error value ~f:(fun e ->
    Sexp.to_string_hum (Runtime.Preparation_error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let target () =
  match Inference.Selection.view (inference_selection ()) with
  | Captured target -> target
  | Unresolved -> assert false
;;

let history_id namespace =
  History_entry.Id.create ~namespace ~sequence:0 |> Result.ok_or_failwith
;;

let patch model = C.Patch.create ~model ~settings:[] () |> protocol_ok

let%expect_test "configuration patches reject ambiguous fields and roundtrip omission" =
  let patch = patch "model-B" in
  let json = C.Patch.to_json patch in
  assert (
    Document_schema.Json.equal
      json
      (C.Patch.of_json json |> protocol_ok |> C.Patch.to_json));
  print_endline (Jsonaf.to_string json);
  let null_profile = `Object [ "profile", `Null; "settings", `Array [] ] in
  assert (Result.is_error (C.Patch.of_json null_profile));
  assert (Result.is_error (C.Patch.create ~settings:[] ()));
  let setting name value =
    R.Setting.create ~name ~value ~provenance:Execution_override ~limits:document_limits
    |> inference_ok
  in
  let absent = setting "temperature" Absent in
  let null = setting "temperature" Null in
  assert (not (R.Setting.equal absent null));
  assert (Result.is_error (C.Patch.create ~settings:[ absent; null ] ()));
  assert (Result.is_error (C.Patch.create ~settings:[ setting "credentials" Absent ] ()));
  let unavailable =
    C.{ revision = 0L; selected = None; capture = None; pending = false }
  in
  C.of_json (C.to_json unavailable) |> protocol_ok |> ignore;
  [%expect {| {"model":"model-B","settings":[]} |}]
;;

let%test_unit "selection preserves opaque fields and rejects another paid account" =
  let original = target () in
  let original =
    match R.Target.to_json original with
    | `Object fields ->
      R.Target.of_json
        (`Object (fields @ [ "future_private", `Object [ "lexeme", `Number "1.00" ] ]))
        ~limits:document_limits
      |> inference_ok
    | _ -> assert false
  in
  let changed =
    T.apply original ~patch:(patch "model-B") ~profile_target:None |> protocol_ok
  in
  assert (String.equal (R.Target.model changed) "model-B");
  assert (
    Document_schema.Json.equal
      (match
         Document_schema.Json.field (R.Target.to_json original) ~name:"future_private"
       with
       | Value x -> x
       | _ -> assert false)
      (match
         Document_schema.Json.field (R.Target.to_json changed) ~name:"future_private"
       with
       | Value x -> x
       | _ -> assert false));
  let approved =
    R.Target.create
      ~adapter:(R.Target.adapter original)
      ~profile:"other"
      ~profile_revision:(Some "2")
      ~account:(Some "another-paid-account")
      ~endpoint:(R.Target.endpoint original)
      ~model:"profile-default"
      ~settings:[]
      ~limits:document_limits
    |> inference_ok
  in
  let patch = C.Patch.create ~profile:"other" ~settings:[] () |> protocol_ok in
  assert (Result.is_error (T.apply original ~patch ~profile_target:(Some approved)))
;;

let%test_unit "qualified context reuse is pure and invalidates stale bindings" =
  let current = ref true in
  let preparations = ref 0 in
  let adapter =
    Runtime.Adapter.create
      ~id:"fixture"
      ~limits:Runtime.Limits.default
      ~bind:(fun _ -> if !current then Ok () else Error Target_unavailable)
      ~preflight_history:(fun ~target:_ _ -> Ok ())
      ~prepare:(fun ~preparation_id:_ _ ->
        incr preparations;
        Error Unsupported_setting)
      ()
    |> runtime_ok
  in
  let selected = target () in
  let previous = Runtime.Context.create adapter ~target:selected |> runtime_ok in
  let fresh = Runtime.Context.create adapter ~target:selected |> runtime_ok in
  assert (phys_equal previous (Runtime.Context.reuse_unchanged fresh ~previous));
  let changed =
    R.Target.with_model selected ~model:"B" ~limits:document_limits |> inference_ok
  in
  let changed_context = Runtime.Context.create adapter ~target:changed |> runtime_ok in
  assert (
    phys_equal changed_context (Runtime.Context.reuse_unchanged changed_context ~previous));
  current := false;
  assert (phys_equal fresh (Runtime.Context.reuse_unchanged fresh ~previous));
  [%test_eq: int] 0 !preparations
;;

let%test_unit "durable configuration revision survives encoding and rejects replay gaps" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let next =
      Agent_session.Session_delta.apply initial (Configuration_revision_changed 1L)
      |> protocol_ok
    in
    assert (
      Result.is_error
        (Agent_session.Session_delta.apply next (Configuration_revision_changed 1L)));
    assert (
      Result.is_error
        (Agent_session.Session_delta.apply next (Configuration_revision_changed 3L)));
    let stored = Agent_session.Session_state_document.authored next in
    let document =
      Agent_session.Session_state_document.encode stored ~limits:document_limits
      |> document_ok
    in
    let restored =
      Agent_session.Session_state_document.decode document ~limits:document_limits
      |> document_ok
    in
    [%test_eq: int64]
      1L
      (Agent_session.Session_state_document.value restored).spec.configuration_revision)
;;

let%expect_test "active A capture remains immutable while the next root capture selects B"
  =
  let fixture =
    Inference_fixture.create
      ~namespace:"configuration"
      ~default_model:"A"
      ~post_stream:(fun ~sw:_ ~inputs:_ -> Seq.empty)
  in
  let selected =
    Inference_fixture.capture_config fixture Chat_response.Config.default |> runtime_ok
  in
  let initial_context = Inference_fixture.resolve fixture selected |> runtime_ok in
  let selection =
    Inference.Selection.captured selected ~limits:document_limits |> inference_ok
  in
  Job_fixtures.with_actor
    ~prepare_state:(fun state ->
      { state with spec = { state.spec with inference_target = selection } })
    (fun _ _ actor writer _ ->
       let policy =
         Agent_session.Configuration_policy.
           { select_profile =
               (fun ~current:_ ~profile:_ ->
                 Error (Agent_protocol.Error.invalid_request "not configured"))
           ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
           ; resolve =
               (fun target ->
                 Inference_fixture.resolve fixture target
                 |> Result.map_error ~f:(fun _ ->
                   Agent_protocol.Error.invalid_request "unavailable"))
           }
       in
       A.set_configuration_policy actor policy |> protocol_ok;
       let captured, captured_r = Eio.Promise.create () in
       let release, release_r = Eio.Promise.create () in
       let dispatched, dispatched_r = Eio.Promise.create () in
       let finish, finish_r = Eio.Promise.create () in
       A.set_operation_worker
         actor
         (Some
            (Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
               let port = Option.value_exn caps.root_context in
               port.with_context
                 ~previous:initial_context
                 ~history:input.history
                 (fun context ~on_dispatch ->
                    assert (
                      String.equal (R.Target.model (Runtime.Context.target context)) "A");
                    Eio.Promise.resolve captured_r ();
                    Eio.Promise.await release;
                    on_dispatch
                      (T.safe_view (Runtime.Context.target context) |> protocol_ok);
                    Eio.Promise.resolve dispatched_r ();
                    Eio.Promise.await finish;
                    assert (
                      String.equal (R.Target.model (Runtime.Context.target context)) "A"));
               port.with_context
                 ~previous:initial_context
                 ~history:input.history
                 (fun context ~on_dispatch ->
                    assert (
                      String.equal (R.Target.model (Runtime.Context.target context)) "B");
                    on_dispatch
                      (T.safe_view (Runtime.Context.target context) |> protocol_ok));
               Completed
                 { final_history = input.history
                 ; moderator_snapshot = None
                 ; runtime_requests = []
                 })))
       |> protocol_ok;
       let entry =
         Agent_session.History_codec.user_text
           ~id:(history_id "configuration-user")
           "hello"
         |> Agent_session.History_codec.to_protocol
       in
       A.submit_message
         ~submitting_principal:principal_id
         actor
         ~attachment_id:writer.id
         entry
       |> protocol_ok
       |> ignore;
       Eio.Promise.await captured;
       let before = A.configuration actor |> protocol_ok in
       assert (C.equal_phase (Option.value_exn before.capture).phase Preparing);
       let request =
         C.Update_request.
           { session_id
           ; attachment_id = writer.id
           ; expected_generation = 0
           ; expected_revision = 0L
           ; patch = patch "B"
           ; idempotency_key =
               Agent_protocol.Idempotency_key.of_string "configuration-B" |> protocol_ok
           }
       in
       let changed = A.update_configuration actor request |> protocol_ok in
       assert changed.pending;
       [%test_eq: int64] 1L changed.revision;
       assert (Result.is_error (A.update_configuration actor request));
       Eio.Promise.resolve release_r ();
       Eio.Promise.await dispatched;
       let active = A.configuration actor |> protocol_ok in
       assert (C.equal_phase (Option.value_exn active.capture).phase Effective);
       assert active.pending;
       Eio.Promise.resolve finish_r ();
       await_idle actor |> ignore;
       let final = A.configuration actor |> protocol_ok in
       assert (not final.pending);
       [%test_eq: int64] 1L (Option.value_exn final.capture).revision;
       print_endline "A dispatched unchanged; B captured next; stale update rejected");
  [%expect {| A dispatched unchanged; B captured next; stale update rejected |}]
;;

let%expect_test "resolution failures and cancellation release preparing ownership" =
  let fixture =
    Inference_fixture.create
      ~namespace:"configuration-cleanup"
      ~default_model:"A"
      ~post_stream:(fun ~sw:_ ~inputs:_ -> Seq.empty)
  in
  let selected =
    Inference_fixture.capture_config fixture Chat_response.Config.default |> runtime_ok
  in
  let previous = Inference_fixture.resolve fixture selected |> runtime_ok in
  let selection =
    Inference.Selection.captured selected ~limits:document_limits |> inference_ok
  in
  Job_fixtures.with_actor
    ~prepare_state:(fun state ->
      { state with spec = { state.spec with inference_target = selection } })
    (fun _ _ actor writer _ ->
       let fail_resolution = ref true in
       let policy =
         Agent_session.Configuration_policy.
           { select_profile =
               (fun ~current:_ ~profile:_ ->
                 Error (Agent_protocol.Error.invalid_request "not configured"))
           ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
           ; resolve =
               (fun target ->
                 if !fail_resolution
                 then Error (Agent_protocol.Error.invalid_request "unavailable")
                 else
                   Inference_fixture.resolve fixture target
                   |> Result.map_error ~f:(fun _ ->
                     Agent_protocol.Error.invalid_request "unavailable"))
           }
       in
       A.set_configuration_policy actor policy |> protocol_ok;
       A.set_operation_worker
         actor
         (Some
            (Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
               let port = Option.value_exn caps.root_context in
               assert (
                 Result.is_error
                   (Result.try_with (fun () ->
                      port.with_context
                        ~previous
                        ~history:input.history
                        (fun _ ~on_dispatch:_ -> assert false))));
               assert (Option.is_none (A.configuration actor |> protocol_ok).capture);
               fail_resolution := false;
               let began, began_r = Eio.Promise.create () in
               let never, _ = Eio.Promise.create () in
               Eio.Fiber.first
                 (fun () ->
                    port.with_context
                      ~previous
                      ~history:input.history
                      (fun _ ~on_dispatch:_ ->
                         Eio.Promise.resolve began_r ();
                         Eio.Promise.await never))
                 (fun () -> Eio.Promise.await began);
               assert (Option.is_none (A.configuration actor |> protocol_ok).capture);
               port.with_context
                 ~previous
                 ~history:input.history
                 (fun context ~on_dispatch ->
                    on_dispatch
                      (T.safe_view (Runtime.Context.target context) |> protocol_ok));
               assert (
                 C.equal_phase
                   (Option.value_exn (A.configuration actor |> protocol_ok).capture).phase
                   Retained);
               Completed
                 { final_history = input.history
                 ; moderator_snapshot = None
                 ; runtime_requests = []
                 })))
       |> protocol_ok;
       let entry =
         Agent_session.History_codec.user_text ~id:(history_id "cleanup-user") "hello"
         |> Agent_session.History_codec.to_protocol
       in
       A.submit_message
         ~submitting_principal:principal_id
         actor
         ~attachment_id:writer.id
         entry
       |> protocol_ok
       |> ignore;
       await_idle actor |> ignore;
       assert (Option.is_some (A.configuration actor |> protocol_ok).capture);
       print_endline
         "failed resolution and cancelled preparation cleared; next capture dispatched");
  [%expect
    {| failed resolution and cancelled preparation cleared; next capture dispatched |}]
;;

let%test_unit "configuration receipt roundtrip carries no private target" =
  let receipt =
    Agent_protocol.Command_receipt.Committed
      (Configuration_updated { session_id; revision = 7L })
  in
  let json = Agent_protocol.Command_receipt.to_json receipt in
  let decoded = Agent_protocol.Command_receipt.of_json json |> protocol_ok in
  assert (Document_schema.Json.equal json (Agent_protocol.Command_receipt.to_json decoded));
  assert (not (String.is_substring (Jsonaf.to_string json) ~substring:"profile"))
;;

let%expect_test
    "production turn loop selects B after a blocked A tool; child and job stay A"
  =
  let requests = ref [] in
  let streams = ref 0 in
  let root_scope = ref None in
  let on_prepare request =
    (* Update validation has no model history; these are actual root/child requests. *)
    if not (List.is_empty (R.history request))
    then requests := !requests @ [ R.Target.model (R.target request) ]
  in
  let post_stream ~sw:_ ~inputs =
    incr streams;
    match !streams with
    | 1 ->
      let open Openai.Responses.Response_stream in
      let item =
        Openai.Responses.Response_stream.Item.Function_call
          { name = "block"
          ; arguments = "{}"
          ; call_id = "configuration-call"
          ; _type = "function_call"
          ; id = Some "configuration-item"
          ; status = Some "completed"
          }
      in
      Stdlib.List.to_seq
        [ Output_item_done { item; output_index = 0; type_ = "response.output_item.done" }
        ]
    | 2 -> Seq.empty (* fixed admitted child execution during blocked A tool *)
    | 3 ->
      assert (
        List.exists inputs ~f:(function
          | Openai.Responses.Item.Function_call_output _ -> true
          | _ -> false));
      Seq.empty
    | _ -> failwith "unexpected extra provider request"
  in
  let fixture =
    Inference_fixture.create_with_observer
      ~on_prepare
      ~namespace:"configuration-loop"
      ~default_model:"A"
      ~post_stream
  in
  let selected =
    Inference_fixture.capture_config fixture Chat_response.Config.default |> runtime_ok
  in
  let initial_context = Inference_fixture.resolve fixture selected |> runtime_ok in
  let selection =
    Inference.Selection.captured selected ~limits:document_limits |> inference_ok
  in
  Job_fixtures.with_actor
    ~prepare_state:(fun state ->
      { state with spec = { state.spec with inference_target = selection } })
    (fun env sw actor writer _ ->
       let policy =
         Agent_session.Configuration_policy.
           { select_profile =
               (fun ~current:_ ~profile:_ ->
                 Error (Agent_protocol.Error.invalid_request "not configured"))
           ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
           ; resolve =
               (fun target ->
                 Inference_fixture.resolve fixture target
                 |> Result.map_error ~f:(fun _ ->
                   Agent_protocol.Error.invalid_request "unavailable"))
           }
       in
       A.set_configuration_policy actor policy |> protocol_ok;
       let job = Inference_target_tests.job (A.state actor |> protocol_ok) in
       A.add_job actor job |> protocol_ok |> ignore;
       let started, started_r = Eio.Promise.create () in
       let release, release_r = Eio.Promise.create () in
       let tool_tbl = String.Table.create () in
       Hashtbl.set tool_tbl ~key:"block" ~data:(fun ~invocation:_ _ ->
         Eio.Promise.resolve started_r ();
         Eio.Promise.await release;
         (* Child execution admitted with A must remain on its own immutable context. *)
         Eio.Switch.run (fun sw ->
           let request =
             R.create
               ~target:selected
               ~history:
                 [ Agent_session.History_codec.user_text
                     ~id:(history_id "child-A")
                     "child"
                 ]
               ~tools:[]
               ~assets:[]
               ~limits:Transcript.Admission.default
             |> inference_ok
           in
           let child =
             Inference_client.Execution.create
               ~context:initial_context
               ~identity:(Inference_fixture.identity fixture)
               ~relation:
                 (Nested
                    { scope = Transcript.Scope.key (Option.value_exn !root_scope)
                    ; call_entry_id = None
                    ; call_alias = Some "configuration-call"
                    })
               ~before_dispatch:ignore
               ~on_attempt:ignore
               ~on_completion:ignore
               ~on_observation:ignore
           in
           Inference_client.Execution.run child ~sw ~request ~on_event:ignore |> ignore);
         Openai.Responses.Tool_output.Output.Text "released");
       let config = Authoring_input_tests.worker_config env post_stream in
       let worker =
         Agent_session.Turn_worker.create
           ~root_binding:(Chat_response.Root_binding.create (Runtime.Session.create ~sw))
           { config with
             inference_context = initial_context
           ; inference_identity = Inference_fixture.identity fixture
           ; tool_tbl
           ; on_inference_attempt =
               (fun attempt -> root_scope := Some (Runtime.Attempt.scope attempt))
           ; permission_profile =
               permission_policy
                 ~tool_default:Allow
                 ~fallback:Fallback_deny
                 ~evaluator:None
                 ~reviewer:None
           }
       in
       A.set_operation_worker actor (Some worker) |> protocol_ok;
       let entry =
         Agent_session.History_codec.user_text ~id:(history_id "loop-user") "hello"
         |> Agent_session.History_codec.to_protocol
       in
       A.submit_message
         ~submitting_principal:principal_id
         actor
         ~attachment_id:writer.id
         entry
       |> protocol_ok
       |> ignore;
       Eio.Promise.await started;
       let request =
         C.Update_request.
           { session_id
           ; attachment_id = writer.id
           ; expected_generation = 0
           ; expected_revision = 0L
           ; patch = patch "B"
           ; idempotency_key =
               Agent_protocol.Idempotency_key.of_string "loop-B" |> protocol_ok
           }
       in
       A.update_configuration actor request |> protocol_ok |> ignore;
       let job_binding = List.hd_exn (A.state actor |> protocol_ok).model_job_targets in
       let job_target =
         match
           Inference.Selection.view (Agent_session.Model_job_target.source job_binding)
         with
         | Captured target -> target
         | Unresolved -> assert false
       in
       assert (String.equal (R.Target.model job_target) "A");
       Eio.Promise.resolve release_r ();
       let finished = await_idle actor in
       assert (Option.is_none finished.active_operation);
       [%test_eq: string list] [ "A"; "A"; "B" ] !requests;
       [%test_eq: int] 3 !streams;
       print_endline "real root A -> blocked tool -> child A -> root B; durable job A");
  [%expect {| real root A -> blocked tool -> child A -> root B; durable job A |}]
;;

let%expect_test
    "yielding update validation keeps actor responsive and fences its exact basis"
  =
  List.iter
    [ `Stable; `History; `Configuration; `Attachment; `Cancellation ]
    ~f:(fun mode ->
      let fixture =
        Inference_fixture.create
          ~namespace:"configuration-validation"
          ~default_model:"A"
          ~post_stream:(fun ~sw:_ ~inputs:_ -> Seq.empty)
      in
      let selected =
        Inference_fixture.capture_config fixture Chat_response.Config.default
        |> runtime_ok
      in
      let selection =
        Inference.Selection.captured selected ~limits:document_limits |> inference_ok
      in
      Job_fixtures.with_actor
        ~prepare_state:(fun state ->
          { state with spec = { state.spec with inference_target = selection } })
        (fun _ sw actor writer _ ->
           let began, began_r = Eio.Promise.create () in
           let release, release_r = Eio.Promise.create () in
           let policy =
             Agent_session.Configuration_policy.
               { select_profile =
                   (fun ~current:_ ~profile:_ ->
                     Error (Agent_protocol.Error.invalid_request "not configured"))
               ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
               ; resolve =
                   (fun target ->
                     if String.equal (R.Target.model target) "B"
                     then (
                       Eio.Promise.resolve began_r ();
                       Eio.Promise.await release);
                     Inference_fixture.resolve fixture target
                     |> Result.map_error ~f:(fun _ ->
                       Agent_protocol.Error.invalid_request "unavailable"))
               }
           in
           A.set_configuration_policy actor policy |> protocol_ok;
           let request model =
             C.Update_request.
               { session_id
               ; attachment_id = writer.id
               ; expected_generation = 0
               ; expected_revision = 0L
               ; patch = patch model
               ; idempotency_key =
                   Agent_protocol.Idempotency_key.of_string ("validation-" ^ model)
                   |> protocol_ok
               }
           in
           let inspect () =
             Eio.Promise.await began;
             (* Would deadlock if resolver waited inside the mailbox. *)
             [%test_eq: int64] 0L (A.configuration actor |> protocol_ok).revision;
             A.state actor |> protocol_ok |> ignore
           in
           match mode with
           | `Cancellation ->
             Eio.Fiber.first
               (fun () -> A.update_configuration actor (request "B") |> ignore)
               inspect;
             [%test_eq: int64] 0L (A.configuration actor |> protocol_ok).revision;
             assert (Option.is_none (A.configuration actor |> protocol_ok).capture);
             A.update_configuration actor (request "C") |> protocol_ok |> ignore;
             print_endline "cancelled validation: no durable effect; actor reusable"
           | `Stable | `History | `Configuration | `Attachment ->
             let done_, done_r = Eio.Promise.create () in
             Eio.Fiber.fork ~sw (fun () ->
               Eio.Promise.resolve done_r (A.update_configuration actor (request "B")));
             inspect ();
             (match mode with
              | `Stable -> A.reserve_history_block actor ~count:1 |> protocol_ok |> ignore
              | `History ->
                let entry =
                  Agent_session.History_codec.user_text
                    ~id:(history_id "validation-deferred")
                    "later"
                  |> Agent_session.History_codec.to_protocol
                in
                A.defer_history actor ~attachment_id:writer.id [ entry ]
                |> protocol_ok
                |> ignore
              | `Configuration ->
                A.update_configuration actor (request "C") |> protocol_ok |> ignore
              | `Attachment -> A.detach actor writer.id |> protocol_ok |> ignore
              | `Cancellation -> assert false);
             Eio.Promise.resolve release_r ();
             let result = Eio.Promise.await done_ in
             (match mode, result with
              | `Stable, Ok view ->
                [%test_eq: int64] 1L view.revision;
                print_endline "unrelated revision: admitted"
              | (`History | `Configuration), Error error ->
                assert (Agent_protocol.Error.equal_code error.code Conflict);
                print_endline "history/configuration changed: conflict"
              | `Attachment, Error _ -> print_endline "writer detached: denied"
              | _ -> failwith "unexpected validation race result")));
  [%expect
    {|
    unrelated revision: admitted
    history/configuration changed: conflict
    history/configuration changed: conflict
    writer detached: denied
    cancelled validation: no durable effect; actor reusable
  |}]
;;

let%expect_test
    "root graph replacement owns bounded resources and close rejects new preparation"
  =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let opened = ref 0
      and closed = ref 0 in
      let target = target () in
      let prepare ~preparation_id:_ _ =
        Error Runtime.Preparation_error.Invalid_preparation
      in
      let resolve revision =
        let target =
          match R.Target.to_json target with
          | `Object fields ->
            R.Target.of_json
              (`Object
                  (List.Assoc.add
                     fields
                     ~equal:String.equal
                     "profile_revision"
                     (`String revision)))
              ~limits:document_limits
            |> inference_ok
          | _ -> assert false
        in
        let adapter =
          Runtime.Adapter.create
            ~id:(R.Target.adapter target)
            ~limits:Runtime.Limits.default
            ~bind:(fun _ -> Ok ())
            ~preflight_history:(fun ~target:_ _ -> Ok ())
            ~prepare
            ~open_session:(fun _ ~policy:_ ->
              incr opened;
              Ok
                (Runtime.Adapter.Session_binding.create ~prepare ~close:(fun () ->
                   incr closed)))
            ()
          |> runtime_ok
        in
        Runtime.Context.create adapter ~target |> runtime_ok
      in
      let owner = Runtime.Session.create ~sw in
      let original =
        Runtime.Context.with_session (resolve "original") owner |> runtime_ok
      in
      let receiver = Chat_response.Root_binding.create owner in
      for revision = 1 to 30 do
        Chat_response.Root_binding.with_context
          receiver
          ~resolved:(resolve (Int.to_string revision))
          ~f:(fun _ -> ())
        |> runtime_ok;
        assert (!opened - !closed = 2)
      done;
      let request context =
        R.create
          ~target:(Runtime.Context.target context)
          ~history:[]
          ~tools:[]
          ~assets:[]
          ~limits:document_limits
        |> inference_ok
      in
      Chat_response.Root_binding.with_context
        receiver
        ~resolved:(resolve "30")
        ~f:(fun context ->
          Chat_response.Root_binding.close receiver;
          match
            Runtime.Context.prepare
              context
              ~preparation_id:"after-close"
              (request context)
          with
          | Error Session_closed -> ()
          | _ -> failwith "closed root admitted new preparation")
      |> runtime_ok;
      assert (!opened - !closed = 1);
      assert (not (Runtime.Session.is_closed owner));
      (match
         Runtime.Context.prepare original ~preparation_id:"moderator" (request original)
       with
       | Error Invalid_preparation -> ()
       | _ -> failwith "original moderator resource was retired");
      Runtime.Session.close owner;
      assert (!opened = !closed);
      let foreign_owner = Runtime.Session.create ~sw in
      let registration =
        Runtime.Session.register_release foreign_owner ignore
        |> Result.map_error ~f:(fun _ -> "registration closed")
        |> Result.ok_or_failwith
      in
      assert (Result.is_error (Runtime.Session.release_registration owner registration));
      Runtime.Session.release_registration foreign_owner registration
      |> Result.map_error ~f:(fun _ -> "foreign registration")
      |> Result.ok_or_failwith;
      print_endline
        "30 changes retain one root plus moderator; close blocks preparation; foreign \
         registration rejects"));
  [%expect
    {|30 changes retain one root plus moderator; close blocks preparation; foreign registration rejects|}]
;;

let%expect_test
    "failed or cancelled root binding candidates preserve prior resource and release \
     borrow"
  =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let opened = ref 0
      and closed = ref 0 in
      let make name action =
        let target =
          R.Target.with_model (target ()) ~model:name ~limits:document_limits
          |> inference_ok
        in
        let target =
          match R.Target.to_json target with
          | `Object fields ->
            R.Target.of_json
              (`Object
                  (List.Assoc.add
                     fields
                     ~equal:String.equal
                     "profile_revision"
                     (`String name)))
              ~limits:document_limits
            |> inference_ok
          | _ -> assert false
        in
        Runtime.Adapter.create
          ~id:(R.Target.adapter target)
          ~limits:Runtime.Limits.default
          ~bind:(fun _ -> Ok ())
          ~preflight_history:(fun ~target:_ _ -> Ok ())
          ~prepare:(fun ~preparation_id:_ _ ->
            Error Runtime.Preparation_error.Invalid_preparation)
          ~open_session:(fun _ ~policy:_ -> action ())
          ()
        |> runtime_ok
        |> fun adapter -> Runtime.Context.create adapter ~target |> runtime_ok
      in
      let good =
        make "good" (fun () ->
          incr opened;
          Ok
            (Runtime.Adapter.Session_binding.create
               ~prepare:(fun ~preparation_id:_ _ ->
                 Error Runtime.Preparation_error.Invalid_preparation)
               ~close:(fun () -> incr closed)))
      in
      let owner = Runtime.Session.create ~sw in
      let receiver = Chat_response.Root_binding.create owner in
      let use context =
        Chat_response.Root_binding.with_context receiver ~resolved:context ~f:ignore
      in
      use good |> runtime_ok;
      assert (
        Result.is_error
          (use
             (make "failed" (fun () -> Error Runtime.Preparation_error.Target_unavailable))));
      let cancelled =
        try
          use (make "cancelled" (fun () -> raise Eio.Time.Timeout)) |> ignore;
          false
        with
        | Eio.Time.Timeout -> true
      in
      assert cancelled;
      use good |> runtime_ok;
      assert (!opened = 1 && !closed = 0);
      Chat_response.Root_binding.with_context receiver ~resolved:good ~f:(fun _ ->
        assert (Result.is_error (use good)))
      |> runtime_ok;
      let entered, entered_signal = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let suspended =
        make "suspended" (fun () ->
          Eio.Promise.resolve entered_signal ();
          Eio.Promise.await never)
      in
      Eio.Fiber.first
        (fun () -> use suspended |> ignore)
        (fun () -> Eio.Promise.await entered);
      use good |> runtime_ok;
      assert (!opened = 1 && !closed = 0);
      let closing_candidate =
        make "closing" (fun () ->
          incr opened;
          Runtime.Session.close owner;
          Ok
            (Runtime.Adapter.Session_binding.create
               ~prepare:(fun ~preparation_id:_ _ ->
                 Error Runtime.Preparation_error.Invalid_preparation)
               ~close:(fun () -> incr closed)))
      in
      assert (Result.is_error (use closing_candidate));
      assert (!opened = 2 && !closed = 2);
      assert (Result.is_error (use good));
      print_endline
        "candidate failure/cancellation preserve old resource; borrow releases; closed \
         graph never reopens"));
  [%expect
    {|candidate failure/cancellation preserve old resource; borrow releases; closed graph never reopens|}]
;;
