open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module I = P.Invocation
module D = Document_schema
module SD = A.Session_state_document

let set = Goal_catalog_tests.set
let member = Goal_catalog_tests.member

let capture invocation origin =
  I.capture_inference_origin invocation origin |> protocol_ok
;;

let%expect_test "invocation origin is explicit, validated and lifetime immutable" =
  let historical = invocation_fixture () in
  let run_id = P.Id.Run.of_string "run_invocation_origin" |> protocol_ok in
  List.iter [ P.Inference_origin.Unbound; Run run_id ] ~f:(fun origin ->
    let admitted = capture historical origin in
    let wire = I.to_json admitted in
    let without_origin = function
      | `Object fields ->
        `Object
          (List.filter fields ~f:(fun (name, _) ->
             not (String.equal name "inference_origin")))
      | _ -> assert false
    in
    List.iter
      [ without_origin wire
      ; set wire "inference_origin" `Null
      ; set wire "inference_origin" (`String "invalid")
      ]
      ~f:(fun malformed -> assert (Result.is_error (I.of_json malformed)));
    let missing_storage =
      I.Storage.to_json historical |> without_origin |> I.Storage.of_json |> protocol_ok
    in
    assert (Option.is_none missing_storage.inference_origin);
    let decoded = wire |> I.of_json |> protocol_ok in
    assert (I.equal admitted decoded);
    assert (
      I.equal admitted (I.Storage.to_json admitted |> I.Storage.of_json |> protocol_ok));
    assert (Result.is_error (I.capture_inference_origin admitted origin));
    List.iter
      [ historical, admitted; admitted, historical ]
      ~f:(fun (previous, incoming) ->
        assert (Result.is_error (I.validate_transition ~previous:(Some previous) incoming)));
    let dispatched = I.dispatch admitted |> protocol_ok in
    let resolved =
      I.resolve dispatched ~session_id ~generation:0 (Complete `Null) |> protocol_ok
    in
    assert (
      Option.equal
        P.Inference_origin.equal
        admitted.inference_origin
        resolved.inference_origin));
  assert (Result.is_error (A.Invocation_inference_provenance.of_invocation historical));
  assert (
    I.equal historical (I.Storage.to_json historical |> I.Storage.of_json |> protocol_ok));
  let forged = P.Id.Run.t_of_sexp (Sexp.Atom "invalid") in
  assert (Result.is_error (I.capture_inference_origin historical (Run forged)));
  print_endline "strict optional origin and immutable resolution";
  [%expect {| strict optional origin and immutable resolution |}]
;;

let%expect_test
    "complete invocation carriers retain unknown origin and reject contradictions"
  =
  Run_admission_tests.with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    start (request "invocation-origin-carrier") |> protocol_ok |> ignore;
    let basis = A.Session_actor.state actor |> protocol_ok in
    let run = List.hd_exn (A.Run_state.runs (Option.value_exn basis.run_state)) in
    (* Storage history only. This imported claim grants no live invocation,
       execution permission, worker proof or new Run-owned work. *)
    let invocation = capture (invocation_fixture ()) (Run run.id) in
    let state = { basis with invocations = [ invocation ] } in
    A.Session_state.validate state |> protocol_ok;
    let document =
      SD.authored state |> SD.encode ~limits:document_limits |> document_ok
    in
    let json = D.Document.json document in
    let raw_invocation = I.Storage.to_json invocation in
    let raw_invocation =
      set
        raw_invocation
        "inference_origin"
        (set
           (member raw_invocation "inference_origin")
           "future_origin"
           (`String "retained"))
    in
    let raw =
      set
        json
        "payload"
        (set (member json "payload") "invocations" (`Array [ raw_invocation ]))
    in
    let decode raw =
      D.Document.inspect ~limits:document_limits raw
      |> document_ok
      |> SD.decode ~limits:document_limits
      |> document_ok
    in
    let carrier = decode raw in
    assert (
      D.Json.equal
        raw
        (D.Document.json (SD.encode carrier ~limits:document_limits |> document_ok)));
    let conflicting =
      set
        raw_invocation
        "inference_origin"
        (set
           (member raw_invocation "inference_origin")
           "future_origin"
           (`String "conflict"))
    in
    let conflicting =
      set
        raw
        "payload"
        (set (member raw "payload") "invocations" (`Array [ conflicting ]))
      |> decode
    in
    assert (Result.is_error (SD.adopt carrier ~limits:document_limits conflicting));
    let erased = { state with invocations = [ invocation_fixture () ] } in
    assert (
      Result.is_error (SD.encode (SD.with_value carrier erased) ~limits:document_limits));
    assert (
      Result.is_error (SD.adopt carrier ~limits:document_limits (SD.authored erased)));
    assert (Result.is_error (A.Session_delta.apply state (Created erased)));
    assert (Result.is_error (A.Session_state.validate { state with run_state = None }));
    let child =
      I.create
        { invocation.context with
          id = P.Id.Invocation.of_string "inv_origin_child" |> protocol_ok
        ; parent_invocation = Some invocation.context.id
        }
      |> protocol_ok
      |> fun child -> capture child Unbound
    in
    assert (
      Result.is_error
        (A.Session_state.validate { state with invocations = [ invocation; child ] })));
  print_endline "final-state ancestry, immutable adoption and exact future children";
  [%expect {| final-state ancestry, immutable adoption and exact future children |}]
;;

let%expect_test
    "real foreground capture rejects supplied claims and failed saves expose none"
  =
  List.iter [ false; true ] ~f:(fun fail_first ->
    let reject = ref fail_first in
    let saved = ref false in
    with_handoff_actor
      ~reject:(fun transition ->
        !reject
        && List.exists
             transition.A.Session_transition.state.invocations
             ~f:(fun invocation ->
               P.Id.Invocation.equal
                 invocation.context.id
                 (invocation_fixture ()).context.id))
      ~make_worker:(fun _env actor_ready ->
        A.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let original = invocation_fixture () in
          let forged = capture original Unbound in
          assert (
            Result.is_error
              (caps.with_invocation
                 ~invocation:forged
                 (fun ~dispatched:_ ~borrow_agent_actor:_ -> assert false)));
          let run () =
            caps.with_invocation
              ~invocation:original
              (fun ~dispatched ~borrow_agent_actor:_ ->
                 assert (
                   Option.equal
                     P.Inference_origin.equal
                     dispatched.inference_origin
                     (Some Unbound));
                 Ok (Complete `Null))
          in
          if fail_first
          then (
            assert (Result.is_error (run ()));
            let unchanged =
              A.Session_actor.state (Eio.Promise.await actor_ready) |> protocol_ok
            in
            assert (List.is_empty unchanged.invocations);
            reject := false);
          let resolved = run () |> protocol_ok in
          assert (
            Option.equal P.Inference_origin.equal resolved.inference_origin (Some Unbound));
          saved := true;
          Completed
            { final_history = input.history
            ; moderator_snapshot = None
            ; runtime_requests = []
            }))
      (fun _env actor _writer _backend ->
         let state = await_idle actor in
         assert !saved;
         [%test_eq: int] (List.length state.invocations) 1));
  print_endline "actor capture only; failed admission retries the same fresh request";
  [%expect {| actor capture only; failed admission retries the same fresh request |}]
;;

let%expect_test "real Run event and deferred observer preserve terminal Run origin" =
  Run_admission_tests.with_startup_actor (fun actor _ _ request start manager _ _ _ ->
    start (request "origin-delayed-observer") |> protocol_ok |> ignore;
    let state = A.Session_actor.state actor |> protocol_ok in
    let run = List.hd_exn (A.Run_state.runs (Option.value_exn state.run_state)) in
    let observer = run.source.observer in
    let snapshot =
      Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
    in
    let native_calls = ref 0 in
    let ran =
      A.Session_actor.with_current_moderator_event
        actor
        ~operation_id:None
        ~event:Session_start
        ~snapshot:(fun () -> Ok snapshot)
        (fun ~executing ~retirement_reason:_ ~event:_ ~execute ~commit ->
           let invocation =
             I.create
               ~observer
               ~parent_event:executing.context.id
               { (invocation_fixture ()).context with origin = Moderator }
             |> protocol_ok
           in
           let resolved =
             execute ~invocation (fun ~dispatched ~borrow_agent_actor:_ ->
               assert (
                 Option.equal
                   P.Inference_origin.equal
                   dispatched.inference_origin
                   (Some (Run run.id)));
               Int.incr native_calls;
               Ok (Complete (`String "real native result")))
             |> protocol_ok
           in
           assert (
             Option.equal
               P.Inference_origin.equal
               resolved.inference_origin
               (Some (Run run.id)));
           let service =
             A.Session_actor.run_actions actor executing
             |> protocol_ok
             |> Option.value_exn
           in
           let transaction = A.Run_action_service.transaction service in
           let ticket =
             transaction.handlers.stage
               (Finish { terminal = Completed None; relinquish = [] })
             |> Result.ok_or_failwith
           in
           ignore
             (transaction.prepare [ ticket ] |> Result.ok_or_failwith
              : P.Run_action.t option);
           commit
             ~snapshot
             ~requests:
               { request_turn = false; request_compaction = false; end_session = None })
      |> protocol_ok
    in
    assert ran;
    let state = A.Session_actor.state actor |> protocol_ok in
    assert (Option.is_none state.active_operation);
    let terminal = List.hd_exn (A.Run_state.runs (Option.value_exn state.run_state)) in
    (match terminal.lifecycle with
     | Terminal (Completed _) -> ()
     | _ -> failwith "actual Run finish did not complete");
    let observed =
      A.Session_actor.with_idle_moderator_observation_tools
        actor
        ~observer
        (fun ~observing ~execute ~commit ->
           assert (
             Option.equal
               P.Inference_origin.equal
               observing.inference_origin
               (Some (Run run.id)));
           let invocation =
             I.create
               ~observer
               { observing.context with
                 id = P.Id.Invocation.of_string "inv_delayed_origin" |> protocol_ok
               ; parent_invocation = Some observing.context.id
               ; parent_job = None
               ; origin = Moderator
               ; provider_call_id = None
               ; call_entry_id = None
               }
             |> protocol_ok
           in
           let child =
             execute ~invocation (fun ~dispatched ~borrow_agent_actor:_ ->
               assert (
                 Option.equal
                   P.Inference_origin.equal
                   dispatched.inference_origin
                   (Some (Run run.id)));
               Int.incr native_calls;
               Ok (Complete `Null))
             |> protocol_ok
           in
           assert (
             Option.equal
               P.Inference_origin.equal
               child.inference_origin
               (Some (Run run.id)));
           let resolved = I.complete_observation observing |> protocol_ok in
           commit ~resolved ~snapshot)
      |> protocol_ok
    in
    assert observed;
    [%test_eq: int] !native_calls 2;
    let state = A.Session_actor.state actor |> protocol_ok in
    List.iter state.invocations ~f:(fun invocation ->
      assert (
        Option.equal
          P.Inference_origin.equal
          invocation.inference_origin
          (Some (Run run.id)))));
  print_endline
    "actual event native result, real finish, idle observation and child retain exact Run";
  [%expect
    {| actual event native result, real finish, idle observation and child retain exact Run |}]
;;

let%expect_test "genuine unavailable parent allows native work without inventing origin" =
  Job_fixtures.with_actor (fun _env _sw actor _writer _backend ->
    let job = Job_fixtures.add_claimed_job actor in
    assert (Option.is_none job.inference_origin);
    Job_fixtures.with_job actor job (fun ~job:_ ~execute ->
      let root = Job_fixtures.root job in
      let resolved =
        execute ~invocation:root (fun ~dispatched ~borrow_agent_actor:_ ->
          assert (Option.is_none dispatched.inference_origin);
          let child =
            I.create
              { dispatched.context with
                id = P.Id.Invocation.create ()
              ; parent_job = None
              ; parent_invocation = Some dispatched.context.id
              }
            |> protocol_ok
          in
          let child =
            execute ~invocation:child (fun ~dispatched ~borrow_agent_actor:_ ->
              assert (Option.is_none dispatched.inference_origin);
              Ok (Complete `Null))
            |> protocol_ok
          in
          assert (Result.is_error (A.Invocation_inference_provenance.of_invocation child));
          Ok (Complete `Null))
        |> protocol_ok
      in
      assert (Option.is_none resolved.inference_origin);
      Ok ())
    |> protocol_ok;
    Job_fixtures.complete actor job |> protocol_ok |> ignore);
  print_endline "real parent absence propagates; strict attribution remains unavailable";
  [%expect {| real parent absence propagates; strict attribution remains unavailable |}]
;;

let%expect_test
    "consumed occurrence receipts preserve original attribution and strict codec"
  =
  let module E = P.Moderator_execution in
  let module Q = E.Queue_occurrence in
  let source =
    P.Run_source.create
      ~observer:{ script_id = "queue-capture"; source_sha256 = String.make 64 'a' }
      ~generation:0
      ~installation_epoch:1L
    |> protocol_ok
  in
  let context : E.context =
    { id = P.Id.Moderator_execution.of_string "mex_capture" |> protocol_ok
    ; session_id
    ; generation = 0
    ; source = source.observer
    ; operation_id = Some (P.Id.Operation.of_string "op_physical_drainer" |> protocol_ok)
    ; job = None
    ; phase = Internal_event
    ; event = `Null
    ; checkpoint_sha256 = String.make 64 'b'
    ; created_at = timestamp
    }
  in
  let historical = E.create context |> protocol_ok in
  let run_id = P.Id.Run.of_string "run_original_queue" |> protocol_ok in
  let remove json name =
    match json with
    | `Object fields ->
      `Object (List.filter fields ~f:(fun (field, _) -> not (String.equal name field)))
    | _ -> assert false
  in
  (* Context-free evidence fixtures only. Named Run retention and actual head
     admission belong to the complete carrier checks; these confer no authority. *)
  List.iter [ None; Some P.Inference_origin.Unbound; Some (Run run_id) ] ~f:(fun origin ->
    let occurrence = Q.create ~source ~ordinal:7L ~origin |> protocol_ok in
    let admitted = E.create_queued ~queue_occurrence:occurrence context |> protocol_ok in
    let wire = E.to_json admitted in
    assert (D.Json.equal (member wire "schema_version") (`Number "5"));
    assert (E.equal admitted (E.of_json wire |> protocol_ok));
    assert (Q.equal occurrence (Q.t_of_sexp (Q.sexp_of_t occurrence)));
    let expected =
      A.Invocation_inference_provenance.of_event ~runs:None ~jobs:[] admitted
      |> protocol_ok
    in
    assert (Option.equal P.Inference_origin.equal expected origin);
    List.iter
      [ remove wire "queue_occurrence"
      ; set wire "queue_occurrence" `Null
      ; set wire "schema_version" (`Number "4")
      ; set wire "queue_occurrence" (remove (Q.to_json occurrence) "origin")
      ; set wire "queue_occurrence" (set (Q.to_json occurrence) "ordinal" (`String "-1"))
      ; set wire "queue_occurrence" (set (Q.to_json occurrence) "ordinal" (`String "07"))
      ]
      ~f:(fun malformed -> assert (Result.is_error (E.of_json malformed)));
    let malformed =
      Q.to_json occurrence |> fun json -> set json "ordinal" (`String "-1")
    in
    assert (Exn.does_raise (fun () -> Q.t_of_sexp (Jsonaf.sexp_of_t malformed) |> ignore));
    let other = Q.create ~source ~ordinal:8L ~origin |> protocol_ok in
    let reassigned = E.create_queued ~queue_occurrence:other context |> protocol_ok in
    List.iter
      [ historical, admitted; admitted, historical; admitted, reassigned ]
      ~f:(fun (previous, incoming) ->
        assert (Result.is_error (E.validate_transition ~previous:(Some previous) incoming)));
    let completed =
      E.complete
        admitted
        ~checkpoint_sha256:(String.make 64 'c')
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    assert (Option.equal Q.equal completed.queue_occurrence (Some occurrence));
    let completed = E.to_json completed |> E.of_json |> protocol_ok in
    assert (
      Option.equal
        P.Inference_origin.equal
        origin
        (A.Invocation_inference_provenance.of_event ~runs:None ~jobs:[] completed
         |> protocol_ok));
    assert (
      Result.is_error
        (E.create_queued ~queue_occurrence:occurrence { context with phase = Turn_start }));
    assert (
      Result.is_error
        (E.create_queued ~queue_occurrence:occurrence { context with generation = 1 })));
  List.iter [ 1; 2; 3; 4 ] ~f:(fun version ->
    let old =
      E.to_json historical
      |> fun json -> set json "schema_version" (`Number (Int.to_string version))
    in
    assert (Option.is_none (E.of_json old |> protocol_ok).queue_occurrence));
  assert (Result.is_error (Q.create ~source ~ordinal:(-1L) ~origin:None));
  print_endline "strict v5 capture; unknown stays unknown; immutable through completion";
  [%expect {| strict v5 capture; unknown stays unknown; immutable through completion |}]
;;
