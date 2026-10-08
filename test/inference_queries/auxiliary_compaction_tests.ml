open! Core
module P = Agent_protocol
module S = Agent_server
module A = Agent_session.Session_actor
module L = Agent_session.Inference_ledger
module O = Inference.Observation
module R = Inference_runtime
module F = Agent_server_test_support

let ok = F.protocol_ok
let state actor = A.state actor |> ok
let rows actor = L.rows (state actor).inference_ledger

let row_for_scope actor scope =
  List.find_exn (rows actor) ~f:(fun row ->
    Transcript.Scope.equal (L.Handle.scope (L.Row.handle row)) scope)
;;

let summary_item =
  Openai.Responses.Response_stream.Item.Output_message
    { role = Assistant
    ; id = "auxiliary-summary"
    ; content =
        [ { annotations = []; text = "Retained offline summary"; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
;;

let summary_stream ~sw:_ ~inputs:_ =
  Stdlib.List.to_seq
    [ Openai.Responses.Response_stream.Output_item_done
        { item = summary_item; output_index = 0; type_ = "response.output_item.done" }
    ]
;;

let with_daemon ~post_stream ~map_policy f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = F.temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let prompt_file = Filename.concat root "prompt.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          "<developer>Retain this policy.</developer><user>Remember the user.</user>";
        Eio.Switch.run (fun sw ->
          let policy =
            F.inference_policy ~default_model:"fixture" ~post_stream |> map_policy
          in
          let daemon =
            S.Daemon.start
              ~sw
              ~env
              ~config:(F.config root root prompt_file)
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:{ S.Daemon.default_options with inference_policy = policy }
              ()
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> S.Daemon.shutdown daemon |> ok)
            ~f:(fun () ->
              let connection = F.connection daemon (F.principal ()) in
              F.initialize connection;
              f env sw daemon connection))))
;;

let entry daemon session_id =
  S.Session_registry.find (S.Daemon.registry daemon) session_id |> Option.value_exn
;;

let await_finished env actor =
  let clock = Eio.Stdenv.clock env in
  Eio.Time.with_timeout_exn clock 5. (fun () ->
    let rec loop () =
      let current = state actor in
      if Option.is_none current.active_operation
      then current
      else (
        Eio.Time.sleep clock 0.01;
        loop ())
    in
    loop ())
;;

let compact env entry attachment =
  let before = state entry.S.Session_registry.actor in
  A.compact
    entry.actor
    ~attachment_id:attachment.P.Session.Attachment.id
    ~expected_revision:(Some before.counters.revision)
  |> ok
  |> ignore;
  await_finished env entry.actor
;;

let ledger_bytes ledger =
  L.to_document ledger
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (L.Error.sexp_of_t error))
  |> Result.ok_or_failwith
  |> Document_schema.Document.to_string
;;

let assert_terminal row =
  match O.Attempt_record.state (L.Row.record row) with
  | Terminal terminal ->
    (match Inference.Event.Terminal.outcome terminal with
     | Completed -> ()
     | _ -> failwith "auxiliary provider did not complete")
  | _ -> failwith "auxiliary attempt did not retain actual terminal evidence"
;;

let%expect_test
    "Factory stopped auxiliary dispatch ACKs ledger; loaded dispatch reuses graph"
  =
  List.iter [ false; true ] ~f:(fun loaded ->
    let dispatches = ref 0 in
    let allocations = ref 0 in
    let admitted = ref 0 in
    let acknowledged = ref 0 in
    let map_policy (base : S.Session_factory.inference_policy) =
      { base with
        runtime_inference_ports =
          (fun actor ->
            let open Result.Let_syntax in
            let%map upstream = base.runtime_inference_ports actor in
            incr allocations;
            { upstream with
              on_admitted =
                (fun ~scope ~accounting_id ->
                  let row = row_for_scope actor scope in
                  assert (
                    O.Observation_id.equal
                      accounting_id
                      (L.Handle.accounting_id (L.Row.handle row)));
                  (match O.Attempt_record.state (L.Row.record row) with
                   | Prepared -> ()
                   | _ -> failwith "upstream ran before Prepared ACK");
                  incr admitted;
                  upstream.on_admitted ~scope ~accounting_id)
            ; on_attempt =
                (fun attempt ->
                  (match
                     O.Attempt_record.state
                       (L.Row.record (row_for_scope actor (R.Attempt.scope attempt)))
                   with
                   | Running -> ()
                   | _ -> failwith "backend ran before Running ACK");
                  incr acknowledged;
                  upstream.on_attempt attempt)
            })
      }
    in
    with_daemon
      ~post_stream:(fun ~sw ~inputs ->
        incr dispatches;
        summary_stream ~sw ~inputs)
      ~map_policy
      (fun env _sw daemon connection ->
         let session, attachment =
           F.create_session ~start_immediately:loaded connection
         in
         let current = entry daemon session.id in
         assert (Bool.equal loaded (S.Runtime_owner.is_loaded current.runtime));
         [%test_eq: int] 0 !dispatches;
         [%test_eq: int] 1 !allocations;
         let before = state current.actor in
         let first = compact env current attachment in
         if not loaded then S.Runtime_owner.unload_and_wait current.runtime |> ok;
         let second = compact env current attachment in
         [%test_eq: int] before.identity.generation second.identity.generation;
         [%test_eq: int]
           (before.conversation.compaction_generation + 1)
           first.conversation.compaction_generation;
         [%test_eq: int]
           (before.conversation.compaction_generation + 2)
           second.conversation.compaction_generation;
         [%test_eq: int] 2 !dispatches;
         [%test_eq: int] 2 !admitted;
         [%test_eq: int] 2 !acknowledged;
         [%test_eq: int] (if loaded then 1 else 3) !allocations;
         assert (Bool.equal loaded (S.Runtime_owner.is_loaded current.runtime));
         let retained = L.rows second.inference_ledger in
         [%test_eq: int] 2 (List.length retained);
         List.iter retained ~f:assert_terminal;
         let sources =
           List.map retained ~f:(fun row ->
             (Transcript.Scope.key (L.Handle.scope (L.Row.handle row))).source)
         in
         assert (
           Bool.equal
             loaded
             (Transcript.Source_id.equal
                (List.nth_exn sources 0)
                (List.nth_exn sources 1)));
         let turns =
           P.Inference_query.Summary.turns (L.summary second.inference_ledger)
         in
         [%test_eq: int64] 0L turns.completed;
         [%test_eq: int64] 0L turns.pending;
         let ledger = ledger_bytes second.inference_ledger in
         let indexed =
           Agent_store.Session_index.find
             (Agent_store.Session_store.session_index (S.Daemon.store daemon))
             session.id
           |> Option.value_exn
         in
         ignore
           (S.Session_registry.remove (S.Daemon.registry daemon) session.id
            : S.Session_registry.entry option);
         current.close ();
         let restored =
           S.Session_factory.read_session (S.Daemon.factory daemon) indexed |> ok
         in
         [%test_eq: string] ledger (ledger_bytes restored.inference_ledger);
         [%test_eq: int] 2 !dispatches;
         print_s
           [%sexp
             (loaded : bool)
           , "two actual admissions; no extra runtime; durable ledger retained"]));
  [%expect
    {|
    (false "two actual admissions; no extra runtime; durable ledger retained")
    (true "two actual admissions; no extra runtime; durable ledger retained")
    |}]
;;

let%expect_test
    "stopped auxiliary cancellation joins request and tracking before owner cleanup"
  =
  let entered, entered_u = Eio.Promise.create () in
  let forever, _ = Eio.Promise.create () in
  let cancelled = ref false in
  let post_stream ~sw:_ ~inputs:_ =
    let pending () =
      Eio.Promise.resolve entered_u ();
      try
        Eio.Promise.await forever;
        Stdlib.Seq.Nil
      with
      | Eio.Cancel.Cancelled _ as exn ->
        cancelled := true;
        raise exn
    in
    fun () ->
      Stdlib.Seq.Cons
        ( Openai.Responses.Response_stream.Output_item_added
            { item = summary_item
            ; output_index = 0
            ; type_ = "response.output_item.added"
            }
        , pending )
  in
  with_daemon ~post_stream ~map_policy:Fn.id (fun env _sw daemon connection ->
    let session, attachment = F.create_session connection in
    let current = entry daemon session.id in
    let before = state current.actor in
    A.compact
      current.actor
      ~attachment_id:attachment.id
      ~expected_revision:(Some before.counters.revision)
    |> ok
    |> ignore;
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Promise.await entered);
    A.stop current.actor ~attachment_id:attachment.id ~mode:Cancel |> ok |> ignore;
    ignore (await_finished env current.actor : Agent_session.Session_state.t);
    S.Runtime_owner.unload_and_wait current.runtime |> ok;
    let after = state current.actor in
    assert !cancelled;
    assert (not (S.Runtime_owner.is_loaded current.runtime));
    [%test_eq: int]
      before.conversation.compaction_generation
      after.conversation.compaction_generation;
    [%test_eq: int] 1 (List.length (L.rows after.inference_ledger));
    (match
       O.Attempt_record.state (L.Row.record (List.hd_exn (L.rows after.inference_ledger)))
     with
     | Interrupted { reason = Cancelled; delivery = Response_started } -> ()
     | _ -> failwith "cancellation lost actual delivery or terminal ownership");
    print_endline
      "cancelled request joined; stopped owner released; actual interruption persisted");
  [%expect
    {| cancelled request joined; stopped owner released; actual interruption persisted |}]
;;

let%expect_test "resolver target mismatch refuses before auxiliary owner or dispatch" =
  let allocations = ref 0 in
  let mismatch = ref false in
  let map_policy (base : S.Session_factory.inference_policy) =
    { base with
      resolve_inference_context =
        (fun target ->
          if !mismatch
          then (
            let target =
              Inference.Request.Target.with_model
                target
                ~model:"wrong-model"
                ~limits:Transcript.Admission.default
              |> Result.map_error ~f:(fun error ->
                Inference_runtime.Preparation_error.Invalid_request error)
            in
            Result.bind target ~f:base.resolve_inference_context)
          else base.resolve_inference_context target)
    ; runtime_inference_ports =
        (fun actor ->
          incr allocations;
          base.runtime_inference_ports actor)
    }
  in
  with_daemon
    ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "mismatched resolver dispatched")
    ~map_policy
    (fun env _sw daemon connection ->
       let session, attachment = F.create_session connection in
       let current = entry daemon session.id in
       let before = state current.actor in
       [%test_eq: int] 1 !allocations;
       mismatch := true;
       let after = compact env current attachment in
       [%test_eq: int] 1 !allocations;
       assert (List.is_empty (L.rows after.inference_ledger));
       [%test_eq: int]
         before.conversation.compaction_generation
         after.conversation.compaction_generation;
       assert (not (S.Runtime_owner.is_loaded current.runtime));
       let events =
         match
           Agent_session.Durable_event_log.replay
             current.durable_events
             ~after_sequence:before.counters.event_sequence
             ~through_sequence:after.counters.event_sequence
         with
         | Available events -> events
         | Snapshot_required -> failwith "mismatch terminal left retained event window"
       in
       let failures =
         List.filter_map events ~f:(fun (event : P.Event.Durable.t) ->
           match P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> ok with
           | Operation_failed { kind = Compaction; state = Failed failure; _ } ->
             Some failure
           | _ -> None)
       in
       let failure = List.hd_exn failures in
       [%test_eq: int] 1 (List.length failures);
       [%test_eq: P.Error.code] Conflict failure.code;
       [%test_eq: string] "selected compaction inference target changed" failure.message;
       print_endline "mismatched context refused with no owner, attempt, or activation");
  [%expect {| mismatched context refused with no owner, attempt, or activation |}]
;;
