open! Core
open Fixtures
open Recovery_window_actor_tests
module Effective = Recovery_effective_actor_tests
module Record = A.Recovery_effective_record
module Attempt = A.Recovery_effective_attempt
module Accounting = A.Inference_ledger
module Runtime = Inference_runtime
module Graph = Agent_server.Graph_tracking

type mode =
  | Confirm
  | Before_ack
  | Before_send
  | After_send
  | Stale_preparation
[@@deriving equal]

exception Interrupted

let request_ok result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let receipt state =
  let records = Effective.effective_records state in
  Record.attempt (List.last_exn records) |> Option.value_exn
;;

let expect state ~tracked phase =
  let actual = receipt state in
  assert (Bool.equal (Attempt.tracked actual) tracked);
  assert (Attempt.equal_phase (Attempt.phase actual) phase);
  actual
;;

let runtime_ok result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Runtime.Preparation_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let prepare_fixture runtime =
  let target =
    Inference_fixture.capture_config runtime Chat_response.Config.default |> runtime_ok
  in
  let previous = Inference_fixture.resolve runtime target |> runtime_ok in
  let configuration =
    let request =
      Inference.Request.create
        ~target
        ~history:[]
        ~tools:[]
        ~assets:[]
        ~limits:Transcript.Admission.default
      |> request_ok
    in
    Runtime.Context.prepare previous ~preparation_id:"recovery-seed" request
    |> runtime_ok
    |> Runtime.Prepared.configuration
  in
  target, previous, configuration
;;

let saturated state configuration rows =
  let source = Inference_tracking_tests.source "recovery-pressure" in
  let inference_ledger =
    List.init rows ~f:Fn.id
    |> List.fold ~init:state.State.inference_ledger ~f:(fun ledger _ ->
      match
        Accounting.admit
          ledger
          ~source
          ~relation:Root
          ~operation_id:None
          ~invocation_id:None
          ~configuration
      with
      | Ok (ledger, _, Tracked) -> ledger
      | Ok (_, _, Untracked _) | Error _ -> failwith "could not seed accounting pressure")
  in
  { state with inference_ledger }
;;

let load fixture journal initial =
  Store.Recovery.load
    ~env:fixture.env
    ~journal
    ~snapshot_directory:(Store.Session_store.Handle.snapshot_directory fixture.handle)
    ~max_snapshot_payload_length:payload_limit
    ~session_id
    ~initial
    ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
    ~apply:(Persistence.apply_transaction ~limits:document_limits)
    ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
    ~validate:Persistence.validate
  |> store_ok
;;

let no_archive _ _ = failwith "inference receipt test reached the archive writer"

let with_attempts ?snapshot_payload_limit ?on_commit ~pressure f ~finish =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let mode = ref Confirm in
      let dispatches = ref 0 in
      let post_stream ~sw:_ ~inputs:_ =
        incr dispatches;
        match !mode with
        | After_send -> raise Interrupted
        | Confirm | Stale_preparation -> Seq.empty
        | Before_ack | Before_send -> failwith "interruption reached the provider"
      in
      let runtime =
        Inference_fixture.create
          ~namespace:"recovery-attempts"
          ~default_model:"model"
          ~post_stream
      in
      let target, previous, configuration = prepare_fixture runtime in
      let inference_target =
        Inference.Selection.captured target ~limits:document_limits |> request_ok
      in
      let base =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let base = { base with spec = { base.spec with inference_target } } in
      let initial_state = saturated base configuration pressure in
      let storage = Job_artifact_fixtures.create env sw initial_state in
      let owner =
        P.Session_ref.create
          ~server_id:(Store.Session_store.server_id storage.sessions)
          ~session_id
      in
      let fixture : fixture =
        { env; sessions = storage.sessions; handle = storage.session; owner }
      in
      let initial = Persistence.Restored.authored initial_state in
      let active_journal = journal fixture Store.Journal.create in
      let before_commit =
        Option.map on_commit ~f:(fun hook -> hook fixture active_journal)
      in
      let after =
        with_actor
          ?snapshot_payload_limit
          ?before_commit
          fixture
          sw
          active_journal
          ~restored:initial
          ~previous_transaction_hash:None
          ~archive:no_archive
          (fun actor _ ->
             let attachment_id = attach actor in
             let policy =
               A.Configuration_policy.
                 { select_profile =
                     (fun ~current:_ ~profile:_ ->
                       Error (P.Error.invalid_request "fixed target"))
                 ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
                 ; resolve =
                     (fun target ->
                       Inference_fixture.resolve runtime target
                       |> Result.map_error ~f:(fun _ ->
                         P.Error.invalid_request "missing target"))
                 }
             in
             Actor.set_configuration_policy actor policy |> protocol_ok;
             let preparation_id = ref 0 in
             let worker_failure, publish_failure = Eio.Promise.create () in
             let run
                   ~sw
                   ~(input : A.Operation_worker.Input.t)
                   (caps : A.Operation_worker.Capabilities.t)
               : A.Operation_worker.outcome
               =
               let upstream =
                 { (Inference_tracking_tests.upstream (fun ~scope:_ ~accounting_id:_ ->
                      if equal_mode !mode Before_ack then raise Interrupted))
                   with
                   new_preparation_id =
                     (fun () ->
                       incr preparation_id;
                       sprintf "recovery-attempt-%d" !preparation_id)
                 }
               in
               let graph =
                 Graph.create
                   actor
                   ~source:(Inference_tracking_tests.source "foreground")
                   ~upstream
                 |> protocol_ok
               in
               Exn.protect
                 ~finally:(fun () ->
                   Graph.seal graph |> protocol_ok;
                   Graph.finish graph |> protocol_ok)
                 ~f:(fun () ->
                   let identity =
                     Graph.bind_identity graph ~allocate:caps.allocate_inference
                   in
                   let root = Option.value_exn caps.root_context in
                   root.with_context
                     ~previous
                     ~history:input.history
                     (fun
                         context
                          ~select_checkpoint:_
                          ~select_prepared:_
                          ~on_prepared
                          ~on_dispatch
                        ->
                        let request =
                          Inference.Request.create
                            ~target:(Runtime.Context.target context)
                            ~history:input.history
                            ~tools:[]
                            ~assets:[]
                            ~limits:Transcript.Admission.default
                          |> request_ok
                        in
                        let identity =
                          match !mode with
                          | Stale_preparation ->
                            { identity with
                              with_attempt =
                                (fun prepared ~relation ~f ->
                                  let clone =
                                    Runtime.Context.prepare
                                      context
                                      ~preparation_id:
                                        (Runtime.Prepared.preparation_id prepared)
                                      request
                                    |> runtime_ok
                                  in
                                  let original =
                                    Runtime.Prepared.configuration prepared
                                  in
                                  let copied = Runtime.Prepared.configuration clone in
                                  assert (
                                    Inference.Observation.Configuration.equal
                                      original
                                      copied);
                                  assert (not (phys_equal original copied));
                                  identity.with_attempt clone ~relation ~f)
                            }
                          | Confirm | Before_ack | Before_send | After_send -> identity
                        in
                        let latest_completion = ref None in
                        match
                          Inference_client.run
                            context
                            ~sw
                            ~identity
                            ~relation:Root
                            ~request
                            ~before_dispatch:on_prepared
                            ~on_attempt:(fun attempt ->
                              on_dispatch (Runtime.Attempt.configuration attempt);
                              Graph.on_attempt graph attempt;
                              if equal_mode !mode Before_send then raise Interrupted)
                            ~on_completion:(fun completion ->
                              Graph.on_completion graph completion;
                              latest_completion := Some completion;
                              let sequence =
                                (state actor).counters.transaction_sequence
                              in
                              Graph.on_completion graph completion;
                              assert (
                                Int64.equal
                                  sequence
                                  (state actor).counters.transaction_sequence))
                            ~on_event:ignore
                            ~on_observation:(Graph.on_observation graph)
                        with
                        | Ok _
                          when equal_mode !mode Confirm
                               || equal_mode !mode Stale_preparation ->
                          let original = Option.value_exn !latest_completion in
                          let sequence = (state actor).counters.transaction_sequence in
                          (match Graph.on_completion graph original with
                           | exception Graph.Rejected _ -> ()
                           | () -> failwith "released completion kept its graph route");
                          assert (
                            Int64.equal
                              sequence
                              (state actor).counters.transaction_sequence)
                        | exception Interrupted
                          when equal_mode !mode Before_ack
                               || equal_mode !mode Before_send
                               || equal_mode !mode After_send -> ()
                        | _ -> failwith "unexpected inference fixture outcome"));
               Completed
                 { final_history = input.history
                 ; runtime_requests = []
                 ; moderator_snapshot = None
                 }
             in
             let worker =
               A.Operation_worker.create ~run:(fun ~sw ~input caps ->
                 try run ~sw ~input caps with
                 | exn ->
                   ignore (Eio.Promise.try_resolve publish_failure exn : bool);
                   raise exn)
             in
             Actor.set_operation_worker actor (Some worker) |> protocol_ok;
             Actor.start actor ~attachment_id |> protocol_ok |> ignore;
             let sequence = ref 120 in
             let turn next =
               mode := next;
               incr sequence;
               Actor.submit_message
                 ~submitting_principal:principal_id
                 actor
                 ~attachment_id
                 (history !sequence "private turn body")
               |> protocol_ok
               |> ignore;
               Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
                 Eio.Fiber.first
                   (fun () -> await_idle actor)
                   (fun () -> raise (Eio.Promise.await worker_failure)))
             in
             let enable () =
               Actor.enable_recovery_windows
                 actor
                 ~actor:(actor_proof ())
                 ~attachment_id
                 ~authorize:(fun _ -> Ok ())
               |> protocol_ok
               |> ignore
             in
             f turn enable;
             state actor)
      in
      finish fixture sw initial after !dispatches))
;;

let reopened fixture initial =
  let reopened = journal fixture Store.Journal.open_existing in
  reopened, load fixture reopened initial
;;

let replay_at fixture sw initial transactions ~directory_name ~matches ~expected =
  let directory =
    Store.Session_store.Handle.journal_directory fixture.handle ^ "-" ^ directory_name
  in
  let journal =
    Store.Journal.create
      ~env:fixture.env
      ~directory
      ~max_payload_length:payload_limit
      ~max_segment_bytes:(128 * 1024 * 1024 |> Int64.of_int)
      ~max_segment_frames:4096
    |> store_ok
  in
  let rec write_prefix store transactions =
    match transactions with
    | [] -> failwith "authenticated journal prefix did not reach its attempt stage"
    | transaction :: following ->
      ignore
        (Store.Journal.append
           journal
           ~durability:Flush
           ~flags:0
           ~payload:(Store.Transaction.encode transaction)
         |> store_ok);
      let store =
        Persistence.apply_transaction ~limits:document_limits store transaction
        |> store_ok
      in
      if not (matches (Persistence.Restored.state store))
      then write_prefix store following
  in
  write_prefix initial transactions;
  let journal =
    Store.Journal.open_existing
      ~env:fixture.env
      ~directory
      ~max_payload_length:payload_limit
      ~max_segment_bytes:(128 * 1024 * 1024 |> Int64.of_int)
      ~max_segment_frames:4096
    |> store_ok
  in
  let prefix = load fixture journal initial in
  assert (matches (Persistence.Restored.state prefix.state));
  with_actor
    fixture
    sw
    journal
    ~restored:prefix.state
    ~previous_transaction_hash:prefix.latest_transaction_hash
    ~archive:no_archive
    (fun actor _ ->
       Actor.reconcile_inference_recovery actor |> protocol_ok;
       ignore (expect (state actor) ~tracked:true expected);
       let sequence = (state actor).counters.transaction_sequence in
       Actor.reconcile_inference_recovery actor |> protocol_ok;
       assert (Int64.equal sequence (state actor).counters.transaction_sequence))
;;

let settled origin delivery : Attempt.phase = Settled { origin; delivery }

let%test_unit "actual root delivery and interrupted journal prefixes replay accurately" =
  let confirmed = ref None in
  with_attempts
    ~pressure:0
    (fun turn enable ->
       let disabled = turn Confirm in
       assert (List.is_empty (Effective.effective_records disabled));
       enable ();
       let state = turn Confirm in
       confirmed := Some (expect state ~tracked:true (settled Returned Response_started));
       ignore
         (expect
            (turn Before_ack)
            ~tracked:true
            (settled Interrupted Definitely_not_submitted));
       ignore
         (expect
            (turn Before_send)
            ~tracked:true
            (settled Interrupted Definitely_not_submitted));
       ignore
         (expect (turn After_send) ~tracked:true (settled Interrupted Possibly_submitted)))
    ~finish:(fun fixture sw initial _ dispatches ->
      assert (Int.equal dispatches 3);
      let original = Option.value_exn !confirmed in
      let journal, reopened = reopened fixture initial in
      let recovered = Persistence.Restored.state reopened.state in
      let records = Effective.effective_records recovered in
      assert (Int.equal (List.length records) 4);
      assert (
        Attempt.equal (Record.attempt (List.hd_exn records) |> Option.value_exn) original);
      let transactions = Effective.journal_transactions journal in
      let marker = P.Session_storage_semantics.recovery_effective_attempt in
      assert (
        List.count transactions ~f:(fun transaction ->
          if
            List.mem
              (delta_kinds transaction)
              "recovery_effective_attempt_changed"
              ~equal:String.equal
          then (
            assert (
              List.mem
                (D.Document.required_semantics transaction.delta)
                marker
                ~equal:String.equal);
            true)
          else false)
        = 11);
      let matches phase state =
        List.last (Effective.effective_records state)
        |> Option.bind ~f:Record.attempt
        |> Option.exists ~f:(fun attempt ->
          Int64.equal (Attempt.ordinal attempt) (Attempt.ordinal original)
          && Attempt.equal_phase (Attempt.phase attempt) phase)
      in
      replay_at
        fixture
        sw
        initial
        transactions
        ~directory_name:"allocated"
        ~matches:(matches Allocated)
        ~expected:(settled Interrupted Definitely_not_submitted);
      replay_at
        fixture
        sw
        initial
        transactions
        ~directory_name:"running"
        ~matches:(matches Running)
        ~expected:(settled Interrupted Possibly_submitted))
;;

let%test_unit "confirmed response remains after ordinary accounting retirement and reopen"
  =
  let confirmed = ref None in
  with_attempts
    ~pressure:255
    (fun turn enable ->
       enable ();
       let first =
         expect (turn Confirm) ~tracked:true (settled Returned Response_started)
       in
       confirmed := Some first;
       let next = turn Confirm in
       assert (
         Option.is_none
           (Accounting.find next.inference_ledger ~ordinal:(Attempt.ordinal first))))
    ~finish:(fun fixture _ initial _ dispatches ->
      assert (Int.equal dispatches 2);
      let _, reopened = reopened fixture initial in
      let first =
        Persistence.Restored.state reopened.state
        |> Effective.effective_records
        |> List.hd_exn
      in
      assert (
        Attempt.equal
          (Option.value_exn (Record.attempt first))
          (Option.value_exn !confirmed)))
;;

let%test_unit "authentic receipt persists when detailed accounting cannot allocate a row" =
  with_attempts
    ~pressure:256
    (fun turn enable ->
       enable ();
       let after = turn Confirm in
       let attempt = expect after ~tracked:false (settled Returned Response_started) in
       assert (
         Option.is_none
           (Accounting.find after.inference_ledger ~ordinal:(Attempt.ordinal attempt))))
    ~finish:(fun fixture _ initial _ dispatches ->
      assert (Int.equal dispatches 1);
      let _, reopened = reopened fixture initial in
      ignore
        (expect
           (Persistence.Restored.state reopened.state)
           ~tracked:false
           (settled Returned Response_started)))
;;

let%test_unit "equal replacement configuration cannot bind the original preparation" =
  with_attempts
    ~pressure:0
    (fun turn enable ->
       enable ();
       let after = turn Stale_preparation in
       let prepared = List.hd_exn (Effective.effective_records after) in
       assert (Option.is_none (Record.attempt prepared)))
    ~finish:(fun fixture _ initial _ dispatches ->
      assert (Int.equal dispatches 1);
      let _, reopened = reopened fixture initial in
      let prepared =
        Persistence.Restored.state reopened.state
        |> Effective.effective_records
        |> List.hd_exn
      in
      assert (Option.is_none (Record.attempt prepared)))
;;

let snapshot_payload_bytes (state : State.t) ~hash =
  let payload =
    A.Session_state_document.encode
      (A.Session_state_document.authored state)
      ~limits:document_limits
    |> document_ok
  in
  let open Store in
  let snapshot =
    Snapshot.create
      ~limits:document_limits
      ~session_id
      ~transaction_sequence:state.counters.transaction_sequence
      ~transaction_hash:(Some hash)
      ~event_sequence:state.counters.event_sequence
      ~created_at:state.identity.updated_at
      ~prompt_artifact:(P.Id.Prompt_revision.to_string state.spec.prompt_revision_id)
      ~workspace_identity:state.spec.workspace_instance.conflict_domain
      ~payload
    |> store_ok
  in
  Snapshot.stored snapshot
  |> Snapshot.Stored.record
  |> Document_record.stored_bytes
  |> String.length
;;

type receipt_stage =
  | Prepared
  | Allocated
  | Running
  | Settled
[@@deriving equal]

let receipt_stage state =
  match Effective.effective_records state with
  | [] -> None
  | [ record ] ->
    Some
      (match Record.attempt record with
       | None -> Prepared
       | Some attempt ->
         (match Attempt.phase attempt with
          | Allocated -> Allocated
          | Running -> Running
          | Settled _ -> Settled))
  | _ -> failwith "unexpected recovery preparation count"
;;

let check_normal_accounting (state : State.t) =
  let row = Accounting.rows state.inference_ledger |> List.hd_exn in
  match Inference.Observation.Attempt_record.state (Accounting.Row.record row) with
  | Terminal terminal ->
    assert (
      Inference.Event.Terminal.equal_delivery
        (Inference.Event.Terminal.delivery terminal)
        Response_started)
  | Prepared | Running | Interrupted _ ->
    failwith "normal inference accounting was not committed"
;;

let check_phase_encodings state =
  let actual = receipt state in
  let compact phase = Attempt.with_phase actual phase |> Attempt.to_jsonaf in
  let phases : Attempt.phase list =
    [ Allocated
    ; Running
    ; settled Returned Definitely_not_submitted
    ; settled Returned Possibly_submitted
    ; settled Returned Response_started
    ; settled Interrupted Definitely_not_submitted
    ; settled Interrupted Possibly_submitted
    ; settled Interrupted Response_started
    ]
  in
  let encoded_length phase = compact phase |> Jsonaf.to_string |> String.length in
  List.iter phases ~f:(fun phase ->
    assert (Int.equal (encoded_length phase) (encoded_length Allocated)));
  let fields =
    match compact Running with
    | `Object fields ->
      List.filter fields ~f:(fun (name, _) -> not (String.equal name "phase_code"))
    | _ -> assert false
  in
  let named = `Object (fields @ [ "phase", `Object [ "kind", `String "running" ] ]) in
  let decoded = Attempt.of_jsonaf named |> protocol_ok in
  assert (Attempt.equal_phase (Attempt.phase decoded) Running);
  assert (D.Json.equal named (Attempt.to_jsonaf decoded));
  assert (not (Attempt.same_storage_reservation actual decoded))
;;

let%expect_test "optional recovery capacity uses the prospective physical snapshot" =
  let sizes =
    with_attempts
      ~pressure:0
      (fun turn enable ->
         enable ();
         ignore (turn Confirm))
      ~finish:(fun fixture _ initial _ dispatches ->
        assert (Int.equal dispatches 1);
        let journal, _ = reopened fixture initial in
        let _, sizes =
          List.fold
            (Effective.journal_transactions journal)
            ~init:(initial, [])
            ~f:(fun (previous, sizes) transaction ->
              let next =
                Persistence.apply_transaction ~limits:document_limits previous transaction
                |> store_ok
              in
              let state = Persistence.Restored.state next in
              let sizes =
                match receipt_stage state with
                | None -> sizes
                | Some stage when List.Assoc.mem sizes stage ~equal:equal_receipt_stage ->
                  sizes
                | Some stage ->
                  ( stage
                  , snapshot_payload_bytes
                      state
                      ~hash:(Store.Transaction.hash transaction) )
                  :: sizes
              in
              next, sizes)
        in
        sizes)
  in
  let bytes stage = List.Assoc.find_exn sizes stage ~equal:equal_receipt_stage in
  let prepared = bytes Prepared in
  let allocated = bytes Allocated in
  assert (prepared < allocated && allocated < bytes Settled);
  let turn limit expected =
    with_attempts
      ~pressure:0
      ~snapshot_payload_limit:limit
      (fun turn enable ->
         enable ();
         let after = turn Confirm in
         check_normal_accounting after;
         assert (Option.equal equal_receipt_stage (receipt_stage after) expected))
      ~finish:(fun fixture _ initial _ dispatches ->
        assert (Int.equal dispatches 1);
        let _, reopened = reopened fixture initial in
        let after = Persistence.Restored.state reopened.state in
        check_normal_accounting after;
        if Option.equal equal_receipt_stage expected (Some Settled)
        then check_phase_encodings after;
        assert (Option.equal equal_receipt_stage (receipt_stage after) expected))
  in
  turn (prepared - 1) None;
  turn prepared (Some Prepared);
  turn allocated (Some Settled);
  print_endline "prepared evidence and the fresh binding can be omitted independently";
  print_endline "an admitted receipt settles after the accounting snapshot grows";
  [%expect
    {|
    prepared evidence and the fresh binding can be omitted independently
    an admitted receipt settles after the accounting snapshot grows
    |}]
;;

let%expect_test "once the optional binding enters the writer it cannot fall back" =
  let restore = ref None in
  let on_commit (fixture : fixture) journal state =
    if
      Option.is_none !restore
      && Option.equal equal_receipt_stage (receipt_stage state) (Some Allocated)
    then (
      let segment =
        Filename.concat
          (Store.Session_store.Handle.journal_directory fixture.handle)
          (Store.Journal.current_segment journal |> Store.Journal_segment.Id.filename)
      in
      let root = Eio.Stdenv.fs fixture.env in
      let actual = Eio.Path.(root / segment) in
      let held = Eio.Path.(root / (segment ^ ".fixture-held")) in
      Eio.Path.rename actual held;
      restore
      := Some
           (fun () ->
             if Eio.Path.is_directory actual then Eio.Path.rmdir actual;
             Eio.Path.rename held actual);
      Eio.Path.mkdir ~perm:0o700 actual);
    Ok ()
  in
  with_attempts
    ~pressure:0
    ~on_commit
    (fun turn enable ->
       enable ();
       Exn.protect
         ~finally:(fun () -> Option.iter !restore ~f:(fun restore -> restore ()))
         ~f:(fun () ->
           match turn Confirm with
           | exception Graph.Rejected { code = Persistence_error; message; _ } ->
             assert (String.is_substring message ~substring:"append journal segment")
           | exception exn -> raise exn
           | _ -> failwith "a failed journal append retried without its receipt"))
    ~finish:(fun fixture _ initial _ dispatches ->
      assert (Option.is_some !restore);
      assert (Int.equal dispatches 0);
      let _, reopened = reopened fixture initial in
      let state = Persistence.Restored.state reopened.state in
      assert (Option.equal equal_receipt_stage (receipt_stage state) (Some Prepared));
      assert (Int64.equal (Accounting.last_attempt_ordinal state.inference_ledger) 0L));
  print_endline
    "after actual append failure, replay retained only the admitted preparation";
  [%expect
    {| after actual append failure, replay retained only the admitted preparation |}]
;;
