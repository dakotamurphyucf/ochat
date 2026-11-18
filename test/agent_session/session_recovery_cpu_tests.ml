open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module Cpu = A.Session_recovery_cpu

let limits = document_limits
let set = Document_adoption_tests.set
let map_field = Document_adoption_tests.map_field

let encode document =
  A.Session_state_document.encode document ~limits |> document_ok |> D.Document.to_string
;;

let error_equal left right =
  Sexp.equal
    (Agent_store.Store_error.sexp_of_t left)
    (Agent_store.Store_error.sexp_of_t right)
;;

let assert_documents expected actual =
  assert (
    Result.equal
      (Option.equal (fun left right -> String.equal (encode left) (encode right)))
      error_equal
      expected
      actual)
;;

let restore_direct snapshot =
  A.Session_persistence.restore_snapshot ~limits snapshot
  |> Result.map ~f:(fun restored ->
    Some (A.Session_persistence.Restored.state_document restored))
;;

let apply_direct document transaction =
  match document with
  | None -> Error (Agent_store.Store_error.Corrupt "journal replay has no snapshot state")
  | Some document ->
    A.Session_persistence.apply_document document ~limits transaction
    |> Result.map ~f:Option.some
;;

let equivalent_direct left right =
  let open Result.Let_syntax in
  let encode = function
    | None ->
      Error (Agent_store.Store_error.Corrupt "journal replay has no snapshot state")
    | Some document ->
      A.Session_state_document.encode document ~limits
      |> Result.map ~f:(fun document -> D.Document.json document |> Jsonaf.to_string)
      |> Result.map_error ~f:(fun error -> Agent_store.Store_error.Document error)
  in
  let%bind left = encode left in
  let%map right = encode right in
  String.equal left right
;;

let validate_direct = function
  | None ->
    Error (Agent_store.Store_error.Corrupt "durable session has no valid snapshot")
  | Some document ->
    A.Session_state.validate (A.Session_state_document.value document)
    |> Result.map_error ~f:(fun error ->
      Agent_store.Store_error.Corrupt error.P.Error.message)
;;

let snapshot_with_payload initial payload =
  let template = snapshot_of_state initial |> store_ok in
  Agent_store.Snapshot.with_value
    template
    ~limits
    { (Agent_store.Snapshot.value template) with payload }
  |> store_ok
;;

let carrier initial number =
  state_document initial
  |> fun document ->
  D.Document.inspect
    ~limits
    (D.Document.json document
     |> fun json ->
     set json "future_recovery" (`Object [ "b", `Number number; "a", `Null ])
     |> fun json ->
     map_field json "payload" ~f:(fun payload ->
       map_field payload "identity" ~f:(fun identity ->
         set identity "future_identity" (`String (String.make 128_000 'x')))))
  |> document_ok
;;

let transaction initial ~generation =
  Agent_store.Transaction.create
    ~limits
    ~session_id:initial.A.Session_state.identity.session_id
    ~generation
    ~transaction_sequence:1L
    ~previous_transaction_hash:None
    ~session_revision:1L
    ~first_event_sequence:None
    ~last_event_sequence:None
    ~accepted_at_ns:
      (P.Timestamp.to_time_ns timestamp |> Time_ns.to_int_ns_since_epoch |> Int64.of_int)
    ~command_audit:None
    ~delta:(delta_document (Reset_generation 1))
    ~durable_events:[]
  |> store_ok
;;

let%expect_test "fixed recovery CPU preserves complete originals and direct error order" =
  with_actor_workspace (fun env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let payload = carrier initial "1e+00" in
    let payload_before = D.Document.to_string payload in
    let snapshot = snapshot_with_payload initial payload in
    let original = restore_direct snapshot |> store_ok |> Option.value_exn in
    let original_before = encode original in
    let tx = transaction initial ~generation:1 in
    let tx_before = Agent_store.Transaction.encode tx in
    let cpu = Cpu.create ~domain_mgr:(Eio.Stdenv.domain_mgr env) ~limits in
    Eio.Fiber.both
      (fun () -> assert_documents (restore_direct snapshot) (Cpu.restore cpu snapshot))
      (fun () ->
         assert_documents
           (apply_direct (Some original) tx)
           (Cpu.apply cpu (Some original) tx));
    let expected = apply_direct (Some original) tx |> store_ok in
    assert (Cpu.equivalent cpu expected expected |> store_ok);
    assert (String.is_substring (encode (Option.value_exn expected)) ~substring:"1e+00");
    let alternate =
      A.Session_state_document.decode ~limits (carrier initial "1.00") |> document_ok
    in
    let reordered =
      D.Document.inspect
        ~limits
        (set
           (D.Document.json payload)
           "future_recovery"
           (`Object [ "a", `Null; "b", `Number "1e+00" ]))
      |> document_ok
      |> A.Session_state_document.decode ~limits
      |> document_ok
    in
    let explicit_null =
      D.Document.inspect ~limits (set (D.Document.json payload) "future_absent" `Null)
      |> document_ok
      |> A.Session_state_document.decode ~limits
      |> document_ok
    in
    List.iter [ alternate; reordered; explicit_null ] ~f:(fun different ->
      [%test_eq: bool]
        false
        (Cpu.equivalent cpu (Some original) (Some different) |> store_ok));
    let malformed =
      D.Document.inspect
        ~limits
        (D.Document.json payload
         |> fun json ->
         map_field json "payload" ~f:(fun value ->
           set value "pending_initial_start" (`String "invalid-native-bool")))
      |> document_ok
      |> snapshot_with_payload initial
    in
    assert (Result.is_error (restore_direct malformed));
    assert_documents (restore_direct malformed) (Cpu.restore cpu malformed);
    let wrong_generation = transaction initial ~generation:0 in
    assert (Result.is_error (apply_direct (Some original) wrong_generation));
    assert_documents
      (apply_direct (Some original) wrong_generation)
      (Cpu.apply cpu (Some original) wrong_generation);
    assert_documents (apply_direct None tx) (Cpu.apply cpu None tx);
    let invalid =
      A.Session_state_document.with_value original { initial with schema_version = -1 }
    in
    assert (Result.is_error (validate_direct (Some invalid)));
    assert (Result.is_error (A.Session_state_document.encode invalid ~limits));
    List.iter [ None; Some original; Some invalid ] ~f:(fun document ->
      assert (
        Result.equal
          Unit.equal
          error_equal
          (validate_direct document)
          (Cpu.validate_native cpu document)));
    List.iter
      [ None, Some invalid; Some invalid, None; Some original, Some alternate ]
      ~f:(fun (left, right) ->
        assert (
          Result.equal
            Bool.equal
            error_equal
            (equivalent_direct left right)
            (Cpu.equivalent cpu left right)));
    [%test_eq: string] payload_before (D.Document.to_string snapshot.payload);
    [%test_eq: string] original_before (encode original);
    [%test_eq: string] tx_before (Agent_store.Transaction.encode tx));
  print_endline
    "complete carrier/number spelling and originals retained; direct failures ordered";
  [%expect
    {| complete carrier/number spelling and originals retained; direct failures ordered |}]
;;

module Observed_domain_manager = struct
  type hold =
    { entered : unit Eio.Promise.u
    ; release : unit Eio.Promise.t
    }

  type t =
    { manager : Eio.Domain_manager.ty Eio.Resource.t
    ; hold : hold option Atomic.t
    ; injected : exn option Atomic.t
    ; worker_trace : string option Atomic.t
    ; started : int Atomic.t
    ; finished : int Atomic.t
    ; active : int Atomic.t
    ; peak : int Atomic.t
    }

  let rec observe_peak t count =
    let previous = Atomic.get t.peak in
    if count > previous && not (Atomic.compare_and_set t.peak previous count)
    then observe_peak t count
  ;;

  let raise_in_worker marker = raise marker

  let run t operation =
    let (Eio.Resource.T (data, ops)) = t.manager in
    let module Native = (val Eio.Resource.get ops Eio.Domain_manager.Pi.Mgr) in
    Native.run data (fun ~cancelled ->
      ignore (Atomic.fetch_and_add t.started 1 : int);
      observe_peak t (Int.succ (Atomic.fetch_and_add t.active 1));
      Exn.protect
        ~finally:(fun () ->
          ignore (Atomic.fetch_and_add t.active (-1) : int);
          ignore (Atomic.fetch_and_add t.finished 1 : int))
        ~f:(fun () ->
          (* Execute the real fixed operation on the actual native worker before
             observing transport terminality. No alternate codec is supplied. *)
          let result = operation ~cancelled in
          (match Atomic.exchange t.hold None with
           | None -> ()
           | Some hold ->
             Eio.Promise.resolve hold.entered ();
             Eio.Cancel.protect (fun () -> Eio.Promise.await hold.release);
             (match Eio.Promise.peek cancelled with
              | None -> ()
              | Some cancellation -> raise cancellation));
          match Atomic.exchange t.injected None with
          | None -> result
          | Some marker ->
            (* This dedicated domain must keep tracing enabled until the native
               provider captures the raised trace and joins the terminated worker. *)
            Stdlib.Printexc.record_backtrace true;
            (try raise_in_worker marker with
             | exn ->
               let trace = Stdlib.Printexc.get_raw_backtrace () in
               Atomic.set
                 t.worker_trace
                 (Some (Stdlib.Printexc.raw_backtrace_to_string trace));
               Stdlib.Printexc.raise_with_backtrace exn trace)))
  ;;

  let run_raw t operation = Eio.Domain_manager.run_raw t.manager operation

  let create manager =
    let observed =
      { manager
      ; hold = Atomic.make None
      ; injected = Atomic.make None
      ; worker_trace = Atomic.make None
      ; started = Atomic.make 0
      ; finished = Atomic.make 0
      ; active = Atomic.make 0
      ; peak = Atomic.make 0
      }
    in
    ( observed
    , Eio.Resource.T
        ( observed
        , Eio.Domain_manager.Pi.mgr
            (module struct
              type nonrec t = t

              let run = run
              let run_raw = run_raw
            end) ) )
  ;;
end

let%expect_test "shared recovery CPU permit survives queued and admitted cancellation" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
      Eio.Switch.run (fun sw ->
        let initial =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let snapshot = snapshot_with_payload initial (carrier initial "1e+00") in
        let observed, manager =
          Observed_domain_manager.create (Eio.Stdenv.domain_mgr env)
        in
        let cpu = Cpu.create ~domain_mgr:manager ~limits in
        let entered, entered_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        Atomic.set observed.hold (Some { entered = entered_u; release });
        Exn.protect
          ~finally:(fun () ->
            if Option.is_none (Eio.Promise.peek release)
            then Eio.Promise.resolve release_u ())
          ~f:(fun () ->
            let first_context, first_context_u = Eio.Promise.create () in
            let first_done, first_done_u = Eio.Promise.create () in
            let fork context_u done_u =
              Eio.Fiber.fork ~sw (fun () ->
                Eio.Cancel.sub (fun cancellation ->
                  Eio.Promise.resolve context_u cancellation;
                  let cancelled =
                    try
                      ignore (Cpu.restore cpu snapshot : (_, _) Result.t);
                      false
                    with
                    | Eio.Cancel.Cancelled _ -> true
                  in
                  Eio.Promise.resolve done_u cancelled))
            in
            fork first_context_u first_done_u;
            Eio.Promise.await entered;
            let second_context, second_context_u = Eio.Promise.create () in
            let second_done, second_done_u = Eio.Promise.create () in
            fork second_context_u second_done_u;
            let second = Eio.Promise.await second_context in
            Eio.Fiber.yield ();
            [%test_eq: int] 1 (Atomic.get observed.started);
            Eio.Cancel.cancel second (Failure "cancel queued recovery CPU");
            assert (Eio.Promise.await second_done);
            [%test_eq: int] 1 (Atomic.get observed.started);
            Eio.Cancel.cancel
              (Eio.Promise.await first_context)
              (Failure "cancel admitted recovery CPU");
            Eio.Fiber.yield ();
            assert (Option.is_none (Eio.Promise.peek first_done));
            [%test_eq: int] 0 (Atomic.get observed.finished);
            (* The parent domain remains runnable while its real large-document CPU
           worker is held; cancellation does not release that worker's permit. *)
            let heartbeat = ref false in
            Eio.Fiber.fork ~sw (fun () -> heartbeat := true);
            Eio.Fiber.yield ();
            assert !heartbeat;
            Eio.Promise.resolve release_u ();
            assert (Eio.Promise.await first_done);
            [%test_eq: int] 0 (Atomic.get observed.active);
            [%test_eq: int] 1 (Atomic.get observed.finished);
            assert_documents (restore_direct snapshot) (Cpu.restore cpu snapshot);
            [%test_eq: int] 2 (Atomic.get observed.started);
            [%test_eq: int] 2 (Atomic.get observed.finished);
            [%test_eq: int] 1 (Atomic.get observed.peak)))));
  print_endline
    "one CPU worker; queued cancellation admits none; admitted cancellation joins before \
     reuse";
  [%expect
    {| one CPU worker; queued cancellation admits none; admitted cancellation joins before reuse |}]
;;

let%expect_test
    "recovery CPU worker exceptions retain identity trace and permit terminality"
  =
  with_actor_workspace (fun env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let snapshot = snapshot_of_state initial |> store_ok in
    let observed, manager = Observed_domain_manager.create (Eio.Stdenv.domain_mgr env) in
    let cpu = Cpu.create ~domain_mgr:manager ~limits in
    let marker = Failure "actual recovery CPU worker transport" in
    Atomic.set observed.injected (Some marker);
    let previous = Stdlib.Printexc.backtrace_status () in
    Exn.protect
      ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous)
      ~f:(fun () ->
        Stdlib.Printexc.record_backtrace true;
        let actual, trace =
          try
            ignore (Cpu.restore cpu snapshot : (_, _) Result.t);
            failwith "injected worker exception was lost"
          with
          | exn ->
            ( exn
            , Stdlib.Printexc.raw_backtrace_to_string
                (Stdlib.Printexc.get_raw_backtrace ()) )
        in
        assert (phys_equal marker actual);
        let original = Option.value_exn (Atomic.get observed.worker_trace) in
        assert (not (String.is_empty original));
        assert (String.is_prefix trace ~prefix:original));
    [%test_eq: int] 0 (Atomic.get observed.active);
    [%test_eq: int] 1 (Atomic.get observed.finished);
    assert_documents (restore_direct snapshot) (Cpu.restore cpu snapshot);
    [%test_eq: int] 2 (Atomic.get observed.finished));
  print_endline "original worker exception and trace retained after join; permit reusable";
  [%expect {| original worker exception and trace retained after join; permit reusable |}]
;;
