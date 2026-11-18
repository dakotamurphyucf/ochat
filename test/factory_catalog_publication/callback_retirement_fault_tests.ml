open! Core
open Agent_server_test_support
open Retry_support
module Legacy = Legacy_idle_completion_tests
module Existing_fault = Legacy_idle_completion_fault_tests
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Receipt = P.Moderator_execution
module Store = Agent_store

(* Compose the real Manager CPS and actual Actor claim. These scripts use no
   Tool/Model/Schedule bridge; Process.run uses their compiled shell capability. *)
module Callback = struct
  let failure message = P.Error.create Invalid_state ~message ~retryable:false ()

  let run_one (fixture : Legacy.Fixture.t) =
    let manager = Option.value_exn fixture.runtime.moderator_manager in
    let host_rejection = ref None in
    let reject error =
      if Option.is_none !host_rejection then host_rejection := Some error;
      error.P.Error.message
    in
    Manager.handle_legacy_callback_entries
      manager
      ~selection:Queue_head
      ~session_id:
        (P.Id.Session.to_string
           (Legacy.Physical.state fixture.physical).identity.session_id)
      ~now_ms:0
      ~history:[]
      ~available_tools:[]
      ~session_meta:(`Object [])
      ~with_admission:(fun callback ~snapshot:_ run ->
        let outcome = ref None in
        let result =
          Actor.with_legacy_queued_callback
            fixture.physical.actor
            ~operation_id:None
            ~callback
            (fun ~scope:_ ~commit ->
               let binding : Manager.callback_binding =
                 { services =
                     { model_recipes = String.Map.empty
                     ; tool_call =
                         (fun ~name:_ ~args:_ -> Error "599 source declares no Tool")
                     ; after_ms =
                         (fun ~delay_ms:_ ~payload:_ ->
                           Error "599 source declares no Schedule")
                     ; cancel = (fun ~id:_ -> Error "599 source declares no Schedule")
                     }
                 ; authorize =
                     (fun () ->
                       if Manager.callback_is_current manager callback
                       then Ok ()
                       else Error "599 actual Manager callback expired")
                 ; prepare =
                     (fun ~(outcome : Moderation.Outcome.t) ~snapshot ->
                       let requests : P.Invocation.follow_up =
                         { request_turn =
                             Chat_response.Runtime_semantics.request_turn
                               outcome.runtime_requests
                         ; request_compaction =
                             Chat_response.Runtime_semantics.request_compaction
                               outcome.runtime_requests
                         ; end_session =
                             Chat_response.Runtime_semantics.should_end_session
                               outcome.runtime_requests
                         }
                       in
                       Ok
                         { Manager.persist =
                             (fun () ->
                               commit
                                 ~snapshot
                                 ~requests
                                 ~notifications:outcome.ui_notifications
                               |> Result.map_error ~f:reject)
                         ; install = ignore
                         })
                 }
               in
               run binding
               |> Result.map_error ~f:failure
               |> Result.map ~f:(fun actual -> outcome := Some actual))
          |> Result.map_error ~f:reject
        in
        match result, !outcome with
        | Ok true, Some actual -> Ok actual
        | Ok false, _ -> Error "599 actual callback was not admitted"
        | Ok true, None -> Error "599 actual callback returned without its outcome"
        | Error message, _ -> Error message)
    |> Result.map_error ~f:(fun message ->
      Option.value !host_rejection ~default:(failure message))
  ;;
end

let same_state before after =
  assert (
    Jsonaf.exactly_equal
      (Existing_fault.state_json before)
      (Existing_fault.state_json after))
;;

(* The real first legacy claim installs a source-only index. No Run or work is
   invented by that factual source carrier. *)
let assert_no_work (state : A.Session_state.t) =
  let marks =
    A.Canonical_usage_fold.Marks.of_ledger state.inference_ledger
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (A.Canonical_usage_fold.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  [%test_eq: int64 * int64] (0L, 0L) (marks.turn, marks.attempt);
  assert (List.is_empty (A.Inference_ledger.rows state.inference_ledger));
  assert (Option.is_none state.active_operation && Option.is_none state.failure);
  assert (Option.is_none state.goal_catalog && Option.is_none state.goal_descendants);
  assert (List.is_empty state.jobs && List.is_empty state.schedules);
  Option.iter state.run_state ~f:(fun index ->
    assert (List.is_empty (A.Run_state.runs index));
    assert (List.is_empty (A.Run_state.intents index));
    assert (List.is_empty (A.Run_state.receipts index));
    assert (List.is_empty (A.Run_state.goal_links index));
    assert (List.is_empty (A.Run_state.job_deliveries index)))
;;

let internal_receipt (state : A.Session_state.t) =
  List.filter state.moderator_executions ~f:(fun receipt ->
    Receipt.equal_phase receipt.Receipt.context.phase Internal_event)
  |> function
  | [ receipt ] -> receipt
  | [] | _ :: _ :: _ -> failwith "expected one actual queued callback receipt"
;;

let terminal_receipt (transaction : Store.Transaction.t) =
  Existing_fault.changes transaction
  |> List.filter_map ~f:(function
    | A.Session_delta.Moderator_execution_changed receipt -> Some receipt
    | _ -> None)
  |> function
  | [ receipt ] -> Some receipt
  | [] -> None
  | _ :: _ :: _ -> failwith "terminal callback wrote multiple receipts"
;;

let manager_snapshot fixture =
  Option.value_exn fixture.Legacy.Fixture.runtime.moderator_manager
  |> Manager.identity_snapshot
  |> Result.ok_or_failwith
  |> A.Runtime_builder.encode_moderator_snapshot
;;

let capture_diagnostics env f =
  let lines = ref [] in
  let logger : Eio.Debug.traceln =
    { traceln =
        (fun ?__POS__:_ format ->
          Format.kasprintf (fun line -> lines := line :: !lines) format)
    }
  in
  let result = Eio.Fiber.with_binding (Eio.Stdenv.debug env)#traceln logger f in
  result, List.rev !lines
;;

let assert_diagnostic lines message =
  [%test_eq: string list] [ "queued moderator cleanup failed: " ^ message ] lines
;;

module Fault = struct
  type selected =
    { session : P.Id.Session.t
    ; journal : string
    }

  type t =
    { gate : Legacy.Process_gate.t
    ; mutable selected : selected option
    ; mutable candidate : Store.Transaction.t option
    ; mutable journal_path : string option
    ; mutable injected : exn option
    ; mutable hits : int
    }

  let create () =
    { gate = Legacy.Process_gate.create ()
    ; selected = None
    ; candidate = None
    ; journal_path = None
    ; injected = None
    ; hits = 0
    }
  ;;

  let select (actual : Store.Transaction.t) =
    match terminal_receipt actual with
    | None -> false
    | Some receipt ->
      Receipt.equal_phase receipt.context.phase Internal_event
      &&
        (match receipt.status with
        | Failed error -> String.equal error.code "event.handler_failed"
        | Running | Completed _ | Interrupted _ -> false)
  ;;

  let wrap t env =
    Crash_fault_io.wrap
      (Legacy.Process_gate.wrap t.gate env)
      ~boundary:After_sync
      ~matches:(fun path ->
        Option.exists t.selected ~f:(fun selected ->
          String.equal selected.journal (Filename.dirname path)
          && String.is_suffix path ~suffix:".log"))
      ~reached:(fun path ->
        let selected = Option.value_exn t.selected in
        let scan =
          Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
          |> Store.Journal_segment.scan_contents ~max_payload_length:1048576
          |> Existing.store_ok
        in
        match List.last scan.entries with
        | Some entry when Int.equal (Store.Frame.flags entry.frame) 0 ->
          let actual = Existing_fault.transaction entry in
          if P.Id.Session.equal selected.session actual.session_id && select actual
          then (
            assert (Option.is_none t.candidate);
            t.candidate <- Some actual;
            t.journal_path <- Some path;
            t.hits <- t.hits + 1;
            let injected =
              Core_unix.Unix_error (EIO, "599 callback terminal after sync", path)
            in
            t.injected <- Some injected;
            raise injected)
        | None | Some _ -> ())
  ;;

  let arm t (fixture : Legacy.Fixture.t) =
    let handle = fixture.physical.handle in
    t.selected
    <- Some
         { session = Store.Session_store.Handle.session_id handle
         ; journal = Store.Session_store.Handle.journal_directory handle
         };
    t.gate.cache
    <- Some
         (Filename.concat (Store.Session_store.Handle.cache_directory handle) "cache.bin")
  ;;
end

let assert_checkpoint_unchanged fixture original =
  assert (Jsonaf.exactly_equal original (manager_snapshot fixture));
  ignore
    (Existing_fault.assert_checkpoint (Some original) ~count:0 ~queued:1
     : Session.Moderator_state.Identity_snapshot.t)
;;

let actual_frames fixture = Legacy.Physical.frames fixture.Legacy.Fixture.physical

let write_report fixture fault ~snapshot_digest ~before ~terminal ~primary ~cleanup =
  let selected = Option.value_exn fault.Fault.candidate in
  let report =
    `Object
      [ "session_id", P.Id.Session.to_json selected.session_id
      ; "initial_snapshot_digest", `String snapshot_digest
      ; "before", Existing_fault.state_json before
      ; "terminal", Receipt.to_json terminal
      ; "transaction_payload", `String (Store.Transaction.encode selected)
      ; "transaction_sha256", `String (Store.Transaction.hash selected)
      ; "journal_path", `String (Option.value_exn fault.journal_path)
      ; "injected", `String (Exn.to_string (Option.value_exn fault.injected))
      ; "primary", primary
      ; ( "cleanup"
        , match cleanup with
          | None -> `Null
          | Some error -> P.Error.to_json error )
      ]
  in
  Eio.Flow.copy_string
    (Jsonaf.to_string report ^ "\n")
    (Eio.Stdenv.stdout fixture.Legacy.Fixture.env);
  (* Both callers have returned. Unknown persistence means
     generic Actor/Graph/Store finalizers are NOT claimed joined. *)
  Core_unix.exit_immediately 73
;;

let run_uncertain_child root =
  let fault = Fault.create () in
  let source =
    String.substr_replace_all
      Legacy.source
      ~pattern:"| `Tick -> Task.pure(state + 1)"
      ~with_:"| `Tick -> Task.fail(\"599 actual callback failure\")"
  in
  assert (not (String.equal source Legacy.source));
  Legacy.Fixture.with_runtime
    ~test_root:root
    ~source
    ~wrap_env:(Fault.wrap fault)
    (fun fixture ->
       let snapshot_digest =
         Existing_fault.initial_snapshot fixture.env fixture.physical.handle
       in
       Legacy.Fixture.enqueue fixture (Variant ("Tick", []));
       let original = manager_snapshot fixture in
       Fault.arm fault fixture;
       let result, diagnostics =
         capture_diagnostics fixture.env (fun () -> Callback.run_one fixture)
       in
       let primary =
         match result with
         | Error failure -> failure
         | Ok _ -> failwith "actual failing compiled callback unexpectedly succeeded"
       in
       [%test_eq: P.Error.code] Invalid_state primary.code;
       assert (not primary.retryable);
       assert (
         String.is_substring primary.message ~substring:"599 actual callback failure");
       let before = Legacy.Physical.state fixture.physical in
       assert_no_work before;
       let running = internal_receipt before in
       assert (Receipt.equal_status Running running.status);
       let selected = Option.value_exn fault.candidate in
       let terminal = Option.value_exn (terminal_receipt selected) in
       assert (Receipt.equal_context running.context terminal.context);
       (match terminal.status with
        | Failed error -> [%test_eq: string] "event.handler_failed" error.code
        | Running | Completed _ | Interrupted _ ->
          failwith "wrong actual terminal candidate");
       [%test_eq: int64]
         (Int64.succ before.counters.transaction_sequence)
         selected.transaction_sequence;
       [%test_eq: int64] (Int64.succ before.counters.revision) selected.session_revision;
       [%test_eq: string option]
         (A.Session_persistence.transaction_hash fixture.physical.durable)
         selected.previous_transaction_hash;
       assert (not (Store.Commit_writer.is_available fixture.physical.writer));
       let injected = Option.value_exn fault.injected in
       let path = Option.value_exn fault.journal_path in
       let expected =
         Store.Store_error.of_exn ~operation:"append journal segment" ~path injected
         |> Existing_fault.persistence_error
       in
       let drain () =
         match Actor.drain_callback_retirement fixture.physical.actor with
         | Error failure ->
           Existing_fault.same_error expected failure;
           failure
         | Ok () -> failwith "entered writer was incorrectly retired without recovery"
       in
       let cleanup = drain () in
       assert_diagnostic diagnostics cleanup.message;
       let frames = actual_frames fixture in
       let failed_close () =
         try
           Legacy.within fixture.env "actual uncertain callback strong close" (fun () ->
             Owner.close_and_wait fixture.owner);
           failwith "unknown callback owner was reported joined"
         with
         | Owner.Cleanup_failed failure ->
           assert (phys_equal cleanup failure);
           Existing_fault.same_error cleanup failure
       in
       failed_close ();
       ignore (drain () : P.Error.t);
       failed_close ();
       same_state before (Legacy.Physical.state fixture.physical);
       [%test_eq: string list] frames (actual_frames fixture);
       assert_checkpoint_unchanged fixture original;
       [%test_eq: int] 1 fault.hits;
       [%test_eq: int] 0 fault.gate.cache_renames;
       [%test_eq: int] 0 !(fixture.providers);
       assert (Owner.is_loaded fixture.owner);
       write_report
         fixture
         fault
         ~snapshot_digest
         ~before
         ~terminal
         ~primary:(P.Error.to_json primary)
         ~cleanup:(Some cleanup))
;;

let run_child root = run_uncertain_child root

let completed_child_output ?(mode = "callback-terminal-writer-unknown") env root =
  Eio.Switch.run (fun sw ->
    let manager = Eio.Stdenv.process_mgr env in
    let output, sink = Eio.Process.pipe ~sw manager in
    let diagnostics, errors = Eio.Process.pipe ~sw manager in
    let child =
      Eio.Process.spawn
        ~sw
        manager
        ~env:[| "PATH=/usr/bin:/bin" |]
        ~stdout:sink
        ~stderr:errors
        [ "./goal_native_fault_child.exe"; root; mode ]
    in
    Eio.Flow.close sink;
    Eio.Flow.close errors;
    let report, diagnostics =
      Eio.Fiber.pair
        (fun () -> Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:1048576 output))
        (fun () ->
           Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:32768 diagnostics))
    in
    (* Status-first preserves assertion/backtrace evidence from a child that
       fails before publishing its completed report. *)
    (match Eio.Process.await child with
     | `Exited 73 -> ()
     | `Exited code ->
       raise_s
         [%sexp
           "callback fault child failed"
         , (code : int)
         , (report : string)
         , (diagnostics : string)]
     | `Signaled signal ->
       raise_s
         [%sexp
           "callback fault child signaled"
         , (signal : int)
         , (report : string)
         , (diagnostics : string)]);
    report, diagnostics)
;;

let child_report ?mode env root =
  let report, diagnostics = completed_child_output ?mode env root in
  [%test_eq: string] "" (String.strip diagnostics);
  Jsonaf.of_string report
;;

let decode_state raw =
  D.Document.inspect ~limits:D.Limits.default raw
  |> document_ok
  |> A.Session_state_document.decode ~limits:D.Limits.default
  |> document_ok
  |> A.Session_state_document.value
;;

module Terminal_kind = struct
  type t =
    | Failed_callback
    | Cancelled_callback
end

let recover ?(kind = Terminal_kind.Failed_callback) env sw root report =
  let env = (env :> Eio_unix.Stdenv.base) in
  let member name = Jsonaf.member_exn name report in
  let selected =
    member "transaction_payload"
    |> Jsonaf.string_exn
    |> Store.Transaction.decode
    |> Existing.store_ok
  in
  let terminal = member "terminal" |> Receipt.of_json |> protocol_ok in
  let before = decode_state (member "before") in
  assert_no_work before;
  let running = internal_receipt before in
  assert (Receipt.equal_status Running running.status);
  assert (Receipt.equal_context running.context terminal.context);
  [%test_eq: P.Id.Session.t] selected.session_id before.identity.session_id;
  [%test_eq: string]
    (member "transaction_sha256" |> Jsonaf.string_exn)
    (Store.Transaction.hash selected);
  [%test_eq: int64]
    (Int64.succ before.counters.transaction_sequence)
    selected.transaction_sequence;
  [%test_eq: int64] (Int64.succ before.counters.revision) selected.session_revision;
  (match Existing_fault.changes selected with
   | [ Moderator_execution_changed actual ] -> assert (Receipt.equal terminal actual)
   | _ -> failwith "terminal callback transaction changed unrelated work");
  (match kind, terminal.status with
   | Failed_callback, Failed failure ->
     [%test_eq: string] "event.handler_failed" failure.code;
     let primary = member "primary" |> P.Error.of_json |> protocol_ok in
     [%test_eq: P.Error.code] Invalid_state primary.code;
     assert (String.is_substring primary.message ~substring:"599 actual callback failure");
     let cleanup = member "cleanup" |> P.Error.of_json |> protocol_ok in
     let expected_error =
       Store.Store_error.Io
         { operation = "append journal segment"
         ; path = member "journal_path" |> Jsonaf.string_exn
         ; message = member "injected" |> Jsonaf.string_exn
         }
       |> Existing_fault.persistence_error
     in
     Existing_fault.same_error expected_error cleanup
   | Cancelled_callback, Interrupted reason ->
     [%test_eq: string]
       "queued moderator handler interrupted before checkpoint commit"
       reason;
     let metadata = member "metadata_path" |> Jsonaf.string_exn in
     let injected =
       Core_unix.Unix_error (EIO, "599 callback publication after rename", metadata)
     in
     [%test_eq: string] (Exn.to_string injected) (member "injected" |> Jsonaf.string_exn);
     let publication =
       Store.Store_error.of_exn ~operation:"replace" ~path:metadata injected
     in
     let expected_exception =
       match Result.try_with (fun () -> Existing.store_ok (Error publication)) with
       | Error exn -> exn
       | Ok _ -> failwith "physical fixture did not raise its Store publication error"
     in
     [%test_eq: string]
       (Exn.to_string expected_exception)
       (member "fatal" |> Jsonaf.string_exn);
     [%test_eq: string]
       (Exn.to_string
          (Eio.Cancel.Cancelled (Failure "599 exact actual callback caller cancelled")))
       (member "caller_cancelled" |> Jsonaf.string_exn)
   | Failed_callback, (Running | Completed _ | Interrupted _)
   | Cancelled_callback, (Running | Completed _ | Failed _) ->
     failwith "recovered callback terminal status changed");
  let expected =
    { before with
      identity =
        { before.identity with
          updated_at =
            Time_ns.of_int_ns_since_epoch (Int64.to_int_exn selected.accepted_at_ns)
            |> P.Timestamp.of_time_ns
        }
    ; moderator_executions =
        List.map before.moderator_executions ~f:(fun previous ->
          if P.Id.Moderator_execution.equal previous.context.id terminal.context.id
          then terminal
          else previous)
    ; counters =
        { before.counters with
          transaction_sequence = selected.transaction_sequence
        ; revision = selected.session_revision
        ; event_sequence =
            Option.value
              selected.last_event_sequence
              ~default:before.counters.event_sequence
        }
    }
  in
  A.Session_state.validate expected |> protocol_ok;
  (match kind with
   | Failed_callback -> ()
   | Cancelled_callback -> same_state expected (decode_state (member "persisted")));
  let store =
    Store.Session_store.open_existing
      ~env
      ~sw
      ~root:(Filename.concat root "data")
      ~process_start_identity:None
      ~lock_nonce:"599-parent-physical-recovery"
    |> Existing.store_ok
  in
  protect_cleanup
    ~finally:(fun () -> Store.Session_store.close store |> Existing.store_ok)
    (fun () ->
       let handle =
         Store.Session_store.open_session
           store
           ~sw
           ~actor_lock_nonce:"599-recovered-session"
           selected.session_id
         |> Existing.store_ok
       in
       protect_cleanup
         ~finally:(fun () ->
           Store.Session_store.close_session store handle |> Existing.store_ok)
         (fun () ->
            let snapshot_digest = member "initial_snapshot_digest" |> Jsonaf.string_exn in
            [%test_eq: string]
              snapshot_digest
              (Existing_fault.initial_snapshot env handle);
            let snapshot_directory =
              Store.Session_store.Handle.snapshot_directory handle
            in
            let snapshot =
              Store.Snapshot.load_current
                ~env
                ~directory:snapshot_directory
                ~max_payload_length:1048576
              |> Existing.store_ok
              |> Option.value_exn
            in
            let initial =
              A.Session_persistence.restore_snapshot
                ~limits:D.Limits.default
                snapshot.snapshot
              |> Existing.store_ok
            in
            let journal =
              Store.Journal.open_existing
                ~env
                ~directory:(Store.Session_store.Handle.journal_directory handle)
                ~max_payload_length:1048576
                ~max_segment_bytes:4194304L
                ~max_segment_frames:32
              |> Existing.store_ok
            in
            let frames () =
              let scan : Store.Journal.scan =
                Store.Journal.scan journal |> Existing.store_ok
              in
              List.map scan.entries ~f:(fun (entry : Store.Journal.entry) ->
                Store.Frame.flags entry.frame, Store.Frame.payload entry.frame)
            in
            let original_frames = frames () in
            let transactions =
              List.filter_map original_frames ~f:(fun (flags, payload) ->
                if Int.equal flags 0
                then Some (Store.Transaction.decode payload |> Existing.store_ok)
                else None)
            in
            let selected_frames =
              List.filter transactions ~f:(fun (actual : Store.Transaction.t) ->
                Int64.equal actual.transaction_sequence selected.transaction_sequence)
            in
            (match selected_frames with
             | [ actual ] ->
               [%test_eq: string]
                 (Store.Transaction.encode selected)
                 (Store.Transaction.encode actual);
               [%test_eq: string]
                 (Store.Transaction.hash selected)
                 (Store.Transaction.hash actual)
             | [] | _ :: _ :: _ ->
               failwith "exact terminal transaction missing or duplicated after crash");
            let previous =
              List.find_exn transactions ~f:(fun (actual : Store.Transaction.t) ->
                Int64.equal
                  actual.transaction_sequence
                  before.counters.transaction_sequence)
            in
            [%test_eq: string option]
              (Some (Store.Transaction.hash previous))
              selected.previous_transaction_hash;
            let read () =
              Store.Recovery.read
                ~env
                ~journal
                ~snapshot_directory
                ~max_snapshot_payload_length:1048576
                ~session_id:selected.session_id
                ~initial
                ~restore_snapshot:
                  (A.Session_persistence.restore_snapshot ~limits:D.Limits.default)
                ~apply:(A.Session_persistence.apply_transaction ~limits:D.Limits.default)
                ~validate_transaction:
                  (A.Session_persistence.validate_transaction ~limits:D.Limits.default)
                ~validate:A.Session_persistence.validate
              |> Existing.store_ok
            in
            let recovered = read () in
            assert (not recovered.repaired_crash_tail);
            [%test_eq: int64]
              selected.transaction_sequence
              recovered.latest_transaction_sequence;
            [%test_eq: string option]
              (Some (Store.Transaction.hash selected))
              recovered.latest_transaction_hash;
            [%test_eq: int64] selected.session_revision recovered.latest_session_revision;
            let current = A.Session_persistence.Restored.state recovered.state in
            same_state expected current;
            assert_no_work current;
            ignore
              (Existing_fault.assert_checkpoint current.moderator ~count:0 ~queued:1
               : Session.Moderator_state.Identity_snapshot.t);
            same_state current (A.Session_persistence.Restored.state (read ()).state);
            [%test_eq: (int * string) list] original_frames (frames ());
            [%test_eq: string]
              snapshot_digest
              (Existing_fault.initial_snapshot env handle)))
;;

let check () =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
             let report = child_report env root in
             recover env sw root report))))
;;

let%expect_test
    "completed uncertain callback terminal writer retains exact owner until physical \
     recovery"
  =
  check ();
  print_endline
    "original handler failure retained; completed uncertain terminal, failed close and \
     exact physical replay without callback retry";
  [%expect
    {| original handler failure retained; completed uncertain terminal, failed close and exact physical replay without callback retry |}]
;;

module Publication_fault = struct
  type t =
    { gate : Legacy.Process_gate.t
    ; mutable physical : Legacy.Physical.t option
    ; mutable candidate : Store.Transaction.t option
    ; mutable journal_path : string option
    ; mutable injected : exn option
    ; mutable persisted : A.Session_state.t option
    ; mutable persisted_hash : string option
    ; mutable writer_hits : int
    ; mutable publication_hits : int
    }

  let create () =
    { gate = Legacy.Process_gate.create ()
    ; physical = None
    ; candidate = None
    ; journal_path = None
    ; injected = None
    ; persisted = None
    ; persisted_hash = None
    ; writer_hits = 0
    ; publication_hits = 0
    }
  ;;

  let metadata physical =
    Filename.concat
      (Store.Session_store.Handle.directory physical.Legacy.Physical.handle)
      "metadata.sexp"
  ;;

  let wrap t env =
    let wrapped = Legacy.Process_gate.wrap t.gate env in
    let wrapped =
      Crash_fault_io.wrap
        wrapped
        ~boundary:After_sync
        ~matches:(fun path ->
          Option.exists t.physical ~f:(fun physical ->
            String.equal
              (Store.Session_store.Handle.journal_directory physical.handle)
              (Filename.dirname path)
            && String.is_suffix path ~suffix:".log"))
        ~reached:(fun path ->
          let physical = Option.value_exn t.physical in
          let scan =
            Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
            |> Store.Journal_segment.scan_contents ~max_payload_length:1048576
            |> Existing.store_ok
          in
          match List.last scan.entries with
          | Some entry when Int.equal (Store.Frame.flags entry.frame) 0 ->
            let actual = Existing_fault.transaction entry in
            if
              P.Id.Session.equal
                (Store.Session_store.Handle.session_id physical.handle)
                actual.session_id
            then (
              match terminal_receipt actual with
              | Some receipt when Receipt.equal_phase receipt.context.phase Internal_event
                ->
                (match receipt.status with
                 | Interrupted _ ->
                   assert (Option.is_none t.candidate);
                   t.candidate <- Some actual;
                   t.journal_path <- Some path;
                   t.writer_hits <- t.writer_hits + 1
                 | Running | Completed _ | Failed _ -> ())
              | None | Some _ -> ())
          | None | Some _ -> ())
    in
    Crash_fault_io.wrap
      wrapped
      ~boundary:After_rename
      ~matches:(fun path ->
        Option.exists t.physical ~f:(fun physical ->
          Option.is_some t.candidate
          && String.equal (Filename.dirname path) (Filename.dirname (metadata physical))
          && String.is_prefix (Filename.basename path) ~prefix:"metadata.sexp.tmp-"))
      ~reached:(fun path ->
        let physical = Option.value_exn t.physical in
        [%test_eq: string] (metadata physical) path;
        t.publication_hits <- t.publication_hits + 1;
        [%test_eq: int] 1 t.publication_hits;
        (* Pure observations of the actual durable owner after its real writer
           ACK. No Actor RPC or authority lookup occurs inside this IO boundary. *)
        t.persisted
        <- Some
             (A.Session_persistence.Restored.state
                (A.Session_persistence.restored physical.durable));
        t.persisted_hash <- A.Session_persistence.transaction_hash physical.durable;
        let injected =
          Core_unix.Unix_error (EIO, "599 callback publication after rename", path)
        in
        t.injected <- Some injected;
        raise injected)
  ;;

  let arm t (fixture : Legacy.Fixture.t) executable =
    t.physical <- Some fixture.physical;
    t.gate.executable <- Some executable;
    t.gate.cache
    <- Some
         (Filename.concat
            (Store.Session_store.Handle.cache_directory fixture.physical.handle)
            "cache.bin")
  ;;
end

let rec fatal_causes = function
  | Eio.Exn.Multiple failures ->
    List.concat_map failures ~f:(fun (exn, _) -> fatal_causes exn)
  | exn -> [ exn ]
;;

let run_cancelled_publication_child root =
  let fault = Publication_fault.create () in
  let executable = Eio_main.run (fun _ -> Eio_posix.Low_level.realpath "/bin/cat") in
  Legacy.Fixture.with_runtime
    ~test_root:root
    ~source:(Legacy.process_source ~executable)
    ~wrap_env:(Publication_fault.wrap fault)
    (fun fixture ->
       let snapshot_digest =
         Existing_fault.initial_snapshot fixture.env fixture.physical.handle
       in
       let input =
         Filename.concat
           (Legacy.Physical.state fixture.physical).spec.workspace_instance.canonical_root
             .native_path
           "legacy-capture-gate.txt"
       in
       Eio.Path.save
         ~create:(`Exclusive 0o600)
         Eio.Path.(Eio.Stdenv.fs fixture.env / input)
         "actual retained native process";
       Legacy.Fixture.enqueue fixture (Variant ("Tick", []));
       let original = manager_snapshot fixture in
       Publication_fault.arm fault fixture executable;
       let sentinel = Failure "599 exact actual callback caller cancelled" in
       (* Only observation of the completed failing caller is shielded. The actual
         Actor was created earlier under fixture.sw and fails that real switch. *)
       Eio.Cancel.protect (fun () ->
         let active_caller = ref None in
         let context, resolve_context = Eio.Promise.create () in
         let task =
           Eio.Fiber.fork_promise ~sw:fixture.sw (fun () ->
             capture_diagnostics fixture.env (fun () ->
               Result.try_with (fun () ->
                 Eio.Cancel.sub (fun caller ->
                   active_caller := Some caller;
                   Eio.Promise.resolve resolve_context caller;
                   Exn.protect
                     ~finally:(fun () -> active_caller := None)
                     ~f:(fun () -> Callback.run_one fixture)))))
         in
         let finish () =
           Option.iter !active_caller ~f:(fun caller -> Eio.Cancel.cancel caller sentinel);
           Legacy.Process_gate.release fault.gate;
           ignore
             (Legacy.within fixture.env "join actual callback finally" (fun () ->
                Eio.Promise.await_exn task))
         in
         protect_cleanup ~finally:finish (fun () ->
           Legacy.within fixture.env "actual callback Process fingerprint" (fun () ->
             Eio.Fiber.first
               (fun () -> Eio.Promise.await fault.gate.entered)
               (fun () ->
                  let result, _ = Eio.Promise.await_exn task in
                  match result with
                  | Error exn -> raise exn
                  | Ok (Error failure) -> protocol_ok (Error failure)
                  | Ok (Ok _) -> failwith "actual callback completed before fingerprint"));
           let before = Legacy.Physical.state fixture.physical in
           let running = internal_receipt before in
           assert (Receipt.equal_status Running running.status);
           assert_no_work before;
           Eio.Cancel.cancel (Eio.Promise.await context) sentinel;
           let result, diagnostics =
             Legacy.within fixture.env "actual cancelled callback terminal" (fun () ->
               Eio.Promise.await_exn task)
           in
           let cancelled =
             match result with
             | Error (Eio.Cancel.Cancelled cause as exn) ->
               assert (phys_equal cause sentinel);
               exn
             | Error exn -> raise exn
             | Ok _ -> failwith "original actual callback cancellation was lost"
           in
           (* Reply resolution precedes Switch.fail in Eio's fork wrapper. Wait
             for the actual public switch error; a fixed yield count is no proof. *)
           let fatal =
             Legacy.within fixture.env "actual Actor fatal switch error" (fun () ->
               let rec await () =
                 match Eio.Switch.get_error fixture.sw with
                 | None ->
                   Eio.Fiber.yield ();
                   await ()
                 | Some (Eio.Cancel.Cancelled cause) -> cause
                 | Some exn ->
                   raise_s [%sexp "unexpected switch error projection", (exn : exn)]
               in
               await ())
           in
           let physical = fixture.physical in
           let publication =
             Store.Store_error.of_exn
               ~operation:"replace"
               ~path:(Publication_fault.metadata physical)
               (Option.value_exn fault.injected)
           in
           let expected_exception =
             match Result.try_with (fun () -> Existing.store_ok (Error publication)) with
             | Error exn -> exn
             | Ok _ ->
               failwith "physical fixture did not raise its Store publication error"
           in
           [%test_eq: string list]
             [ Exn.to_string expected_exception ]
             (List.map (fatal_causes fatal) ~f:Exn.to_string);
           assert_diagnostic diagnostics (Exn.to_string expected_exception);
           let selected = Option.value_exn fault.candidate in
           let terminal = Option.value_exn (terminal_receipt selected) in
           assert (Receipt.equal_context running.context terminal.context);
           (match terminal.status with
            | Interrupted reason ->
              [%test_eq: string]
                "queued moderator handler interrupted before checkpoint commit"
                reason
            | Running | Completed _ | Failed _ ->
              failwith "actual cancellation did not persist Interrupted");
           let persisted = Option.value_exn fault.persisted in
           assert (Receipt.equal terminal (internal_receipt persisted));
           [%test_eq: int64]
             selected.transaction_sequence
             persisted.counters.transaction_sequence;
           [%test_eq: int64] selected.session_revision persisted.counters.revision;
           [%test_eq: string option]
             (Some (Store.Transaction.hash selected))
             fault.persisted_hash;
           [%test_eq: string option]
             fault.persisted_hash
             (A.Session_persistence.transaction_hash physical.durable);
           same_state
             persisted
             (A.Session_persistence.Restored.state
                (A.Session_persistence.restored physical.durable));
           assert_no_work persisted;
           assert (Option.equal Jsonaf.exactly_equal before.moderator persisted.moderator);
           assert (Jsonaf.exactly_equal original (manager_snapshot fixture));
           [%test_eq: int] 1 fault.writer_hits;
           [%test_eq: int] 1 fault.publication_hits;
           [%test_eq: int] 1 fault.gate.fingerprint_opens;
           [%test_eq: int] 0 fault.gate.cache_renames;
           [%test_eq: int] 0 !(fixture.providers);
           Legacy.Process_gate.release fault.gate;
           let report =
             `Object
               [ "session_id", P.Id.Session.to_json selected.session_id
               ; "initial_snapshot_digest", `String snapshot_digest
               ; "before", Existing_fault.state_json before
               ; "terminal", Receipt.to_json terminal
               ; "transaction_payload", `String (Store.Transaction.encode selected)
               ; "transaction_sha256", `String (Store.Transaction.hash selected)
               ; "persisted", Existing_fault.state_json persisted
               ; "journal_path", `String (Option.value_exn fault.journal_path)
               ; "metadata_path", `String (Publication_fault.metadata physical)
               ; "injected", `String (Exn.to_string (Option.value_exn fault.injected))
               ; "fatal", `String (Exn.to_string fatal)
               ; "caller_cancelled", `String (Exn.to_string cancelled)
               ]
           in
           Eio.Flow.copy_string
             (Jsonaf.to_string report ^ "\n")
             (Eio.Stdenv.stdout fixture.env);
           (* Actual Actor fatal owner + original caller cancellation observed.
             No post-fatal Actor RPC, positive close, or joined finalizer claim. *)
           Core_unix.exit_immediately 73)))
;;

let%expect_test
    "actual callback cancellation survives fatal post-ACK publication and physical \
     recovery"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
             let report = child_report ~mode:"callback-cancelled-publication" env root in
             recover ~kind:Cancelled_callback env sw root report))));
  print_endline
    "original cancellation and fatal publication retained; exact Interrupted ACK \
     physically recovered without callback replay";
  [%expect
    {| original cancellation and fatal publication retained; exact Interrupted ACK physically recovered without callback replay |}]
;;
