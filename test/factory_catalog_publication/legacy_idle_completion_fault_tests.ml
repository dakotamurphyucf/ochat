open! Core
open Agent_server_test_support
open Retry_support
module Legacy = Legacy_idle_completion_tests
module Store = Agent_store
module Ledger = A.Inference_ledger

let checkpoint raw = A.Moderator_checkpoint.decode raw |> protocol_ok |> Option.value_exn

let assert_checkpoint raw ~count ~queued =
  let actual = checkpoint raw in
  (match actual.current_state with
   | Int actual -> [%test_eq: int] count actual
   | _ -> failwith "legacy checkpoint is not the actual integer script state");
  [%test_eq: int] queued (List.length actual.queued_internal_events);
  assert (not actual.halted);
  actual
;;

let assert_no_work ?operation (current : A.Session_state.t) =
  let marks =
    A.Canonical_usage_fold.Marks.of_ledger current.inference_ledger
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (A.Canonical_usage_fold.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  [%test_eq: int64 * int64] (0L, 0L) (marks.turn, marks.attempt);
  assert (List.is_empty (Ledger.rows current.inference_ledger));
  (match operation, current.active_operation with
   | None, None -> ()
   | Some expected, Some actual ->
     assert (
       Jsonaf.exactly_equal (P.Operation.to_json expected) (P.Operation.to_json actual))
   | None, Some _ | Some _, None -> failwith "wrong actual recovered operation");
  assert (Option.is_none current.failure);
  assert (Option.is_none current.run_state && Option.is_none current.goal_catalog);
  assert (Option.is_none current.goal_descendants)
;;

let state_json current = state_document current |> Existing.store_ok |> D.Document.json

let events (transaction : Store.Transaction.t) =
  List.map transaction.durable_events ~f:(fun document ->
    A.Durable_event_document.decode ~limits:D.Limits.default document
    |> document_ok
    |> A.Durable_event_document.value)
;;

let changes (transaction : Store.Transaction.t) =
  A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
  |> document_ok
  |> A.Session_delta_document.value
  |> Child_usage_fault_tests.Fault.leaves
;;

let transaction (entry : Store.Journal_segment.entry) =
  Store.Transaction.decode (Store.Frame.payload entry.frame) |> Existing.store_ok
;;

let compaction_started (transaction : Store.Transaction.t) =
  List.filter_map (events transaction) ~f:(fun event ->
    match
      P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
    with
    | Operation_started operation -> Some operation
    | _ -> None)
  |> function
  | [ (operation : P.Operation.t) ] when P.Operation.equal_kind operation.kind Compaction
    -> Some operation
  | [] -> None
  | _ -> failwith "unexpected foreground operation in actual compaction fixture"
;;

let assert_compaction_start (transaction : Store.Transaction.t) operation =
  [%test_eq: P.Operation.kind] Compaction operation.P.Operation.kind;
  (match operation.state with
   | Starting -> ()
   | Running | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ ->
     failwith "unknown compaction writer did not retain Starting operation");
  (match compaction_started transaction with
   | Some actual ->
     assert (
       Jsonaf.exactly_equal (P.Operation.to_json operation) (P.Operation.to_json actual))
   | None -> failwith "actual compaction-start event missing");
  match changes transaction with
  | [ History_block_reserved reserved
    ; Active_operation_changed (Some actual)
    ; Lifecycle_changed lifecycle
    ] ->
    assert (Int64.(reserved > 0L));
    assert (
      Jsonaf.exactly_equal (P.Operation.to_json operation) (P.Operation.to_json actual));
    [%test_eq: P.Session.desired_state] Running lifecycle.desired;
    (match lifecycle.observed with
     | Compacting actual -> [%test_eq: P.Id.Operation.t] operation.id actual
     | _ -> failwith "actual compaction lifecycle did not retain the same operation")
  | _ -> failwith "actual compaction writer changed unexpected state"
;;

module Fault = struct
  type target =
    | Checkpoint
    | Compaction_start

  let function_name = function
    | Checkpoint -> "579 legacy checkpoint after sync"
    | Compaction_start -> "579 legacy compaction after sync"
  ;;

  type selected =
    { session : P.Id.Session.t
    ; journal : string
    ; before : Session.Moderator_state.Identity_snapshot.t
    }

  type candidate =
    { transaction : Store.Transaction.t
    ; moderator : Jsonaf.t
    }

  type compaction =
    { checkpoint : candidate
    ; transaction : Store.Transaction.t
    ; operation : P.Operation.t
    }

  type t =
    { target : target
    ; mutable compaction : compaction option
    ; mutable selected : selected option
    ; mutable candidate : candidate option
    ; mutable cache_directory : string option
    ; mutable hits : int
    ; mutable cache_renames : int
    ; mutable injected : exn option
    }

  let create ?(target = Checkpoint) () =
    { target
    ; compaction = None
    ; selected = None
    ; candidate = None
    ; cache_directory = None
    ; hits = 0
    ; cache_renames = 0
    ; injected = None
    }
  ;;

  (* Pure inspection of actual journal evidence; no Actor or authority lookup is
     permitted from the I/O callback. A future compaction case can reuse these
     same transaction/event/checkpoint helpers without a second writer adapter. *)
  let select selected actual =
    if not (P.Id.Session.equal selected.session actual.Store.Transaction.session_id)
    then None
    else (
      match changes actual with
      | [ Moderator_changed (Some moderator) ] ->
        let after = checkpoint (Some moderator) in
        (match after.current_state with
         | Int 1
           when String.equal selected.before.script_id after.script_id
                && String.equal
                     selected.before.script_source_hash
                     after.script_source_hash
                && List.is_empty after.queued_internal_events
                && not after.halted ->
           assert (
             not
               (List.exists (events actual) ~f:(fun event ->
                  P.Event.Durable.equal_kind event.kind Operation_started)));
           Some { transaction = actual; moderator }
         | _ -> None)
      | _ -> None)
  ;;

  let wrap t env =
    let with_cache =
      Crash_fault_io.wrap
        env
        ~boundary:After_rename
        ~matches:(fun path ->
          Option.exists t.cache_directory ~f:(fun directory ->
            String.equal directory (Filename.dirname path)
            && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
            && String.is_suffix path ~suffix:".tmp"))
        ~reached:(fun _ -> t.cache_renames <- t.cache_renames + 1)
    in
    Crash_fault_io.wrap
      with_cache
      ~boundary:After_sync
      ~matches:(fun path ->
        Option.exists t.selected ~f:(fun selected ->
          String.equal selected.journal (Filename.dirname path)
          && String.is_suffix path ~suffix:".log"))
      ~reached:(fun path ->
        Option.iter t.selected ~f:(fun selected ->
          let scan =
            Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
            |> Store.Journal_segment.scan_contents ~max_payload_length:1048576
            |> Existing.store_ok
          in
          match List.last scan.entries with
          | Some entry when Int.equal (Store.Frame.flags entry.frame) 0 ->
            let actual = transaction entry in
            let inject () =
              t.selected <- None;
              t.hits <- t.hits + 1;
              let primary = Core_unix.Unix_error (EIO, function_name t.target, path) in
              t.injected <- Some primary;
              raise primary
            in
            (match select selected actual with
             | Some candidate ->
               assert (Option.is_none t.candidate);
               t.candidate <- Some candidate;
               (match t.target with
                | Checkpoint -> inject ()
                | Compaction_start -> ())
             | None ->
               (match t.target, t.candidate, compaction_started actual with
                | Compaction_start, Some checkpoint, Some operation ->
                  assert_compaction_start actual operation;
                  [%test_eq: int64]
                    Int64.(checkpoint.transaction.transaction_sequence + 1L)
                    actual.transaction_sequence;
                  [%test_eq: int64]
                    Int64.(checkpoint.transaction.session_revision + 1L)
                    actual.session_revision;
                  [%test_eq: string option]
                    (Some (Store.Transaction.hash checkpoint.transaction))
                    actual.previous_transaction_hash;
                  t.compaction <- Some { checkpoint; transaction = actual; operation };
                  inject ()
                | Checkpoint, _, _
                | Compaction_start, None, _
                | Compaction_start, Some _, None -> ()))
          | None | Some _ -> ()))
  ;;
end

let initial_snapshot env handle =
  let installed =
    Store.Snapshot.load_current
      ~env
      ~directory:(Store.Session_store.Handle.snapshot_directory handle)
      ~max_payload_length:1048576
    |> Existing.store_ok
    |> Option.value_exn
  in
  [%test_eq: int64] 0L installed.snapshot.transaction_sequence;
  assert (Option.is_none installed.snapshot.transaction_hash);
  let restored =
    A.Session_persistence.restore_snapshot ~limits:D.Limits.default installed.snapshot
    |> Existing.store_ok
  in
  A.Session_persistence.validate restored |> Existing.store_ok;
  let current = A.Session_persistence.Restored.state restored in
  assert (Option.is_none current.moderator);
  assert_no_work current;
  Store.Snapshot.stored installed.snapshot
  |> Store.Snapshot.Stored.record
  |> Store.Document_record.stored_digest
;;

let same_error expected actual =
  assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json actual))
;;

let persistence_error source =
  P.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum (Store.Store_error.sexp_of_t source))
    ~retryable:true
    ()
;;

let run_child ?(target = Fault.Checkpoint) test_root =
  let fault = Fault.create ~target () in
  let source =
    match target with
    | Checkpoint -> Legacy.source
    | Compaction_start -> Legacy_idle_compaction_tests.source
  in
  Legacy.Fixture.with_runtime
    ~test_root
    ~wrap_env:(Fault.wrap fault)
    ~source
    (fun fixture ->
       let actor = fixture.physical.actor in
       let handle = fixture.physical.handle in
       let snapshot_digest = initial_snapshot fixture.env handle in
       Legacy.Fixture.enqueue fixture (Variant ("Tick", []));
       let before = Legacy.Physical.state fixture.physical in
       let before_snapshot = assert_checkpoint before.moderator ~count:0 ~queued:1 in
       assert_no_work before;
       fault.cache_directory <- Some (Store.Session_store.Handle.cache_directory handle);
       fault.selected
       <- Some
            { session = before.identity.session_id
            ; journal = Store.Session_store.Handle.journal_directory handle
            ; before = before_snapshot
            };
       let primary =
         match Owner.drain_idle_moderator fixture.owner with
         | Ok _ -> failwith "synced legacy metadata writer unexpectedly acknowledged"
         | Error failure -> failure
       in
       let injected = Option.value_exn fault.injected in
       let path =
         match injected with
         | Core_unix.Unix_error (EIO, function_, path) ->
           [%test_eq: string] (Fault.function_name target) function_;
           [%test_eq: string]
             (Store.Session_store.Handle.journal_directory handle)
             (Filename.dirname path);
           path
         | _ -> failwith "metadata fault did not retain the actual injected EIO"
       in
       let expected =
         Store.Store_error.of_exn ~operation:"append journal segment" ~path injected
         |> persistence_error
       in
       same_error expected primary;
       [%test_eq: int] 1 fault.hits;
       assert (not (Store.Commit_writer.is_available fixture.physical.writer));
       let candidate = Option.value_exn fault.candidate in
       [%test_eq: int64]
         Int64.(before.counters.transaction_sequence + 1L)
         candidate.transaction.transaction_sequence;
       [%test_eq: int64]
         Int64.(before.counters.revision + 1L)
         candidate.transaction.session_revision;
       let selected, acknowledged =
         match target with
         | Checkpoint ->
           [%test_eq: string option]
             (A.Session_persistence.transaction_hash fixture.physical.durable)
             candidate.transaction.previous_transaction_hash;
           candidate.transaction, before
         | Compaction_start ->
           let compaction = Option.value_exn fault.compaction in
           let acknowledged = Legacy.Physical.state fixture.physical in
           [%test_eq: int64]
             candidate.transaction.transaction_sequence
             acknowledged.counters.transaction_sequence;
           [%test_eq: int64]
             candidate.transaction.session_revision
             acknowledged.counters.revision;
           [%test_eq: string option]
             (Some (Store.Transaction.hash candidate.transaction))
             (A.Session_persistence.transaction_hash fixture.physical.durable);
           assert (
             Jsonaf.exactly_equal
               (state_json acknowledged)
               (state_json
                  (A.Session_persistence.Restored.state
                     (A.Session_persistence.restored fixture.physical.durable))));
           assert_no_work acknowledged;
           assert (
             Jsonaf.exactly_equal
               (Option.value_exn acknowledged.moderator)
               candidate.moderator);
           compaction.transaction, acknowledged
       in
       [%test_eq: int64]
         acknowledged.counters.revision
         (Store.Session_store.Handle.metadata handle).session.revision;
       ignore
         (assert_checkpoint (Some candidate.moderator) ~count:1 ~queued:0
          : Session.Moderator_state.Identity_snapshot.t);
       assert (
         Jsonaf.exactly_equal
           (state_json acknowledged)
           (state_json (Actor.state actor |> protocol_ok)));
       let live_snapshot =
         fixture.runtime.moderator_manager
         |> Option.value_exn
         |> Chat_response.Moderator_manager.identity_snapshot
         |> Result.ok_or_failwith
         |> A.Runtime_builder.encode_moderator_snapshot
       in
       ignore
         (assert_checkpoint (Some live_snapshot) ~count:1 ~queued:0
          : Session.Moderator_state.Identity_snapshot.t);
       [%test_eq: int] 0 !(fixture.providers);
       assert (Owner.is_loaded fixture.owner);
       assert (not (Owner.drain_idle_moderator fixture.owner |> protocol_ok));
       assert (
         Jsonaf.exactly_equal
           (state_json acknowledged)
           (state_json (Legacy.Physical.state fixture.physical)));
       [%test_eq: int] 1 fault.hits;
       let failed_close () =
         try
           Legacy.within fixture.env "truthful unknown metadata close" (fun () ->
             Owner.close_and_wait fixture.owner);
           failwith "unknown legacy checkpoint was reported fully retired"
         with
         | Owner.Cleanup_failed failure -> failure
       in
       let cleanup = failed_close () in
       assert (phys_equal primary cleanup);
       same_error primary cleanup;
       same_error primary (failed_close ());
       let after_close_snapshot =
         fixture.runtime.moderator_manager
         |> Option.value_exn
         |> Chat_response.Moderator_manager.identity_snapshot
         |> Result.ok_or_failwith
         |> A.Runtime_builder.encode_moderator_snapshot
       in
       assert (Jsonaf.exactly_equal live_snapshot after_close_snapshot);
       assert (Owner.is_loaded fixture.owner);
       [%test_eq: int] 0 fault.cache_renames;
       [%test_eq: int] 1 fault.hits;
       [%test_eq: int] 0 !(fixture.providers);
       let current = Legacy.Physical.state fixture.physical in
       assert (Jsonaf.exactly_equal (state_json acknowledged) (state_json current));
       assert_no_work current;
       let report =
         `Object
           ([ "primary", P.Error.to_json primary
            ; "cleanup", P.Error.to_json cleanup
            ; "injected", `String (Exn.to_string injected)
            ; "journal_path", `String path
            ; "session_id", P.Id.Session.to_json current.identity.session_id
            ; "initial_snapshot_digest", `String snapshot_digest
            ; ( "acknowledged_revision"
              , `String (Int64.to_string acknowledged.counters.revision) )
            ; ( "transaction_sequence"
              , `String (Int64.to_string selected.transaction_sequence) )
            ; "transaction_sha256", `String (Store.Transaction.hash selected)
            ; "session_revision", `String (Int64.to_string selected.session_revision)
            ; "moderator", candidate.moderator
            ]
            @
            match target with
            | Checkpoint -> []
            | Compaction_start ->
              let compaction = Option.value_exn fault.compaction in
              [ ( "checkpoint_sequence"
                , `String
                    (Int64.to_string
                       compaction.checkpoint.transaction.transaction_sequence) )
              ; ( "checkpoint_sha256"
                , `String (Store.Transaction.hash compaction.checkpoint.transaction) )
              ; "operation", P.Operation.to_json compaction.operation
              ])
       in
       (* The real writer and public close have completed with errors. Exit here,
         before the generic fixture finally can force unrelated Actor/Graph cleanup. *)
       Eio.Flow.copy_string
         (Jsonaf.to_string report ^ "\n")
         (Eio.Stdenv.stdout fixture.env);
       Core_unix.exit_immediately 73)
;;

let child_report ?(target = Fault.Checkpoint) env root =
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
        [ "./goal_native_fault_child.exe"
        ; root
        ; (match target with
           | Checkpoint -> "legacy-idle-checkpoint-unknown"
           | Compaction_start -> "legacy-idle-compaction-unknown")
        ]
    in
    Eio.Flow.close sink;
    Eio.Flow.close errors;
    let report, diagnostics =
      Eio.Fiber.pair
        (fun () -> Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:32768 output))
        (fun () ->
           Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:16384 diagnostics))
    in
    (match Eio.Process.await child with
     | `Exited 73 -> ()
     | `Exited code ->
       raise_s
         [%sexp
           "legacy metadata fault child failed", (code : int), (diagnostics : string)]
     | `Signaled signal ->
       raise_s
         [%sexp
           "legacy metadata fault child signaled", (signal : int), (diagnostics : string)]);
    if not (String.is_empty (String.strip diagnostics))
    then raise_s [%sexp "unexpected legacy metadata fault stderr", (diagnostics : string)];
    Jsonaf.of_string report)
;;

(* This standalone host deliberately uses Require_grant. Read the actual
   current Store/Recovery contract rather than asking the default Factory loader
   to invent an archived profile or managed runtime authority. *)
let recover ?(target = Fault.Checkpoint) env sw root report =
  let env = (env :> Eio_unix.Stdenv.base) in
  let member name = Jsonaf.member_exn name report in
  let session_id = member "session_id" |> P.Id.Session.of_json |> protocol_ok in
  let number name = member name |> Jsonaf.string_exn |> Int64.of_string in
  let operation =
    match target with
    | Checkpoint -> None
    | Compaction_start -> Some (member "operation" |> P.Operation.of_json |> protocol_ok)
  in
  let primary = member "primary" |> P.Error.of_json |> protocol_ok in
  let cleanup = member "cleanup" |> P.Error.of_json |> protocol_ok in
  let expected =
    Store.Store_error.Io
      { operation = "append journal segment"
      ; path = member "journal_path" |> Jsonaf.string_exn
      ; message = member "injected" |> Jsonaf.string_exn
      }
    |> persistence_error
  in
  same_error expected primary;
  same_error primary cleanup;
  let store =
    Store.Session_store.open_existing
      ~env
      ~sw
      ~root:(Filename.concat root "data")
      ~process_start_identity:None
      ~lock_nonce:"579-parent-physical-recovery"
    |> Existing.store_ok
  in
  protect_cleanup
    ~finally:(fun () -> Store.Session_store.close store |> Existing.store_ok)
    (fun () ->
       let handle =
         Store.Session_store.open_session
           store
           ~sw
           ~actor_lock_nonce:"579-recovered-session"
           session_id
         |> Existing.store_ok
       in
       protect_cleanup
         ~finally:(fun () ->
           Store.Session_store.close_session store handle |> Existing.store_ok)
         (fun () ->
            [%test_eq: string]
              (member "initial_snapshot_digest" |> Jsonaf.string_exn)
              (initial_snapshot env handle);
            [%test_eq: int64]
              (number "acknowledged_revision")
              (Store.Session_store.Handle.metadata handle).session.revision;
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
            let selected =
              List.filter transactions ~f:(fun (actual : Store.Transaction.t) ->
                Int64.equal actual.transaction_sequence (number "transaction_sequence"))
              |> function
              | [ actual ] -> actual
              | [] | _ :: _ :: _ ->
                failwith "synced legacy checkpoint missing after hard exit"
            in
            [%test_eq: string]
              (member "transaction_sha256" |> Jsonaf.string_exn)
              (Store.Transaction.hash selected);
            [%test_eq: int64] (number "session_revision") selected.session_revision;
            assert (P.Id.Session.equal session_id selected.session_id);
            (match target with
             | Checkpoint ->
               (match changes selected with
                | [ Moderator_changed (Some actual) ] ->
                  assert (Jsonaf.exactly_equal actual (member "moderator"))
                | _ -> failwith "selected metadata transaction changed during recovery");
               List.iter (events selected) ~f:(fun event ->
                 assert (not (P.Event.Durable.equal_kind event.kind Operation_started)))
             | Compaction_start ->
               let operation = Option.value_exn operation in
               assert_compaction_start selected operation;
               let checkpoint =
                 List.find_exn transactions ~f:(fun (actual : Store.Transaction.t) ->
                   Int64.equal actual.transaction_sequence (number "checkpoint_sequence"))
               in
               [%test_eq: string]
                 (member "checkpoint_sha256" |> Jsonaf.string_exn)
                 (Store.Transaction.hash checkpoint);
               [%test_eq: int64]
                 Int64.(checkpoint.transaction_sequence + 1L)
                 selected.transaction_sequence;
               [%test_eq: int64]
                 Int64.(checkpoint.session_revision + 1L)
                 selected.session_revision;
               [%test_eq: string option]
                 (Some (Store.Transaction.hash checkpoint))
                 selected.previous_transaction_hash;
               (match changes checkpoint with
                | [ Moderator_changed (Some actual) ] ->
                  assert (Jsonaf.exactly_equal actual (member "moderator"))
                | _ -> failwith "acknowledged checkpoint prefix changed during recovery"));
            let read () =
              Store.Recovery.read
                ~env
                ~journal
                ~snapshot_directory
                ~max_snapshot_payload_length:1048576
                ~session_id
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
            assert_no_work ?operation current;
            ignore
              (assert_checkpoint current.moderator ~count:1 ~queued:0
               : Session.Moderator_state.Identity_snapshot.t);
            assert (
              Jsonaf.exactly_equal
                (Option.value_exn current.moderator)
                (member "moderator"));
            assert (
              Jsonaf.exactly_equal
                (state_json current)
                (state_json (A.Session_persistence.Restored.state (read ()).state)));
            [%test_eq: (int * string) list] original_frames (frames ());
            [%test_eq: string]
              (member "initial_snapshot_digest" |> Jsonaf.string_exn)
              (initial_snapshot env handle);
            (* Recovery validates/replays session data; it does not falsely claim
                that the failed metadata/index publication has been repaired. *)
            [%test_eq: int64]
              (number "acknowledged_revision")
              (Store.Session_store.Handle.metadata handle).session.revision))
;;

let%expect_test
    "uncertain aggregate legacy checkpoint survives hard exit and physical recovery"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
             let report = child_report env root in
             recover env sw root report))));
  print_endline
    "actual metadata After_sync uncertainty retained; failed close, hard exit, exact \
     physical checkpoint replay";
  [%expect
    {| actual metadata After_sync uncertainty retained; failed close, hard exit, exact physical checkpoint replay |}]
;;

let run_compaction_child root = run_child ~target:Fault.Compaction_start root

let%expect_test
    "uncertain compaction start retains acknowledged legacy checkpoint across hard exit"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
             let report = child_report ~target:Fault.Compaction_start env root in
             recover ~target:Fault.Compaction_start env sw root report))));
  print_endline
    "checkpoint ACK retained; uncertain real compaction-start frame replayed without \
     launch or cleanup fiction";
  [%expect
    {| checkpoint ACK retained; uncertain real compaction-start frame replayed without launch or cleanup fiction |}]
;;
