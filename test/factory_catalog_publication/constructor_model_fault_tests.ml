open! Core
open Agent_server_test_support
open Retry_support
module Runtime = Goal_resume_runtime_tests
module B = A.Runtime_builder
module Store = Agent_store
module Ledger = A.Inference_ledger
module Observation = Inference.Observation

let constructor_transcript inputs =
  `Array (List.map inputs ~f:Res.Item.jsonaf_of_t)
  |> Jsonaf.to_string
  |> fun transcript -> String.is_substring transcript ~substring:Runtime.constructor_input
;;

let capture f =
  try Ok (f ()) with
  | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let joined = function
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let new_job before current =
  List.filter current.A.Session_state.jobs ~f:(fun job ->
    not
      (List.exists before.A.Session_state.jobs ~f:(fun old ->
         P.Id.Job.equal old.id job.id)))
  |> Runtime.one_actual "actual constructor Model Job"
;;

let actual_job current job =
  List.find_exn current.A.Session_state.jobs ~f:(fun actual ->
    P.Id.Job.equal actual.P.Job.id job.P.Job.id)
;;

let proofs current job =
  let work = A.Run_job_occurrence.work job |> protocol_ok in
  Option.to_list current.A.Session_state.run_state
  |> List.concat_map ~f:A.Run_state.runs
  |> List.concat_map ~f:(fun run -> run.P.Run.terminal_work)
  |> List.filter ~f:(fun proof -> P.Run_work.equal proof.P.Run_work.Terminal.work work)
;;

let assert_running (job : P.Job.t) =
  [%test_eq: P.Job.kind] Model_call job.kind;
  [%test_eq: int] 1 job.attempt;
  assert (P.Job.equal_delivery Not_required job.delivery);
  (match job.status with
   | Running -> ()
   | Queued
   | Waiting_permission _
   | Waiting_completion _
   | Succeeded
   | Failed _
   | Cancelled
   | Interrupted _ -> failwith "actual constructor did not reach Running");
  assert (Option.is_some job.started_at);
  assert (Option.is_none job.completed_at);
  assert (Option.is_none job.result)
;;

let%expect_test
    "strong close joins a live constructor and preserves its actual cancellation ACK"
  =
  let entered, enter = Eio.Promise.create () in
  let unwinding, unwind = Eio.Promise.create () in
  let released, release = Eio.Promise.create () in
  let constructor_providers = ref 0 in
  let fault = Inference_close_tests.Fault.create () in
  let release_once () =
    if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
  in
  Runtime.with_cold_episode
    ~wrap_env:(Inference_close_tests.Fault.wrap_env fault)
    ~post_stream:(fun ~sw:_ ~inputs ->
      if constructor_transcript inputs
      then (
        Int.incr constructor_providers;
        Eio.Promise.resolve enter ();
        match Eio.Promise.await (fst (Eio.Promise.create ())) with
        | _ -> failwith "live constructor gate unexpectedly returned"
        | exception (Eio.Cancel.Cancelled _ as exn) ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          Eio.Cancel.protect (fun () ->
            Eio.Promise.resolve unwind ();
            Eio.Promise.await released);
          Exn.raise_with_original_backtrace exn backtrace)
      else completed_response "601 initial provider before live cancellation")
    Legacy
    Continue
    (fun _env
      daemon
      entry
      _old_runtime
      counters
      _probe
      session
      _admission
      _initial
      _stopped
      actor
      request ->
       let handle = Option.value_exn entry.Registry.store_handle in
       fault.cache
       <- Some
            (Filename.concat
               (Store.Session_store.Handle.cache_directory handle)
               "cache.bin");
       Owner.ensure_loaded entry.runtime |> protocol_ok;
       let pending =
         Owner.with_loaded_runtime entry.runtime (fun runtime -> Ok runtime)
         |> protocol_ok
       in
       let activation = Option.value_exn pending.B.moderator_activation in
       assert (activation.pending ());
       Eio.Switch.run (fun sw ->
         protect_cleanup ~finally:release_once (fun () ->
           let resource = Runtime.Resource_retention.Control.create () in
           Runtime.with_resource_retention
             ~control:resource
             ~sw
             entry.runtime
             Shared_resources
             (fun () ->
                Owner.unload_and_wait entry.runtime |> protocol_ok;
                let attachment, _ =
                  Actor.attach entry.actor ~mode:Read_write ~subscribe:false
                  |> protocol_ok
                in
                let before = state entry in
                let request_done, finish_request = Eio.Promise.create () in
                Eio.Fiber.fork ~sw (fun () ->
                  Eio.Promise.resolve
                    finish_request
                    (capture (fun () ->
                       Runtime.Service.resume_episode
                         (Runtime.public daemon)
                         ~entry
                         ~actor
                         ~session
                         ~request)));
                Eio.Fiber.first
                  (fun () -> Eio.Promise.await entered)
                  (fun () ->
                     ignore (joined (Eio.Promise.await request_done));
                     failwith "resume returned before actual constructor provider entry");
                let running = state entry in
                let job = new_job before running in
                assert_running job;
                let callback =
                  List.filter running.moderator_executions ~f:(fun receipt ->
                    not
                      (List.exists before.moderator_executions ~f:(fun old ->
                         P.Id.Moderator_execution.equal
                           old.P.Moderator_execution.context.id
                           receipt.context.id)))
                  |> Runtime.one_actual "live constructor callback"
                in
                assert (P.Moderator_execution.equal_status Running callback.status);
                let native =
                  List.filter (Ledger.rows running.inference_ledger) ~f:(fun row ->
                    match Observation.Attempt_record.state (Ledger.Row.record row) with
                    | Running -> true
                    | Prepared | Terminal _ | Interrupted _ -> false)
                  |> Runtime.one_actual "live constructor native allocation"
                  |> Ledger.Row.handle
                in
                let marks = Inference_close_tests.marks running in
                [%test_eq: int] 1 !constructor_providers;
                [%test_eq: int] 2 !(counters.providers);
                [%test_eq: int] 0 fault.cache_renames;
                (* The source-stage lease now owns the original product. The test's
              external borrower must not be part of strong close's join set. *)
                Runtime.Resource_retention.Control.release_and_join resource;
                [%test_eq: int] 0 fault.cache_renames;
                let close_done, finish_close = Eio.Promise.create () in
                protect_cleanup ~finally:release_once (fun () ->
                  Eio.Fiber.fork ~sw (fun () ->
                    Eio.Promise.resolve
                      finish_close
                      (capture (fun () -> Owner.close_and_wait entry.runtime)));
                  Eio.Fiber.first
                    (fun () -> Eio.Promise.await unwinding)
                    (fun () ->
                       joined (Eio.Promise.await close_done);
                       failwith "strong close returned before its live provider joined");
                  (* Actual source cancellation reached the provider. While its real
                unwind is protected, a genuine operator selects Cancelled; this
                same Job/Run/Init ACK must survive the late producer exit. *)
                  let winner =
                    Actor.cancel_job
                      entry.actor
                      ~attachment_id:attachment.id
                      ~job_id:job.id
                      ~expected_generation:job.generation
                      ~expected_attempt:job.attempt
                      ()
                    |> protocol_ok
                  in
                  (match winner.status with
                   | Cancelled -> ()
                   | Queued
                   | Running
                   | Waiting_permission _
                   | Waiting_completion _
                   | Succeeded
                   | Failed _
                   | Interrupted _ ->
                     failwith "operator cancellation did not install its winner");
                  assert (Option.is_none winner.result);
                  release_once ();
                  joined (Eio.Promise.await close_done);
                  (match Eio.Promise.await request_done with
                   | Error (Eio.Cancel.Cancelled _, _) -> ()
                   | Ok (Ok (Resume_failed _)) -> ()
                   | Ok (Error error) ->
                     raise_s
                       [%sexp "resume lost accepted command receipt", (error : P.Error.t)]
                   | Ok (Ok (Resumed _ | Retained _ | Rejected _ | Uncertain _)) ->
                     failwith
                       "cancelled constructor returned a successful or unaccepted command"
                   | Error (exn, backtrace) ->
                     Exn.raise_with_original_backtrace exn backtrace);
                  let final = state entry in
                  assert (
                    Jsonaf.exactly_equal
                      (P.Job.to_json winner)
                      (P.Job.to_json (actual_job final job)));
                  let proof =
                    proofs final job
                    |> Runtime.one_actual "cancelled constructor Run proof"
                  in
                  [%test_eq: P.Run_work.Terminal.outcome] Cancelled proof.outcome;
                  let terminal =
                    List.find_exn final.moderator_executions ~f:(fun receipt ->
                      P.Id.Moderator_execution.equal
                        callback.context.id
                        receipt.P.Moderator_execution.context.id)
                  in
                  (match terminal.status with
                   | Interrupted _ -> ()
                   | Running | Completed _ | Failed _ ->
                     failwith "cancelled callback did not retain its actual interruption");
                  let ended =
                    Ledger.find
                      final.inference_ledger
                      ~ordinal:(Ledger.Handle.ordinal native)
                    |> Option.value_exn
                  in
                  assert (Ledger.Handle.equal native (Ledger.Row.handle ended));
                  (match Observation.Attempt_record.state (Ledger.Row.record ended) with
                   | Interrupted { reason = Cancelled; delivery = Possibly_submitted } ->
                     ()
                   | Interrupted { reason; delivery } ->
                     raise_s
                       [%sexp
                         "unexpected live cancellation evidence"
                       , (reason : Observation.Attempt_record.interruption)
                       , (delivery : Inference.Event.Terminal.delivery)]
                   | Prepared | Running | Terminal _ ->
                     failwith "live native request lost its truthful interrupted delivery");
                  assert (
                    A.Canonical_usage_fold.Marks.equal
                      marks
                      (Inference_close_tests.marks final));
                  assert (not (Owner.is_loaded entry.runtime));
                  [%test_eq: int] 1 fault.cache_renames;
                  [%test_eq: int] 1 !constructor_providers;
                  [%test_eq: int] 2 !(counters.providers);
                  Owner.close_and_wait entry.runtime;
                  assert (
                    Jsonaf.exactly_equal
                      (P.Job.to_json winner)
                      (P.Job.to_json (actual_job (state entry) job)));
                  [%test_eq: int] 1 fault.cache_renames)))));
  print_endline
    "actual live producer joined; operator Job/Run winner preserved; physical close once \
     without replay";
  [%expect
    {| actual live producer joined; operator Job/Run winner preserved; physical close once without replay |}]
;;

module Physical = struct
  module Existing_fault = Legacy_idle_completion_fault_tests

  let max_payload_length = 64 * 1024 * 1024
  let changes = Existing_fault.changes

  let current_snapshot env ~directory =
    Store.Snapshot.load_current ~env ~directory ~max_payload_length
    |> Existing.store_ok
    |> Option.value_exn
  ;;

  (* Each caller selects its own physical anchor and expected session. Reusing
     the reader preserves that basis for every read without repairing the tail. *)
  let recovery_reader env handle ~session_id ~initial_snapshot
    : unit -> A.Session_persistence.Restored.t Store.Recovery.t
    =
    let limits = D.Limits.default in
    let restore_snapshot = A.Session_persistence.restore_snapshot ~limits in
    let initial = restore_snapshot initial_snapshot |> Existing.store_ok in
    let snapshot_directory = Store.Session_store.Handle.snapshot_directory handle in
    let journal =
      Store.Journal.open_existing
        ~env
        ~directory:(Store.Session_store.Handle.journal_directory handle)
        ~max_payload_length
        ~max_segment_bytes:4194304L
        ~max_segment_frames:32
      |> Existing.store_ok
    in
    fun () ->
      Store.Recovery.read
        ~env
        ~journal
        ~snapshot_directory
        ~max_snapshot_payload_length:max_payload_length
        ~session_id
        ~initial
        ~restore_snapshot
        ~apply:(A.Session_persistence.apply_transaction ~limits)
        ~validate_transaction:(A.Session_persistence.validate_transaction ~limits)
        ~validate:A.Session_persistence.validate
      |> Existing.store_ok
  ;;

  let snapshot_anchor env handle =
    let installed =
      current_snapshot
        env
        ~directory:(Store.Session_store.Handle.snapshot_directory handle)
    in
    (* Actual Factory creation commits Created, then checkpoints that ACK before
       loading/activating the runtime. Standalone hosts may begin at tx0; this
       fixture must preserve its genuine tx1 anchor instead. *)
    [%test_eq: int64] 1L installed.snapshot.transaction_sequence;
    let transaction_hash = Option.value_exn installed.snapshot.transaction_hash in
    assert (
      P.Id.Session.equal
        (Store.Session_store.Handle.session_id handle)
        installed.snapshot.session_id);
    let restored =
      A.Session_persistence.restore_snapshot ~limits:D.Limits.default installed.snapshot
      |> Existing.store_ok
    in
    A.Session_persistence.validate restored |> Existing.store_ok;
    let current = A.Session_persistence.Restored.state restored in
    [%test_eq: int64] 1L current.counters.transaction_sequence;
    [%test_eq: int64] 1L current.counters.revision;
    [%test_eq: int64] 1L current.counters.event_sequence;
    assert (Option.is_none current.moderator);
    assert (List.is_empty current.jobs);
    assert (List.is_empty current.moderator_executions);
    assert (Option.is_none current.run_state && Option.is_none current.goal_catalog);
    let digest =
      Store.Snapshot.stored installed.snapshot
      |> Store.Snapshot.Stored.record
      |> Store.Document_record.stored_digest
    in
    `Object
      [ "digest", `String digest
      ; "filename", `String installed.filename
      ; ( "transaction_sequence"
        , `String (Int64.to_string installed.snapshot.transaction_sequence) )
      ; "transaction_hash", `String transaction_hash
      ; "revision", `String (Int64.to_string current.counters.revision)
      ; "event_sequence", `String (Int64.to_string installed.snapshot.event_sequence)
      ]
  ;;

  let ledger_json ledger =
    Ledger.to_document ledger
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Ledger.Error.sexp_of_t error))
    |> Result.ok_or_failwith
    |> D.Document.json
  ;;

  module Boundary = struct
    type t =
      | Terminal
      | Claim_publication
  end

  module Cut = struct
    type selected =
      { session : P.Id.Session.t
      ; journal : string
      ; metadata : string
      ; old_jobs : P.Id.Job.t list
      }

    type candidate =
      { transaction : Store.Transaction.t
      ; job : P.Job.t
      ; journal_path : string
      }

    type t =
      { boundary : Boundary.t
      ; mutable selected : selected option
      ; mutable candidate : candidate option
      ; mutable injected : exn option
      ; mutable writer_hits : int
      ; mutable publication_hits : int
      }

    let create boundary =
      { boundary
      ; selected = None
      ; candidate = None
      ; injected = None
      ; writer_hits = 0
      ; publication_hits = 0
      }
    ;;

    let arm t handle current =
      t.selected
      <- Some
           { session = Store.Session_store.Handle.session_id handle
           ; journal = Store.Session_store.Handle.journal_directory handle
           ; metadata =
               Filename.concat
                 (Store.Session_store.Handle.directory handle)
                 "metadata.sexp"
           ; old_jobs = List.map current.A.Session_state.jobs ~f:(fun job -> job.P.Job.id)
           }
    ;;

    let new_model selected transaction =
      List.filter_map (changes transaction) ~f:(function
        | A.Session_delta.Job_changed job
          when P.Job.equal_kind Model_call job.kind
               && not (List.mem selected.old_jobs job.id ~equal:P.Id.Job.equal) ->
          Some job
        | _ -> None)
      |> function
      | [] -> None
      | [ job ] -> Some job
      | _ -> failwith "constructor writer combined multiple Model Jobs"
    ;;

    let select t selected transaction =
      match new_model selected transaction with
      | None -> None
      | Some job ->
        (match t.boundary, job.status with
         | Terminal, Succeeded | Claim_publication, Running ->
           [%test_eq: int] 1 job.attempt;
           assert (P.Job.equal_delivery Not_required job.delivery);
           assert (P.Id.Session.equal selected.session job.session_id);
           Some job
         | ( Terminal
           , ( Queued
             | Running
             | Waiting_permission _
             | Waiting_completion _
             | Failed _
             | Cancelled
             | Interrupted _ ) )
         | ( Claim_publication
           , ( Queued
             | Waiting_permission _
             | Waiting_completion _
             | Succeeded
             | Failed _
             | Cancelled
             | Interrupted _ ) ) -> None)
    ;;

    let wrap t env =
      let wrapped =
        Crash_fault_io.wrap
          env
          ~boundary:After_sync
          ~matches:(fun path ->
            Option.exists t.selected ~f:(fun selected ->
              String.equal selected.journal (Filename.dirname path)
              && String.is_suffix path ~suffix:".log"))
          ~reached:(fun path ->
            let selected = Option.value_exn t.selected in
            let scan =
              Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
              |> Store.Journal_segment.scan_contents ~max_payload_length
              |> Existing.store_ok
            in
            match List.last scan.entries with
            | Some frame when Int.equal (Store.Frame.flags frame.frame) 0 ->
              let transaction =
                Store.Transaction.decode (Store.Frame.payload frame.frame)
                |> Existing.store_ok
              in
              if P.Id.Session.equal selected.session transaction.session_id
              then (
                match select t selected transaction with
                | None -> ()
                | Some job ->
                  assert (Option.is_none t.candidate);
                  t.candidate <- Some { transaction; job; journal_path = path };
                  t.writer_hits <- t.writer_hits + 1;
                  (match t.boundary with
                   | Claim_publication -> ()
                   | Terminal ->
                     let injected =
                       Core_unix.Unix_error (EIO, "601 Model terminal after sync", path)
                     in
                     t.injected <- Some injected;
                     raise injected))
            | Some _ | None -> ())
      in
      Crash_fault_io.wrap
        wrapped
        ~boundary:After_rename
        ~matches:(fun path ->
          match t.boundary, t.selected, t.candidate with
          | Claim_publication, Some selected, Some _ ->
            String.equal (Filename.dirname selected.metadata) (Filename.dirname path)
            && String.is_prefix (Filename.basename path) ~prefix:"metadata.sexp.tmp-"
          | Terminal, _, _ | Claim_publication, None, _ | Claim_publication, Some _, None
            -> false)
        ~reached:(fun path ->
          let selected = Option.value_exn t.selected in
          [%test_eq: string] selected.metadata path;
          t.publication_hits <- t.publication_hits + 1;
          [%test_eq: int] 1 t.publication_hits;
          let injected = Failure "601 positive Model claim publication sentinel" in
          t.injected <- Some injected;
          raise injected)
    ;;
  end

  let candidate_fields cut =
    let candidate = Option.value_exn cut.Cut.candidate in
    [ "session_id", P.Id.Session.to_json candidate.transaction.session_id
    ; "job", P.Job.to_json candidate.job
    ; "transaction_payload", `String (Store.Transaction.encode candidate.transaction)
    ; "transaction_sha256", `String (Store.Transaction.hash candidate.transaction)
    ; "journal_path", `String candidate.journal_path
    ; "injected", `String (Exn.to_string (Option.value_exn cut.injected))
    ]
  ;;

  let write_report env fields =
    Eio.Flow.copy_string
      (Jsonaf.to_string (`Object fields) ^ "\n")
      (Eio.Stdenv.stdout env);
    (* The actual owner cannot prove these writes unattempted. Generic fixture
       finalizers are not recovery and must not rewrite the selected suffix. *)
    Core_unix.exit_immediately 73
  ;;

  let terminal_run_proof transaction job =
    let work = A.Run_job_occurrence.work job |> protocol_ok in
    let proof =
      List.filter_map (changes transaction) ~f:(function
        | A.Session_delta.Run_state_changed index -> Some index
        | _ -> None)
      |> List.concat_map ~f:A.Run_state.runs
      |> List.concat_map ~f:(fun run -> run.P.Run.terminal_work)
      |> List.filter ~f:(fun proof ->
        P.Run_work.equal work proof.P.Run_work.Terminal.work)
      |> Runtime.one_actual "same Model terminal writer Run proof"
    in
    [%test_eq: P.Run_work.Terminal.outcome] Succeeded proof.outcome;
    proof
  ;;

  let run_terminal_child test_root =
    let cut = Cut.create Terminal in
    let cache = Inference_close_tests.Fault.create () in
    let constructor_providers = ref 0 in
    Runtime.with_cold_episode
      ~test_root
      ~wrap_env:(fun env -> Cut.wrap cut (Inference_close_tests.Fault.wrap_env cache env))
      ~post_stream:(fun ~sw:_ ~inputs ->
        if constructor_transcript inputs then Int.incr constructor_providers;
        completed_response "601 exact returned provider result")
      Legacy
      Continue
      (fun env
        daemon
        entry
        _old_runtime
        counters
        _probe
        session
        _admission
        _initial
        _stopped
        actor
        request ->
         let handle = Option.value_exn entry.Registry.store_handle in
         cache.cache
         <- Some
              (Filename.concat
                 (Store.Session_store.Handle.cache_directory handle)
                 "cache.bin");
         let anchor = snapshot_anchor env handle in
         Owner.ensure_loaded entry.runtime |> protocol_ok;
         Eio.Switch.run (fun sw ->
           Runtime.with_resource_retention ~sw entry.runtime Shared_resources (fun () ->
             Owner.unload_and_wait entry.runtime |> protocol_ok;
             let attachment, _ =
               Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
             in
             let before_request = state entry in
             Cut.arm cut handle before_request;
             let (), diagnostics =
               Callback_retirement_fault_tests.capture_diagnostics env (fun () ->
                 let primary =
                   match
                     Runtime.Service.resume_episode
                       (Runtime.public daemon)
                       ~entry
                       ~actor
                       ~session
                       ~request
                     |> protocol_ok
                   with
                   | Resume_failed { error; _ } -> error
                   | Rejected error | Uncertain error ->
                     raise_s
                       [%sexp
                         "Model terminal fault preceded command ACK", (error : P.Error.t)]
                   | Resumed _ | Retained _ ->
                     failwith "uncertain Model terminal installed runtime"
                 in
                 let candidate = Option.value_exn cut.candidate in
                 [%test_eq: int] 1 cut.writer_hits;
                 [%test_eq: int] 0 cut.publication_hits;
                 [%test_eq: int] 1 !constructor_providers;
                 [%test_eq: int] 2 !(counters.providers);
                 let expected =
                   Store.Store_error.of_exn
                     ~operation:"append journal segment"
                     ~path:candidate.journal_path
                     (Option.value_exn cut.injected)
                   |> Existing_fault.persistence_error
                 in
                 [%test_eq: P.Error.code] Invalid_state primary.code;
                 assert (String.is_substring primary.message ~substring:expected.message);
                 let before = state entry in
                 let job = new_job before_request before in
                 assert_running job;
                 assert (P.Id.Job.equal job.id candidate.job.id);
                 [%test_eq: int64]
                   (Int64.succ before.counters.transaction_sequence)
                   candidate.transaction.transaction_sequence;
                 (match candidate.job.status with
                  | Succeeded -> ()
                  | Queued
                  | Running
                  | Waiting_permission _
                  | Waiting_completion _
                  | Failed _
                  | Cancelled
                  | Interrupted _ ->
                    failwith "fault did not select original Model success");
                 let returned = Option.value_exn candidate.job.result in
                 [%test_eq: string]
                   "601 exact returned provider result"
                   (Jsonaf.member_exn "final_text" returned |> Jsonaf.string_exn);
                 assert (Option.is_some candidate.job.completed_at);
                 let callback =
                   List.filter before.moderator_executions ~f:(fun receipt ->
                     not
                       (List.exists before_request.moderator_executions ~f:(fun old ->
                          P.Id.Moderator_execution.equal
                            old.P.Moderator_execution.context.id
                            receipt.context.id)))
                   |> Runtime.one_actual "callback retained behind unknown Model terminal"
                 in
                 assert (
                   P.Moderator_execution.equal_phase Session_resume callback.context.phase);
                 assert (P.Moderator_execution.equal_status Running callback.status);
                 ignore
                   (terminal_run_proof candidate.transaction candidate.job
                    : P.Run_work.Terminal.t);
                 let same_error result =
                   match result with
                   | Error error ->
                     if
                       not
                         (Jsonaf.exactly_equal
                            (P.Error.to_json expected)
                            (P.Error.to_json error))
                     then
                       raise_s
                         [%sexp
                           "constructor uncertainty error mismatch"
                         , (expected : P.Error.t)
                         , (error : P.Error.t)]
                   | Ok _ -> failwith "unknown constructor writer was reported joined"
                 in
                 (* A moderator checkpoint edit would reject the retained callback
                    borrow before reaching persistence. Exercise an independent,
                    otherwise valid metadata write against the uncertain writer. *)
                 let patch =
                   P.Session_metadata.Patch.create
                     ~name:(Set "must not publish after unknown constructor write")
                     ~set_labels:[]
                     ~remove_labels:[]
                   |> protocol_ok
                 in
                 same_error
                   (Actor.update_metadata
                      entry.actor
                      ~attachment_id:attachment.id
                      ~expected_metadata_revision:before.identity.metadata_revision
                      ~patch
                      ());
                 same_error (Owner.unload_and_wait entry.runtime);
                 same_error (Owner.unload_and_wait entry.runtime);
                 (match capture (fun () -> Owner.close_and_wait entry.runtime) with
                  | Error (Owner.Cleanup_failed error, _) ->
                    Existing_fault.same_error expected error
                  | Error (exn, backtrace) ->
                    Exn.raise_with_original_backtrace exn backtrace
                  | Ok () -> failwith "unknown terminal writer allowed physical close");
                 assert (
                   Jsonaf.exactly_equal
                     (Existing_fault.state_json before)
                     (Existing_fault.state_json (state entry)));
                 [%test_eq: int] 1 cut.writer_hits;
                 [%test_eq: int] 0 cache.cache_renames;
                 [%test_eq: int] 1 !constructor_providers;
                 [%test_eq: int] 2 !(counters.providers);
                 write_report
                   env
                   (candidate_fields cut
                    @ [ "kind", `String "terminal"
                      ; "before", Existing_fault.state_json before
                      ; "initial_snapshot", anchor
                      ; "primary", P.Error.to_json primary
                      ; "cleanup", P.Error.to_json expected
                      ]))
             in
             ignore (diagnostics : string list);
             failwith "terminal crash report unexpectedly returned")))
  ;;

  let run_claim_publication_child test_root =
    let cut = Cut.create Claim_publication in
    let cache = Inference_close_tests.Fault.create () in
    let test_switch = ref None in
    let constructor_providers = ref 0 in
    Runtime.with_cold_episode
      ~test_root
      ~test_switch
      ~wrap_env:(fun env -> Cut.wrap cut (Inference_close_tests.Fault.wrap_env cache env))
      ~post_stream:(fun ~sw:_ ~inputs ->
        if constructor_transcript inputs then Int.incr constructor_providers;
        completed_response "601 provider must not run after lost claim reply")
      Legacy
      Continue
      (fun env
        daemon
        entry
        _old_runtime
        counters
        _probe
        session
        _admission
        _initial
        _stopped
        actor
        request ->
         let handle = Option.value_exn entry.Registry.store_handle in
         cache.cache
         <- Some
              (Filename.concat
                 (Store.Session_store.Handle.cache_directory handle)
                 "cache.bin");
         let anchor = snapshot_anchor env handle in
         Owner.ensure_loaded entry.runtime |> protocol_ok;
         Eio.Switch.run (fun sw ->
           Runtime.with_resource_retention ~sw entry.runtime Shared_resources (fun () ->
             Owner.unload_and_wait entry.runtime |> protocol_ok;
             let before = state entry in
             Cut.arm cut handle before;
             (* The real Actor was created on the daemon's original switch. Only
               observation/reporting is protected from that switch's fatal exit.
               The actual caller still receives the Actor's real failed reply. *)
             Eio.Cancel.protect (fun () ->
               let caller, _diagnostics =
                 Callback_retirement_fault_tests.capture_diagnostics env (fun () ->
                   capture (fun () ->
                     Runtime.Service.resume_episode
                       (Runtime.public daemon)
                       ~entry
                       ~actor
                       ~session
                       ~request))
               in
               let candidate = Option.value_exn cut.candidate in
               [%test_eq: int] 1 cut.writer_hits;
               [%test_eq: int] 1 cut.publication_hits;
               [%test_eq: int] 0 !constructor_providers;
               [%test_eq: int] 1 !(counters.providers);
               [%test_eq: int] 0 cache.cache_renames;
               assert_running candidate.job;
               (match caller with
                | Error _ | Ok (Error _) | Ok (Ok (Resume_failed _)) -> ()
                | Ok (Ok (Resumed _ | Retained _ | Rejected _ | Uncertain _)) ->
                  failwith "fatal positive claim lost its accepted constructor command");
               let actual_switch = Option.value_exn !test_switch in
               let rec await_fatal () =
                 match Eio.Switch.get_error actual_switch with
                 | None ->
                   Eio.Fiber.yield ();
                   await_fatal ()
                 | Some (Eio.Cancel.Cancelled cause) -> cause
                 | Some exn -> exn
               in
               let fatal = await_fatal () in
               let causes = Callback_retirement_fault_tests.fatal_causes fatal in
               (match causes with
                | [ actual ] -> assert (phys_equal actual (Option.value_exn cut.injected))
                | _ ->
                  raise_s [%sexp "unexpected fatal claim causes", (causes : exn list)]);
               (* No RPC or positive close follows the actual fatal owner. The
                 persisted claim below is an IO fact, never a runtime receipt. *)
               write_report
                 env
                 (candidate_fields cut
                  @ [ "kind", `String "claim"
                    ; "before", Existing_fault.state_json before
                    ; "initial_snapshot", anchor
                    ; "fatal", `String (Exn.to_string fatal)
                    ])))))
  ;;

  let recover env sw root report =
    let member name = Jsonaf.member_exn name report in
    let selected =
      member "transaction_payload"
      |> Jsonaf.string_exn
      |> Store.Transaction.decode
      |> Existing.store_ok
    in
    let job = member "job" |> P.Job.of_json |> protocol_ok in
    let before = Callback_retirement_fault_tests.decode_state (member "before") in
    assert (P.Id.Session.equal selected.session_id job.session_id);
    [%test_eq: string]
      (member "transaction_sha256" |> Jsonaf.string_exn)
      (Store.Transaction.hash selected);
    let terminal = String.equal (member "kind" |> Jsonaf.string_exn) "terminal" in
    let expected =
      if terminal
      then (
        let expected_error =
          Store.Store_error.Io
            { operation = "append journal segment"
            ; path = member "journal_path" |> Jsonaf.string_exn
            ; message = member "injected" |> Jsonaf.string_exn
            }
          |> Existing_fault.persistence_error
        in
        Existing_fault.same_error
          expected_error
          (member "cleanup" |> P.Error.of_json |> protocol_ok);
        Some
          (A.Session_persistence.apply_transaction
             ~limits:D.Limits.default
             (A.Session_persistence.Restored.authored before)
             selected
           |> Existing.store_ok
           |> A.Session_persistence.Restored.state))
      else (
        [%test_eq: string] "claim" (member "kind" |> Jsonaf.string_exn);
        [%test_eq: string]
          (Exn.to_string (Failure "601 positive Model claim publication sentinel"))
          (member "injected" |> Jsonaf.string_exn);
        [%test_eq: string]
          (member "injected" |> Jsonaf.string_exn)
          (member "fatal" |> Jsonaf.string_exn);
        assert_running job;
        None)
    in
    let store =
      Store.Session_store.open_existing
        ~env
        ~sw
        ~root:(Filename.concat root "data")
        ~process_start_identity:None
        ~lock_nonce:"601-constructor-physical-recovery"
      |> Existing.store_ok
    in
    protect_cleanup
      ~finally:(fun () -> Store.Session_store.close store |> Existing.store_ok)
      (fun () ->
         let handle =
           Store.Session_store.open_session
             store
             ~sw
             ~actor_lock_nonce:"601-recovered-constructor"
             selected.session_id
           |> Existing.store_ok
         in
         protect_cleanup
           ~finally:(fun () ->
             Store.Session_store.close_session store handle |> Existing.store_ok)
           (fun () ->
              assert (
                Jsonaf.exactly_equal
                  (member "initial_snapshot")
                  (snapshot_anchor env handle));
              let snapshot_directory =
                Store.Session_store.Handle.snapshot_directory handle
              in
              let snapshot =
                Store.Snapshot.load_current
                  ~env
                  ~directory:snapshot_directory
                  ~max_payload_length
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
                  ~max_payload_length
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
              let original = frames () in
              let selected_frames =
                List.filter_map original ~f:(fun (flags, payload) ->
                  if Int.equal flags 0
                  then (
                    let transaction =
                      Store.Transaction.decode payload |> Existing.store_ok
                    in
                    if
                      Int64.equal
                        selected.transaction_sequence
                        transaction.transaction_sequence
                    then Some transaction
                    else None)
                  else None)
              in
              let anchor_transaction =
                List.filter_map original ~f:(fun (flags, payload) ->
                  if Int.equal flags 0
                  then (
                    let transaction =
                      Store.Transaction.decode payload |> Existing.store_ok
                    in
                    if
                      Int64.equal
                        transaction.transaction_sequence
                        snapshot.snapshot.transaction_sequence
                    then Some transaction
                    else None)
                  else None)
                |> Runtime.one_actual "actual Factory Created snapshot journal anchor"
              in
              [%test_eq: string option]
                snapshot.snapshot.transaction_hash
                (Some (Store.Transaction.hash anchor_transaction));
              [%test_eq: string option] None anchor_transaction.previous_transaction_hash;
              [%test_eq: int64] 1L anchor_transaction.session_revision;
              (match changes anchor_transaction with
               | [ A.Session_delta.Created _ ] -> ()
               | _ -> failwith "Factory first snapshot no longer anchors actual Created");
              let actual =
                Runtime.one_actual "exact recovered Model transaction" selected_frames
              in
              [%test_eq: string]
                (Store.Transaction.encode selected)
                (Store.Transaction.encode actual);
              let read () =
                Store.Recovery.read
                  ~env
                  ~journal
                  ~snapshot_directory
                  ~max_snapshot_payload_length:max_payload_length
                  ~session_id:selected.session_id
                  ~initial
                  ~restore_snapshot:
                    (A.Session_persistence.restore_snapshot ~limits:D.Limits.default)
                  ~apply:
                    (A.Session_persistence.apply_transaction ~limits:D.Limits.default)
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
              [%test_eq: int64]
                selected.session_revision
                recovered.latest_session_revision;
              let current = A.Session_persistence.Restored.state recovered.state in
              Option.iter expected ~f:(fun expected ->
                assert (
                  Jsonaf.exactly_equal
                    (Existing_fault.state_json expected)
                    (Existing_fault.state_json current)));
              assert (
                Jsonaf.exactly_equal
                  (P.Job.to_json job)
                  (P.Job.to_json (actual_job current job)));
              if terminal
              then (
                let proof = terminal_run_proof selected job in
                assert (
                  List.exists (proofs current job) ~f:(P.Run_work.Terminal.equal proof)))
              else (
                assert_running (actual_job current job);
                let work = A.Run_job_occurrence.work job |> protocol_ok in
                assert (
                  List.exists
                    (Option.to_list current.run_state
                     |> List.concat_map ~f:A.Run_state.runs)
                    ~f:(fun run ->
                      List.exists run.P.Run.owned_work ~f:(P.Run_work.equal work)));
                assert (List.is_empty (proofs current job));
                assert (
                  Jsonaf.exactly_equal
                    (ledger_json before.inference_ledger)
                    (ledger_json current.inference_ledger));
                assert (
                  A.Canonical_usage_fold.Marks.equal
                    (Inference_close_tests.marks before)
                    (Inference_close_tests.marks current));
                let callback =
                  List.filter current.moderator_executions ~f:(fun receipt ->
                    not
                      (List.exists before.moderator_executions ~f:(fun old ->
                         P.Id.Moderator_execution.equal
                           old.P.Moderator_execution.context.id
                           receipt.context.id)))
                  |> Runtime.one_actual "recovered callback before lost claim reply"
                in
                assert (
                  P.Moderator_execution.equal_phase Session_resume callback.context.phase);
                assert (P.Moderator_execution.equal_status Running callback.status));
              assert (
                Jsonaf.exactly_equal
                  (Existing_fault.state_json current)
                  (Existing_fault.state_json
                     (A.Session_persistence.Restored.state (read ()).state)));
              [%test_eq: (int * string) list] original (frames ());
              assert (
                Jsonaf.exactly_equal
                  (member "initial_snapshot")
                  (snapshot_anchor env handle))))
  ;;
end

let run_terminal_child = Physical.run_terminal_child
let run_claim_publication_child = Physical.run_claim_publication_child

let%expect_test
    "returned constructor Model terminal uncertainty recovers exact result without replay"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
             let report =
               Callback_retirement_fault_tests.child_report
                 ~mode:"constructor-model-terminal-unknown"
                 env
                 root
             in
             Physical.recover env sw root report))));
  print_endline
    "returned Model result and atomic Run proof recovered exactly; unknown writer never \
     resubmitted";
  [%expect
    {| returned Model result and atomic Run proof recovered exactly; unknown writer never resubmitted |}]
;;

let%expect_test
    "fatal positive constructor claim retains durable Running without provider execution"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root))
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 90. (fun () ->
             let report =
               Callback_retirement_fault_tests.child_report
                 ~mode:"constructor-model-claim-publication"
                 env
                 root
             in
             Physical.recover env sw root report))));
  print_endline
    "actual claim ACK recovered after fatal publication; joined caller and zero \
     constructor provider";
  [%expect
    {| actual claim ACK recovered after fatal publication; joined caller and zero constructor provider |}]
;;
