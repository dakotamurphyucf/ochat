open! Core
open Agent_server_test_support
open Retry_support
module Runtime = Goal_resume_runtime_tests
module Goal = Goal_service_tests
module Staging = Goal_resume_tests
module B = A.Runtime_builder

module Ack_cut = struct
  type selection =
    { session : P.Id.Session.t
    ; live : bool ref
    }

  type captured =
    { receipt : P.Moderator_execution.t
    ; checkpoint : Jsonaf.t option
    }

  type t =
    { mutable selected : selection option
    ; mutable captured : captured option
    ; mutable hits : int
    }

  let create () = { selected = None; captured = None; hits = 0 }

  let reached t env path =
    match t.selected with
    | None -> ()
    | Some selected ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
           |> Existing.store_ok
         in
         if P.Id.Session.equal selected.session transaction.session_id
         then (
           let changes =
             A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
             |> Goal_admission_fault_tests.atoms
           in
           let receipt =
             List.find_map changes ~f:(function
               | A.Session_delta.Moderator_execution_changed
                   ({ context = { phase = Session_resume; _ }; status = Completed _; _ }
                    as receipt) -> Some receipt
               | _ -> None)
           in
           let checkpoint =
             List.find_map changes ~f:(function
               | A.Session_delta.Moderator_changed checkpoint -> Some checkpoint
               | _ -> None)
           in
           match receipt, checkpoint with
           | Some receipt, Some checkpoint ->
             t.selected <- None;
             t.hits <- t.hits + 1;
             t.captured <- Some { receipt; checkpoint };
             (* This exact writer has synced. Retire only the genuine request
                authority; preserve its ACK and every actual callback fact. *)
             selected.live := false
           | None, _ | Some _, None -> ())
       | Some _ | None -> ())
  ;;

  let wrap_env t env =
    Crash_fault_io.wrap
      env
      ~boundary:After_sync
      ~matches:(fun path ->
        String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(reached t env)
  ;;
end

let resumes current =
  List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
    P.Moderator_execution.equal_phase
      receipt.P.Moderator_execution.context.phase
      Session_resume)
;;

let%expect_test
    "scoped legacy ACK survives failed install and a fresh binding without callback \
     replay"
  =
  let cut = Ack_cut.create () in
  Runtime.with_cold_episode
    ~wrap_env:(Ack_cut.wrap_env cut)
    Legacy
    Continue
    (fun
        env
         daemon
         entry
         _old_runtime
         counters
         probe
         session
         admission
         _initial
         stopped
         _actor
         request
       ->
       (* Ordinary cold loading retains the actual pending Installed_resume. It
         does not execute it: the Actor is genuinely stopped and no claim exists. *)
       Owner.ensure_loaded entry.Registry.runtime |> protocol_ok;
       let pending =
         Owner.with_loaded_runtime entry.runtime (fun runtime -> Ok runtime)
         |> protocol_ok
       in
       let activation = Option.value_exn pending.B.moderator_activation in
       assert (activation.pending ());
       assert (not (activation.startup_pending ()));
       assert (List.is_empty (resumes (state entry)));
       let manager = Option.value_exn pending.moderator_manager in
       let native = Option.value_exn pending.native_runtime in
       let tools = Option.value_exn pending.moderator_script_tools in
       Eio.Switch.run (fun sw ->
         Runtime.with_resource_retention ~sw entry.runtime Shared_resources (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok;
           assert (not (Owner.is_loaded entry.runtime));
           let live = ref true in
           let first_actor = Staging.guarded Staging.resume_principal live in
           cut.selected <- Some { session = stopped.identity.session_id; live };
           let first_receipt, primary =
             match
               Runtime.Service.resume_episode
                 (Runtime.public daemon)
                 ~entry
                 ~actor:first_actor
                 ~session
                 ~request
               |> protocol_ok
             with
             | Resume_failed { receipt; error } -> receipt, error
             | Rejected error | Uncertain error ->
               raise_s
                 [%sexp "pending constructor lost its command ACK", (error : P.Error.t)]
             | Resumed _ | Retained _ -> failwith "retired request installed a runtime"
           in
           if not (Int.equal cut.hits 1)
           then
             raise_s
               [%sexp
                 "expected callback ACK cut was not reached"
               , (primary : P.Error.t)
               , (cut.hits : int)];
           assert (not !live);
           [%test_eq: P.Error.code] Permission_denied primary.code;
           Staging.assert_receipt request stopped first_receipt;
           let captured = Option.value_exn cut.captured in
           let acknowledged = state entry in
           (match resumes acknowledged with
            | [ actual ] -> assert (P.Moderator_execution.equal captured.receipt actual)
            | actual -> raise_s [%sexp (actual : P.Moderator_execution.t list)]);
           assert (
             Option.equal Jsonaf.exactly_equal captured.checkpoint acknowledged.moderator);
           assert (not (activation.pending ()));
           assert (not (Owner.is_loaded entry.runtime));
           [%test_eq: int] 2 !(counters.providers);
           let ports = !(counters.inference_ports) in
           (* A fresh command requires a genuinely held episode, not a fabricated
             retry token or an old receipt presented as permission. *)
           Goal_admitted_guard_tests.command
             daemon
             entry
             ~key:"591-post-ack-hold"
             (Hold (Goal.goal_id, Operator_pause))
           |> protocol_ok
           |> ignore;
           let held = state entry in
           let request = Staging.exact_request held ~key:"591-post-ack-new-binding" in
           let actor = Staging.guarded Staging.resume_principal (ref true) in
           let receipt, operation =
             Runtime.Service.resume_episode
               (Runtime.public daemon)
               ~entry
               ~actor
               ~session
               ~request
             |> protocol_ok
             |> Runtime.started
           in
           Eio.Promise.await probe.entered;
           Owner.with_loaded_runtime entry.runtime (fun actual ->
             assert (not (phys_equal pending.worker actual.worker));
             assert (phys_equal manager (Option.value_exn actual.moderator_manager));
             assert (phys_equal native (Option.value_exn actual.native_runtime));
             assert (phys_equal tools (Option.value_exn actual.moderator_script_tools));
             Ok ())
           |> protocol_ok;
           let before_response = state entry in
           assert (
             List.equal
               P.Moderator_execution.equal
               (resumes acknowledged)
               (resumes before_response));
           assert (
             Option.equal
               Jsonaf.exactly_equal
               captured.checkpoint
               before_response.moderator);
           [%test_eq: int64]
             acknowledged.conversation.next_history_sequence
             before_response.conversation.next_history_sequence;
           [%test_eq: int] 2 !(counters.providers);
           [%test_eq: int] ports !(counters.inference_ports);
           Runtime.Provider_probe.release probe;
           let final = Runtime.await_turns env entry counters ~providers:3 ~completed:2 in
           Runtime.assert_started ~attempts:3L held final admission operation;
           assert (
             List.equal P.Moderator_execution.equal (resumes acknowledged) (resumes final));
           [%test_eq: int] ports !(counters.inference_ports);
           Runtime.repeat daemon entry session actor request receipt counters env)));
  print_endline
    "actual scoped ACK retained across rejected installation; fresh worker joins without \
     callback replay";
  [%expect
    {| actual scoped ACK retained across rejected installation; fresh worker joins without callback replay |}]
;;

let%expect_test
    "failed retained legacy constructor preserves callback and graph facts without \
     releasing shared resources"
  =
  let module Fault = Inference_close_tests.Fault in
  let module Ledger = A.Inference_ledger in
  let module Observation = Inference.Observation in
  let module Store = Agent_store in
  let fault = Fault.create () in
  let constructor_providers = ref 0 in
  let constructor_primary = ref None in
  let original_cleanup = ref None in
  let observed_guards = ref None in
  let assert_disposal_guard error =
    let expected =
      P.Error.create
        Conflict
        ~message:"background execution still owns the loaded runtime"
        ~retryable:true
        ()
    in
    assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json error))
  in
  let shutdown_report = ref None in
  Runtime.with_cold_episode
    ~observe_shutdown:(fun report ->
      assert (Option.is_none !shutdown_report);
      shutdown_report := Some report)
    ~wrap_env:(Fault.wrap_env fault)
    ~post_stream:(fun ~sw:_ ~inputs ->
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      if String.is_substring transcript ~substring:Runtime.constructor_input
      then (
        Int.incr constructor_providers;
        (* The genuine current Init graph has already acknowledged Running.
           Reject terminal/release and callback settlement before their writers. *)
        fault.phase <- Reject_settlement);
      completed_response "600 actual retained constructor provider")
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
       let store = Daemon.store daemon in
       let handle = Option.value_exn entry.Registry.store_handle in
       fault.marker
       <- Some
            (Filename.concat
               (Store.Data_root.indexes_path (Store.Session_store.data_root store))
               "sessions.recovery-required");
       fault.cache
       <- Some
            (Filename.concat
               (Store.Session_store.Handle.cache_directory handle)
               "cache.bin");
       let disarm () = fault.phase <- Quiet in
       protect_cleanup ~finally:disarm (fun () ->
         Owner.ensure_loaded entry.Registry.runtime |> protocol_ok;
         let pending =
           Owner.with_loaded_runtime entry.runtime (fun runtime -> Ok runtime)
           |> protocol_ok
         in
         let activation = Option.value_exn pending.B.moderator_activation in
         assert (activation.pending ());
         assert (not (activation.startup_pending ()));
         let retirement_error =
           Eio.Switch.run (fun sw ->
             Runtime.with_resource_retention ~sw entry.runtime Shared_resources (fun () ->
               Owner.unload_and_wait entry.runtime |> protocol_ok;
               assert (not (Owner.is_loaded entry.runtime));
               [%test_eq: int] 0 fault.cache_renames;
               let before = state entry in
               let before_marks = Inference_close_tests.marks before in
               let original_jobs = List.map before.jobs ~f:(fun job -> job.P.Job.id) in
               let original_receipts =
                 List.map before.moderator_executions ~f:(fun receipt ->
                   receipt.P.Moderator_execution.context.id)
               in
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
                       "retained constructor did not reach its command ACK"
                     , (error : P.Error.t)]
                 | Resumed _ | Retained _ ->
                   failwith "failed retained constructor installed a runtime"
               in
               constructor_primary := Some primary;
               [%test_eq: P.Error.code] Invalid_state primary.code;
               [%test_eq: bool] false primary.retryable;
               assert (
                 String.is_substring
                   primary.message
                   ~substring:"566 prewriter projection read");
               assert (fault.settlement_rejections > 0);
               [%test_eq: int] 1 !constructor_providers;
               [%test_eq: int] 2 !(counters.providers);
               assert (not (activation.pending ()));
               assert (not (Owner.is_loaded entry.runtime));
               [%test_eq: int] 0 fault.cache_renames;
               let failed = state entry in
               let running =
                 List.filter (Ledger.rows failed.inference_ledger) ~f:(fun row ->
                   match Observation.Attempt_record.state (Ledger.Row.record row) with
                   | Running -> true
                   | Prepared | Terminal _ | Interrupted _ -> false)
                 |> Runtime.one_actual "actual retained constructor native registration"
               in
               let native = Ledger.Row.handle running in
               let constructor_job =
                 List.filter failed.jobs ~f:(fun job ->
                   not (List.mem original_jobs job.P.Job.id ~equal:P.Id.Job.equal))
                 |> Runtime.one_actual "actual newly admitted constructor Model Job"
               in
               [%test_eq: P.Job.kind] Model_call constructor_job.kind;
               assert (P.Job.equal_delivery Not_required constructor_job.delivery);
               [%test_eq: int] 1 constructor_job.attempt;
               (match constructor_job.retry_policy with
                | Never -> ()
                | Safe_retry _ | Idempotent _ ->
                  failwith "constructor Job acquired retry policy");
               (match constructor_job.status with
                | Running -> ()
                | Queued
                | Waiting_permission _
                | Waiting_completion _
                | Succeeded
                | Failed _
                | Cancelled
                | Interrupted _ ->
                  failwith "rejected Model settlement changed its actual Running claim");
               assert (Option.is_some constructor_job.started_at);
               assert (Option.is_none constructor_job.completed_at);
               assert (Option.is_none constructor_job.result);
               let constructor_work =
                 A.Run_job_occurrence.work constructor_job |> protocol_ok
               in
               let callback =
                 List.filter failed.moderator_executions ~f:(fun receipt ->
                   not
                     (List.mem
                        original_receipts
                        receipt.P.Moderator_execution.context.id
                        ~equal:P.Id.Moderator_execution.equal))
                 |> Runtime.one_actual "actual retained constructor callback"
               in
               assert (
                 P.Moderator_execution.equal_phase Session_resume callback.context.phase);
               (match callback.status with
                | Running -> ()
                | Completed _ | Failed _ | Interrupted _ ->
                  failwith "rejected callback terminal writer installed a receipt");
               let admitted = Inference_close_tests.marks failed in
               [%test_eq: int64] (Int64.succ before_marks.attempt) admitted.attempt;
               [%test_eq: int64] before_marks.turn admitted.turn;
               let ports = !(counters.inference_ports) in
               (* This public strong retirement retains the live resource loan.
                OCH-599 must retry the exact ended receipt before replacement;
                OCH-600 must retain the separate current Init graph. *)
               let retirement_error =
                 match Owner.unload_and_wait entry.runtime with
                 | Error error ->
                   [%test_eq: P.Error.code] Persistence_error error.code;
                   assert (
                     String.is_substring
                       error.message
                       ~substring:"566 prewriter projection read");
                   error
                 | Ok () -> failwith "failed constructor cleanup was reported joined"
               in
               let retained = state entry in
               let same_callback =
                 List.find_exn retained.moderator_executions ~f:(fun receipt ->
                   P.Id.Moderator_execution.equal
                     callback.context.id
                     receipt.P.Moderator_execution.context.id)
               in
               assert (P.Moderator_execution.equal callback same_callback);
               let same_job =
                 List.find_exn retained.jobs ~f:(fun job ->
                   P.Id.Job.equal job.id constructor_job.id)
               in
               assert (
                 Jsonaf.exactly_equal
                   (P.Job.to_json constructor_job)
                   (P.Job.to_json same_job));
               (match Owner.unload entry.runtime with
                | Error error -> [%test_eq: P.Error.code] Conflict error.code
                | Ok () ->
                  failwith "finite unload detached pending constructor Job custody");
               let same_native =
                 Ledger.find
                   retained.inference_ledger
                   ~ordinal:(Ledger.Handle.ordinal native)
                 |> Option.value_exn
               in
               assert (Ledger.Handle.equal native (Ledger.Row.handle same_native));
               (match
                  Observation.Attempt_record.state (Ledger.Row.record same_native)
                with
                | Running -> ()
                | Prepared | Terminal _ | Interrupted _ ->
                  failwith "failed factual finish changed the acknowledged native row");
               [%test_eq: int] 0 fault.cache_renames;
               [%test_eq: int] ports !(counters.inference_ports);
               [%test_eq: int] 2 !(counters.providers);
               disarm ();
               Owner.unload_and_wait entry.runtime |> protocol_ok;
               Owner.unload_and_wait entry.runtime |> protocol_ok;
               let final = state entry in
               assert (
                 A.Canonical_usage_fold.Marks.equal
                   admitted
                   (Inference_close_tests.marks final));
               let ended =
                 Ledger.find
                   final.inference_ledger
                   ~ordinal:(Ledger.Handle.ordinal native)
                 |> Option.value_exn
               in
               assert (Ledger.Handle.equal native (Ledger.Row.handle ended));
               (match Observation.Attempt_record.state (Ledger.Row.record ended) with
                | Interrupted { reason = Host_interrupted; delivery = Response_started }
                  -> ()
                | Prepared | Running | Terminal _ | Interrupted _ ->
                  raise_s
                    [%sexp
                      "same current Init graph was not factually joined"
                    , (Observation.Attempt_record.to_json (Ledger.Row.record ended)
                       : Jsonaf.t)]);
               let completed_job =
                 List.find_exn final.jobs ~f:(fun job ->
                   P.Id.Job.equal job.id constructor_job.id)
               in
               [%test_eq: int] constructor_job.generation completed_job.generation;
               [%test_eq: int] constructor_job.attempt completed_job.attempt;
               assert (
                 P.Id.Session.equal constructor_job.session_id completed_job.session_id);
               assert (Jsonaf.exactly_equal constructor_job.payload completed_job.payload);
               assert (
                 Option.equal
                   P.Job.equal_launch
                   constructor_job.launch
                   completed_job.launch);
               assert (
                 Option.equal
                   P.Inference_origin.equal
                   constructor_job.inference_origin
                   completed_job.inference_origin);
               assert (
                 Option.equal
                   P.Timestamp.equal
                   constructor_job.started_at
                   completed_job.started_at);
               assert (Option.is_some completed_job.completed_at);
               assert (Option.is_none completed_job.result);
               (match completed_job.status with
                | Failed error ->
                  [%test_eq: P.Error.code] Internal_error error.code;
                  assert (
                    String.is_substring
                      error.message
                      ~substring:"566 prewriter projection read")
                | Queued
                | Running
                | Waiting_permission _
                | Waiting_completion _
                | Succeeded
                | Cancelled
                | Interrupted _ ->
                  failwith
                    "actual Model failure was lost or rewritten as generic interruption");
               let job_proofs =
                 Option.to_list final.run_state
                 |> List.concat_map ~f:(fun index -> A.Run_state.runs index)
                 |> List.concat_map ~f:(fun run -> run.P.Run.terminal_work)
                 |> List.filter ~f:(fun proof ->
                   P.Run_work.equal proof.P.Run_work.Terminal.work constructor_work)
               in
               let proof =
                 Runtime.one_actual "actual constructor Job Run proof" job_proofs
               in
               [%test_eq: P.Run_work.Terminal.outcome] Failed proof.outcome;
               Owner.unload_and_wait entry.runtime |> protocol_ok;
               let repeated = state entry in
               let repeated_job =
                 List.find_exn repeated.jobs ~f:(fun job ->
                   P.Id.Job.equal job.id constructor_job.id)
               in
               assert (
                 Jsonaf.exactly_equal
                   (P.Job.to_json completed_job)
                   (P.Job.to_json repeated_job));
               let repeated_proofs =
                 Option.to_list repeated.run_state
                 |> List.concat_map ~f:(fun index -> A.Run_state.runs index)
                 |> List.concat_map ~f:(fun run -> run.P.Run.terminal_work)
                 |> List.filter ~f:(fun actual ->
                   P.Run_work.equal actual.P.Run_work.Terminal.work constructor_work)
               in
               assert (List.equal P.Run_work.Terminal.equal job_proofs repeated_proofs);
               let terminal =
                 List.find_exn final.moderator_executions ~f:(fun receipt ->
                   P.Id.Moderator_execution.equal
                     callback.context.id
                     receipt.P.Moderator_execution.context.id)
               in
               assert (
                 P.Moderator_execution.equal_context callback.context terminal.context);
               (match terminal.status with
                | Failed error ->
                  [%test_eq: string] "event.handler_failed" error.code;
                  [%test_eq: bool] false error.retryable
                | Running | Completed _ | Interrupted _ ->
                  failwith "ended retained callback did not settle its selected failure");
               assert (not (Owner.is_loaded entry.runtime));
               [%test_eq: int] 0 fault.cache_renames;
               [%test_eq: int] 1 !constructor_providers;
               [%test_eq: int] 2 !(counters.providers);
               [%test_eq: int] ports !(counters.inference_ports);
               retirement_error))
         in
         (* The former last resource loan now leaves. Physical release is once.
            Strict close still rejects the retained original diagnostic barrier;
            actual reported disposal settles custody without erasing that failure. *)
         [%test_eq: int] 1 fault.cache_renames;
         (* Exercise the actual Registry rollback API on its SAME registered
            Entry and Handle. A failed Strict runtime prefix must be retained
            before any Entry closer can destroy the Actor or writer. This is
            transport qualification, not a claim that startup ran again. *)
         let registry = Daemon.registry daemon in
         let session_id = P.Session_ref.session_id session in
         assert (not (Registry.has_pending_cleanup registry session_id));
         Registry.rollback_recovered
           registry
           ~primary:
             (Registry.Cleanup_failure.rejected (Option.value_exn !constructor_primary))
           [ entry ];
         assert (Registry.has_pending_cleanup registry session_id);
         assert (Registry.retains_cleanup_handle registry handle);
         let retained = Registry.find registry session_id |> Option.value_exn in
         assert (phys_equal retained.actor entry.actor);
         assert (phys_equal retained.runtime entry.runtime);
         assert (phys_equal (Option.value_exn retained.store_handle) handle);
         Actor.state entry.actor |> protocol_ok |> ignore;
         Store.Session_store.Handle.metadata_checked handle |> Existing.store_ok |> ignore;
         (* Keep the original Strict -> Reported -> legacy sequence: delivering
            a current Conflict must not consume the historical cleanup error. *)
         let strict_guard =
           match Owner.close_and_wait entry.runtime with
           | () -> failwith "strict close ignored the original cleanup barrier"
           | exception Owner.Cleanup_failed error ->
             assert_disposal_guard error;
             error
         in
         let disposed = Owner.close_and_wait_reported entry.runtime in
         assert (
           Owner.Disposal_report.equal_disposition
             Joined
             (Owner.Disposal_report.disposition disposed));
         let assert_fault error =
           [%test_eq: P.Error.code] Persistence_error error.P.Error.code;
           [%test_eq: bool] false error.retryable;
           let marker = Option.value_exn fault.marker in
           let actual_fault =
             Store.Store_error.Io
               { operation = "read projection recovery marker"
               ; path = marker
               ; message =
                   Exn.to_string
                     (Core_unix.Unix_error (EIO, "566 prewriter projection read", marker))
               }
           in
           let expected =
             P.Error.create
               Persistence_error
               ~message:(Sexp.to_string_hum (Store.Store_error.sexp_of_t actual_fault))
               ~retryable:false
               ()
           in
           assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json error))
         in
         (* The unload can execute another terminal-write attempt under the
            still-armed fault. Its error and the original Init error share fault
            inputs, not producer identity. Validate each actual fault separately. *)
         assert_fault retirement_error;
         (* Each close attempt executes its own complete-resource guard.
            The current selected origin precedes the source original and the
            retained predecessor guard; equal payloads do not merge producers. *)
         let reported_guard, selected_cleanup =
           match Owner.Disposal_report.failures disposed with
           | [ Protocol guard; Protocol error; Protocol old_guard ] ->
             assert_disposal_guard guard;
             assert (not (phys_equal guard strict_guard));
             assert (phys_equal old_guard strict_guard);
             assert_fault error;
             guard, error
           | _ -> failwith "settled disposal changed actual guard/callback provenance"
         in
         observed_guards := Some (reported_guard, strict_guard);
         original_cleanup := Some selected_cleanup;
         assert (phys_equal disposed (Owner.close_and_wait_reported entry.runtime));
         (* Reported reads consume nothing. The newly executed guard is the
            first undelivered group; the earlier Strict call consumed only its
            own guard. The callback remains immutable report data throughout. *)
         (match Owner.close_and_wait entry.runtime with
          | () -> failwith "reported read consumed the original cleanup diagnostic"
          | exception Owner.Cleanup_failed error ->
            assert (phys_equal reported_guard error));
         Owner.close_and_wait entry.runtime;
         [%test_eq: int] 1 fault.cache_renames;
         [%test_eq: int] 1 !constructor_providers;
         [%test_eq: int] 2 !(counters.providers)));
  let report = Option.value_exn !shutdown_report in
  assert (Agent_server.Session_disposal_report.disposed report);
  let original = Option.value_exn !original_cleanup in
  let reported_guard, strict_guard = Option.value_exn !observed_guards in
  (* Registry retained the actual earlier rollback Strict report before the
     explicit Strict/reported calls. Its original precedes prepared reports. *)
  (match Agent_server.Session_disposal_report.failures report with
   | [ Agent_session.Callback_cleanup.Failure.Protocol rollback_guard
     ; Protocol actual
     ; Protocol actual_reported_guard
     ; Protocol actual_strict_guard
     ] ->
     assert_disposal_guard rollback_guard;
     assert (not (phys_equal rollback_guard reported_guard));
     assert (not (phys_equal rollback_guard strict_guard));
     assert (phys_equal actual original);
     assert (phys_equal actual_reported_guard reported_guard);
     assert (phys_equal actual_strict_guard strict_guard)
   | _ -> failwith "Registry disposal changed actual rollback/close provenance");
  Fault.assert_settlement_diagnostic fault [%expect.output];
  print_endline
    "same failed callback and Init graph settled; shared physical resources released \
     once without constructor replay";
  [%expect
    {| same failed callback and Init graph settled; shared physical resources released once without constructor replay |}]
;;
