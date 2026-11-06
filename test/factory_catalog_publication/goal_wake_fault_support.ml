open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Resume = Goal_resume_runtime_tests
module G = P.Goal

(* Test-only exact journal cut selection and full physical recovery assertions.
   This module has no dependency on Goal_wake_runtime_tests. *)
module Cut = struct
  type t =
    | Before_sync
    | After_sync
  [@@deriving sexp_of]

  let boundary = function
    | Before_sync -> Crash_fault_io.Before_sync
    | After_sync -> Crash_fault_io.After_sync
  ;;
end

module State = struct
  type selection =
    { state : A.Session_state.t
    ; admission : P.Run_receipt.t
    ; wake : P.Run_wake.t
    }

  type candidate =
    { transaction : Agent_store.Transaction.t
    ; executing : P.Moderator_execution.t
    ; index : A.Run_state.t
    }

  type t =
    { mutable selected : selection option
    ; mutable candidate : candidate option
    ; mutable failure : exn option
    ; mutable hits : int
    }

  let create () = { selected = None; candidate = None; failure = None; hits = 0 }

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
         if P.Id.Session.equal transaction.session_id selected.state.identity.session_id
         then (
           let delta =
             A.Session_delta_document.decode
               ~limits:Document_schema.Limits.default
               transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           let changes = Goal_admission_fault_tests.atoms delta in
           let executing =
             List.find_map changes ~f:(function
               | A.Session_delta.Moderator_execution_changed receipt ->
                 (match receipt.context.phase, receipt.status with
                  | Internal_event, Running -> Some receipt
                  | ( ( Session_start
                      | Session_resume
                      | Turn_start
                      | Message_appended
                      | Pre_tool_call
                      | Post_tool_response
                      | Turn_end
                      | Internal_event )
                    , (Running | Completed _ | Failed _ | Interrupted _) ) -> None)
               | _ -> None)
           in
           let index =
             List.find_map changes ~f:(function
               | A.Session_delta.Run_state_changed index -> Some index
               | _ -> None)
           in
           match executing, index with
           | Some executing, Some index ->
             let consumed =
               List.exists (A.Run_state.intents index) ~f:(fun intent ->
                 match intent.action, intent.disposition with
                 | Wait wake, Consumed None -> P.Run_wake.equal wake selected.wake
                 | (Continue | Wait _ | Finish _), (Pending | Consumed _ | Retired) ->
                   false)
             in
             let same_run =
               Option.exists
                 (A.Run_state.find index selected.admission.run_id)
                 ~f:(fun run ->
                   P.Run.Lifecycle.equal run.lifecycle Active
                   && P.Run_source.equal run.source selected.admission.source
                   && P.Id.Principal.equal
                        run.principal_id
                        selected.admission.principal_id
                   && Option.is_some (A.Run_state.goal_link index run.id)
                   (* V1 claims have no legacy queue_occurrence. Match the actual
                      private timer frame using the canonical pure decoder and
                      the authentic retained producer snapshot instead. This is
                      journal selection DATA, never execution authority. *)
                   && protocol_ok
                        (A.Run_wake_owner.claim_matches
                           selected.state
                           ~run
                           ~wake:selected.wake
                           ~executing)
                   && List.exists run.owned_work ~f:(fun work ->
                     match work.P.Run_work.key with
                     | Retained (Moderator_execution id) ->
                       P.Id.Moderator_execution.equal id executing.context.id
                     | Operation _ | Retained _ -> false))
             in
             if consumed && same_run
             then (
               t.selected <- None;
               t.candidate <- Some { transaction; executing; index };
               t.hits <- t.hits + 1;
               let failure =
                 Core_unix.Unix_error (EIO, "545 Goal wake claim sync", path)
               in
               t.failure <- Some failure;
               raise failure)
           | Some _, None | None, (Some _ | None) -> ())
       | Some _ | None -> ())
  ;;
end

exception Observed_claim_fault of exn

let assert_no_queued_effect (current : A.Session_state.t) before counters =
  [%test_eq: int] 1 !(counters.providers);
  assert (
    List.equal P.Invocation.equal before.A.Session_state.invocations current.invocations);
  assert (A.Goal_accounting.equal (Goal.accounting before) (Goal.accounting current));
  assert (Option.is_none current.active_operation)
;;

let reopen_claim_fault
      ~cut
      ~(recovery_env : Eio_unix.Stdenv.base)
      ~before_reopen
      env
      sw
      root
      daemon
      policy
      entry
      counters
      admission
      cold
      wake
      candidate
  =
  let old_store = Daemon.store daemon in
  Daemon.shutdown daemon |> protocol_ok;
  assert (Agent_store.Session_store.is_closed old_store);
  let recovered =
    Daemon.start
      ~sw
        (* Recovery must retain the same advanced wall/monotonic clock view.
         The original fixture environment still owns its real timeout and FS;
         using its wall clock here would rewind committed Run timestamps. *)
      ~env:recovery_env
      ~config:(config root root (Filename.concat root "parent.chatmd"))
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { Daemon.default_options with
          inference_policy = policy
        ; qualify_chatml_extensions = true
        }
      ()
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
    (fun () ->
       assert (not (phys_equal old_store (Daemon.store recovered)));
       let loaded =
         Registry.load
           (Daemon.registry recovered)
           cold.A.Session_state.identity.session_id
         |> protocol_ok
       in
       assert (not (phys_equal entry.Registry.actor loaded.actor));
       let current = Retry_rotation_tests.await_recovered loaded before_reopen in
       let run = Resume.run current admission in
       (match run.lifecycle with
        | Terminal Interrupted -> ()
        | Admitted | Active | Waiting _ | Terminal _ ->
          raise_s [%sexp "recovered failed claim Run is not interrupted", (run : P.Run.t)]);
       Resume.assert_episode cold current admission;
       assert_no_queued_effect current cold counters;
       assert (G.State.equal (Open (Held Source_review)) (G.state (Goal.goal current)));
       let claimed =
         List.find current.moderator_executions ~f:(fun receipt ->
           P.Id.Moderator_execution.equal
             receipt.context.id
             candidate.State.executing.context.id)
       in
       (match cut with
        | Cut.Before_sync -> ()
        | After_sync -> assert (Option.is_some claimed));
       (* A missing writer ACK is uncertainty, never a fictitious rollback. If the
         candidate survived replay, its exact occurrence may only be interrupted. *)
       Option.iter claimed ~f:(fun actual ->
         let written =
           A.Run_state.find candidate.State.index run.id |> Option.value_exn
         in
         assert (List.equal P.Run_work.equal written.owned_work run.owned_work);
         List.iter written.terminal_work ~f:(fun proof ->
           assert (List.mem run.terminal_work proof ~equal:P.Run_work.Terminal.equal));
         let work =
           P.Run_work.create
             ~key:(Retained (Moderator_execution actual.context.id))
             ~generation:actual.context.generation
           |> protocol_ok
         in
         let proof =
           List.filter run.terminal_work ~f:(fun proof ->
             P.Run_work.equal proof.P.Run_work.Terminal.work work)
           |> Resume.one_actual "recovered unacknowledged callback proof"
         in
         assert (P.Run_work.Terminal.equal_outcome Unconfirmed proof.outcome);
         assert (
           P.Moderator_execution.equal_context actual.context candidate.executing.context);
         assert (
           Option.equal
             P.Moderator_execution.Queue_occurrence.equal
             actual.queue_occurrence
             candidate.executing.queue_occurrence);
         match actual.status with
         | Interrupted _ -> ()
         | Running | Completed _ | Failed _ ->
           failwith "unacknowledged queued claim became an executing/successful callback");
       let stable = Resume.settled_snapshot loaded in
       let frames = Existing.frames env loaded in
       let rec reject_replay () =
         match Factory.deliver_goal_wake (Daemon.factory recovered) loaded ~wake with
         | Error error when P.Error.equal_code Conflict error.code && error.retryable ->
           (* The actual scheduler may briefly own the source lane after recovery.
              Retry only that transient exclusion; it cannot grant old custody. *)
           Eio.Fiber.yield ();
           reject_replay ()
         | Error error -> raise_s [%sexp (error : P.Error.t)]
         | Ok No_work -> ()
         | Ok (Suspended_goal _ | Handed_off _ | Controlled _) ->
           failwith "recovery recreated old Goal wake custody"
       in
       reject_replay ();
       Existing.assert_same stable loaded;
       [%test_eq: string list] frames (Existing.frames env loaded);
       [%test_eq: int] 1 !(counters.providers))
;;
