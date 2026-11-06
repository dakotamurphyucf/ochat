open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Adapter = Descendant_turn_adapter_tests
module GD = A.Goal_descendants
module Pending = A.Goal_descendant_pending
module Origin = Agent_store.Delegation_goal_origin
module Store = Agent_store

module Close_read_fault = struct
  type phase =
    | Disabled
    | Await_reserve
    | Await_refresh
    | Parked_refresh
    | Reject_close

  type t =
    { caller_key : unit Eio.Fiber.key
    ; mutable phase : phase
    ; mutable selected : (P.Id.Session.t * Origin.Scope.t) option
    ; parked : unit Eio.Promise.t
    ; signal_parked : unit Eio.Promise.u
    ; never : unit Eio.Promise.t
    ; mutable failed_reads : int
    }

  let create () =
    let parked, signal_parked = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    { caller_key = Eio.Fiber.create_key ()
    ; phase = Disabled
    ; selected = None
    ; parked
    ; signal_parked
    ; never
    ; failed_reads = 0
    }
  ;;

  let select t ~root ~scope =
    t.selected <- Some (root, scope);
    t.phase <- Await_reserve
  ;;

  let journal_synced t env path =
    match t.phase, t.selected with
    | Await_reserve, Some (root, scope) ->
      let scan : Store.Journal_segment.scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Source_preparation_tests.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Store.Frame.flags frame.frame) 0 ->
         let transaction =
           Store.Transaction.decode (Store.Frame.payload frame.frame)
           |> Source_preparation_tests.store_ok
         in
         if P.Id.Session.equal transaction.session_id root
         then (
           let delta =
             A.Session_delta_document.decode
               ~limits:Document_schema.Limits.default
               transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
           in
           let selected =
             Child_usage_fault_tests.Fault.leaves delta
             |> List.exists ~f:(function
               | A.Session_delta.Goal_descendants_changed update ->
                 (match GD.Update.reason update with
                  | Reserve selected ->
                    Origin.Scope.equal selected.scope scope
                    &&
                      (match A.Goal_descendant_occurrence.kind selected.occurrence with
                      | Host_turn _ -> true
                      | Attempt _ -> false)
                  | Register_new_root _
                  | Register_first_unrelated_child _
                  | Register_child _
                  | Register_branch _
                  | Register_retained_branch _
                  | Record_included _
                  | Import _
                  | Close_before_writer _ -> false)
               | _ -> false)
           in
           if selected then t.phase <- Await_refresh)
       | Some _ | None -> ())
    | (Disabled | Await_refresh | Parked_refresh | Reject_close), _ | Await_reserve, None
      -> ()
  ;;

  let selected_archive t path =
    Option.exists t.selected ~f:(fun (root, _) ->
      String.is_suffix path ~suffix:("/" ^ P.Id.Session.to_string root ^ "/ARCHIVED"))
  ;;

  let before_stat t path =
    match t.phase with
    | Await_refresh when Option.is_some (Eio.Fiber.get t.caller_key) ->
      (* Reserve's own postcommit metadata callback also stats ARCHIVED, on the
         Actor fiber. Only the real outer submitter may park this refresh. *)
      t.phase <- Parked_refresh;
      Eio.Promise.resolve t.signal_parked ();
      Exn.protect
        ~finally:(fun () ->
          match t.phase with
          | Parked_refresh -> t.phase <- Reject_close
          | Disabled | Await_reserve | Await_refresh | Reject_close -> ())
        ~f:(fun () -> Eio.Promise.await t.never)
    | Reject_close ->
      t.failed_reads <- t.failed_reads + 1;
      raise (Core_unix.Unix_error (EIO, "567 root prewriter close read", path))
    | Disabled | Await_reserve | Await_refresh | Parked_refresh -> ()
  ;;

  let wrap t env =
    let observed =
      Crash_fault_io.wrap
        env
        ~boundary:After_sync
        ~matches:(fun path ->
          String.is_substring path ~substring:"/journal/"
          && String.is_suffix path ~suffix:".log")
        ~reached:(journal_synced t env)
    in
    Crash_fault_io.wrap
      observed
      ~boundary:Before_stat
      ~matches:(selected_archive t)
      ~reached:(before_stat t)
  ;;
end

let equal_error (expected : P.Error.t) (actual : P.Error.t) =
  assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json actual))
;;

let%expect_test "retained Writer Run and no-Turn input bypass unrelated sealed cleanup" =
  let fault = Close_read_fault.create () in
  let provider_calls = ref 0 in
  let provider_check = ref None in
  let provider_failure = ref None in
  Birth.with_native_goal_child
    ~wrap_env:(Close_read_fault.wrap fault)
    ~post_creation_stream:(fun _ ->
      Option.iter !provider_check ~f:(fun check ->
        Int.incr provider_calls;
        try check () with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          provider_failure := Some (exn, backtrace);
          Exn.raise_with_original_backtrace exn backtrace);
      completed_response "567 actual Writer Run completed")
    (fun env sw daemon parent child scope reference _counters ->
       let within phase f =
         try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. f with
         | Eio.Time.Timeout -> failwith ("567 replay timed out: " ^ phase)
       in
       Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()) |> protocol_ok;
       Adapter.start_child daemon child;
       let run_scope =
         Owner.with_loaded_runtime child.runtime (fun runtime ->
           let open Result.Let_syntax in
           let%bind observer =
             Option.bind
               runtime.A.Runtime_builder.moderator_manager
               ~f:Chat_response.Moderator_manager.invocation_observer
             |> Result.of_option
                  ~error:(P.Error.invalid_request "actual Run observer absent")
           in
           A.Run_admission.Scope.create_authenticated
             ~actor:original
             ~observer
             ~startup_pending:(fun () ->
               Option.value_map
                 runtime.moderator_activation
                 ~default:false
                 ~f:(fun activation -> activation.startup_pending ()))
             ~authorize)
         |> protocol_ok
       in
       let writer, _ =
         Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
       in
       let caller_context = ref None in
       let unloaded = ref false in
       let cancelled = Failure "567 caller cancelled before child canonical writer" in
       protect_cleanup
         ~finally:(fun () ->
           Option.iter !caller_context ~f:(fun context ->
             Eio.Cancel.cancel context cancelled);
           fault.phase <- Disabled;
           if not !unloaded
           then (
             within "post-disarm factual unload" (fun () ->
               Owner.unload_and_wait child.runtime)
             |> protocol_ok;
             unloaded := true);
           Actor.detach child.actor writer.id |> protocol_ok)
         (fun () ->
            let history text =
              let id = A.History_id_source.allocate child.history_ids |> protocol_ok in
              Owner.parse_user_content
                child.runtime
                ~id
                { kind = Plain_text; text; attachments = [] }
              |> protocol_ok
              |> A.History_codec.to_protocol
            in
            (* Materialize each real writer input before freezing either request;
               history allocation is an ordinary owned metadata advance. *)
            let first_entry = history "567 retained ordinary Writer Run" in
            let failed_entry =
              history "567 fresh Writer cancelled before canonical write"
            in
            let changed_entry = history "567 different request for retained Writer key" in
            let make_request ~key ~input =
              let current = state child in
              P.Run_start.create
                ~session_id:current.identity.session_id
                ~attachment_id:writer.id
                ~generation:current.identity.generation
                ~expected_revision:current.counters.revision
                ~mode:Single_turn
                ~input
                ~key:(P.Idempotency_key.of_string key |> protocol_ok)
              |> protocol_ok
            in
            let user text =
              P.Run_start.Input.User_submission
                { kind = Plain_text; text; attachments = [] }
            in
            let session =
              P.Session_ref.create
                ~server_id:(Store.Session_store.server_id (Daemon.store daemon))
                ~session_id:reference.child_session_id
            in
            let submit request entry =
              Actor.admit_run
                child.actor
                ~scope:run_scope
                ~request
                ~session
                ~request_sha256:
                  (A.Run_admission_request.request_sha256
                     (A.Run_admission_request.of_writer request)
                   |> protocol_ok)
                ~entry
            in
            let first_request =
              make_request
                ~key:"567-writer-retained"
                ~input:(user "567 retained ordinary Writer Run")
            in
            provider_check
            := Some
                 (fun () ->
                   Adapter.confirmed parent child scope reference ~turns:1L ~attempts:1L
                   |> ignore);
            let first = submit first_request (Some first_entry) |> protocol_ok in
            within "first Writer terminal" (fun () ->
              Goal_native_test_support.await_quiet child)
            |> ignore;
            Actor.with_moderator_checkpoint child.actor (fun () -> Ok ()) |> protocol_ok;
            Option.iter !provider_failure ~f:(fun (exn, backtrace) ->
              Exn.raise_with_original_backtrace exn backtrace);
            [%test_eq: int] 1 !provider_calls;
            let before = state child in
            assert (Option.is_none before.active_operation);
            assert (Option.is_none before.failure);
            (match before.lifecycle.observed with
             | Idle -> ()
             | Stopped
             | Queued_for_slot
             | Starting
             | Recovering
             | Running_turn _
             | Compacting _
             | Waiting_for_permission _
             | Stopping
             | Failed _ -> failwith "Writer replay fixture is not Idle");
            let run =
              A.Run_state.find (Option.value_exn before.run_state) first.run_id
              |> Option.value_exn
            in
            (match run.lifecycle with
             | Terminal (Completed _) -> ()
             | Admitted | Active | Waiting _
             | Terminal (Failed _ | Cancelled | Limited | Interrupted) ->
               failwith "actual Writer Run did not complete");
            let before_marks = Adapter.marks child in
            [%test_eq: int64] 1L before_marks.turn;
            [%test_eq: int64] 1L before_marks.attempt;
            let before_branch = Adapter.branch parent scope reference in
            [%test_eq: int64] 2L (GD.Branch.confirmed_prefix before_branch);
            let no_turn = make_request ~key:"567-no-turn" ~input:Authored_start in
            let no_turn_error = submit no_turn None |> rejected in
            [%test_eq: P.Error.code] Conflict no_turn_error.code;
            assert (phys_equal before (state child));
            let failed_request =
              make_request
                ~key:"567-prewriter-cancelled"
                ~input:(user "567 fresh Writer cancelled before canonical write")
            in
            let changed_request =
              P.Run_start.create
                ~session_id:first_request.session_id
                ~attachment_id:first_request.attachment_id
                ~generation:first_request.generation
                ~expected_revision:first_request.expected_revision
                ~mode:first_request.mode
                ~input:(user "567 different request for retained Writer key")
                ~key:first_request.key
              |> protocol_ok
            in
            provider_check
            := Some (fun () -> failwith "unwritten Writer Run launched provider");
            Close_read_fault.select fault ~root:(state parent).identity.session_id ~scope;
            let submission =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Eio.Cancel.sub (fun context ->
                  caller_context := Some context;
                  Eio.Fiber.with_binding fault.caller_key () (fun () ->
                    submit failed_request (Some failed_entry))))
            in
            within "post-Reserve outer read" (fun () -> Eio.Promise.await fault.parked);
            let pending =
              GD.Branch.pending (Adapter.branch parent scope reference)
              |> Option.value_exn
            in
            [%test_eq: int64] 3L (Pending.next_prefix pending);
            assert (phys_equal before (state child));
            Eio.Cancel.cancel (Option.value_exn !caller_context) cancelled;
            (match Result.try_with (fun () -> Eio.Promise.await_exn submission) with
             | Error (Eio.Cancel.Cancelled cause) -> assert (phys_equal cause cancelled)
             | Error exn -> raise exn
             | Ok _ -> failwith "prewriter caller did not preserve cancellation");
            caller_context := None;
            let rec close_failed () =
              if Int.equal fault.failed_reads 0
              then (
                Eio.Fiber.yield ();
                close_failed ())
            in
            within "owned close prewriter failure" close_failed;
            let failures = fault.failed_reads in
            let assert_unchanged () =
              [%test_eq: int] 1 !provider_calls;
              assert (phys_equal before (state child));
              assert (
                Pending.equal
                  pending
                  (GD.Branch.pending (Adapter.branch parent scope reference)
                   |> Option.value_exn));
              [%test_eq: int64] before_marks.turn (Adapter.marks child).turn;
              [%test_eq: int64] before_marks.attempt (Adapter.marks child).attempt
            in
            let replay = submit first_request (Some first_entry) |> protocol_ok in
            assert (P.Run_receipt.equal first replay);
            let mismatch = submit changed_request (Some changed_entry) |> rejected in
            [%test_eq: P.Error.code] Invalid_request mismatch.code;
            [%test_eq: string]
              "run request key was committed with another digest"
              mismatch.message;
            equal_error no_turn_error (submit no_turn None |> rejected);
            [%test_eq: int] failures fault.failed_reads;
            assert_unchanged ();
            (* The first explicit fresh call may join the original close promise.
               After it returns, the next call deterministically retries the same
               known prewriter read once; this is not uncertain-writer replay. *)
            submit failed_request (Some failed_entry) |> rejected |> ignore;
            let failures = fault.failed_reads in
            submit failed_request (Some failed_entry) |> rejected |> ignore;
            [%test_eq: int] (failures + 1) fault.failed_reads;
            assert_unchanged ();
            fault.phase <- Disabled;
            within "explicit factual unload drain" (fun () ->
              Owner.unload_and_wait child.runtime)
            |> protocol_ok;
            unloaded := true;
            let closed = Adapter.branch parent scope reference in
            assert (Option.is_none (GD.Branch.pending closed));
            [%test_eq: int64] 2L (GD.Branch.confirmed_prefix closed);
            [%test_eq: int64]
              (Int64.succ (GD.Branch.last_nonce before_branch))
              (GD.Branch.last_nonce closed);
            (match GD.Branch.last closed with
             | Some (No_writer_closed actual) -> assert (Pending.equal pending actual)
             | None | Some (Included _) ->
               failwith "actual sealed reservation was not closed");
            [%test_eq: int64] before_marks.turn (Adapter.marks child).turn;
            [%test_eq: int64] before_marks.attempt (Adapter.marks child).attempt;
            [%test_eq: int] 1 !provider_calls));
  print_endline
    "actual Writer replay/no-Turn bypass sealed cleanup; fresh work retries; factual \
     unload closes once";
  [%expect
    {| actual Writer replay/no-Turn bypass sealed cleanup; fresh work retries; factual unload closes once |}]
;;
