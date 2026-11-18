open! Core
open Fixtures
module A = Agent_session.Session_actor
module P = Agent_protocol
module Completion = A.Idle_moderator_completion
module C = A.Retirement_cleanup
module Owner = Owner_termination_tests

let%expect_test
    "cancelled owner refuses actual idle cleanup launch without stranding its join"
  =
  let armed = ref false in
  let entered, enter = Eio.Promise.create () in
  let release, resume = Eio.Promise.create () in
  let release_publication () =
    armed := false;
    ignore (Eio.Promise.try_resolve resume ())
  in
  Owner.with_owner_env
    ~state_committed:(fun _ _ ->
      if !armed
      then
        Eio.Cancel.protect (fun () ->
          ignore (Eio.Promise.try_resolve enter ());
          Eio.Promise.await release))
    (fun _env callers actor _backend cancel_owner exited ->
       let writer, _ = A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok in
       let original = Agent_authority_tests.authentication (Atomic.make true) in
       let expected = A.state actor |> protocol_ok in
       let scope =
         A.begin_initialization_authenticated actor ~actor:original ~expected
         |> protocol_ok
       in
       A.complete_initialization actor ~scope ~candidate:expected |> protocol_ok |> ignore;
       A.end_initialization actor ~scope |> protocol_ok;
       A.start actor ~actor:original ~attachment_id:writer.id |> protocol_ok |> ignore;
       let idle = A.state actor |> protocol_ok in
       [%test_eq: P.Session.desired_state] Running idle.lifecycle.desired;
       (match idle.lifecycle.observed with
        | Idle -> ()
        | Stopped
        | Queued_for_slot
        | Starting
        | Recovering
        | Running_turn _
        | Compacting _
        | Waiting_for_permission _
        | Stopping
        | Failed _ ->
          raise_s
            [%sexp
              "expected idle Actor", (idle.lifecycle.observed : P.Session.observed_state)]);
       let completion = Completion.create actor in
       Completion.claim completion |> protocol_ok |> Option.value_exn |> ignore;
       let publication = ref None in
       let first_join = ref None in
       Exn.protect
         ~finally:(fun () ->
           Eio.Cancel.protect (fun () ->
             release_publication ();
             Completion.leave_capture completion;
             Option.iter !first_join ~f:(fun task -> ignore (Eio.Promise.await_exn task));
             Option.iter !publication ~f:(fun task -> ignore (Eio.Promise.await_exn task))))
         ~f:(fun () ->
           let failure =
             P.Error.create
               Interrupted
               ~message:"original idle source cancelled"
               ~retryable:false
               ()
           in
           (* Start actual A before the producer exits. The outside observer binds
              its join before retirement captures that same Running attempt. *)
           Completion.cancel completion ~failure;
           let observing, observe = Eio.Promise.create () in
           let joined =
             Eio.Fiber.fork_promise ~sw:callers (fun () ->
               Owner.capture (fun () ->
                 Eio.Promise.resolve observe ();
                 (* There is no yield between this signal and binding A's join. *)
                 Completion.join_retirement completion))
           in
           first_join := Some joined;
           Eio.Promise.await observing;
           assert (Option.is_none (Eio.Promise.peek joined));
           let cleanup =
             A.retire_runtime_worker_with_cleanup actor ~closing:false |> protocol_ok
           in
           let captured = C.snapshot cleanup |> protocol_ok in
           assert (C.Snapshot.equal_progress (C.Snapshot.progress captured) Pending);
           assert (List.is_empty (C.Snapshot.failures captured));
           armed := true;
           let task =
             Eio.Fiber.fork_promise ~sw:callers (fun () ->
               Owner.capture (fun () -> A.reserve_history_block actor ~count:1))
           in
           publication := Some task;
           (match
              Eio.Fiber.first
                (fun () ->
                   Eio.Promise.await entered;
                   `Entered)
                (fun () -> `Finished (Eio.Promise.await_exn task))
            with
            | `Entered -> ()
            | `Finished (Error (exn, backtrace)) ->
              Exn.raise_with_original_backtrace exn backtrace
            | `Finished (Ok _) -> failwith "actual publication did not reach its gate");
           cancel_owner ();
           (* The protected actual publication keeps the cancelled owner switch
             unfinished. Waiting for [exited] here would miss the silent-fork cut. *)
           assert (Option.is_none (Eio.Promise.peek exited));
           (* A's genuine worker cancellation finishes its retained join. Only
              then may B be a separate launch against the cancelling switch. *)
           (match Eio.Promise.await_exn joined with
            | Ok (Error error) ->
              [%test_eq: P.Error.code] Interrupted error.code;
              assert (String.is_substring error.message ~substring:"was interrupted")
            | Ok (Ok ()) ->
              failwith "live idle cleanup completed before its producer exit"
            | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
           Completion.leave_capture completion;
           Completion.cancel completion ~failure;
           let launch_failure () =
             match Completion.join_retirement completion with
             | Error error ->
               [%test_eq: P.Error.code] Interrupted error.code;
               assert (
                 String.is_substring error.message ~substring:"could not be scheduled");
               error
             | Ok () -> failwith "cancelled owner certified an unlaunched idle cleanup"
           in
           let first = launch_failure () in
           let repeated = launch_failure () in
           assert (phys_equal first repeated);
           assert (Option.is_none (Eio.Promise.peek exited));
           let terminal = C.advance cleanup ~actor ~after:captured |> protocol_ok in
           assert (C.Snapshot.equal_progress (C.Snapshot.progress terminal) Terminal);
           let actual_failures snapshot =
             match C.Snapshot.failures snapshot with
             | [ Raised ((Eio.Cancel.Cancelled _ as earlier), _)
               ; Raised ((Eio.Cancel.Cancelled _ as later), _)
               ] -> earlier, later
             | _ -> failwith "idle cleanup lost or duplicated an actual A/B failure"
           in
           let earlier, later = actual_failures terminal in
           let repeated_terminal =
             C.advance cleanup ~actor ~after:terminal |> protocol_ok
           in
           assert (
             C.Snapshot.equal_progress (C.Snapshot.progress repeated_terminal) Terminal);
           let repeated_earlier, repeated_later = actual_failures repeated_terminal in
           assert (phys_equal earlier repeated_earlier);
           assert (phys_equal later repeated_later);
           assert (C.Snapshot.equal_progress (C.Snapshot.progress captured) Pending);
           assert (List.is_empty (C.Snapshot.failures captured));
           (* Both observations happen while the real owner is still Cancelling,
              so Terminal cannot be supplied by the finished-Actor bypass. *)
           assert (Option.is_none (Eio.Promise.peek exited));
           release_publication ();
           (match Eio.Promise.await_exn task with
            | Ok result -> result |> protocol_ok |> ignore
            | Error (Eio.Cancel.Cancelled _, _) -> ()
            | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
           match Eio.Promise.await_exn exited with
           | Error (Eio.Cancel.Cancelled _, _) -> ()
           | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
           | Ok () -> failwith "cancelled Actor owner returned normally"));
  print_endline
    "actual idle cleanup launch refused; same negative join; owner gate released before \
     joins";
  [%expect
    {| actual idle cleanup launch refused; same negative join; owner gate released before joins |}]
;;
