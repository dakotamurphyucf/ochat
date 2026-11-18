open Core
open Fixtures
module Owner = Agent_server.Runtime_owner
module Delegated = Agent_server.Delegated_runtime

let%expect_test
    "independent construction survives parent unload and joins activity before resources"
  =
  List.iter [ false; true ] ~f:(fun close_parent ->
    Job_fixtures.with_actor (fun _env sw actor _writer _backend ->
      let ready, ready_u = Eio.Promise.create () in
      let cleaning, cleaning_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let cleaned = ref false in
      let parent_closes = ref 0 in
      let child_closes = ref 0 in
      let revoked = ref 0 in
      let original =
        Runtime_lease_tests.runtime
          ~close:(fun () ->
            assert !cleaned;
            [%test_eq: int] 1 !child_closes;
            Int.incr parent_closes)
          ()
      in
      let parent =
        Owner.create ~actor ~initial:(Some original) ~build:(fun () ->
          failwith "unexpected parent resource rebuild")
      in
      let child =
        Delegated.prepare_independent
          ~sw
          ~parent
          ~on_revoked:(fun () ->
            assert !cleaned;
            Int.incr revoked;
            Ok ())
          ~build:(fun ~sw parent_runtime ->
            assert (phys_equal parent_runtime original);
            Ok
              (Runtime_lease_tests.runtime
                 ~activity:(Agent_session.Runtime_activity.create ~sw)
                 ~close:(fun () ->
                   assert !cleaned;
                   Int.incr child_closes)
                 ()))
        |> protocol_ok
      in
      Owner.unload_and_wait parent |> protocol_ok;
      assert (not (Owner.is_loaded parent));
      [%test_eq: int] 0 !parent_closes;
      let activity = Option.value_exn child.activity in
      let worker =
        Eio.Fiber.fork_promise ~sw (fun () ->
          Result.try_with (fun () ->
            Agent_session.Runtime_activity.run activity (fun () ->
              [%test_eq: int] 0 !parent_closes;
              Exn.protect
                ~finally:(fun () ->
                  Eio.Cancel.protect (fun () ->
                    Eio.Promise.resolve cleaning_u ();
                    Eio.Promise.await release;
                    cleaned := true))
                ~f:(fun () ->
                  Eio.Promise.resolve ready_u ();
                  Eio.Promise.await never))))
      in
      Eio.Promise.await ready;
      let closing =
        Eio.Fiber.fork_promise ~sw (fun () ->
          match close_parent with
          | false -> Agent_session.Runtime_builder.close child
          | true -> Owner.close_and_wait parent)
      in
      Eio.Promise.await cleaning;
      assert (Option.is_none (Eio.Promise.peek closing));
      [%test_eq: int] 0 !parent_closes;
      [%test_eq: int] 0 !child_closes;
      Eio.Promise.resolve release_u ();
      (match Eio.Promise.await_exn worker with
       | Error (Eio.Cancel.Cancelled _) -> ()
       | _ -> failwith "child activity outlived runtime close");
      Eio.Promise.await_exn closing;
      Agent_session.Runtime_builder.close child;
      Owner.close_and_wait parent;
      [%test_eq: int] 1 !parent_closes;
      [%test_eq: int] 1 !child_closes;
      [%test_eq: int] (if close_parent then 1 else 0) !revoked;
      (match Option.value_exn child.check_execution () with
       | Error { code = Interrupted; _ } -> ()
       | _ -> failwith "closed child remained executable");
      print_s
        [%sexp
          (close_parent : bool)
        , "activity survived unload; close joined activity, child cleanup and resources"]));
  [%expect
    {|
    (false
     "activity survived unload; close joined activity, child cleanup and resources")
    (true
     "activity survived unload; close joined activity, child cleanup and resources") |}]
;;

let%expect_test
    "parent cancellation during child preparation joins cleanup and returns a typed error"
  =
  Job_fixtures.with_actor (fun _env sw actor _writer _backend ->
    let started, started_u = Eio.Promise.create () in
    let cleaning, cleaning_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let cleaned = ref false in
    let closes = ref 0 in
    let runtime =
      Runtime_lease_tests.runtime
        ~close:(fun () ->
          assert !cleaned;
          Int.incr closes)
        ()
    in
    let parent =
      Owner.create ~actor ~initial:(Some runtime) ~build:(fun () ->
        failwith "unexpected parent rebuild")
    in
    Exn.protect
      ~finally:(fun () -> Owner.close_and_wait parent)
      ~f:(fun () ->
        let caller =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Delegated.prepare
              ~sw
              ~parent
              ~on_revoked:(fun () ->
                failwith "unpublished child has no lifecycle callback")
              ~build:(fun ~sw:_ _ ->
                Exn.protect
                  ~finally:(fun () ->
                    Eio.Cancel.protect (fun () ->
                      Eio.Promise.resolve cleaning_u ();
                      Eio.Promise.await release;
                      cleaned := true))
                  ~f:(fun () ->
                    Eio.Promise.resolve started_u ();
                    Eio.Promise.await never;
                    failwith "cancelled build continued")))
        in
        Eio.Promise.await started;
        let stopping =
          Eio.Fiber.fork_promise ~sw (fun () -> Owner.unload_and_wait parent)
        in
        Eio.Promise.await cleaning;
        [%test_eq: int] 0 !closes;
        assert (Owner.is_loaded parent);
        Eio.Promise.resolve release_u ();
        (match Eio.Promise.await_exn caller with
         | Error { code = Interrupted; _ } -> ()
         | _ -> failwith "preparation cancellation did not return a typed error");
        Eio.Promise.await_exn stopping |> protocol_ok;
        [%test_eq: int] 1 !closes;
        assert (not (Eio.Fiber.is_cancelled ()));
        print_endline
          "construction cleanup joined; parent retired once; caller remains live"));
  [%expect {| construction cleanup joined; parent retired once; caller remains live |}]
;;

module Cleanup_site = struct
  type t =
    | Hook
    | Original_close
    | Revocation
  [@@deriving sexp_of]
end

let with_recorded_backtraces f =
  let original = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace true;
  Exn.protect ~finally:(fun () -> Stdlib.Printexc.record_backtrace original) ~f
;;

let raise_recorded failure trace =
  try raise failure with
  | exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    trace := Some backtrace;
    Exn.raise_with_original_backtrace exn backtrace
;;

let assert_trace_prefix ~original actual =
  let prefix = Stdlib.Printexc.raw_backtrace_to_string original in
  assert (not (String.is_empty prefix));
  assert (String.is_prefix (Stdlib.Printexc.raw_backtrace_to_string actual) ~prefix)
;;

let%expect_test
    "delegated cancellation cannot erase actual hook, revocation or close failure"
  =
  with_recorded_backtraces (fun () ->
    List.iter [ Cleanup_site.Hook; Original_close; Revocation ] ~f:(fun site ->
      Job_fixtures.with_actor (fun env sw actor _writer _backend ->
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
          let parent =
            Owner.create
              ~actor
              ~initial:(Some (Runtime_lease_tests.runtime ~close:ignore ()))
              ~build:(fun () -> failwith "unexpected parent rebuild")
          in
          Exn.protect
            ~finally:(fun () -> Owner.close_and_wait parent)
            ~f:(fun () ->
              let entered, entered_u = Eio.Promise.create () in
              let never, _ = Eio.Promise.create () in
              let joined = ref false
              and hooks = ref 0
              and closes = ref 0
              and revocations = ref 0 in
              let failure = Eio.Cancel.Cancelled Exit in
              let failure_trace = ref None in
              let child =
                Delegated.prepare_independent
                  ~sw
                  ~parent
                  ~on_revoked:(fun () ->
                    assert !joined;
                    Int.incr revocations;
                    match site with
                    | Revocation -> raise_recorded failure failure_trace
                    | Hook | Original_close -> Ok ())
                  ~build:(fun ~sw _ ->
                    Eio.Fiber.fork ~sw (fun () ->
                      Exn.protect
                        ~finally:(fun () -> joined := true)
                        ~f:(fun () ->
                          Eio.Promise.resolve entered_u ();
                          Eio.Promise.await never));
                    Eio.Switch.on_release sw (fun () ->
                      assert !joined;
                      Int.incr hooks;
                      match site with
                      | Hook -> raise_recorded failure failure_trace
                      | Original_close | Revocation -> ());
                    Ok
                      (Runtime_lease_tests.runtime
                         ~close:(fun () ->
                           assert !joined;
                           Int.incr closes;
                           match site with
                           | Original_close -> raise_recorded failure failure_trace
                           | Hook | Revocation -> ())
                         ()))
                |> protocol_ok
              in
              Eio.Promise.await entered;
              (match site with
               | Revocation -> Owner.close_and_wait parent
               | Hook | Original_close -> ());
              for _ = 1 to 2 do
                match Agent_session.Runtime_builder.close child with
                | () -> failwith "actual cancellation-valued cleanup failure disappeared"
                | exception exn ->
                  let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                  assert (phys_equal exn failure);
                  assert_trace_prefix
                    ~original:(Option.value_exn !failure_trace)
                    backtrace
              done;
              [%test_eq: int] 1 !hooks;
              [%test_eq: int] 1 !closes;
              [%test_eq: int]
                (match site with
                 | Revocation -> 1
                 | Hook | Original_close -> 0)
                !revocations;
              (match Option.value_exn child.check_execution () with
               | Error { code = Interrupted; _ } -> ()
               | _ -> failwith "failed terminal runtime remained executable");
              print_s
                [%sexp (site : Cleanup_site.t), "joined once; failure and guard retained"])))));
  [%expect
    {|
    (Hook "joined once; failure and guard retained")
    (Original_close "joined once; failure and guard retained")
    (Revocation "joined once; failure and guard retained")
    |}]
;;

exception Delegated_worker_failure
exception Delegated_cancelled_worker_failure
exception Delegated_hook_failure
exception Delegated_close_failure

let%expect_test
    "delegated construction preserves protocol result and real worker aggregate"
  =
  with_recorded_backtraces (fun () ->
    Job_fixtures.with_actor (fun env sw actor _writer _backend ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        let parent =
          Owner.create
            ~actor
            ~initial:(Some (Runtime_lease_tests.runtime ~close:ignore ()))
            ~build:(fun () -> failwith "unexpected parent rebuild")
        in
        Exn.protect
          ~finally:(fun () -> Owner.close_and_wait parent)
          ~f:(fun () ->
            let failure =
              Agent_protocol.Error.invalid_request "authored build rejection"
            in
            List.iter [ false; true ] ~f:(fun hook_fails ->
              let hooks = ref 0 in
              let hook = Eio.Cancel.Cancelled Exit in
              match
                Delegated.prepare
                  ~sw
                  ~parent
                  ~on_revoked:(fun () -> failwith "unpublished child cannot revoke")
                  ~build:(fun ~sw _ ->
                    Eio.Switch.on_release sw (fun () ->
                      Int.incr hooks;
                      if hook_fails then raise hook);
                    Error failure)
              with
              | Error actual when not hook_fails ->
                assert (phys_equal actual failure);
                [%test_eq: int] 1 !hooks
              | exception actual when hook_fails ->
                assert (phys_equal actual hook);
                [%test_eq: int] 1 !hooks
              | _ -> failwith "authored result/hook precedence changed");
            let worker = Delegated_worker_failure in
            let cancelled_worker = Delegated_cancelled_worker_failure in
            let worker_trace = ref None
            and cancelled_worker_trace = ref None in
            let entered, entered_u = Eio.Promise.create () in
            let never, _ = Eio.Promise.create () in
            (match
               Delegated.prepare
                 ~sw
                 ~parent
                 ~on_revoked:(fun () -> failwith "unpublished child cannot revoke")
                 ~build:(fun ~sw _ ->
                   Eio.Fiber.fork ~sw (fun () ->
                     Eio.Promise.resolve entered_u ();
                     match Eio.Promise.await never with
                     | () -> failwith "worker unexpectedly resumed"
                     | exception Eio.Cancel.Cancelled _ ->
                       Eio.Cancel.protect (fun () ->
                         raise_recorded cancelled_worker cancelled_worker_trace));
                   Eio.Fiber.fork ~sw (fun () ->
                     Eio.Promise.await entered;
                     raise_recorded worker worker_trace);
                   Eio.Promise.await never;
                   failwith "worker-induced callback cancellation escaped")
             with
             | exception Eio.Exn.Multiple failures ->
               [%test_eq: int] 2 (List.length failures);
               List.iter
                 [ worker, worker_trace; cancelled_worker, cancelled_worker_trace ]
                 ~f:(fun (expected, trace) ->
                   let _, observed =
                     List.find_exn failures ~f:(fun (actual, _) ->
                       phys_equal actual expected)
                   in
                   assert_trace_prefix ~original:(Option.value_exn !trace) observed)
             | _ -> failwith "callback cancellation hid actual worker aggregate");
            print_endline
              "protocol kept; hook cancellation exposed; worker aggregate kept"))));
  [%expect {| protocol kept; hook cancellation exposed; worker aggregate kept |}]
;;

let%expect_test
    "delegated work and cleanup preserve actual aggregate and Finally precedence"
  =
  with_recorded_backtraces (fun () ->
    Job_fixtures.with_actor (fun env sw actor _writer _backend ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        let parent =
          Owner.create
            ~actor
            ~initial:(Some (Runtime_lease_tests.runtime ~close:ignore ()))
            ~build:(fun () -> failwith "unexpected parent rebuild")
        in
        Exn.protect
          ~finally:(fun () -> Owner.close_and_wait parent)
          ~f:(fun () ->
            let trigger, trigger_u = Eio.Promise.create () in
            let failed, failed_u = Eio.Promise.create () in
            let hooks = ref 0
            and cancelled_hooks = ref 0
            and closes = ref 0 in
            let worker = Delegated_worker_failure in
            let hook = Delegated_hook_failure in
            let close = Delegated_close_failure in
            let cancelled_hook = Eio.Cancel.Cancelled Exit in
            let trace = ref None
            and cancelled_hook_trace = ref None in
            let cleanup = Delegated.Cleanup.create () in
            let child =
              Delegated.prepare_resources_reported
                ~cleanup
                ~with_resources:(fun f ->
                  Owner.with_background_runtime_observed parent ~capture:ignore f
                  |> Owner.Lease_outcome.observation)
                ~sw
                ~on_revoked:(fun () ->
                  failwith "worker failure cannot become healthy revocation")
                ~build:(fun ~sw _ ->
                  Eio.Fiber.fork ~sw (fun () ->
                    Eio.Promise.await trigger;
                    Eio.Promise.resolve failed_u ();
                    raise_recorded worker trace);
                  Eio.Switch.on_release sw (fun () ->
                    Int.incr hooks;
                    raise hook);
                  (* Eio's aggregate drops this actual Cancelled hook when the
                     non-Cancelled worker/hook already owns the Switch error. *)
                  Eio.Switch.on_release sw (fun () ->
                    Int.incr cancelled_hooks;
                    raise_recorded cancelled_hook cancelled_hook_trace);
                  Ok
                    (Runtime_lease_tests.runtime
                       ~close:(fun () ->
                         Int.incr closes;
                         raise close)
                       ()))
              |> protocol_ok
            in
            Eio.Promise.resolve trigger_u ();
            Eio.Promise.await failed;
            let original = ref None
            and original_switch = ref None in
            for _ = 1 to 2 do
              match Agent_session.Runtime_builder.close child with
              | () -> failwith "aggregate cleanup failure disappeared"
              | exception
                  (Exn.Finally ((Eio.Exn.Multiple failures as actual_switch), actual_close)
                   as actual) ->
                assert (phys_equal actual_close close);
                original_switch := Some actual_switch;
                [%test_eq: int] 2 (List.length failures);
                assert (
                  List.exists failures ~f:(fun (actual, _) -> phys_equal actual hook));
                let _, actual_trace =
                  List.find_exn failures ~f:(fun (actual, _) -> phys_equal actual worker)
                in
                assert_trace_prefix ~original:(Option.value_exn !trace) actual_trace;
                (match !original with
                 | None -> original := Some actual
                 | Some first -> assert (phys_equal first actual))
            done;
            [%test_eq: int] 1 !hooks;
            [%test_eq: int] 1 !cancelled_hooks;
            [%test_eq: int] 1 !closes;
            let module CO = Agent_session.Close_observation in
            let module D = Agent_session.Cleanup_diagnostic in
            let terminal = Option.value_exn (Delegated.Cleanup.terminal cleanup) in
            let has_cancelled_hook diagnostic =
              List.exists (D.failures diagnostic) ~f:(function
                | Raised (actual, _) -> phys_equal actual cancelled_hook
                | Protocol _ -> false)
            in
            let switch_group =
              match List.filter (CO.diagnostics terminal) ~f:has_cancelled_hook with
              | [ actual ] -> actual
              | _ -> failwith "actual Cancelled hook lost or reminted at Switch boundary"
            in
            assert (Option.equal D.equal (CO.primary terminal) (Some switch_group));
            (match D.failures switch_group with
             | Raised (actual, _) :: supporting ->
               assert (phys_equal actual (Option.value_exn !original_switch));
               let _, hook_trace =
                 List.find_map_exn supporting ~f:(function
                   | D.Failure.Raised (actual, trace)
                     when phys_equal actual cancelled_hook -> Some (actual, trace)
                   | Raised _ | Protocol _ -> None)
               in
               assert_trace_prefix
                 ~original:(Option.value_exn !cancelled_hook_trace)
                 hook_trace
             | [] | Protocol _ :: _ -> failwith "Switch result lost its first raw part");
            assert (
              phys_equal terminal (Option.value_exn (Delegated.Cleanup.terminal cleanup)));
            assert (Result.is_error (Option.value_exn child.check_execution ()));
            print_endline "actual worker/hook aggregate and Finally retained; close once"))));
  [%expect {| actual worker/hook aggregate and Finally retained; close once |}]
;;

exception Delegated_caller_cancelled

let%expect_test
    "cancelled delegated caller joins unpublished construction before reraising"
  =
  with_recorded_backtraces (fun () ->
    Job_fixtures.with_actor (fun env sw actor _writer _backend ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
        let parent_closes = ref 0 in
        let parent =
          Owner.create
            ~actor
            ~initial:
              (Some
                 (Runtime_lease_tests.runtime
                    ~close:(fun () -> Int.incr parent_closes)
                    ()))
            ~build:(fun () -> failwith "unexpected parent rebuild")
        in
        Exn.protect
          ~finally:(fun () -> Owner.close_and_wait parent)
          ~f:(fun () ->
            let context, context_u = Eio.Promise.create () in
            let entered, entered_u = Eio.Promise.create () in
            let cleaning, cleaning_u = Eio.Promise.create () in
            let release, release_u = Eio.Promise.create () in
            let never, _ = Eio.Promise.create () in
            let joined = ref false in
            let actual_build_switch = ref None in
            let caller =
              Eio.Fiber.fork_promise ~sw (fun () ->
                try
                  Ok
                    (Eio.Cancel.sub (fun cancel ->
                       Eio.Promise.resolve context_u cancel;
                       Delegated.prepare
                         ~sw
                         ~parent
                         ~on_revoked:(fun () ->
                           failwith "unpublished child cannot revoke")
                         ~build:(fun ~sw _ ->
                           actual_build_switch := Some sw;
                           Exn.protect
                             ~finally:(fun () ->
                               Eio.Cancel.protect (fun () ->
                                 Eio.Promise.resolve cleaning_u ();
                                 Eio.Promise.await release;
                                 joined := true))
                             ~f:(fun () ->
                               Eio.Promise.resolve entered_u ();
                               Eio.Promise.await never;
                               failwith "cancelled caller published runtime"))))
                with
                | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
            in
            let cancel = Eio.Promise.await context in
            Exn.protect
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  Exn.protect
                    ~finally:(fun () -> ignore (Eio.Promise.await caller))
                    ~f:(fun () ->
                      if Option.is_none (Eio.Promise.peek release)
                      then Eio.Promise.resolve release_u ();
                      if Option.is_none (Eio.Promise.peek caller)
                      then Eio.Cancel.cancel cancel Delegated_caller_cancelled)))
              ~f:(fun () ->
                Eio.Promise.await entered;
                Eio.Cancel.cancel cancel Delegated_caller_cancelled;
                Eio.Promise.await cleaning;
                assert (Option.is_none (Eio.Promise.peek caller));
                assert (not !joined);
                (* The same original build Switch is cancelled but cannot finish
                   while its existing protected cleanup waits on release. *)
                let refused = Delegated.Cleanup.create () in
                let entries = ref 0 in
                let actual_refusal =
                  match
                    Delegated.prepare_resources_reported
                      ~cleanup:refused
                      ~with_resources:(fun _ ->
                        Int.incr entries;
                        failwith "failed Switch entered resources")
                      ~sw:(Option.value_exn !actual_build_switch)
                      ~on_revoked:(fun () -> failwith "failed Switch revoked work")
                      ~build:(fun ~sw:_ _ -> failwith "failed Switch built work")
                  with
                  | Error _ | Ok _ -> failwith "failed Switch fabricated preparation"
                  | exception (Eio.Cancel.Cancelled _ as actual) -> actual
                in
                [%test_eq: int] 0 !entries;
                let module C = Delegated.Cleanup in
                let local = Option.value_exn (C.local refused) in
                assert (C.Local.equal_status (C.Local.status local) Joined);
                assert (Option.is_none (C.Local.switch_outcome local));
                assert (Option.is_none (C.Local.runtime_close local));
                (match C.adoption refused with
                 | Not_created -> ()
                 | Original_running | Closed_runtime _ ->
                   failwith "failed Switch fabricated adopted construction");
                (match
                   Agent_session.Close_observation.exit
                     (Option.value_exn (C.terminal refused))
                 with
                 | Raised (actual, trace) ->
                   assert (phys_equal actual actual_refusal);
                   assert (
                     not (String.is_empty (Stdlib.Printexc.raw_backtrace_to_string trace)))
                 | Returned _ -> failwith "failed Switch lost actual entry refusal");
                Eio.Promise.resolve release_u ();
                (match Eio.Promise.await_exn caller with
                 | Error (Eio.Cancel.Cancelled Delegated_caller_cancelled, backtrace) ->
                   assert (
                     not
                       (String.is_empty
                          (Stdlib.Printexc.raw_backtrace_to_string backtrace)))
                 | _ ->
                   failwith "caller cancellation became a fabricated preparation result");
                assert !joined;
                assert (not (Eio.Fiber.is_cancelled ()));
                Owner.close_and_wait parent;
                [%test_eq: int] 1 !parent_closes;
                print_endline "caller cancellation retained; unpublished cleanup joined")))));
  [%expect {| caller cancellation retained; unpublished cleanup joined |}]
;;
