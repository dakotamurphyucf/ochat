open! Core
open Agent_server_test_support
open Retry_support
module Existing = Delegated_load_tests
module Warm = Delegated_warm_resource_tests
module Integration = Native_resource_integration_tests
module Cache_failure = Native_resource_failure_tests.Cache_failure
module Resource_loan = Ordinary_warm_resource_tests.Resource_loan
module Retirement = Owner.Resource_retirement
module Store = Agent_store.Session_store
module B = A.Runtime_builder

let within env phase f =
  try Eio.Time.with_timeout_exn env#clock 10. f with
  | Eio.Time.Timeout -> failwith ("637 resource retirement timed out: " ^ phase)
;;

let cache_path (parent : Registry.entry) =
  Filename.concat
    (Store.Handle.cache_directory (Option.value_exn parent.store_handle))
    "cache.bin"
;;

let run_claim = function
  | Retirement.Run work -> work
  | Join _ -> failwith "637 expected ownership of a new actual retirement"
;;

let join_claim = function
  | Retirement.Join completion -> completion
  | Run _ -> failwith "637 duplicate claim elected a second retirement producer"
;;

let expect_error (expected : P.Error.t) = function
  | Error actual ->
    assert (P.Error.equal_code expected.code actual.P.Error.code);
    [%test_eq: string] expected.message actual.message;
    [%test_eq: bool] expected.retryable actual.retryable;
    assert (Jsonaf.exactly_equal expected.data actual.data)
  | Ok () -> failwith "637 expected the actual retained retirement failure"
;;

let assert_source_live (runtime : B.t) =
  match A.Prompt_source_lease.availability runtime.B.source_lease with
  | Live -> ()
  | Missing_receipt | Unavailable _ | Revoked | Closed ->
    failwith "637 retained cleanup lost the actual source switch"
;;

(* A lexical host loan deliberately has no registered dependency consumer.
   The complete retirement must refuse it before acting on the tracked child. *)
let with_unregistered_lifetime ~sw owner f =
  let entered, enter = Eio.Promise.create () in
  let released, release = Eio.Promise.create () in
  let task =
    Eio.Fiber.fork_promise ~sw (fun () ->
      Existing.capture_load (fun () ->
        Owner.with_resource_lifetime owner (fun () ->
          Eio.Promise.resolve enter ();
          Eio.Promise.await released;
          Ok ())))
  in
  let join () =
    if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ();
    Eio.Promise.await_exn task |> Existing.restore
  in
  protect_cleanup ~finally:join (fun () ->
    Warm.await_entry entered task;
    f ())
;;

exception Observer_cancelled

let queued_failure env sw parent fault =
  let work =
    Owner.claim_resource_retirement parent.Registry.runtime |> protocol_ok |> run_claim
  in
  let completion = Retirement.completion work in
  let observing, observe = Eio.Promise.create () in
  let observer =
    Eio.Fiber.fork_promise ~sw (fun () ->
      Existing.capture_load (fun () ->
        Eio.Cancel.sub (fun context ->
          Eio.Promise.resolve observe context;
          Retirement.await completion)))
  in
  protect_cleanup
    ~finally:(fun () ->
      let context = Eio.Promise.await observing in
      if Option.is_none (Eio.Promise.peek observer)
      then Eio.Cancel.cancel context Observer_cancelled;
      Eio.Cancel.protect (fun () ->
        within env "join observer and settle claimed producer" (fun () ->
          Eio.Promise.await_exn observer |> ignore;
          Retirement.run work)))
    (fun () ->
       let context = Eio.Promise.await observing in
       Eio.Cancel.cancel context Observer_cancelled;
       within
         env
         "join cancelled observer without acknowledging unfinished attempt"
         (fun () ->
            match Eio.Promise.await_exn observer with
            | Raised (Eio.Cancel.Cancelled Observer_cancelled, _) -> ()
            | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
            | Returned result ->
              raise_s
                [%sexp
                  "637 unfinished observer returned", (result : (unit, P.Error.t) result)]);
       assert (Option.is_none (Retirement.peek completion));
       let failure =
         P.Error.create
           Conflict
           ~message:"637 injected worker launch failure"
           ~retryable:false
           ()
       in
       Retirement.fail_to_start work (Protocol failure);
       Retirement.run work;
       let attempts = fault.Cache_failure.attempts in
       (* The cancelled observer did not deliver this later producer failure: the
     first synchronous cleanup observes it rather than silently retrying. *)
       within env "deliver unseen launch failure before any implicit retry" (fun () ->
         Owner.unload_and_wait parent.runtime |> expect_error failure);
       [%test_eq: int] attempts fault.attempts;
       let retry =
         Owner.retry_resource_retirement parent.runtime ~failed:completion
         |> protocol_ok
         |> run_claim
       in
       within env "retry actual queued target on the same reusable Owner" (fun () ->
         Retirement.run retry;
         Retirement.await (Retirement.completion retry) |> protocol_ok);
       Retirement.await completion |> expect_error failure;
       Retirement.run work;
       Retirement.fail_to_start
         work
         (Protocol (P.Error.invalid_request "losing duplicate producer"));
       Retirement.await completion |> expect_error failure)
;;

let%expect_test
    "Owner retirement strengthens actual unload and preserves queued failure delivery"
  =
  let fault = Cache_failure.create () in
  Existing.with_child
    ~wrap_env:(Cache_failure.wrap fault)
    (fun env sw _daemon parent child _source providers ->
       Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
       within env "release tracked child resources" (fun () ->
         Owner.unload_and_wait child.runtime |> protocol_ok);
       fault.cache <- Some (cache_path parent);
       let initial = Warm.loaded parent in
       let before = Warm.settled parent in
       let entered, enter = Eio.Promise.create () in
       let exiting, exit = Eio.Promise.create () in
       let released, release = Eio.Promise.create () in
       let release_callback () =
         if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
       in
       let active_context = ref None in
       let callback =
         Eio.Fiber.fork_promise ~sw (fun () ->
           Existing.capture_load (fun () ->
             Eio.Cancel.sub (fun context ->
               active_context := Some context;
               Exn.protect
                 ~finally:(fun () -> active_context := None)
                 ~f:(fun () ->
                   Owner.with_background_runtime parent.runtime (fun actual ->
                     Integration.same_native initial actual;
                     Eio.Promise.resolve enter actual;
                     Exn.protect
                       ~finally:(fun () ->
                         Eio.Cancel.protect (fun () ->
                           Eio.Promise.resolve exit ();
                           Eio.Promise.await released))
                       ~f:(fun () -> Eio.Fiber.await_cancel ()))))))
       in
       let ordinary = ref None in
       (* Exercise the real lexical host borrow without the Factory's tracked
          dependency relation. *)
       let loan = Resource_loan.create ~sw parent in
       let completion =
         protect_cleanup
           ~finally:(fun () ->
             Resource_loan.release loan;
             release_callback ();
             Option.iter !active_context ~f:(fun context ->
               Eio.Cancel.cancel context Observer_cancelled);
             Eio.Cancel.protect (fun () ->
               within env "join actual callback and ordinary producer" (fun () ->
                 Eio.Promise.await_exn callback |> ignore;
                 Option.iter !ordinary ~f:(fun task ->
                   Eio.Promise.await_exn task |> ignore);
                 Resource_loan.join loan)))
           (fun () ->
              let borrower = Resource_loan.await loan in
              Integration.same_native initial borrower;
              within env "retain actual parent runtime in cancellable callback" (fun () ->
                Warm.await_entry entered callback |> ignore);
              let producer =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Existing.capture_load (fun () -> Owner.unload_and_wait parent.runtime))
              in
              ordinary := Some producer;
              (* Ordinary retirement has cancelled this actual stop-sensitive
                 lease and must join its protected finalizer before detaching.
                 The lexical native resource loan still survives Stop. *)
              within env "actual ordinary callback cancellation cut" (fun () ->
                Eio.Fiber.first
                  (fun () -> Eio.Promise.await exiting)
                  (fun () ->
                     Eio.Promise.await_exn producer |> Existing.restore;
                     failwith "637 ordinary unload returned before lease finalizer"));
              let completion =
                Owner.claim_resource_retirement parent.runtime
                |> protocol_ok
                |> join_claim
              in
              let duplicate =
                Owner.claim_resource_retirement parent.runtime
                |> protocol_ok
                |> join_claim
              in
              assert (Option.is_none (Retirement.peek completion));
              assert (Option.is_none (Retirement.peek duplicate));
              Owner.ensure_loaded parent.runtime |> Existing.expect_background_busy;
              [%test_eq: int] 0 fault.attempts;
              release_callback ();
              let failure =
                within
                  env
                  "join strengthened actual cleanup with surviving borrower"
                  (fun () ->
                     Eio.Promise.await_exn callback |> Warm.expect_cancelled;
                     match Eio.Promise.await_exn producer with
                     | Returned (Error error) ->
                       [%test_eq: P.Error.code] Conflict error.code;
                       [%test_eq: string]
                         "native resource borrowers require tracked dependency retirement"
                         error.message;
                       assert error.retryable;
                       error
                     | Returned (Ok ()) ->
                       failwith "637 strengthened ordinary unload ignored surviving loan"
                     | Raised (exn, backtrace) ->
                       Exn.raise_with_original_backtrace exn backtrace)
              in
              Retirement.await completion |> expect_error failure;
              Retirement.await duplicate |> expect_error failure;
              [%test_eq: int] 0 fault.attempts;
              [%test_eq: int] 0 fault.publications;
              assert (not (Owner.is_loaded parent.runtime));
              assert_source_live borrower;
              Owner.ensure_loaded parent.runtime |> Existing.expect_background_busy;
              Integration.no_lifecycle_replay before (Warm.settled parent);
              completion)
       in
       let retry =
         Owner.retry_resource_retirement parent.runtime ~failed:completion
         |> protocol_ok
         |> run_claim
       in
       within env "retry same retained resources after host loan joins" (fun () ->
         Retirement.run retry;
         Retirement.await (Retirement.completion retry) |> protocol_ok);
       [%test_eq: int] 1 fault.attempts;
       [%test_eq: int] 1 fault.publications;
       (match A.Prompt_source_lease.availability initial.source_lease with
        | Closed -> ()
        | Missing_receipt | Live | Unavailable _ | Revoked ->
          failwith "637 strengthened cleanup left original source open");
       Integration.no_lifecycle_replay before (Warm.settled parent);
       within env "reload SAME canonical Owner after reusable retirement" (fun () ->
         Owner.ensure_loaded parent.runtime |> protocol_ok);
       let fresh = Warm.loaded parent in
       assert (
         not
           (phys_equal
              (Option.value_exn initial.native_runtime)
              (Option.value_exn fresh.native_runtime)));
       within env "queued producer and cancelled-observer delivery" (fun () ->
         queued_failure env sw parent fault);
       [%test_eq: int] 2 fault.attempts;
       [%test_eq: int] 2 fault.publications;
       [%test_eq: int] 0 !providers);
  print_endline
    "actual unload strengthened; same Owner reloads; cancelled observer preserves unseen \
     failure";
  [%expect
    {| actual unload strengthened; same Owner reloads; cancelled observer preserves unseen failure |}]
;;

let%expect_test
    "Owner complete retirement preserves real surviving borrower and same-holder failure \
     retry"
  =
  let fault = Cache_failure.create () in
  let watched = ref None in
  let preparations = ref 0 in
  Existing.with_child
    ~wrap_env:(Cache_failure.wrap fault)
    ~on_inference_ports:(fun actor ->
      if Option.exists !watched ~f:(phys_equal actor) then Int.incr preparations)
    (fun env sw _daemon parent child _source providers ->
       Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
       watched := Some parent.actor;
       fault.cache <- Some (cache_path parent);
       within env "close original ordinary units" (fun () ->
         Owner.unload_and_wait child.runtime |> protocol_ok;
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 1 fault.attempts;
       [%test_eq: int] 1 fault.publications;
       within env "retain genuine native-only parent A with actual child B" (fun () ->
         Owner.ensure_loaded child.runtime |> protocol_ok);
       [%test_eq: int] 1 !preparations;
       assert (not (Owner.is_loaded parent.runtime));
       let borrower = Warm.loaded child in
       assert_source_live borrower;
       let before = Warm.settled parent in
       let refused, survivor_failure =
         with_unregistered_lifetime ~sw parent.runtime (fun () ->
           let work =
             Owner.claim_resource_retirement parent.runtime |> protocol_ok |> run_claim
           in
           let refused = Retirement.completion work in
           let duplicate =
             Owner.claim_resource_retirement parent.runtime |> protocol_ok |> join_claim
           in
           within env "refuse unidentified surviving native consumer" (fun () ->
             Retirement.run work);
           let survivor_failure =
             match Retirement.await refused with
             | Error error ->
               assert (P.Error.equal_code error.code Conflict);
               [%test_eq: string]
                 "native resource borrowers require tracked dependency retirement"
                 error.message;
               assert error.retryable;
               error
             | Ok () ->
               failwith "637 complete retirement closed a surviving native borrower"
           in
           Retirement.await duplicate |> expect_error survivor_failure;
           assert_source_live borrower;
           [%test_eq: int] 1 !preparations;
           [%test_eq: int] 1 fault.attempts;
           [%test_eq: int] 1 fault.publications;
           (* The retained source is cleanup custody, not permission for fresh work. *)
           Owner.ensure_loaded parent.runtime |> Existing.expect_background_busy;
           Integration.no_lifecycle_replay before (Warm.settled parent);
           refused, survivor_failure)
       in
       within env "release genuine child B without fresh execution authority" (fun () ->
         Owner.unload_and_wait child.runtime |> protocol_ok);
       [%test_eq: int] 1 fault.attempts;
       let cache = Option.value_exn fault.cache in
       let bytes () = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / cache) in
       let original_bytes = bytes () in
       fault.armed <- true;
       let retry =
         Owner.retry_resource_retirement parent.runtime ~failed:refused
         |> protocol_ok
         |> run_claim
       in
       let failed_close = Retirement.completion retry in
       within env "publish original cache exception without losing target" (fun () ->
         Retirement.run retry);
       let original_exception, original_backtrace =
         match Retirement.peek failed_close with
         | Some (Raised ((Core_unix.Unix_error (EIO, operation, path) as exn), backtrace))
           ->
           [%test_eq: string] "635 native final cache publication" operation;
           assert (Cache_failure.temporary fault path);
           exn, backtrace
         | Some (Raised (exn, backtrace)) ->
           Exn.raise_with_original_backtrace exn backtrace
         | Some (Returned (Error error)) ->
           raise_s
             [%sexp "637 raised cache failure became protocol error", (error : P.Error.t)]
         | Some (Returned (Ok ())) ->
           failwith "637 complete retirement swallowed the original cache failure"
         | None -> failwith "637 cache retirement producer did not settle"
       in
       let observe_cache_failure () =
         (* Identity is deliberate: this handle must retain the SAME captured
            exception and raw trace, including after a later retry succeeds. *)
         (match Retirement.peek failed_close with
          | Some (Raised (exn, backtrace)) ->
            assert (phys_equal exn original_exception);
            assert (phys_equal backtrace original_backtrace)
          | Some (Returned result) ->
            raise_s
              [%sexp
                "637 immutable raised outcome changed"
              , (result : (unit, P.Error.t) result)]
          | None -> failwith "637 completed cache retirement became unfinished");
         match Retirement.await failed_close with
         | Ok () ->
           failwith "637 complete retirement swallowed the original cache failure"
         | Error error ->
           raise_s
             [%sexp "637 raised cache failure became protocol error", (error : P.Error.t)]
         | exception exn ->
           let backtrace = Stdlib.Printexc.get_raw_backtrace () in
           if not (phys_equal exn original_exception)
           then Exn.raise_with_original_backtrace exn backtrace
       in
       observe_cache_failure ();
       [%test_eq: int] 1 fault.failures;
       [%test_eq: int] 2 fault.attempts;
       [%test_eq: int] 1 fault.publications;
       [%test_eq: string] original_bytes (bytes ());
       [%test_eq: int] 1 !preparations;
       Retirement.run retry;
       Retirement.fail_to_start
         retry
         (Protocol (P.Error.invalid_request "losing completed producer"));
       observe_cache_failure ();
       Retirement.await refused |> expect_error survivor_failure;
       [%test_eq: int] 2 fault.attempts;
       let final =
         Owner.retry_resource_retirement parent.runtime ~failed:failed_close
         |> protocol_ok
         |> run_claim
       in
       within env "retry SAME retained common holder and join physical scope" (fun () ->
         Retirement.run final;
         Retirement.await (Retirement.completion final) |> protocol_ok);
       [%test_eq: int] 3 fault.attempts;
       [%test_eq: int] 2 fault.publications;
       [%test_eq: int] 1 !preparations;
       observe_cache_failure ();
       Retirement.run final;
       Retirement.await (Retirement.completion final) |> protocol_ok;
       [%test_eq: int] 3 fault.attempts;
       Integration.no_lifecycle_replay before (Warm.settled parent);
       within env "reload same canonical Entry after complete retirement" (fun () ->
         Owner.ensure_loaded parent.runtime |> protocol_ok);
       [%test_eq: int] 2 !preparations;
       within env "close fresh ordinary unit" (fun () ->
         Owner.unload_and_wait parent.runtime |> protocol_ok);
       [%test_eq: int] 4 fault.attempts;
       [%test_eq: int] 3 fault.publications;
       [%test_eq: int] 0 !providers);
  print_endline
    "real B retained on refusal; original cache exception immutable; same-holder retry; \
     same Owner reloads";
  [%expect
    {| real B retained on refusal; original cache exception immutable; same-holder retry; same Owner reloads |}]
;;

let%expect_test
    "signalled retirement joins an installed Goal source finalizer before public Stop"
  =
  let module Retirement = Owner.Resource_retirement in
  let module Load = Delegated_load_tests in
  let module Goal = Goal_resume_runtime_tests in
  let module Source_wait = Cold_policy_negative_tests.Source_wait in
  let module Cache_failure = Native_resource_failure_tests.Cache_failure in
  let module Store = Agent_store.Session_store in
  let fault = Cache_failure.create () in
  let test_switch = ref None in
  let gate = Source_wait.create () in
  let release_source () = Source_wait.release gate in
  let wrap_env env =
    let env = Cache_failure.wrap fault env in
    Source_wait.wrap_matching gate env ~matches:(fun path ->
      String.equal (Filename.basename path) Goal.resume_context_file)
  in
  Goal.with_cold_episode
    ~test_switch
    ~wrap_env
    V1
    Continue
    (fun
        env
         daemon
         entry
         _old_runtime
         counters
         _probe
         session
         admission
         initial
         stopped
         actor
         request
       ->
       let sw = Option.value_exn !test_switch in
       let within f = Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f in
       fault.cache
       <- Some
            (Filename.concat
               (Store.Handle.cache_directory (Option.value_exn entry.store_handle))
               "cache.bin");
       gate.armed <- true;
       (* This Owner already ran the original episode; handoff history is
          cumulative and cannot identify this new construction's handoff. *)
       let previous_handoff = Owner.has_activation_handoff entry.runtime in
       let stopping = ref None in
       let operation =
         Eio.Fiber.fork_promise ~sw (fun () ->
           Load.capture_load (fun () ->
             Goal.Service.resume_episode
               (Goal.public daemon)
               ~entry
               ~actor
               ~session
               ~request))
       in
       protect_cleanup
         ~finally:(fun () ->
           (* Release real protected work before every join, also after a failed
              assertion. Actual Factory/Owner finalizers remain the only closers. *)
           release_source ();
           Eio.Cancel.protect (fun () ->
             within (fun () -> Eio.Promise.await_exn operation |> ignore);
             Option.iter !stopping ~f:(fun task ->
               within (fun () -> Eio.Promise.await_exn task |> Load.restore))))
         (fun () ->
            within (fun () ->
              Eio.Fiber.first
                (fun () -> Eio.Promise.await gate.entered)
                (fun () ->
                   Eio.Promise.await_exn operation |> ignore;
                   failwith
                     "638 Goal source returned before actual post-install source read"));
            (* This positive fact distinguishes this cut from the earlier native
               source/Init waits. Inner construction owns an Installed binding;
               unsuccessful resume must mark it for exact source-finalizer unload. *)
            assert (Owner.is_loaded entry.runtime);
            assert (not (Owner.source_lane_idle entry.runtime));
            [%test_eq: bool] previous_handoff (Owner.has_activation_handoff entry.runtime);
            [%test_eq: int] 1 !(counters.providers);
            [%test_eq: int] 1 gate.reads;
            [%test_eq: int] 0 fault.attempts;
            let before = Actor.state entry.actor |> protocol_ok in
            assert (Option.is_none before.active_operation);
            let work =
              match Owner.signal_resource_retirement entry.runtime |> protocol_ok with
              | Retirement.Run work -> work
              | Join _ -> failwith "638 post-install signal did not elect pending work"
            in
            let completion = Retirement.completion work in
            let duplicate =
              match Owner.signal_resource_retirement entry.runtime |> protocol_ok with
              | Retirement.Join same -> same
              | Run _ -> failwith "638 post-install duplicate elected a second producer"
            in
            assert (phys_equal completion duplicate);
            Owner.ensure_loaded entry.runtime |> Load.expect_background_busy;
            let stop =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Load.capture_load (fun () -> Owner.unload_and_wait entry.runtime))
            in
            stopping := Some stop;
            Eio.Fiber.yield ();
            (* Actor state still progresses while public Stop owns pending and
               its actual source read is held. No private prefix can join
               pending while pending awaits this same Source_extent. *)
            ignore (Actor.state entry.actor |> protocol_ok : A.Session_state.t);
            assert (Option.is_none (Eio.Promise.peek operation));
            assert (Option.is_none (Eio.Promise.peek stop));
            assert (Option.is_none (Retirement.peek completion));
            [%test_eq: int] 0 fault.attempts;
            release_source ();
            let outcome = within (fun () -> Eio.Promise.await_exn operation) in
            (match outcome with
             | Raised (Eio.Cancel.Cancelled _, _) -> ()
             | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
             | Returned (Error error)
             | Returned (Ok (Goal.Result.Resume_failed { error; _ } | Rejected error)) ->
               assert (
                 P.Error.equal_code error.code Conflict
                 || P.Error.equal_code error.code Interrupted
                 || P.Error.equal_code error.code Invalid_state)
             | Returned (Ok (Resumed _ | Retained _ | Uncertain _)) ->
               failwith "638 signalled Goal continuation returned positive or uncertain");
            within (fun () -> Eio.Promise.await_exn stop |> Load.restore);
            (match Retirement.peek completion with
             | Some (Returned (Ok ())) -> ()
             | Some (Returned (Error error)) -> raise_s [%sexp (error : P.Error.t)]
             | Some (Raised (exn, backtrace)) ->
               Exn.raise_with_original_backtrace exn backtrace
             | None -> failwith "638 public Stop returned before installed source joined");
            Retirement.await duplicate |> protocol_ok;
            Retirement.run work;
            assert (not (Owner.is_loaded entry.runtime));
            assert (Owner.source_lane_idle entry.runtime);
            [%test_eq: bool] previous_handoff (Owner.has_activation_handoff entry.runtime);
            [%test_eq: int] 1 fault.attempts;
            [%test_eq: int] 1 fault.publications;
            [%test_eq: int] 1 !(counters.providers);
            [%test_eq: int] 1 gate.reads;
            let after = Actor.state entry.actor |> protocol_ok in
            assert (Option.is_none after.active_operation);
            Goal.assert_episode stopped after admission;
            List.iter (Goal.startup_receipts initial) ~f:(fun previous ->
              assert (
                List.exists
                  after.moderator_executions
                  ~f:(P.Moderator_execution.equal previous)));
            let resumes =
              List.filter after.moderator_executions ~f:(fun receipt ->
                P.Moderator_execution.equal_phase receipt.context.phase Session_resume)
            in
            let resumed = Goal.one_actual "638 actual cancelled Session_resume" resumes in
            (match resumed.status with
             | Failed _ | Interrupted _ -> ()
             | Running | Completed _ ->
               failwith "638 signalled source callback remained live or committed");
            Owner.unload_and_wait entry.runtime |> protocol_ok;
            [%test_eq: int] 1 fault.attempts;
            [%test_eq: int] 1 fault.publications));
  print_endline
    "actual install ACK preceded signal; public Stop joined same source and one physical \
     close without self-joining pending";
  [%expect
    {| actual install ACK preceded signal; public Stop joined same source and one physical close without self-joining pending |}]
;;
