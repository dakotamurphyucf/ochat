open! Core
open Agent_server_test_support
open Retry_support
module Cold = Cold_policy_resource_tests
module Warm = Delegated_warm_resource_tests
module Load = Delegated_load_tests
module E = P.Moderator_execution
module Store = Agent_store.Session_store

let selected_lifecycle (current : A.Session_state.t) =
  let pointer =
    A.Run_state.moderator_lifecycle (Option.value_exn current.run_state)
    |> Option.value_exn
  in
  let receipt =
    match
      List.filter current.moderator_executions ~f:(fun receipt ->
        P.Id.Moderator_execution.equal
          receipt.E.context.id
          (A.Moderator_lifecycle.execution_id pointer))
    with
    | [ receipt ] -> receipt
    | _ -> failwith "634 negative lost its unique actual lifecycle receipt"
  in
  pointer, receipt
;;

let expect_error ~message = function
  | Error error ->
    assert (P.Error.equal_code error.P.Error.code Conflict);
    [%test_eq: string] message error.message;
    assert (not error.retryable)
  | Ok _ -> failwith "634 negative unexpectedly admitted policy restoration"
;;

let policy_receipts (current : A.Session_state.t) =
  List.filter current.moderator_executions ~f:(fun receipt ->
    E.equal_phase receipt.E.context.phase Pre_tool_call)
;;

let assert_unloaded (parent : Registry.entry) =
  assert (not (Owner.is_loaded parent.runtime));
  assert (not (Owner.has_activation_handoff parent.runtime))
;;

let prepare (parent : Registry.entry) (actual : Cold.native_request) =
  Owner.prepare_delegated_tool
    parent.runtime
    ~request:actual.request
    ~governance:actual.governance
    ~delegation:actual.delegation
    ~event:(Pre_tool_call actual.request.call)
    ~authorize:actual.authorize
;;

let with_phase f =
  let phase = ref "enter Factory negative" in
  match f phase with
  | () -> ()
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.traceln "634 Factory negative phase: %s" !phase with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let%expect_test "latest actual failed Resume denies cold Factory policy ingress" =
  with_phase (fun phase ->
    Cold.with_seed
      ~phase
      Running_unloaded
      (fun env _sw _root daemon parent saved counters ->
         let before = Warm.settled parent in
         let writer, _ =
           Actor.attach parent.actor ~mode:Read_write ~subscribe:false |> protocol_ok
         in
         protect_cleanup
           ~finally:(fun () -> Actor.detach parent.actor writer.id |> protocol_ok)
           (fun () ->
              phase := "ordinary activation before any restored policy use";
              let outcome =
                match
                  Owner.ensure_loaded_authenticated parent.runtime ~actor:original
                with
                | Error _ as failure -> failure
                | Ok () ->
                  Actor.start ~actor:original parent.actor ~attachment_id:writer.id
                  |> protocol_ok
                  |> ignore;
                  phase := "drive exactly one genuine pending Resume";
                  Factory.drain_idle_moderator (Daemon.factory daemon) parent
                  |> Result.map ~f:ignore
              in
              let failed = Warm.settled parent in
              let pointer, receipt = selected_lifecycle failed in
              assert (E.equal_phase receipt.context.phase Session_resume);
              (match receipt.status with
               | Failed failure ->
                 [%test_eq: string] "event.handler_failed" failure.P.Invocation.code;
                 [%test_eq: string]
                   "queued moderator handler exited without committing its checkpoint"
                   failure.message;
                 assert (not failure.retryable)
               | Running | Completed _ | Interrupted _ ->
                 raise_s [%sexp "634 real Resume did not fail", (receipt : E.t)]);
              (* The genuine terminal receipt, rather than an arbitrary load
                 refusal, establishes this fixture's intended failure. *)
              (match outcome with
               | Ok () -> ()
               | Error error ->
                 assert (P.Error.equal_code error.P.Error.code Invalid_state);
                 [%test_eq: string] "moderator event failed" error.message;
                 assert (not error.retryable));
              [%test_eq: int] 1 (List.length (Warm.completed before Session_start));
              assert (Option.equal Jsonaf.exactly_equal before.moderator failed.moderator);
              let failed_queue =
                A.Moderator_checkpoint.queue failed.moderator
                |> protocol_ok
                |> Option.value_exn
              in
              assert (
                Jsonaf.exactly_equal
                  (A.Moderator_queue.to_jsonaf saved.queue)
                  (A.Moderator_queue.to_jsonaf failed_queue));
              [%test_eq: int]
                (List.length (Warm.completed before Session_start))
                (List.length (Warm.completed failed Session_start));
              [%test_eq: int]
                1
                (List.count failed.moderator_executions ~f:(fun candidate ->
                   E.equal_phase candidate.E.context.phase Session_resume));
              assert (P.Run_source.equal saved.source (Warm.source failed));
              assert (List.is_empty (policy_receipts failed));
              [%test_eq: int] saved.provider_count !(counters.providers);
              phase := "join actual failed ordinary Product unload";
              Owner.unload_and_wait parent.runtime |> protocol_ok;
              assert_unloaded parent;
              let baseline = Warm.settled parent in
              let ports = !(counters.inference_ports) in
              let denied = ref None in
              phase := "fresh genuine Caller enters failed cold target";
              Cold.with_native_request
                ~phase
                env
                daemon
                ~target_actor:parent.actor
                baseline
                ~original
                ~f:(fun actual -> denied := Some (prepare parent actual));
              expect_error
                ~message:"current installation has no completed policy-restoration basis"
                (Option.value_exn !denied);
              let after = Warm.settled parent in
              let after_pointer, after_receipt = selected_lifecycle after in
              assert (A.Moderator_lifecycle.equal pointer after_pointer);
              assert (E.equal receipt after_receipt);
              assert (P.Run_source.equal saved.source (Warm.source after));
              assert (Option.equal Jsonaf.exactly_equal baseline.moderator after.moderator);
              assert (
                List.equal
                  E.equal
                  baseline.moderator_executions
                  after.moderator_executions);
              [%test_eq: int] ports !(counters.inference_ports);
              [%test_eq: int] saved.provider_count !(counters.providers);
              assert_unloaded parent;
              assert (Owner.source_lane_idle parent.runtime))));
  print_endline
    "real failed Resume superseded retained StartCompleted; genuine cold Caller denied \
     before native rebuild or policy/provider effects";
  [%expect
    {| real failed Resume superseded retained StartCompleted; genuine cold Caller denied before native rebuild or policy/provider effects |}]
;;

module Source_wait = struct
  type t =
    { mutable path : string option
    ; mutable armed : bool
    ; mutable reads : int
    ; entered : unit Eio.Promise.t
    ; enter : unit Eio.Promise.u
    ; released : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    }

  let create () =
    let entered, enter = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    { path = None; armed = false; reads = 0; entered; enter; released; release }
  ;;

  let release t =
    t.armed <- false;
    if Option.is_none (Eio.Promise.peek t.released) then Eio.Promise.resolve t.release ()
  ;;

  let wrap_matching t env ~matches =
    Crash_fault_io.wrap
      env
      ~boundary:Before_open_in
      ~matches:(fun path -> t.armed && matches path)
      ~reached:(fun _ ->
        t.armed <- false;
        t.reads <- t.reads + 1;
        Eio.Promise.resolve t.enter ();
        (* The actual native-source read stays owned through Stop until the
           test releases it; shutdown cannot strand a protected wait. *)
        Eio.Cancel.protect (fun () -> Eio.Promise.await t.released))
  ;;

  let wrap t env =
    wrap_matching t env ~matches:(fun path -> Option.exists t.path ~f:(String.equal path))
  ;;
end

let expect_source_retirement_cancel = function
  | Owner.Scope_outcome.Raised (Eio.Cancel.Cancelled Exit, _) -> ()
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  | Returned (Error error) ->
    raise_s [%sexp "634 source retirement returned a typed error", (error : P.Error.t)]
  | Returned (Ok ()) -> failwith "634 retired source construction unexpectedly returned"
;;

let%expect_test "signalled retirement joins cold Factory source IO through public Stop" =
  with_phase (fun phase ->
    let gate = Source_wait.create () in
    Cold.with_seed
      ~wrap_env:(Source_wait.wrap gate)
      ~phase
      Running_unloaded
      (fun env sw _root daemon parent saved counters ->
         let before = Warm.settled parent in
         let basis = A.Policy_restore_basis.of_state before |> protocol_ok in
         let _, before_receipt = selected_lifecycle before in
         gate.path
         <- Some
              (Filename.concat
                 (Store.Handle.cache_directory (Option.value_exn parent.store_handle))
                 "cache.bin");
         let within f = Eio.Time.with_timeout_exn env#clock 30. f in
         let denied = ref None in
         let retiring = ref None in
         let writer, _ =
           Actor.attach parent.actor ~mode:Read_write ~subscribe:false |> protocol_ok
         in
         gate.armed <- true;
         let operation =
           Eio.Fiber.fork_promise ~sw (fun () ->
             Load.capture_load (fun () ->
               Cold.with_native_request
                 ~phase
                 env
                 daemon
                 ~target_actor:parent.actor
                 before
                 ~original
                 ~f:(fun actual -> denied := Some (prepare parent actual));
               Ok ()))
         in
         protect_cleanup
           ~finally:(fun () ->
             Source_wait.release gate;
             let first = ref None in
             let join f =
               try within f with
               | exn ->
                 if Option.is_none !first
                 then first := Some (exn, Stdlib.Printexc.get_raw_backtrace ())
             in
             join (fun () ->
               let outcome = Eio.Promise.await_exn operation in
               match !retiring with
               | None -> Load.restore outcome
               | Some _ -> expect_source_retirement_cancel outcome);
             Option.iter !retiring ~f:(fun pending ->
               join (fun () -> Eio.Promise.await_exn pending |> protocol_ok));
             join (fun () -> Actor.detach parent.actor writer.id |> protocol_ok);
             Option.iter !first ~f:(fun (exn, backtrace) ->
               Exn.raise_with_original_backtrace exn backtrace))
           (fun () ->
              phase := "await actual post-begin cache read";
              within (fun () ->
                Eio.Fiber.first
                  (fun () -> Eio.Promise.await gate.entered)
                  (fun () ->
                     Eio.Promise.await_exn operation |> Load.restore;
                     failwith "634 policy returned before actual source wait"));
              [%test_eq: int] 1 gate.reads;
              phase
              := "refuse external complete retirement before actual source stage \
                  finalizes";
              (match Owner.claim_resource_retirement parent.runtime with
               | Error error ->
                 assert (P.Error.equal_code error.code Conflict);
                 [%test_eq: string]
                   "active source preparation must finish before resource retirement"
                   error.message;
                 assert error.retryable
               | Ok _ ->
                 failwith "637 retirement claimed over the actual live source stage");
              phase := "prove actual Actor construction exclusion and progress";
              let current = Actor.state parent.actor |> protocol_ok in
              assert (P.Id.Session.equal current.identity.session_id saved.session_id);
              assert_unloaded parent;
              assert (not (Owner.source_lane_idle parent.runtime));
              let callbacks = ref 0 in
              (match
                 Actor.with_quiescent_state parent.actor ~f:(fun _ ->
                   Int.incr callbacks;
                   Ok ())
               with
               | Error error ->
                 assert (P.Error.equal_code error.P.Error.code Conflict);
                 [%test_eq: string]
                   "policy restoration owns runtime construction"
                   error.message
               | Ok _ -> failwith "634 selected cache read preceded actual construction");
              [%test_eq: int] 0 !callbacks;
              assert (List.is_empty (policy_receipts current));
              [%test_eq: int]
                saved.resume_count
                (List.length (Warm.completed current Session_resume));
              [%test_eq: int] saved.provider_count !(counters.providers);
              phase := "actual Stop ACK while native read remains held";
              Actor.stop parent.actor ~attachment_id:writer.id ~mode:Cancel
              |> protocol_ok
              |> ignore;
              let stopped = Actor.state parent.actor |> protocol_ok in
              assert (P.Session.equal_desired_state stopped.lifecycle.desired Stopped);
              [%test_eq: int64] (Int64.succ before.stop_epoch) stopped.stop_epoch;
              expect_error
                ~message:"current installation has no completed policy-restoration basis"
                (A.Policy_restore_basis.check_policy basis ~state:stopped);
              phase := "signal actual source retirement before external work";
              let module Retirement = Owner.Resource_retirement in
              let work =
                match Owner.signal_resource_retirement parent.runtime |> protocol_ok with
                | Retirement.Run work -> work
                | Join _ -> failwith "638 source signal did not elect actual pending work"
              in
              let completion = Retirement.completion work in
              let duplicate =
                match Owner.signal_resource_retirement parent.runtime |> protocol_ok with
                | Retirement.Join same -> same
                | Run _ -> failwith "638 duplicate source signal elected another producer"
              in
              assert (phys_equal completion duplicate);
              assert (Option.is_none (Retirement.peek completion));
              Owner.ensure_loaded parent.runtime |> Load.expect_background_busy;
              phase := "public Stop takes over SAME queued source retirement";
              let closing =
                Eio.Fiber.fork_promise ~sw (fun () ->
                  Owner.unload_and_wait parent.runtime)
              in
              retiring := Some closing;
              Eio.Fiber.yield ();
              assert (Option.is_none (Eio.Promise.peek operation));
              assert (Option.is_none (Eio.Promise.peek closing));
              assert (Option.is_none (Retirement.peek completion));
              [%test_eq: int] 1 gate.reads;
              [%test_eq: int] saved.provider_count !(counters.providers);
              Source_wait.release gate;
              within (fun () ->
                Eio.Promise.await_exn operation |> expect_source_retirement_cancel);
              within (fun () -> Eio.Promise.await_exn closing |> protocol_ok);
              (match Retirement.peek completion with
               | Some (Returned (Ok ())) -> ()
               | Some (Returned (Error error)) -> raise_s [%sexp (error : P.Error.t)]
               | Some (Raised (exn, backtrace)) ->
                 Exn.raise_with_original_backtrace exn backtrace
               | None -> failwith "638 public Stop returned before SAME pending joined");
              Retirement.await duplicate |> protocol_ok;
              Retirement.run work;
              [%test_eq: int] 1 gate.reads;
              assert (Option.is_none !denied);
              phase := "observe denied construction after joined cleanup";
              let after = Warm.settled parent in
              let pointer, receipt = selected_lifecycle after in
              assert (A.Moderator_lifecycle.equal saved.lifecycle pointer);
              assert (E.equal before_receipt receipt);
              assert (List.is_empty (policy_receipts after));
              [%test_eq: int]
                saved.resume_count
                (List.length (Warm.completed after Session_resume));
              [%test_eq: int] saved.provider_count !(counters.providers);
              assert_unloaded parent;
              assert (Owner.source_lane_idle parent.runtime);
              Owner.unload_and_wait parent.runtime |> protocol_ok)));
  print_endline
    "actual source wait kept one signalled attempt; public Stop joined the held read and \
     actual finalizers before success; no publication or policy/provider work";
  [%expect
    {| actual source wait kept one signalled attempt; public Stop joined the held read and actual finalizers before success; no publication or policy/provider work |}]
;;
