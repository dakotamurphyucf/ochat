open! Core
open Agent_server_test_support
open Retry_support
module S = Cold_policy_recovered_support
module Owner = Agent_server.Runtime_owner
module Scope = Owner.Policy_restoration_source
module Actor = A.Session_actor
module Store = Agent_store.Session_store

let within env f = Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f

(* History reservation is real construction work and can advance counters/high
   watermarks. Publication denial cannot advance the policy checkpoint, selected
   lifecycle/source, receipt, actual canonical history or background work. *)
let assert_no_execution (before : A.Session_state.t) (after : A.Session_state.t) =
  assert (P.Id.Session.equal before.identity.session_id after.identity.session_id);
  [%test_eq: int] before.identity.generation after.identity.generation;
  [%test_eq: int64] before.stop_epoch after.stop_epoch;
  assert (P.Session.equal_desired_state before.lifecycle.desired after.lifecycle.desired);
  assert (
    Sexp.equal
      ([%sexp_of: P.Session.observed_state] before.lifecycle.observed)
      ([%sexp_of: P.Session.observed_state] after.lifecycle.observed));
  assert (
    A.Session_state.Runtime_initialization.equal
      before.runtime_initialization
      after.runtime_initialization);
  assert (Option.equal Jsonaf.exactly_equal before.moderator after.moderator);
  assert (Option.equal A.Run_state.equal before.run_state after.run_state);
  let pointer state =
    Option.bind state.A.Session_state.run_state ~f:A.Run_state.moderator_lifecycle
  in
  assert (Option.equal A.Moderator_lifecycle.equal (pointer before) (pointer after));
  assert (
    P.Run_source.equal
      (Delegated_warm_resource_tests.source before)
      (Delegated_warm_resource_tests.source after));
  assert (
    List.equal
      P.Moderator_execution.equal
      before.moderator_executions
      after.moderator_executions);
  assert (
    List.equal
      P.History.equal_entry
      before.conversation.canonical_history
      after.conversation.canonical_history);
  let same_json to_json left right =
    List.equal Jsonaf.exactly_equal (List.map left ~f:to_json) (List.map right ~f:to_json)
  in
  assert (same_json P.Job.to_json before.jobs after.jobs);
  assert (same_json P.Invocation.to_json before.invocations after.invocations);
  assert (same_json P.Schedule.to_json before.schedules after.schedules);
  [%test_eq: bool] before.halted after.halted;
  [%test_eq: string option] before.halt_reason after.halt_reason;
  assert (
    Option.equal
      Jsonaf.exactly_equal
      (Option.map before.failure ~f:P.Error.to_json)
      (Option.map after.failure ~f:P.Error.to_json))
;;

let assert_original_denial = function
  | Error failure ->
    [%test_eq: P.Error.code] Permission_denied failure.P.Error.code;
    [%test_eq: bool] false failure.retryable;
    [%test_eq: string]
      "original agent execution authority is no longer current"
      failure.message
  | Ok _ -> failwith "634 revoked original published a restored Owner anchor"
;;

let assert_lock_held daemon session_id =
  let store = Daemon.store daemon in
  Eio.Switch.run (fun sw ->
    match
      Store.open_session
        store
        ~sw
        ~actor_lock_nonce:(P.Id.Transaction.create () |> P.Id.Transaction.to_string)
        session_id
    with
    | Error (Locked _) -> ()
    | Error failure ->
      raise_s
        [%sexp
          "634 unexpected retained lock error", (failure : Agent_store.Store_error.t)]
    | Ok handle ->
      Store.close_session store handle |> Existing.store_ok;
      failwith "634 pending restored physical close released its actual Handle lock")
;;

let%expect_test
    "restored Owner retains its real Cell loan before revoked original publication"
  =
  let phase = ref "634 Owner custody host" in
  S.with_recovered_host ~phase (fun host with_request ->
    let env = host.S.env in
    let sw = host.sw in
    let daemon = host.daemon in
    let actor = host.actor in
    let owner = S.owner host in
    let before = Actor.state actor |> protocol_ok in
    assert_no_execution host.recovered_state before;
    assert (not (Owner.is_loaded owner));
    assert (not (Owner.has_activation_handoff owner));
    let armed = ref false in
    let refusals = ref 0 in
    (* A real trusted-host currentness policy. It is deliberately nonyielding;
       no promise, Actor request, IO or call-count selection happens here. *)
    let original =
      Operator_authorization.guarded ~principal ~is_current:(fun () ->
        if !armed
        then (
          Int.incr refusals;
          false)
        else true)
    in
    assert (Operator_authorization.is_current original);
    with_request ~original (fun request ->
      request.Cold_policy_resource_tests.authorize () |> protocol_ok;
      let reached, reach = Eio.Promise.create () in
      let released, release = Eio.Promise.create () in
      let release_close () =
        if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
      in
      let cell_publications = ref 0 in
      let owner_publications = ref 0 in
      let physical_releases = ref 0 in
      let before_publish () =
        [%test_eq: int] 0 !cell_publications;
        assert (not !armed);
        [%test_eq: int] 0 !refusals;
        Int.incr cell_publications;
        (* The fixed helper calls this ONLY after actual Build.publish_restored
           returned the first loan, immediately before actual Scope.publish. *)
        armed := true
      in
      let observe_publication publication =
        [%test_eq: int] 1 !cell_publications;
        [%test_eq: int] 0 !owner_publications;
        assert !armed;
        assert (!refusals > 0);
        (match Scope.Publication.custody publication with
         | Owner_retains -> ()
         | Caller_retains ->
           failwith "634 post-transfer original denial lost actual Owner custody");
        (* Fixed helper records custody BEFORE this observer and before resuming
           the original primary; this callback never resumes or closes the loan. *)
        Int.incr owner_publications
      in
      let on_physical_release () =
        [%test_eq: int] 1 !owner_publications;
        [%test_eq: int] 0 !physical_releases;
        Int.incr physical_releases;
        Eio.Promise.resolve reach ();
        Eio.Promise.await released
      in
      let attempt =
        Eio.Fiber.fork_promise ~sw (fun () ->
          S.prepare_owner_policy
            host
            request
            ~before_publish
            ~observe_publication
            ~on_physical_release)
      in
      let rejected_source = ref None in
      protect_cleanup
        ~finally:(fun () ->
          (* Release BEFORE joining either cancellation-protected physical
             closer, including after a rejected-source assertion or timeout. *)
          release_close ();
          Exn.protect
            ~finally:(fun () ->
              Option.iter !rejected_source ~f:(fun rejected ->
                within env (fun () -> Eio.Promise.await_exn rejected) |> ignore))
            ~f:(fun () -> within env (fun () -> Eio.Promise.await_exn attempt) |> ignore))
        (fun () ->
           let phase =
             within env (fun () ->
               Eio.Fiber.first
                 (fun () ->
                    Eio.Promise.await reached;
                    `Closing)
                 (fun () -> `Returned (Eio.Promise.await_exn attempt)))
           in
           (match phase with
            | `Closing -> ()
            | `Returned result ->
              assert_original_denial result;
              failwith "634 publication denial bypassed its actual physical close");
           [%test_eq: int] 1 !cell_publications;
           [%test_eq: int] 1 !owner_publications;
           [%test_eq: int] 1 !physical_releases;
           assert (Option.is_none (Eio.Promise.peek attempt));
           (* Queries must progress while the actual private switch is joining;
             the close path must not retain the Owner mutex or Actor mailbox. *)
           let query =
             Eio.Fiber.fork_promise ~sw (fun () ->
               let loaded = Owner.is_loaded owner in
               let idle = Owner.source_lane_idle owner in
               let current = Actor.state actor |> protocol_ok in
               loaded, idle, current)
           in
           let loaded, idle, current =
             within env (fun () -> Eio.Promise.await_exn query)
           in
           assert (not loaded);
           assert (not idle);
           assert_no_execution before current;
           (* This genuine source owner is rejected before reserving a stage.
              Its finalizer must not select/join the unrelated retained anchor
              while the original physical-close gate is still held. *)
           let source_callbacks = ref 0 in
           let rejected =
             Eio.Fiber.fork_promise ~sw (fun () ->
               Owner.with_delegated_load_source_reported
                 owner
                 ~source_owner:host.source_owner
                 ~custody:(Actor.Delegated_load.Custody.Host None)
                 (fun _source ->
                    Int.incr source_callbacks;
                    Ok ()))
           in
           rejected_source := Some rejected;
           let report =
             (* Expire this observer before the enclosing 30-second fixture,
                so a blocked rejected request reports its own failure boundary. *)
             try
               Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                 Eio.Promise.await_exn rejected)
             with
             | Eio.Time.Timeout as exn ->
               let backtrace = Stdlib.Printexc.get_raw_backtrace () in
               (try
                  print_s
                    [%sexp
                      "rejected source timed out before unrelated close was released"
                    , (Option.is_none (Eio.Promise.peek released) : bool)
                    , (!physical_releases : int)]
                with
                | _ -> ());
               Exn.raise_with_original_backtrace exn backtrace
           in
           (match Owner.Scope_outcome.primary report with
            | Returned (Error { code = Conflict; _ }) -> ()
            | Returned (Error error) -> raise_s [%sexp (error : P.Error.t)]
            | Returned (Ok ()) -> failwith "647 rejected source unexpectedly admitted"
            | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace);
           (match
              Owner.Scope_outcome.cleanup report |> Owner.Scope_outcome.Cleanup.status
            with
            | Never_acquired -> ()
            | Joined | Failed _ ->
              failwith "647 rejected source captured another source's physical cleanup");
           [%test_eq: int] 0 !source_callbacks;
           [%test_eq: int] 1 !physical_releases;
           assert (Option.is_none (Eio.Promise.peek released));
           assert (Option.is_none (Eio.Promise.peek attempt));
           assert (not (Owner.reserve_inactive_close owner));
           (match Owner.ensure_loaded owner with
            | Error failure -> [%test_eq: P.Error.code] Conflict failure.P.Error.code
            | Ok () -> failwith "634 fresh runtime bypassed retained publication cleanup");
           within env (fun () -> assert_lock_held daemon before.identity.session_id);
           assert (not (Owner.has_activation_handoff owner));
           release_close ();
           let result = within env (fun () -> Eio.Promise.await_exn attempt) in
           assert_original_denial result;
           [%test_eq: int] 1 !physical_releases;
           let after = Actor.state actor |> protocol_ok in
           assert_no_execution before after;
           assert (not (Owner.is_loaded owner));
           assert (not (Owner.has_activation_handoff owner));
           assert (Owner.source_lane_idle owner);
           Owner.close_and_wait owner;
           Owner.close_and_wait owner;
           [%test_eq: int] 1 !physical_releases;
           (* RecoveryOwner still owns Actor/writer/Handle here. The shared host
             finalizer joins this actual Owner before releasing those resources. *)
           assert (Store.owns_handle (Daemon.store daemon) host.handle))));
  print_endline
    "actual Cell publication -> Owner custody -> original denial; joined physical close \
     preserves source and primary";
  [%expect
    {| actual Cell publication -> Owner custody -> original denial; joined physical close preserves source and primary |}]
;;
