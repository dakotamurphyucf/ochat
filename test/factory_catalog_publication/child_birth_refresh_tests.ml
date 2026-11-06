open! Core
open Agent_server_test_support
module R = Retry_support
module A = Agent_session
module Actor = A.Session_actor
module Registry = Agent_server.Session_registry
module P = Agent_protocol
module D = Agent_store.Delegation_store

let%expect_test
    "actual child birth Store observation releases mailbox and fences stale capture"
  =
  let creation_fiber = Eio.Fiber.create_key () in
  let selected_parent = ref None in
  let hits = ref 0 in
  let reached, signal_reached = Eio.Promise.create () in
  let release, signal_release = Eio.Promise.create () in
  R.with_recoverable_fixture
    ~wrap_env:(fun env ->
      Crash_fault_io.wrap
        env
        ~boundary:Before_stat
        ~matches:(fun path ->
          Int.equal !hits 0
          && Option.is_some (Eio.Fiber.get creation_fiber)
          && Option.exists !selected_parent ~f:(fun parent ->
            String.is_suffix path ~suffix:("/" ^ parent ^ "/ARCHIVED")))
        ~reached:(fun _ ->
          Int.incr hits;
          Eio.Promise.resolve signal_reached ();
          Eio.Promise.await release))
    (fun env sw root daemon _policy parent runtime _revision counters ->
       let before = R.state parent in
       let definition = Child_usage_fault_tests.definition env root runtime in
       let key = P.Idempotency_key.of_string "554-outside-mailbox-birth" |> protocol_ok in
       selected_parent := Some (P.Id.Session.to_string before.identity.session_id);
       let creation =
         Eio.Fiber.fork_promise ~sw (fun () ->
           Eio.Fiber.with_binding creation_fiber () (fun () ->
             Child_usage_fault_tests.create_after_idle
               (Agent_server.Daemon.factory daemon)
               before.identity.session_id
               key
               definition))
       in
       Eio.Promise.await reached;
       (* This command must ACK while the actual Factory read is still parked.
          The actor fiber does not inherit [creation_fiber], so unrelated host
          observations cannot satisfy the selected-read oracle. *)
       let attachment, _ =
         Actor.attach parent.Registry.actor ~mode:Read_only ~subscribe:false
         |> protocol_ok
       in
       R.protect_cleanup
         ~finally:(fun () -> Actor.detach parent.actor attachment.id |> protocol_ok)
         (fun () ->
            let changed = R.state parent in
            assert (not (phys_equal before changed));
            [%test_eq: int] 0 !(counters.providers);
            Eio.Promise.resolve signal_release ();
            let result = Eio.Promise.await_exn creation in
            let failure = R.rejected result in
            if not ([%equal: P.Error.code] failure.code Conflict)
            then print_s [%sexp (failure : P.Error.t)];
            [%test_eq: P.Error.code] Conflict failure.code;
            [%test_eq: int] 1 !hits;
            [%test_eq: int] 0 !(counters.providers);
            let principal_id = Option.value_exn before.identity.creating_principal in
            let lower_key : D.Key.t =
              { parent_session_id = before.identity.session_id
              ; parent_generation = before.identity.generation
              ; principal_id
              ; idempotency_key = key
              }
            in
            let retained =
              D.find
                (Agent_store.Session_store.delegations (Agent_server.Daemon.store daemon))
                lower_key
              |> Source_preparation_tests.store_ok
            in
            assert (Option.is_none retained)));
  print_endline
    "parent ACK progresses during birth read; stale capture denies before lower reserve \
     or provider";
  [%expect
    {| parent ACK progresses during birth read; stale capture denies before lower reserve or provider |}]
;;
