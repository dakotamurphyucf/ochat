open Core
open Agent_server_test_support
open Fixtures
module B = Agent_session.Automatic_turn_budget

let%expect_test
    "qualified daemon follow-up limits survive runtime unload and reopen until actual \
     user admission"
  =
  let read actor = A.state actor |> protocol_ok in
  let budget actor = Option.value_exn (read actor).automatic_turn_budget in
  with_daemon
    ~sources:(Notification_admission_tests.sources Immediate)
    ~expect_moderator:true
    ~calls:[ "publish", "watch", `Null ]
    ~expected_requests:5
    ~after_turn:(fun env handle entry ->
      let actor = entry.Agent_server.Session_registry.actor in
      let idle () =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
          let rec wait () =
            match (read actor).active_operation with
            | None -> ()
            | Some _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
              wait ()
          in
          wait ())
      in
      let request () =
        A.claim_idle_moderator actor |> protocol_ok |> Option.value_exn |> ignore;
        A.complete_idle_moderator
          actor
          { moderator_snapshot = (read actor).moderator
          ; runtime_requests = [ Request_turn ]
          ; notifications = []
          ; remaining_events = false
          }
        |> protocol_ok;
        idle ()
      in
      [%test_eq: int] 0 (budget actor).followup_turns;
      request ();
      [%test_eq: int] 1 (budget actor).followup_turns;
      let before = budget actor in
      Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
      Agent_server.Runtime_owner.ensure_loaded entry.runtime |> protocol_ok;
      assert (B.equal before (budget actor));
      request ();
      assert (B.equal before (budget actor));
      H.send_message
        handle
        { kind = Plain_text; text = "Continue after the limit."; attachments = [] }
      |> protocol_ok
      |> ignore;
      idle ();
      [%test_eq: int] 0 (budget actor).followup_turns;
      request ();
      [%test_eq: int] 1 (budget actor).followup_turns)
    ~settle:(fun _ _ -> ())
    (fun state ->
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
         |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
         |> protocol_ok
       in
       assert (
         Option.equal B.equal state.automatic_turn_budget restored.automatic_turn_budget);
       print_endline
         "five actual provider requests; reload preserved the limit, user admission \
          reset it");
  [%expect
    {| five actual provider requests; reload preserved the limit, user admission reset it |}]
;;
