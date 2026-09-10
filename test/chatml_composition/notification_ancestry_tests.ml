open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module N = Agent_session.Notification_readiness

let%expect_test
    "actual nested ChatML notification commits once after its outer acknowledgement and \
     restores"
  =
  let original_invocations = ref [] in
  with_daemon
    ~sources:(Notification_admission_tests.sources Nested)
    ~expect_moderator:true
    ~calls:
      [ ( "outer"
        , "run_chatml"
        , `Object
            [ ( "source"
              , `String
                  {|let main input = let* result = Tool.call("watch", input) in
match result with | `Ok(value) -> Task.pure(value) | `Error(code) -> Task.fail(code)|}
              )
            ; "input", `Null
            ; "tools", `Array [ `String "watch" ]
            ] )
      ]
    ~settle:(fun _ registry_entry ->
      let actor = registry_entry.Agent_server.Session_registry.actor in
      let state = A.state actor |> protocol_ok in
      original_invocations := state.invocations;
      let delivery = List.hd_exn state.deliveries in
      (match
         N.check
           ~invocations:state.invocations
           ~jobs:state.jobs
           ~events:state.moderator_executions
           delivery
       with
       | Ok () -> ()
       | Error reason -> raise_s [%sexp (reason : N.blockage)]);
      let id =
        match delivery.status with
        | Committed { history_id; _ } -> history_id
        | _ -> failwith "nested notification was not automatically delivered"
      in
      let entry = Agent_session.Notification_history.create ~id delivery |> protocol_ok in
      let committed =
        P.Delivery.commit delivery ~history_id:id ~now:(P.Timestamp.now ()) |> protocol_ok
      in
      List.iter [ (); () ] ~f:(fun () ->
        let state = A.state actor |> protocol_ok in
        A.commit_extensions
          actor
          ~generation:state.identity.generation
          ~expected_revision:state.counters.revision
          [ Publish (committed, entry) ]
        |> protocol_ok
        |> ignore))
    (fun state ->
       assert (List.equal P.Invocation.equal !original_invocations state.invocations);
       let notifications =
         List.filter state.conversation.canonical_history ~f:(fun entry ->
           match entry.P.History.provenance with
           | Runtime_notification _ -> true
           | _ -> false)
       in
       [%test_eq: int] 1 (List.length notifications);
       let root =
         List.find_exn state.invocations ~f:(fun value ->
           P.Invocation.equal_origin value.context.origin Model)
       in
       let nested =
         List.find_exn state.invocations ~f:(fun value ->
           String.equal value.context.tool_name "watch")
       in
       (match root.status, nested.status, nested.output_entry_id with
        | Published _, Resolved (Pending _), None -> ()
        | _ -> failwith "nested call acquired a fabricated provider output");
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
         |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
         |> protocol_ok
       in
       assert (List.equal P.Invocation.equal state.invocations restored.invocations);
       assert (
         Jsonaf.exactly_equal
           (P.Delivery.to_json (List.hd_exn state.deliveries))
           (P.Delivery.to_json (List.hd_exn restored.deliveries)));
       print_endline
         "outer Published; nested Resolved; one notification; snapshot restored");
  [%expect {| outer Published; nested Resolved; one notification; snapshot restored |}]
;;
