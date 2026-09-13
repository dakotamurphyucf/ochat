open Core
open Fixtures
open Job_fixtures
module P = Agent_protocol
module Setup = Subscription_transaction_tests
module Timers = Schedule_transaction_tests

let reference : Chat_response.Notification_operations.correlation =
  { key = "notice"; invocation_id = None; work = None }
;;

let create actor owner =
  A.create_script_notification
    actor
    ~owner
    ~source:Setup.source
    ~correlation:reference
    ~completion:(Succeeded (`String "ready"))
    ~wake:No_wake
;;

let%expect_test
    "notification staging shares quota and rolls back with the actual owning checkpoint"
  =
  List.iter [ `Accepted; `Rejected; `Unselected; `Abandoned ] ~f:(fun mode ->
    let reject_save = ref false in
    with_actor
      ~reject_save:(fun _ -> !reject_save)
      ~notification_limits:
        { Agent_session.Staged_notifications.default_limits with
          max_pending = 1
        ; max_per_source = 1
        }
      (fun _env _sw actor _writer backend ->
         A.change_moderator actor (Some (Setup.encode Setup.before))
         |> protocol_ok
         |> ignore;
         let parent = add_claimed_job actor in
         let retained = ref None in
         let outcome =
           Timers.with_event actor parent (fun owner commit ->
             let discarded, _ = create actor owner |> protocol_ok in
             (match create actor owner with
              | Error { code = Resource_limit; _ } -> ()
              | _ -> failwith "quota was bypassed");
             A.abort_notification_mutation actor ~owner ~receipt:discarded |> protocol_ok;
             let receipt, value = create actor owner |> protocol_ok in
             retained := Some value;
             assert (List.is_empty (Agent_session.Memory_backend.state backend).deliveries);
             assert (
               Result.is_error
                 (A.read_script_notification
                    actor
                    ~owner
                    ~source:{ Setup.source with source_sha256 = String.make 64 'c' }
                    ~id:value.context.id));
             let read =
               A.read_script_notification
                 actor
                 ~owner
                 ~source:Setup.source
                 ~id:value.context.id
               |> protocol_ok
             in
             assert (
               Jsonaf.exactly_equal (P.Delivery.to_json value) (P.Delivery.to_json read));
             let receipts =
               match mode with
               | `Unselected -> []
               | _ -> [ receipt ]
             in
             A.select_notification_mutations actor ~owner ~source:Setup.source ~receipts
             |> protocol_ok;
             match mode with
             | `Abandoned -> Error (handoff_error "abandoned")
             | _ ->
               (reject_save
                := match mode with
                   | `Rejected -> true
                   | _ -> false);
               let result = Timers.save commit in
               reject_save := false;
               result)
         in
         (match mode, outcome with
          | (`Rejected | `Abandoned), Error _ | (`Accepted | `Unselected), Ok _ -> ()
          | _ -> failwith "unexpected notification checkpoint outcome");
         let state = A.state actor |> protocol_ok in
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         assert (
           Option.is_some (A.with_quiescent_state actor ~f:(fun _ -> Ok ()) |> protocol_ok));
         let restored =
           Agent_session.Session_persistence.restore_snapshot
             (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
           |> store_ok
         in
         assert_same_session_snapshot state restored;
         (match mode, restored.deliveries with
          | `Accepted, [ value ] ->
            assert (
              Jsonaf.exactly_equal
                (P.Delivery.to_json value)
                (P.Delivery.to_json (Option.value_exn !retained)))
          | (`Rejected | `Unselected | `Abandoned), [] -> ()
          | _ -> failwith "notification escaped its transaction");
         print_s
           [%sexp
             (mode : [ `Accepted | `Rejected | `Unselected | `Abandoned ])
           , (List.length restored.deliveries : int)]));
  [%expect
    {|
    (Accepted 1)
    (Rejected 0)
    (Unselected 0)
    (Abandoned 0)
    |}]
;;
