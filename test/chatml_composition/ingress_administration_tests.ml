open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module I = Agent_session.External_ingress
module N = Notification_administration_tests

let%expect_test
    "reset and rebuild retire ingress authority and preserve archived receipts"
  =
  List.iter
    [ N.Reset_keep, false; Reset_drop, true; Rebuild, true ]
    ~f:(fun (replacement, accepted) ->
      let client = ref None in
      with_daemon
        ~sources:(Ingress_tests.sources Complete)
        ~expect_moderator:true
        ~calls:[ "watch-call", "watch", `Null ]
        ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon ->
          let connection = connection daemon (principal ()) in
          client := Some connection;
          connection)
        ~after_turn:(fun env handle entry ->
          let state () = A.state entry.actor |> protocol_ok in
          let initial = state () in
          let registration = List.hd_exn initial.ingress_registrations in
          let submit () =
            Agent_client.Ingress.submit
              (Option.value_exn !client)
              { session_id = initial.identity.session_id
              ; registration_id = registration.context.id
              ; namespace = registration.context.namespace
              ; idempotency_key =
                  P.Idempotency_key.of_string "archived-ingress-retry" |> protocol_ok
              ; payload = `String "accepted-before-reset"
              }
          in
          (match accepted with
           | false -> ()
           | true ->
             submit () |> protocol_ok |> ignore;
             Subscription_tests.await_subscription env entry);
          H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
          let before =
            retry_runtime_busy env (fun () ->
              let before = state () in
              let result =
                match replacement with
                | N.Reset_keep | Reset_drop ->
                  H.reset
                    handle
                    ~expected_revision:before.counters.revision
                    ~keep_history:
                      (match replacement with
                       | Reset_keep -> true
                       | _ -> false)
                    ~keep_tasks:false
                    ~keep_cache:true
                    ~keep_workspace:true
                    ~keep_grants:true
                    ~keep_labels:true
                | Rebuild ->
                  H.rebuild
                    handle
                    ~expected_revision:before.counters.revision
                    ~prompt_choice:Pinned
              in
              Result.map result ~f:(fun _ -> before))
          in
          let old_registration = List.hd_exn before.ingress_registrations in
          [%test_eq: int]
            (if accepted then 1 else 0)
            (List.length old_registration.receipts);
          let replaced = state () in
          [%test_eq: int] (before.identity.generation + 1) replaced.identity.generation;
          assert (List.is_empty replaced.ingress_registrations);
          assert (List.is_empty replaced.subscriptions);
          let archive = List.hd_exn replaced.conversation.compaction_archives in
          let archived =
            Agent_session.Compaction_archive.read
              ~env
              ~handle:(Option.value_exn entry.store_handle)
              ~max_payload_length:(16 * 1024 * 1024)
              archive
            |> protocol_ok
          in
          assert (I.equal old_registration (List.hd_exn archived.ingress_registrations));
          assert (
            P.Subscription.equal
              (List.hd_exn before.subscriptions)
              (List.hd_exn archived.subscriptions));
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          (match submit () with
           | Error { code = Permission_denied; _ } -> ()
           | _ -> failwith "old ingress retry was not rejected after generation change");
          let after = state () in
          assert (
            List.is_empty after.ingress_registrations && List.is_empty after.subscriptions);
          let restored =
            Agent_session.Session_state.sexp_of_t after
            |> Sexp.to_string_mach
            |> Agent_session.Session_persistence.restore_snapshot
            |> Background_recovery_tests.store_ok
          in
          assert (List.is_empty restored.ingress_registrations);
          unload_idle_runtime env entry.runtime;
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          assert (Result.is_error (submit ()));
          print_s
            [%sexp
              (replacement : N.replacement)
            , (accepted : bool)
            , "archive retained; old ingress denied"])
        (fun _ -> ()));
  [%expect
    {|
    (Reset_keep false "archive retained; old ingress denied")
    (Reset_drop true "archive retained; old ingress denied")
    (Rebuild true "archive retained; old ingress denied")
    |}]
;;
