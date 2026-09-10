open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol
module I = Agent_session.External_ingress
module R = Agent_session.Runtime_builder

let script =
  {|
let initial_state = 0
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* subscription = Subscription.create("upgrade-watch", `Some(6000), `No_wake) in
  let* timer = Schedule.after_ms_with_policy(3000, `String("old-timer"), `Deliver_once_immediately) in
  let* armed = Subscription.arm(subscription, 0, `Some(timer), `None) in
  let* registration = Ingress.register(subscription, 1, "external.upgrade", `Bool(true)) in
  let* () = Invocation.resolve(p.context.invocation_id,
    `Pending(`Subscription(subscription), `String("accepted"))) in
  Task.pure(state)
| `Internal_event(_) -> Task.fail("old completion reached replacement moderator")
| _ -> Task.pure(state)
|}
;;

let sources =
  [ ( "agent.chatmd"
    , {|<script id="watcher" language="chatml" kind="moderator" api="extensibility-v1" src="watcher.chatml"/>
<tool name="watch" type="moderator" moderator="watcher" input_schema="any.json" output_schema="string.json" completion_schema="string.json"/>|}
    )
  ; "watcher.chatml", script
  ; "any.json", "true"
  ; "string.json", {|{"type":"string"}|}
  ]
;;

let no_old_callback state =
  assert (
    not
      (R.moderator_snapshot_has_queued_events state.Agent_session.Session_state.moderator
       |> protocol_ok));
  assert (List.is_empty state.deliveries);
  assert (
    not
      (List.exists state.moderator_executions ~f:(fun execution ->
         P.Moderator_execution.equal_phase execution.context.phase Internal_event)))
;;

let%expect_test
    "upgraded moderators reject old ingress and timers while subscriptions expire"
  =
  List.iter
    (List.cartesian_product [ false; true ] [ false; true ])
    ~f:(fun (remove, queued) ->
      let host = ref None in
      with_daemon
        ~sources
        ~expect_moderator:true
        ~expected_schedules:1
        ~calls:[ "watch-call", "watch", `Null ]
        ~connect:(fun ~sw:_ ~env:_ ~root daemon ->
          let client = connection daemon (principal ()) in
          host := Some (root, daemon, client);
          client)
        ~after_turn:(fun env handle entry ->
          let state () = A.state entry.actor |> protocol_ok in
          H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
          (match queued with
           | false -> ()
           | true ->
             Background_shell_tests.wait env (fun () ->
               match (List.hd_exn (state ()).schedules).status with
               | Delivered -> true
               | Scheduled | Delivering -> false
               | _ -> failwith "timer did not reach the stopped moderator queue"));
          let before = state () in
          let subscription = List.hd_exn before.subscriptions in
          let timer = List.hd_exn before.schedules in
          let registration = List.hd_exn before.ingress_registrations in
          assert (Option.is_none subscription.result);
          (match queued, timer.status with
           | false, Scheduled | true, Delivered -> ()
           | _ -> failwith "missed selected upgrade boundary");
          [%test_eq: bool]
            queued
            (R.moderator_snapshot_has_queued_events before.moderator |> protocol_ok);
          let root, daemon, client = Option.value_exn !host in
          let filename, source =
            match remove with
            | false -> "watcher.chatml", script ^ "\nlet replacement_revision = 2\n"
            | true -> "agent.chatmd", "<developer>Coordinator removed.</developer>"
          in
          Prompt_upgrade_fixtures.replace
            env
            ~root
            ~daemon
            ~client
            ~handle
            ~entry
            ~filename
            ~source
            ~allow_migration:true;
          let archive = List.hd_exn (state ()).conversation.compaction_archives in
          let archived =
            Agent_session.Compaction_archive.read
              ~env
              ~handle:(Option.value_exn entry.store_handle)
              ~max_payload_length:(16 * 1024 * 1024)
              archive
            |> protocol_ok
          in
          [%test_eq: bool]
            queued
            (R.moderator_snapshot_has_queued_events archived.moderator |> protocol_ok);
          assert (P.Subscription.equal subscription (List.hd_exn archived.subscriptions));
          assert (
            Jsonaf.exactly_equal
              (P.Schedule.to_json timer)
              (P.Schedule.to_json (List.hd_exn archived.schedules)));
          no_old_callback (state ());
          let submit () =
            Agent_client.Ingress.submit
              client
              { session_id = before.identity.session_id
              ; registration_id = registration.context.id
              ; namespace = registration.context.namespace
              ; idempotency_key =
                  P.Idempotency_key.of_string "old-helper-result" |> protocol_ok
              ; payload = `String "OLD-HELPER-RESULT"
              }
          in
          (match remove, submit () with
           | false, Error { code = Permission_denied; _ }
           | true, Error { code = Invalid_request; _ } -> ()
           | _, result ->
             raise_s
               [%sexp
                 "unexpected old-source ingress outcome"
               , (result : (P.Ingress.Acknowledgement.t, P.Error.t) result)]);
          (match queued with
           | true -> ()
           | false ->
             Background_shell_tests.wait env (fun () ->
               match (List.hd_exn (state ()).schedules).status with
               | Failed { code = Permission_denied; _ } -> true
               | Scheduled | Delivering -> false
               | _ -> failwith "obsolete timer was delivered or silently discarded"));
          Subscription_tests.await_subscription env entry;
          let expired = state () in
          let saved = List.hd_exn expired.subscriptions in
          assert (P.Subscription.equal_context subscription.context saved.context);
          assert (Option.equal P.Completion.equal saved.result (Some Expired));
          [%test_eq: int] (subscription.epoch + 1) saved.epoch;
          let retained = List.hd_exn expired.schedules in
          assert (
            Jsonaf.exactly_equal
              (P.Schedule.to_json { timer with status = retained.status })
              (P.Schedule.to_json retained));
          assert (I.equal registration (List.hd_exn expired.ingress_registrations));
          no_old_callback expired;
          let visible =
            Agent_client.Admin.get_session client before.identity.session_id
            |> protocol_ok
          in
          assert (
            List.exists visible.extension_status ~f:(fun status ->
              match status.kind with
              | Subscription ->
                String.equal status.id (P.Id.Subscription.to_string saved.context.id)
                && String.equal status.state "expired"
              | _ -> false));
          assert (
            Jsonaf.exactly_equal
              (P.Schedule.to_json retained)
              (P.Schedule.to_json (List.hd_exn visible.schedules)));
          let restored =
            Agent_session.Session_state.sexp_of_t expired
            |> Sexp.to_string_mach
            |> Agent_session.Session_persistence.restore_snapshot
            |> Background_recovery_tests.store_ok
          in
          assert (P.Subscription.equal saved (List.hd_exn restored.subscriptions));
          Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          Agent_server.Runtime_owner.drain_idle_moderator entry.runtime
          |> protocol_ok
          |> ignore;
          no_old_callback (state ());
          assert (P.Subscription.equal saved (List.hd_exn (state ()).subscriptions));
          assert (Result.is_error (submit ()));
          print_s
            [%sexp
              (remove : bool)
            , (queued : bool)
            , "old queue archived; old authority excluded; subscription expired"])
        ~settle:Subscription_tests.await_subscription
        (fun _ -> ()));
  [%expect
    {|
    (false false
     "old queue archived; old authority excluded; subscription expired")
    (false true
     "old queue archived; old authority excluded; subscription expired")
    (true false
     "old queue archived; old authority excluded; subscription expired")
    (true true
     "old queue archived; old authority excluded; subscription expired")
    |}]
;;
