open Core
open Agent_server_test_support
open Background_fixtures
module P = Agent_protocol

let agent =
  {|<script id="watcher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = ""
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* id = Subscription.create("expiry", `Some(1000), `No_wake) in
  let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(id), `String("accepted"))) in
  Task.pure(id)
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="watcher" input_schema="any.json"
 output_schema="string.json" completion_schema="string.json"/>
|}
;;

let%expect_test
    "subscriptions expire without another tool call, including overdue restart"
  =
  List.iter [ `Live; `Restart ] ~f:(fun mode ->
    with_background_daemon
      ~agent
      ~sources:Background_subscription_tests.sources
      ~before_recovery:(fun env before ->
        match mode with
        | `Live -> ()
        | `Restart ->
          let subscription =
            List.hd_exn before.Agent_session.Session_state.subscriptions
          in
          let remaining =
            Time_ns.diff
              (P.Timestamp.to_time_ns subscription.context.deadline)
              (P.Timestamp.to_time_ns (P.Timestamp.now ()))
            |> Time_ns.Span.to_sec
          in
          Eio.Time.sleep (Eio.Stdenv.clock env) (Float.max 0. remaining +. 0.05))
      ~check_restored:(fun before after ->
        assert (P.Id.Job.equal before.id after.id);
        [%test_eq: int] before.attempt after.attempt)
      ~after_recovery:(fun env client entry before ->
        let original = List.hd_exn before.Agent_session.Session_state.jobs in
        let _, completion = await env client original in
        assert (P.Completion.equal completion Expired);
        let state = A.state entry.actor |> protocol_ok in
        let subscription = Background_subscription_tests.check_ownership state original in
        assert (Option.equal P.Completion.equal subscription.result (Some Expired));
        assert (
          P.Subscription.equal_context
            (List.hd_exn before.subscriptions).context
            subscription.context);
        [%test_eq: int] 1 subscription.epoch;
        print_s
          [%sexp
            (mode : [ `Live | `Restart ]), (subscription.result : P.Completion.t option)])
      (fun env client entry capabilities ->
         let job = submit entry (B.to_json (tool capabilities "watch" `Null)) in
         Background_pending_restart_tests.waiting env entry.actor job.id |> ignore;
         match mode with
         | `Restart ->
           let state = A.state entry.actor |> protocol_ok in
           assert (Option.is_none (List.hd_exn state.subscriptions).result)
         | `Live ->
           let _, result = await env client job in
           assert (P.Completion.equal result Expired)));
  [%expect
    {|
    (Live (Expired))
    (Restart (Expired))
    |}]
;;
