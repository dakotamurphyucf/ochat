open Core
open Agent_server_test_support
open Background_fixtures
module P = Agent_protocol

type mode =
  | Immediate
  | Deferred
  | Cancel_parent
  | Restart
[@@deriving sexp_of]

let agent mode =
  let delay =
    match mode with
    | Immediate | Deferred -> 20
    | Cancel_parent -> 100000
    | Restart -> 1000
  in
  let finish =
    match mode with
    | Immediate -> {|let* finished = Subscription.complete(id, 1, `String("done")) in|}
    | _ -> ""
  in
  {|<script id="watcher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = { id = ""; ticks = 0 }
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* id = Subscription.create("timer", `Some(10000), `No_wake) in
  let* () = Task.catch(
    (let* discarded = Schedule.after_ms(0, `String("discarded")) in
     let* armed = Subscription.arm(id, 0, `Some(discarded), `None) in
     Task.fail("discard timer and binding")),
    fun message -> Task.pure(())) in
|}
  ^ sprintf
      {|  let* timer = Schedule.after_ms_with_policy(%d, `String("Tool_invoked"), `Deliver_once_immediately) in
|}
      delay
  ^ {|  let* armed = Subscription.arm(id, 0, `Some(timer), `None) in
  let* () = Task.catch(
    (let* invalid = Subscription.arm(id, 1, `Some(timer), `None) in Task.fail("discard rebind")),
    fun message -> Task.pure(())) in
  let* () = Task.catch(
    (let* temporary = Subscription.complete(id, 1, `String("discarded completion")) in Task.fail("discard completion")),
    fun message -> Task.pure(())) in
  let* () = Task.catch(
    (let* () = Schedule.cancel(timer) in Task.fail("discard cancellation")),
    fun message -> Task.pure(())) in
  let* view = Schedule.get(timer) in
|}
  ^ finish
  ^ {|
  let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(id), `String("accepted"))) in
  Task.pure({ id = id; ticks = 0 })
| `Internal_event(payload) -> (match payload with
  | `String("Tool_invoked") ->
    let* finished = Subscription.complete(state.id, 1, `String("done")) in
    Task.pure({ id = state.id; ticks = state.ticks + 1 })
  | _ -> Task.fail("discarded timer executed"))
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="watcher" input_schema="any.json"
 output_schema="string.json" completion_schema="string.json"/>
|}
;;

let%expect_test
    "ChatML timer binding rolls back with catch and completes across callbacks, \
     cancellation and restart"
  =
  List.iter [ Immediate; Deferred; Cancel_parent; Restart ] ~f:(fun mode ->
    let check = function
      | P.Completion.Succeeded (`String "done")
        when match mode with
             | Cancel_parent -> false
             | _ -> true -> ()
      | Cancelled _
        when match mode with
             | Cancel_parent -> true
             | _ -> false -> ()
      | result -> raise_s [%sexp (result : P.Completion.t)]
    in
    with_background_daemon
      ~agent:(agent mode)
      ~sources:Background_subscription_tests.sources
      ~before_recovery:(fun env before ->
        match mode with
        | Restart ->
          let timer = List.hd_exn before.Agent_session.Session_state.schedules in
          let remaining =
            Time_ns.diff
              (P.Timestamp.to_time_ns timer.next_due_at)
              (P.Timestamp.to_time_ns (P.Timestamp.now ()))
            |> Time_ns.Span.to_sec
          in
          Eio.Time.sleep (Eio.Stdenv.clock env) (Float.max 0. remaining +. 0.05)
        | _ -> ())
      ~check_restored:(fun before after ->
        assert (P.Id.Job.equal before.id after.id);
        [%test_eq: int] before.attempt after.attempt)
      ~after_recovery:(fun env client entry before ->
        let parent = List.hd_exn before.Agent_session.Session_state.jobs in
        let _, result = await env client parent in
        check result;
        let state = A.state entry.actor |> protocol_ok in
        let subscription = Background_subscription_tests.check_ownership state parent in
        [%test_eq: int] 1 (List.length state.schedules);
        [%test_eq: int] 2 subscription.epoch;
        let timer = List.hd_exn state.schedules in
        (match (Option.value_exn timer.ownership).subscription with
         | Some (id, 1) -> assert (P.Id.Subscription.equal id subscription.context.id)
         | _ -> failwith "timer binding was lost");
        (match mode, timer.status with
         | (Immediate | Cancel_parent), P.Schedule.Cancelled
         | (Deferred | Restart), Delivered -> ()
         | _ -> failwith "unexpected timer lifecycle");
        print_s
          [%sexp
            (mode : mode), (result : P.Completion.t), (timer.status : P.Schedule.status)])
      (fun env client entry capabilities ->
         let parent = submit entry (B.to_json (tool capabilities "watch" `Null)) in
         (match mode with
          | Cancel_parent | Restart ->
            Background_pending_restart_tests.waiting env entry.actor parent.id |> ignore;
            (match mode with
             | Cancel_parent ->
               A.cancel_job_internal entry.actor ~job_id:parent.id
               |> protocol_ok
               |> ignore
             | _ -> ())
          | _ -> ());
         match mode with
         | Restart ->
           assert (
             Option.is_none
               (List.hd_exn (A.state entry.actor |> protocol_ok).subscriptions).result)
         | _ ->
           let _, result = await env client parent in
           check result));
  [%expect
    {|
    (Immediate (Succeeded (String done)) Cancelled)
    (Deferred (Succeeded (String done)) Delivered)
    (Cancel_parent (Cancelled "job cancelled") Cancelled)
    (Restart (Succeeded (String done)) Delivered)
    |}]
;;

let%expect_test
    "foreground moderator timers share the transactional adapter without extra model \
     requests"
  =
  List.iter [ Immediate; Deferred ] ~f:(fun mode ->
    Fixtures.with_daemon
      ~expect_moderator:true
      ~expected_schedules:1
      ~sources:(("agent.chatmd", agent mode) :: Background_subscription_tests.sources)
      ~calls:[ "watch-call", "watch", `Null ]
      ~settle:Subscription_tests.await_subscription
      (fun state ->
         let subscription = List.hd_exn state.Agent_session.Session_state.subscriptions in
         let timer = List.hd_exn state.schedules in
         assert (
           Option.equal
             P.Completion.equal
             subscription.result
             (Some (Succeeded (`String "done"))));
         print_s [%sexp (mode : mode), (timer.status : P.Schedule.status)]));
  [%expect
    {|
    (Immediate Cancelled)
    (Deferred Delivered)
    |}]
;;
