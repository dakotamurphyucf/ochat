open Core
open Agent_server_test_support
open Background_fixtures
module P = Agent_protocol

type mode =
  | Immediate
  | Deferred
  | Cancel
  | Restart
[@@deriving sexp_of]

let agent mode =
  let immediate =
    match mode with
    | Immediate -> {|let* completed = Subscription.complete(id, 0, `String("done")) in|}
    | Deferred | Cancel | Restart -> ""
  in
  {|<script id="watcher" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = ""
let on_event ctx state event = match event with
| `Tool_invoked(p) -> (match p.context.tool_name with
  | "watch" ->
    let* id = Subscription.create("background", `None, `No_wake) in
|}
  ^ immediate
  ^ {|
    let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(id), `String("accepted"))) in
    Task.pure(id)
  | "finish" ->
    let* completed = Subscription.complete(state, 0, `String("done")) in
    let* () = Invocation.resolve(p.context.invocation_id, `Complete(`String("finished"))) in
    Task.pure(state)
  | _ -> Task.fail("unknown tool"))
| _ -> Task.pure(state)
</script>
<tool name="watch" type="moderator" moderator="watcher" input_schema="any.json" output_schema="string.json" completion_schema="string.json"/>
<tool name="finish" type="moderator" moderator="watcher" input_schema="any.json" output_schema="string.json"/>
|}
;;

let sources = [ "any.json", "true"; "string.json", {|{"type":"string"}|} ]

let capabilities entry =
  Agent_server.Runtime_owner.with_background_runtime
    entry.Agent_server.Session_registry.runtime
    (fun runtime ->
       Ok
         (Agent_session.Script_tool_calls.current_capabilities
            (Option.value_exn runtime.moderator_script_tools)))
  |> protocol_ok
;;

let finish env client entry capabilities =
  let job = submit entry (B.to_json (tool capabilities "finish" `Null)) in
  let _, completion = await env client job in
  assert (P.Completion.equal completion (Succeeded (`String "finished")))
;;

let check_ownership state parent =
  let subscription = List.hd_exn state.Agent_session.Session_state.subscriptions in
  [%test_eq: int] 1 (List.length state.subscriptions);
  (match subscription.context.parent_job with
   | Some (id, attempt) ->
     assert (P.Id.Job.equal id parent.J.id);
     [%test_eq: int] parent.attempt attempt
   | None -> failwith "subscription lost creating attempt");
  let targets =
    List.filter state.invocations ~f:(fun invocation ->
      String.equal invocation.context.tool_name "watch")
  in
  [%test_eq: int] 1 (List.length targets);
  assert (
    P.Id.Invocation.equal
      (List.hd_exn targets).context.id
      subscription.context.invocation_id);
  subscription
;;

let%expect_test
    "background subscription waits release capacity and retain ownership through \
     completion, cancellation and restart"
  =
  List.iter [ Immediate; Deferred; Cancel; Restart ] ~f:(fun mode ->
    let parent_id = ref None in
    let parent state =
      List.find_exn state.Agent_session.Session_state.jobs ~f:(fun job ->
        P.Id.Job.equal job.id (Option.value_exn !parent_id))
    in
    let check_result = function
      | P.Completion.Succeeded (`String "done")
        when match mode with
             | Cancel -> false
             | _ -> true -> ()
      | Cancelled _
        when match mode with
             | Cancel -> true
             | _ -> false -> ()
      | result -> raise_s [%sexp (result : P.Completion.t)]
    in
    with_background_daemon
      ~agent:(agent mode)
      ~sources
      ~per_session_jobs:1
      ~check_restored:(fun before after ->
        assert (P.Id.Job.equal before.id after.id);
        [%test_eq: int] before.attempt after.attempt)
      ~after_recovery:(fun env client entry before ->
        let parent = parent before in
        (match mode with
         | Restart ->
           assert (Option.is_none (check_ownership before parent).result);
           finish env client entry (capabilities entry)
         | _ -> ());
        let _, completion = await env client parent in
        check_result completion;
        let after = A.state entry.actor |> protocol_ok in
        let subscription = check_ownership after parent in
        assert (
          P.Subscription.equal_context
            (List.hd_exn before.subscriptions).context
            subscription.context);
        print_s
          [%sexp
            (mode : mode)
          , (completion : P.Completion.t)
          , (subscription.result : P.Completion.t option)])
      (fun env client entry capabilities ->
         let admitted = submit entry (B.to_json (tool capabilities "watch" `Null)) in
         parent_id := Some admitted.id;
         (match mode with
          | Immediate -> ()
          | Deferred | Cancel | Restart ->
            let dependency =
              Background_pending_restart_tests.waiting env entry.actor admitted.id
            in
            let state = A.state entry.actor |> protocol_ok in
            let subscription = check_ownership state (parent state) in
            assert (
              P.Invocation.equal_work
                dependency.work
                (Subscription subscription.context.id));
            assert (Option.is_none subscription.result));
         (match mode with
          | Deferred -> finish env client entry capabilities
          | Cancel ->
            A.cancel_job_internal entry.actor ~job_id:admitted.id |> protocol_ok |> ignore;
            (* Completing later must retain the cancellation winner. *)
            finish env client entry capabilities
          | Immediate | Restart -> ());
         match mode with
         | Restart -> ()
         | _ ->
           let _, completion = await env client admitted in
           check_result completion));
  [%expect
    {|
    (Immediate (Succeeded (String done)) ((Succeeded (String done))))
    (Deferred (Succeeded (String done)) ((Succeeded (String done))))
    (Cancel (Cancelled "job cancelled")
     ((Cancelled "owning job stopped waiting")))
    (Restart (Succeeded (String done)) ((Succeeded (String done))))
    |}]
;;
