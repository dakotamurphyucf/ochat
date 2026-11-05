open Core
open Agent_server_test_support
open Background_fixtures

type trigger =
  | Lifecycle
  | Queued
  | Handler
  | Pre_tool
  | Observation
[@@deriving sexp_of]

let agent trigger =
  let start_operation =
    match trigger with
    | Lifecycle -> "Tool.spawn"
    | Queued | Handler | Pre_tool | Observation -> "Job.start_tool"
  in
  let start =
    "Task.bind("
    ^ start_operation
    ^ {|("read_file", `Object([
      {key = "root"; value = `String("reports")},
      {key = "file"; value = `String("second.txt")}
    ])), fun job -> Task.pure(true))|}
  in
  let resolve =
    {|Task.bind(Invocation.resolve(p.context.invocation_id,
      `Complete(`String("called"))), fun ignored -> Task.pure(state))|}
  in
  let handler =
    match trigger with
    | Handler ->
      "| `Tool_invoked(p) -> Task.bind(" ^ start ^ ", fun state -> " ^ resolve ^ ")"
    | Lifecycle | Queued | Pre_tool | Observation -> "| `Tool_invoked(p) -> " ^ resolve
  in
  let branch =
    match trigger with
    | Lifecycle -> "| `Session_start -> if state then Task.pure(state) else " ^ start
    | Queued ->
      "| `Session_start -> Task.bind(Runtime.emit(`String(\"launch\")), fun ignored -> \
       Task.pure(state))\n"
      ^ "| `Internal_event(p) -> if state then Task.pure(state) else "
      ^ start
    | Handler -> ""
    | Pre_tool -> "| `Pre_tool_call(p) -> if state then Task.pure(state) else " ^ start
    | Observation ->
      {|
| `Session_start -> Task.bind(Tool.spawn("read_file", `Object([
    {key = "root"; value = `String("reports")},
    {key = "file"; value = `String("report.txt")}
  ])), fun job -> Task.pure(state))
| `Tool_observed(p) -> if state then Task.pure(state) else |}
      ^ start
  in
  native_agent
  ^ {|<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = false
let on_event = fun ctx state event -> match event with
|}
  ^ handler
  ^ "\n"
  ^ branch
  ^ {|
| _ -> Task.pure(state)
</script>
<tool name="counter" type="moderator" moderator="owner"
 input_schema="input.json" output_schema="output.json"/>|}
;;

let rec launched_job env actor =
  let state = A.state actor |> protocol_ok in
  match List.filter state.jobs ~f:(fun job -> Option.is_some job.launch) with
  | [ job ] -> job
  | [] ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    launched_job env actor
  | _ -> failwith "moderator launched duplicate jobs"
;;

let submit_during_loaded_read registry env entry request =
  let initial = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
  Agent_server.Session_registry.with_loaded_read
    registry
    initial.identity.session_id
    (fun actual ->
       assert (phys_equal actual entry);
       let parent = submit entry request in
       let rec await () =
         let current = A.state entry.actor |> protocol_ok in
         let parent_row =
           List.find_exn current.jobs ~f:(fun job ->
             Agent_protocol.Id.Job.equal job.J.id parent.id)
         in
         let terminal (job : J.t) =
           match job.status with
           | Queued | Running | Waiting_permission _ | Waiting_completion _ -> false
           | Succeeded | Failed _ | Cancelled | Interrupted _ -> true
         in
         if terminal parent_row
         then (
           let completion =
             Completion.of_json (Option.value_exn parent_row.result) |> protocol_ok
           in
           (match completion with
            | Succeeded _ -> ()
            | other ->
              raise_s
                [%sexp
                  "parent failed during genuine read reservation", (other : Completion.t)]);
           match List.filter current.jobs ~f:(fun job -> Option.is_some job.J.launch) with
           | [ child ] when terminal child -> ()
           | [] | [ _ ] ->
             Eio.Fiber.yield ();
             await ()
           | _ -> failwith "read-reserved Pre_tool launched duplicate Jobs")
         else (
           Eio.Fiber.yield ();
           await ())
       in
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. await;
       Ok parent)
  |> protocol_ok
;;

let%expect_test "moderator launches survive handler, event and observation commits" =
  List.iter [ Lifecycle; Queued; Handler; Pre_tool; Observation ] ~f:(fun trigger ->
    let registry = ref None in
    with_background_daemon
      ~on_started:(fun daemon -> registry := Some (Agent_server.Daemon.registry daemon))
      ~agent:(agent trigger)
      ~sources:Background_moderator_tests.sources
      ?before_start:
        (match trigger with
         | Observation -> Some (fun () -> ())
         | Lifecycle | Queued | Handler | Pre_tool -> None)
      (fun env client entry capabilities ->
         let observed_parent =
           match trigger with
           | Lifecycle | Queued -> None
           | Handler | Pre_tool ->
             let request = tool capabilities "counter" (`Object []) in
             let parent =
               match trigger with
               | Pre_tool ->
                 submit_during_loaded_read
                   (Option.value_exn !registry)
                   env
                   entry
                   (B.to_json request)
               | Handler -> submit entry (B.to_json request)
               | Lifecycle | Queued | Observation ->
                 failwith "non-handler fixture entered its tool branch"
             in
             let _, completion = await env client parent in
             (match completion with
              | Succeeded _ -> ()
              | other -> raise_s [%sexp (other : Completion.t)]);
             None
           | Observation ->
             let parent =
               await_launch env entry.actor ~request:(native capabilities "report.txt")
             in
             let parent, completion = await env client parent in
             (match completion with
              | Succeeded (`String result) ->
                assert (String.is_substring result ~substring:"background report")
              | other -> raise_s [%sexp (other : Completion.t)]);
             let launch = Option.value_exn parent.launch in
             assert (Option.is_none launch.parent_job);
             (match launch.owner with
              | Moderator_event _ -> ()
              | Invocation _ -> failwith "observed parent was not a lifecycle launch");
             Some parent
         in
         let job =
           match observed_parent with
           | None -> launched_job env entry.actor
           | Some _ ->
             await_launch env entry.actor ~request:(native capabilities "second.txt")
         in
         let _, completion = await env client job in
         let launch = Option.value_exn job.launch in
         let owner =
           match launch.owner with
           | Moderator_event _ -> "event"
           | Invocation _ -> "invocation"
         in
         let state = Background_moderator_tests.await_cleanup env entry.actor in
         (match observed_parent with
          | None ->
            [%test_eq: int]
              1
              (List.count state.jobs ~f:(fun job -> Option.is_some job.launch))
          | Some parent ->
            assert (not (Agent_protocol.Id.Job.equal job.id parent.id));
            (match launch.parent_job with
             | Some (id, attempt) ->
               assert (Agent_protocol.Id.Job.equal id parent.id);
               [%test_eq: int] parent.attempt attempt
             | None -> failwith "observation child lost its actual parent attempt");
            (match launch.owner with
             | Moderator_event _ -> failwith "observation child lost its invocation owner"
             | Invocation id ->
               let producer =
                 List.find_exn state.invocations ~f:(fun invocation ->
                   Agent_protocol.Id.Invocation.equal invocation.context.id id)
               in
               [%test_eq: string] "read_file" producer.context.tool_name;
               assert (
                 Agent_protocol.Invocation.equal_origin producer.context.origin Script);
               assert (Option.is_none producer.context.parent_job);
               assert (Option.is_none producer.parent_event);
               assert (
                 Jsonaf.exactly_equal
                   producer.context.input
                   (`Object [ "root", `String "reports"; "file", `String "report.txt" ]));
               (* Background_execution owns a Script wrapper for the Job; its
                  selected native target is an Invocation child, not another
                  direct Job child. Assert the actual complete chain. *)
               let wrapper_id = Option.value_exn producer.context.parent_invocation in
               assert (not (Agent_protocol.Id.Invocation.equal wrapper_id id));
               let wrapper =
                 List.find_exn state.invocations ~f:(fun invocation ->
                   Agent_protocol.Id.Invocation.equal invocation.context.id wrapper_id)
               in
               [%test_eq: string] "chatml.background" wrapper.context.tool_name;
               assert (
                 Agent_protocol.Invocation.equal_origin wrapper.context.origin Script);
               assert (Option.is_none wrapper.context.parent_invocation);
               assert (Option.is_none wrapper.parent_event);
               assert (
                 Option.equal
                   Agent_protocol.Id.Job.equal
                   wrapper.context.parent_job
                   (Some parent.id));
               (match wrapper.status with
                | Resolved (Complete _) | Published (Complete _) -> ()
                | status -> raise_s [%sexp (status : Agent_protocol.Invocation.status)]));
            [%test_eq: int]
              2
              (List.count state.jobs ~f:(fun job -> Option.is_some job.launch)));
         print_s
           [%sexp (trigger : trigger), (owner : string), (completion : Completion.t)]));
  [%expect
    {|
    (Lifecycle event
     (Succeeded (String  "second.txt:1-1:\
                        \n[total_lines=1]\
                        \nsecond report")))
    (Queued event
     (Succeeded (String  "second.txt:1-1:\
                        \n[total_lines=1]\
                        \nsecond report")))
    (Handler invocation
     (Succeeded (String  "second.txt:1-1:\
                        \n[total_lines=1]\
                        \nsecond report")))
    (Pre_tool event
     (Succeeded (String  "second.txt:1-1:\
                        \n[total_lines=1]\
                        \nsecond report")))
    (Observation invocation
     (Succeeded (String  "second.txt:1-1:\
                        \n[total_lines=1]\
                        \nsecond report")))
    |}]
;;
