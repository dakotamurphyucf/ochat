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
  let start =
    {|Task.bind(Job.start_tool("read_file", `Object([
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
    | Observation -> "| `Tool_observed(p) -> if state then Task.pure(state) else " ^ start
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

let%expect_test "moderator launches survive handler, event and observation commits" =
  List.iter [ Lifecycle; Queued; Handler; Pre_tool; Observation ] ~f:(fun trigger ->
    with_background_daemon
      ~agent:(agent trigger)
      ~sources:Background_moderator_tests.sources
      (fun env client entry capabilities ->
         (match trigger with
          | Lifecycle | Queued -> ()
          | Handler | Pre_tool | Observation ->
            let request =
              match trigger with
              | Observation -> native capabilities "report.txt"
              | Handler | Pre_tool -> tool capabilities "counter" (`Object [])
              | Lifecycle | Queued -> assert false
            in
            let parent = submit entry (B.to_json request) in
            let _, completion = await env client parent in
            (match completion with
             | Succeeded _ -> ()
             | other -> raise_s [%sexp (other : Completion.t)]));
         let job = launched_job env entry.actor in
         let _, completion = await env client job in
         let launch = Option.value_exn job.launch in
         let owner =
           match launch.owner with
           | Moderator_event _ -> "event"
           | Invocation _ -> "invocation"
         in
         let state = Background_moderator_tests.await_cleanup env entry.actor in
         [%test_eq: int]
           1
           (List.count state.jobs ~f:(fun job -> Option.is_some job.launch));
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
