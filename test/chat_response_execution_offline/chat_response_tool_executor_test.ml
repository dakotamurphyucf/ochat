open Core
module Event = Chat_response.Tool_execution_event
module Executor = Chat_response.Tool_executor
module Output = Openai.Responses.Tool_output.Output

let runner ~invocation payload =
  Ochat_function.Invocation.emit
    invocation
    { channel = `Activity; update = Append "working" };
  Output.Text payload
;;

let show_event = function
  | Event.Started { call_id; name; kind; payload } ->
    let kind =
      match kind with
      | `Function -> "function"
      | `Custom -> "custom"
    in
    Printf.sprintf "started %s %s %s %s" call_id name kind payload
  | Progress { call_id; progress = { channel = _; update } } ->
    let update =
      match update with
      | Append text -> "append " ^ text
      | Replace text -> "replace " ^ text
    in
    Printf.sprintf "progress %s %s" call_id update
  | Finished { call_id; outcome; output = _ } ->
    let outcome =
      match outcome with
      | Returned -> "returned"
      | Raised -> "raised"
      | Cancelled -> "cancelled"
    in
    Printf.sprintf "finished %s %s" call_id outcome
  | Trace { call_id; trace = _ } -> "trace " ^ call_id
;;

let print_output = function
  | Output.Text text -> print_endline text
  | Content _ -> failwith "expected text output"
;;

let%expect_test "emits ordered lifecycle around progress and final result" =
  let events = Queue.create () in
  Executor.run
    ~kind:`Function
    ~call_id:"call-1"
    ~name:"echo"
    ~payload:"final"
    ~runner
    ~on_tool_execution:(Queue.enqueue events)
    ()
  |> print_output;
  Queue.iter events ~f:(fun event -> print_endline (show_event event));
  [%expect
    {|
    final
    started call-1 echo function final
    progress call-1 append working
    finished call-1 returned
    |}]
;;

let%expect_test "observer failure cannot affect runner result" =
  Executor.run
    ~kind:`Custom
    ~call_id:"call-2"
    ~name:"custom"
    ~payload:"ok"
    ~runner
    ~on_tool_execution:(fun _ -> failwith "observer")
    ()
  |> print_output;
  [%expect {| ok |}]
;;

let%expect_test "returned lifecycle includes a transient output projection" =
  let output = ref None in
  ignore
    (Executor.run
       ~kind:`Function
       ~call_id:"call-output"
       ~name:"echo"
       ~payload:"projected"
       ~runner
       ~on_tool_execution:(function
         | Event.Finished { output = Some value; _ } -> output := Some value
         | Started _ | Progress _ | Finished _ | Trace _ -> ())
       ()
     : Output.t);
  Option.iter !output ~f:print_output;
  [%expect {| projected |}]
;;

let%expect_test "raised runner emits one raised terminal event and re-raises" =
  let events = Queue.create () in
  let runner ~invocation:_ _ = failwith "runner failure" in
  (try
     Executor.run
       ~kind:`Function
       ~call_id:"call-3"
       ~name:"broken"
       ~payload:""
       ~runner
       ~on_tool_execution:(Queue.enqueue events)
       ()
     |> ignore
   with
   | Failure message -> print_endline message);
  Queue.iter events ~f:(fun event -> print_endline (show_event event));
  [%expect
    {|
    runner failure
    started call-3 broken function
    finished call-3 raised
    |}]
;;

let%expect_test "cancellation emits one cancelled terminal event and re-raises" =
  Eio_main.run
  @@ fun _env ->
  let events = Queue.create () in
  let runner ~invocation:_ _ = raise (Eio.Cancel.Cancelled (Failure "cancel")) in
  (try
     Executor.run
       ~kind:`Function
       ~call_id:"call-4"
       ~name:"cancelled"
       ~payload:""
       ~runner
       ~on_tool_execution:(Queue.enqueue events)
       ()
     |> ignore
   with
   | Eio.Cancel.Cancelled _ -> print_endline "cancelled");
  Queue.iter events ~f:(fun event -> print_endline (show_event event));
  [%expect
    {|
    cancelled
    started call-4 cancelled function
    finished call-4 cancelled
    |}]
;;
