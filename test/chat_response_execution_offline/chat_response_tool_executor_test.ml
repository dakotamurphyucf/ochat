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

let%expect_test "driver fork interception preserves an explicitly selected replacement" =
  List.iter [ Functions.fork.run_with_progress; runner ] ~f:(fun selected ->
    let events = Queue.create () in
    Chat_response.Tool_call.run_tool
      ~kind:Function
      ~call_id:"fork-call"
      ~name:"fork"
      ~payload:"selected result"
      ~tool_tbl:(Hashtbl.create (module String))
      ~runner:selected
      ~on_fork:(Some (fun ~invocation:_ ~call_id:_ ~arguments:_ -> Text "driver fork"))
      ~on_tool_execution:(Queue.enqueue events)
      ()
    |> print_output;
    Queue.iter events ~f:(fun event -> print_endline (show_event event)));
  [%expect
    {|
    driver fork
    started fork-call fork function selected result
    finished fork-call returned
    selected result
    started fork-call fork function selected result
    progress fork-call append working
    finished fork-call returned
    |}]
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

let%expect_test "selected effect checkpoint follows Started and retains terminal failures"
  =
  Eio_main.run (fun _env ->
    List.iter [ `Raised; `Cancelled ] ~f:(fun mode ->
      let legacy = Queue.create () in
      let strict = Queue.create () in
      let selected_live = ref true in
      let checks = ref 0 in
      let executions = ref 0 in
      let sentinel =
        match mode with
        | `Raised -> Failure "selected effect rejected"
        | `Cancelled -> Eio.Cancel.Cancelled (Failure "selected effect cancelled")
      in
      let observed =
        try
          ignore
            (Chat_response.Tool_call.run_tool
               ~kind:Function
               ~call_id:"selected"
               ~name:"echo"
               ~payload:"input"
               ~tool_tbl:(Hashtbl.create (module String))
               ~runner:(fun ~invocation:_ _ ->
                 Int.incr executions;
                 Output.Text "must not execute")
               ~on_fork:None
               ~on_tool_execution:(Queue.enqueue legacy)
               ~on_execution_event:(fun event ->
                 Queue.enqueue strict event;
                 match event with
                 | Started _ -> selected_live := false
                 | Progress _ | Finished _ | Trace _ -> ())
               ~before_effect:(fun () ->
                 Int.incr checks;
                 assert (not !selected_live);
                 raise sentinel)
               ()
             : Output.t);
          None
        with
        | exn -> Some exn
      in
      (* All decisive oracles are outside the checked runner/observers. This is
         a component callback contract, not an Actor authority issuer. *)
      assert (Option.exists observed ~f:(phys_equal sentinel));
      [%test_eq: int] !checks 1;
      [%test_eq: int] !executions 0;
      let started = "started selected echo function input" in
      let terminal =
        match mode with
        | `Raised -> "finished selected raised"
        | `Cancelled -> "finished selected cancelled"
      in
      [%test_eq: string list]
        (List.map (Queue.to_list legacy) ~f:show_event)
        [ started; terminal ];
      [%test_eq: string list]
        (List.map (Queue.to_list strict) ~f:show_event)
        (match mode with
         | `Raised -> [ started; terminal ]
         | `Cancelled -> [ started ]);
      print_endline terminal));
  [%expect
    {|
    finished selected raised
    finished selected cancelled
    |}]
;;
