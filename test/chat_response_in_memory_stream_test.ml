open Core
module CM = Prompt.Chat_markdown
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Res = Openai.Responses
module Session = Session
module Stream = Chat_response.In_memory_stream

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let input_text text = Res.Input_message.Text { text; _type = "input_text" }

let output_text text =
  { Res.Output_message.annotations = []; text; _type = "output_text" }
;;

let print_items (items : Res.Item.t list) =
  List.iter items ~f:(function
    | Res.Item.Input_message message ->
      let role =
        match message.role with
        | Res.Input_message.System -> "system"
        | User -> "user"
        | Assistant -> "assistant"
        | Developer -> "developer"
      in
      let text =
        List.map message.content ~f:(function
          | Res.Input_message.Text { text; _ } -> text
          | Image { image_url; _ } -> Printf.sprintf "<image src=%S>" image_url)
        |> String.concat ~sep:"\n"
      in
      print_endline (Printf.sprintf "input %s %S" role text)
    | Res.Item.Output_message message ->
      let text =
        List.map message.content ~f:(fun content -> content.text)
        |> String.concat ~sep:"\n"
      in
      print_endline (Printf.sprintf "output %s %S" message.id text)
    | item -> print_s [%sexp (item : Res.Item.t)])
;;

let moderator_source =
  {|
    type state = { turn_count : int }
    type event = [ `Turn_start | `Turn_end ]

    let initial_state = { turn_count = 0 }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Turn_start ->
          Task.bind(Turn.prepend_system("policy"), fun ignored_turn ->
          Task.pure(st))
        | `Turn_end -> Task.pure({ turn_count = st.turn_count + 1 })
  |}
;;

let moderator_of_source source =
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
  in
  let capabilities = Chat_response.Moderation.Capabilities.default in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ()) in
  Stream.{ manager; session_id = "session-1"; session_meta = `Null }
;;

let moderator () = moderator_of_source moderator_source

let print_runtime_requests requests =
  print_s [%sexp (requests : Moderation.Runtime_request.t list)]
;;

let print_tool_call = function
  | Res.Item.Function_call call ->
    print_endline (Printf.sprintf "function %s %s" call.name call.arguments)
  | Res.Item.Custom_tool_call call ->
    print_endline (Printf.sprintf "custom %s %s" call.name call.input)
  | item -> print_s [%sexp (item : Res.Item.t)]
;;

let print_synthetic_result = function
  | None -> print_endline "<none>"
  | Some (Res.Tool_output.Output.Text text) -> print_endline text
  | Some (Content parts) ->
    List.iter parts ~f:(function
      | Res.Tool_output.Output_part.Input_text { text } -> print_endline text
      | Input_image { image_url; _ } -> print_endline image_url)
;;

let%expect_test "prepare_turn_inputs keeps no-moderator history unchanged" =
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ; Res.Item.Output_message
        { role = Res.Output_message.Assistant
        ; id = "msg-1"
        ; content = [ output_text "Done" ]
        ; status = "completed"
        ; _type = "message"
        }
    ]
  in
  let items =
    ok_or_fail
      (Stream.prepare_turn_inputs ~moderator:None ~available_tools:[] ~now_ms:1 ~history)
  in
  print_items items;
  [%expect
    {|
    input user "Hello"
    output msg-1 "Done"
    |}]
;;

let%expect_test "prepare_turn_inputs applies moderator overlay before request" =
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ]
  in
  let items =
    ok_or_fail
      (Stream.prepare_turn_inputs
         ~moderator:(Some (moderator ()))
         ~available_tools:[]
         ~now_ms:1
         ~history)
  in
  print_items items;
  [%expect
    {|
    input system "policy"
    input user "Hello"
    |}]
;;

let%expect_test "finish_turn records end-of-turn state changes" =
  let moderator = moderator () in
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ; Res.Item.Output_message
        { role = Res.Output_message.Assistant
        ; id = "msg-1"
        ; content = [ output_text "Done" ]
        ; status = "completed"
        ; _type = "message"
        }
    ]
  in
  ignore
    (ok_or_fail
       (Stream.prepare_turn_inputs
          ~moderator:(Some moderator)
          ~available_tools:[]
          ~now_ms:1
          ~history)
     : Res.Item.t list);
  ok_or_fail
    (Stream.finish_turn
       ~moderator:(Some moderator)
       ~available_tools:[]
       ~now_ms:2
       ~history);
  let snapshot = ok_or_fail (Manager.snapshot moderator.manager) in
  print_s [%sexp (snapshot.current_state : Session.Snapshot.t)];
  [%expect {| (Record ((turn_count (Int 1)))) |}]
;;

let%expect_test "moderate_tool_call can reject a tool call with a synthetic response" =
  let moderator =
    moderator_of_source
      {|
        type state = { seen : int }
        type event = [ `Pre_tool_call(tool_call) ]

        let initial_state = { seen = 0 }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Pre_tool_call(call) ->
              (match call.name with
               | "blocked" ->
                 Task.bind(Tool.reject("denied"), fun ignored ->
                 Task.pure(st))
               | _ ->
                 Task.bind(Tool.approve(), fun ignored ->
                 Task.pure(st)))
      |}
  in
  let call =
    ok_or_fail
      (Stream.moderate_tool_call
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:1
         ~history:[]
         ~kind:Chat_response.Tool_call.Kind.Function
         ~name:"blocked"
         ~payload:{|{"q":"ocaml"}|}
         ~call_id:"call-1"
         ~item_id:(Some "item-1"))
  in
  print_tool_call call.call_item;
  print_synthetic_result call.synthetic_result;
  [%expect
    {|
    function blocked {"q":"ocaml"}
    denied
    |}]
;;

let%expect_test "moderate_tool_call can explicitly approve a tool call" =
  let moderator =
    moderator_of_source
      {|
        type state = { seen : int }
        type event = [ `Pre_tool_call(tool_call) ]

        let initial_state = { seen = 0 }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Pre_tool_call(call) ->
              Task.bind(Tool.approve(), fun ignored ->
              Task.pure(st))
      |}
  in
  let call =
    ok_or_fail
      (Stream.moderate_tool_call
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:1
         ~history:[]
         ~kind:Chat_response.Tool_call.Kind.Function
         ~name:"allowed"
         ~payload:{|{"q":"ocaml"}|}
         ~call_id:"call-1"
         ~item_id:(Some "item-1"))
  in
  print_tool_call call.call_item;
  print_synthetic_result call.synthetic_result;
  print_runtime_requests call.runtime_requests;
  [%expect
    {|
    function allowed {"q":"ocaml"}
    <none>
    ()
    |}]
;;

let%expect_test "moderate_tool_call can rewrite and redirect tool invocations" =
  let moderator =
    moderator_of_source
      {|
        type state = { seen : int }
        type event = [ `Pre_tool_call(tool_call) ]

        let initial_state = { seen = 0 }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Pre_tool_call(call) ->
              (match call.name with
               | "rewrite" ->
                 Task.bind(Tool.rewrite_args(Json.parse("{\"mode\":\"safe\"}")), fun ignored ->
                 Task.pure(st))
               | "redirect" ->
                 Task.bind
                   (Tool.redirect("other", Json.parse("{\"mode\":\"safe\"}")),
                    fun ignored ->
                    Task.pure(st))
               | _ ->
                 Task.bind(Tool.approve(), fun ignored ->
                 Task.pure(st)))
      |}
  in
  let rewritten =
    ok_or_fail
      (Stream.moderate_tool_call
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:1
         ~history:[]
         ~kind:Chat_response.Tool_call.Kind.Function
         ~name:"rewrite"
         ~payload:{|{"mode":"fast"}|}
         ~call_id:"call-1"
         ~item_id:(Some "item-1"))
  in
  print_tool_call rewritten.call_item;
  let redirected =
    ok_or_fail
      (Stream.moderate_tool_call
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:2
         ~history:[]
         ~kind:Chat_response.Tool_call.Kind.Function
         ~name:"redirect"
         ~payload:{|{"mode":"fast"}|}
         ~call_id:"call-2"
         ~item_id:(Some "item-2"))
  in
  print_tool_call redirected.call_item;
  [%expect
    {|
    function rewrite {"mode":"safe"}
    function other {"mode":"safe"}
    |}]
;;

let%expect_test
    "handle_tool_result drains internal events and surfaces compaction requests"
  =
  let moderator =
    moderator_of_source
      {|
        type state = { seen : string array }
        type event = [ `Post_tool_response(tool_result) | `Queued(string) ]

        let initial_state = { seen = Array.make(0, "") }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Post_tool_response(result) ->
              Task.bind(Runtime.request_compaction(), fun ignored_compaction ->
              Task.bind(Runtime.emit(`Queued("later")), fun ignored_emit ->
              Task.pure({ seen = [ result.call_id ] })))
            | `Queued(_) ->
              Task.bind
                (Turn.append_message
                   ({ id = "synthetic-queued"
                    ; role = "assistant"
                    ; content = "queued"
                    ; meta = `Null
                    }),
                 fun ignored_turn ->
                 Task.pure(st))
      |}
  in
  let item =
    Chat_response.Tool_call.output_item
      ~kind:Chat_response.Tool_call.Kind.Function
      ~call_id:"call-1"
      ~output:(Res.Tool_output.Output.Text {|{"ok":true}|})
  in
  let requests =
    ok_or_fail
      (Stream.handle_tool_result
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:1
         ~history:[ item ]
         ~name:"search"
         ~kind:Chat_response.Tool_call.Kind.Function
         ~item)
  in
  print_runtime_requests requests;
  let messages = Manager.effective_messages moderator.manager [] in
  List.iter messages ~f:(fun message ->
    print_endline (Printf.sprintf "%s %s %S" message.id message.role message.content));
  [%expect
    {|
    (Request_compaction)
    synthetic-queued assistant "queued"
    |}]
;;

let%expect_test "handle_tool_result surfaces end-session requests without crashing" =
  let moderator =
    moderator_of_source
      {|
        type state = { seen : int }
        type event = [ `Post_tool_response(tool_result) ]

        let initial_state = { seen = 0 }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Post_tool_response(result) ->
              Task.bind(Runtime.end_session("done"), fun ignored_end ->
              Task.pure({ seen = st.seen + 1 }))
      |}
  in
  let item =
    Chat_response.Tool_call.output_item
      ~kind:Chat_response.Tool_call.Kind.Function
      ~call_id:"call-1"
      ~output:(Res.Tool_output.Output.Text {|{"ok":true}|})
  in
  let requests =
    ok_or_fail
      (Stream.handle_tool_result
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:1
         ~history:[ item ]
         ~name:"search"
         ~kind:Chat_response.Tool_call.Kind.Function
         ~item)
  in
  print_runtime_requests requests;
  [%expect {| ((End_session done)) |}]
;;

let%expect_test "conflicting tool moderation outcomes fail clearly" =
  let moderator =
    moderator_of_source
      {|
        type state = { seen : int }
        type event = [ `Pre_tool_call(tool_call) ]

        let initial_state = { seen = 0 }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Pre_tool_call(call) ->
              Task.bind(Tool.approve(), fun ignored_approve ->
              Task.bind(Tool.reject("denied"), fun ignored_reject ->
              Task.pure(st)))
      |}
  in
  (match
     Stream.moderate_tool_call
       ~moderator:(Some moderator)
       ~available_tools:[]
       ~now_ms:1
       ~history:[]
       ~kind:Chat_response.Tool_call.Kind.Function
       ~name:"blocked"
       ~payload:{|{"q":"ocaml"}|}
       ~call_id:"call-1"
       ~item_id:(Some "item-1")
   with
   | Ok _ -> print_endline "unexpected success"
   | Error msg -> print_endline msg);
  [%expect {| Expected at most one tool moderation action for a single host event. |}]
;;
