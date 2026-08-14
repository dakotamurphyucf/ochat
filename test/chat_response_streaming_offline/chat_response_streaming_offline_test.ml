open Core
module CM = Prompt.Chat_markdown
module Builtin_surface = Chatml.Chatml_builtin_surface
module Lang = Chatml.Chatml_lang
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

let stream_function_call ~output_index ~item_id ~call_id ~arguments =
  ( Res.Response_stream.Output_item_added
      { item =
          Function_call
            { name = "echo"
            ; arguments = ""
            ; call_id
            ; _type = "function_call"
            ; id = Some item_id
            ; status = Some "in_progress"
            }
      ; output_index
      ; type_ = "response.output_item.added"
      }
  , Res.Response_stream.Function_call_arguments_done
      { arguments
      ; item_id
      ; output_index
      ; type_ = "response.function_call_arguments.done"
      } )
;;

let stream_custom_call ~output_index ~item_id ~call_id ~input =
  ( Res.Response_stream.Output_item_added
      { item =
          Custom_function
            { name = "echo"
            ; input = ""
            ; call_id
            ; _type = "custom_tool_call"
            ; id = Some item_id
            }
      ; output_index
      ; type_ = "response.output_item.added"
      }
  , Res.Response_stream.Custom_tool_call_input_done
      { input; item_id; output_index; type_ = "response.custom_tool_call_input.done" } )
;;

let stream_message ~output_index ~item_id text =
  let message : Res.Output_message.t =
    { role = Assistant
    ; id = item_id
    ; content = [ output_text text ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
  in
  let item = Res.Response_stream.Item.Output_message message in
  [ Res.Response_stream.Output_item_added
      { item; output_index; type_ = "response.output_item.added" }
  ; Res.Response_stream.Output_text_delta
      { item_id
      ; output_index
      ; content_index = 0
      ; delta = text
      ; type_ = "response.output_text.delta"
      }
  ; Res.Response_stream.Output_item_done
      { item; output_index; type_ = "response.output_item.done" }
  ]
;;

let input_entry allocator =
  History_entry.create
    ~allocator
    (Res.Item.Input_message
       { role = User; content = [ input_text "hello" ]; _type = "message" })
  |> Result.ok_or_failwith
;;

let entry_kind entry =
  match History_entry.item entry with
  | Res.Item.Input_message _ -> "input"
  | Function_call _ -> "function-call"
  | Custom_tool_call _ -> "custom-call"
  | Function_call_output _ -> "function-output"
  | Custom_tool_call_output _ -> "custom-output"
  | Output_message _ -> "message"
  | _ -> "other"
;;

let run_entry_stream
      env
      ~namespace
      ~responses
      ~tool_tbl
      ?(parallel_tool_calls = false)
      ?(on_history_event = ignore)
      ?(on_history_tool_out = ignore)
      ()
  =
  let allocator =
    History_entry.Allocator.create ~namespace ~next_sequence:0 |> Result.ok_or_failwith
  in
  let responses = Queue.of_list responses in
  let post_stream ~sw:_ ~inputs:_ = Queue.dequeue_exn responses |> Stdlib.List.to_seq in
  let history =
    Stream.run_completion_stream_in_memory_entries
      ~env
      ~allocator
      ~history:[ input_entry allocator ]
      ~on_history_event
      ~on_history_tool_out
      ~tools:(Some [])
      ~tool_tbl
      ~parallel_tool_calls
      ~post_stream
      ()
  in
  allocator, history
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

let show_pending_ui_request = function
  | None -> "none"
  | Some (Manager.Ask_text { prompt }) -> "ask_text " ^ prompt
  | Some (Manager.Ask_choice { prompt; choices }) ->
    "ask_choice " ^ prompt ^ " [" ^ String.concat ~sep:", " (Array.to_list choices) ^ "]"
;;

let moderator_of_source
      ?(surface = Chatml.Chatml_builtin_surface.moderator_surface)
      ?(runtime_policy = Chat_response.Runtime_semantics.default_policy)
      source
  =
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  let artifact =
    ok_or_fail (Manager.Registry.compile_script ~surface Manager.Registry.empty script)
    |> snd
  in
  let capabilities = Chat_response.Moderation.Capabilities.default in
  let manager = ok_or_fail (Manager.create ~artifact ~capabilities ()) in
  Stream.{ manager; session_id = "session-1"; session_meta = `Null; runtime_policy }
;;

let moderator () = moderator_of_source moderator_source

let runtime_policy_with_budget budget =
  { Chat_response.Runtime_semantics.default_policy with budget }
;;

let print_runtime_requests requests =
  print_s [%sexp (requests : Moderation.Runtime_request.t list)]
;;

let one_shot_safe_point_input text =
  let remaining = ref (Some text) in
  Stream.Safe_point_input.
    { consume_entries = (fun () -> [])
    ; consume_compatibility_text =
        (fun () ->
          let next = !remaining in
          remaining := None;
          next)
    }
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

let print_effective_overlay_items (moderator : Stream.moderator) =
  let items = Manager.effective_items moderator.manager [] in
  List.iter items ~f:(fun item ->
    let response_item =
      ok_or_fail (Chat_response.Moderation.Item.to_response_item item)
    in
    match response_item with
    | Res.Item.Output_message msg ->
      let content =
        List.map msg.content ~f:(fun part -> part.text) |> String.concat ~sep:"\n"
      in
      print_endline (Printf.sprintf "%s assistant %S" item.id content)
    | Res.Item.Input_message msg ->
      let role = Res.Input_message.role_to_string msg.role in
      let content =
        List.filter_map msg.content ~f:(function
          | Res.Input_message.Text { text; _ } -> Some text
          | Res.Input_message.Image { image_url; _ } ->
            Some (Printf.sprintf "<image src=\"%s\" />" image_url))
        |> String.concat ~sep:"\n"
      in
      print_endline (Printf.sprintf "%s %s %S" item.id role content)
    | other ->
      print_endline
        (Printf.sprintf
           "%s json %S"
           item.id
           (Jsonaf.to_string (Res.Item.jsonaf_of_t other))))
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
        ; phase = None
        ; _type = "message"
        }
    ]
  in
  let items =
    ok_or_fail
      (Stream.prepare_turn_inputs
         ~moderator:None
         ~available_tools:[]
         ~now_ms:1
         ~history
         ())
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
         ~history
         ())
  in
  print_items items;
  [%expect
    {|
    input system "policy"
    input user "Hello"
    |}]
;;

let%expect_test "prepare_turn_inputs appends safe-point input after overlay history" =
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
         ~safe_point_input:(one_shot_safe_point_input "safe-point")
         ~available_tools:[]
         ~now_ms:1
         ~history
         ())
  in
  print_items items;
  [%expect
    {|
    input system "policy"
    input user "Hello"
    input system "safe-point"
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
        ; phase = None
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
          ~history
          ())
     : Res.Item.t list);
  let requests =
    ok_or_fail
      (Stream.finish_turn
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:2
         ~history)
  in
  print_runtime_requests requests;
  let snapshot = ok_or_fail (Manager.snapshot moderator.manager) in
  print_s [%sexp (snapshot.current_state : Session.Snapshot.t)];
  [%expect
    {|
    ()
    (Record ((turn_count (Int 1))))
    |}]
;;

let%expect_test "self-triggered turn budget advances up to the configured limit" =
  let budget =
    { Chat_response.Runtime_semantics.default_budget_policy with
      max_self_triggered_turns = 2
    }
  in
  let policy = runtime_policy_with_budget budget in
  let first =
    Chat_response.Runtime_semantics.next_self_triggered_turn_budget
      ~policy
      ~request_turn_budget:0
  in
  let second =
    Chat_response.Runtime_semantics.next_self_triggered_turn_budget
      ~policy
      ~request_turn_budget:1
  in
  let third =
    Chat_response.Runtime_semantics.next_self_triggered_turn_budget
      ~policy
      ~request_turn_budget:2
  in
  print_s [%sexp (first : (int, string) result)];
  print_s [%sexp (second : (int, string) result)];
  print_s [%sexp (third : (int, string) result)];
  [%expect
    {|
    (Ok 1)
    (Ok 2)
    (Error "Exceeded maximum consecutive moderator-requested turns (2).")
    |}]
;;

let%expect_test "default self-triggered turn budget preserves the old limit" =
  let policy = Chat_response.Runtime_semantics.default_policy in
  let tenth =
    Chat_response.Runtime_semantics.next_self_triggered_turn_budget
      ~policy
      ~request_turn_budget:9
  in
  let eleventh =
    Chat_response.Runtime_semantics.next_self_triggered_turn_budget
      ~policy
      ~request_turn_budget:10
  in
  print_s [%sexp (tenth : (int, string) result)];
  print_s [%sexp (eleventh : (int, string) result)];
  [%expect
    {|
    (Ok 10)
    (Error "Exceeded maximum consecutive moderator-requested turns (10).")
    |}]
;;

let%expect_test "finish_turn caps internal-event drain using the configured policy limit" =
  let runtime_policy =
    runtime_policy_with_budget
      { Chat_response.Runtime_semantics.default_budget_policy with
        max_internal_event_drain = 2
      }
  in
  let moderator =
    moderator_of_source
      ~runtime_policy
      {|
        type state = { seen : int }
        type event = [ `Turn_end | `Queued(string) ]

        let initial_state = { seen = 0 }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Turn_end -> Task.pure(st)
            | `Queued(text) ->
              Task.bind
                (Turn.append_item(Item.output_text_message("queued-" ++ to_string(st.seen), text)),
                 fun ignored_turn ->
                 Task.pure({ seen = st.seen + 1 }))
      |}
  in
  ok_or_fail
    (Manager.enqueue_internal_event
       moderator.manager
       (Lang.VVariant ("Queued", [ Lang.VString "one" ])));
  ok_or_fail
    (Manager.enqueue_internal_event
       moderator.manager
       (Lang.VVariant ("Queued", [ Lang.VString "two" ])));
  ok_or_fail
    (Manager.enqueue_internal_event
       moderator.manager
       (Lang.VVariant ("Queued", [ Lang.VString "three" ])));
  let requests =
    ok_or_fail
      (Stream.finish_turn
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:1
         ~history:[])
  in
  print_runtime_requests requests;
  print_effective_overlay_items moderator;
  let queued_after_drain =
    ok_or_fail (Manager.snapshot moderator.manager)
    |> fun snapshot ->
    List.length snapshot.Session.Moderator_snapshot.queued_internal_events
  in
  print_endline (Printf.sprintf "queued_after_drain=%d" queued_after_drain);
  [%expect
    {|
    ()
    queued-0 assistant "one"
    queued-1 assistant "two"
    queued_after_drain=1
    |}]
;;

let%expect_test
    "approval suspension blocks turn-driver progression, keeps queued work pending, and \
     is not persisted"
  =
  let source =
    {|
      type state = { approved : string }
      type event = [ `Turn_start | `Queued(string) ]

      let initial_state = { approved = "" }

      let on_event : context -> state -> event -> state task =
        fun ctx st ev ->
          match ev with
          | `Turn_start ->
            Task.bind(Runtime.emit(`Queued("buffered")), fun ignored_emit ->
            Task.bind(Approval.ask_text("continue?"), fun answer ->
            Task.pure({ approved = answer })))
          | `Queued(text) ->
            Task.bind
              (Turn.append_item(Item.output_text_message("queued-" ++ text, text)),
               fun ignored_turn ->
               Task.pure(st))
    |}
  in
  let moderator =
    moderator_of_source ~surface:Chatml.Chatml_builtin_surface.ui_moderator_surface source
  in
  (match
     Stream.prepare_turn_inputs
       ~moderator:(Some moderator)
       ~available_tools:[]
       ~now_ms:1
       ~history:[]
       ()
   with
   | Ok _ -> print_endline "unexpected prepare_turn_inputs success"
   | Error msg -> print_endline msg);
  print_endline
    ("pending=" ^ show_pending_ui_request (Manager.pending_ui_request moderator.manager));
  (match Manager.snapshot moderator.manager with
   | Ok _ -> print_endline "unexpected pending snapshot success"
   | Error msg -> print_endline msg);
  ok_or_fail
    (Manager.enqueue_internal_event
       moderator.manager
       (Lang.VVariant ("Queued", [ Lang.VString "host" ])));
  (match Manager.snapshot moderator.manager with
   | Ok _ -> print_endline "unexpected queued pending snapshot success"
   | Error msg -> print_endline msg);
  (match
     Stream.finish_turn
       ~moderator:(Some moderator)
       ~available_tools:[]
       ~now_ms:2
       ~history:[]
   with
   | Ok _ -> print_endline "unexpected finish_turn success"
   | Error msg -> print_endline msg);
  let resumed =
    ok_or_fail (Manager.resume_ui_request moderator.manager ~response:" approved ")
  in
  print_s [%sexp (List.length resumed : int)];
  let resumed_snapshot = ok_or_fail (Manager.snapshot moderator.manager) in
  print_s [%sexp (resumed_snapshot.current_state : Session.Snapshot.t)];
  print_endline
    (Printf.sprintf
       "queued_after_resume=%d"
       (List.length resumed_snapshot.queued_internal_events));
  ignore
    (ok_or_fail
       (Manager.drain_internal_events
          moderator.manager
          ~session_id:"session-1"
          ~now_ms:3
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null)
     : Moderation.Outcome.t list);
  print_effective_overlay_items moderator;
  [%expect
    {|
    Session is waiting for UI input.
    pending=ask_text continue?
    Cannot snapshot moderator while approval is suspended.
    Cannot snapshot moderator while approval is suspended.
    Session is waiting for UI input.
    1
    (Record ((approved (String approved))))
    queued_after_resume=2
    queued-host assistant "host"
    queued-buffered assistant "buffered"
    |}]
;;

let%expect_test "turn_end leaves deferred safe-point input for the next turn start" =
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ]
  in
  let remaining = ref [ "later" ] in
  let safe_point_input =
    Stream.Safe_point_input.
      { consume_entries = (fun () -> [])
      ; consume_compatibility_text =
          (fun () ->
            match !remaining with
            | [] -> None
            | text :: rest ->
              remaining := rest;
              Some text)
      }
  in
  ignore
    (ok_or_fail
       (Stream.finish_turn ~moderator:None ~available_tools:[] ~now_ms:1 ~history)
     : Moderation.Runtime_request.t list);
  print_s [%sexp (List.length !remaining : int)];
  ignore
    (ok_or_fail
       (Stream.prepare_turn_inputs
          ~moderator:None
          ~safe_point_input
          ~available_tools:[]
          ~now_ms:2
          ~history
          ())
     : Res.Item.t list);
  print_s [%sexp (List.length !remaining : int)];
  [%expect
    {|
    1
    0
    |}]
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
        type event =
          [ `Post_tool_response(tool_result) | `Queued(string) | `Item_appended(item) ]

        let initial_state = { seen = Array.make(0, "") }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Post_tool_response(result) ->
              Task.bind(Runtime.request_compaction(), fun ignored_compaction ->
              Task.bind(Runtime.emit(`Queued("later")), fun ignored_emit ->
              Task.pure({ seen = [ result.call_id ] })))
            | `Item_appended(_) ->
              Task.pure(st)
            | `Queued(_) ->
              Task.bind
                (Turn.append_message
                   ({ id = "synthetic-queued"
                    ; value =
                        Json.parse("{\"type\":\"message\",\"role\":\"assistant\",\"id\":\"synthetic-queued\",\"content\":[{\"annotations\":[],\"text\":\"queued\",\"type\":\"output_text\"}],\"status\":\"completed\"}")
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
  let items = Manager.effective_items moderator.manager [] in
  List.iter items ~f:(fun item ->
    let response_item =
      ok_or_fail (Chat_response.Moderation.Item.to_response_item item)
    in
    match response_item with
    | Res.Item.Output_message msg ->
      let content =
        List.map msg.content ~f:(fun part -> part.text) |> String.concat ~sep:"\n"
      in
      print_endline (Printf.sprintf "%s assistant %S" item.id content)
    | Res.Item.Input_message msg ->
      let role = Res.Input_message.role_to_string msg.role in
      let content =
        List.filter_map msg.content ~f:(function
          | Res.Input_message.Text { text; _ } -> Some text
          | Res.Input_message.Image { image_url; _ } ->
            Some (Printf.sprintf "<image src=\"%s\" />" image_url))
        |> String.concat ~sep:"\n"
      in
      print_endline (Printf.sprintf "%s %s %S" item.id role content)
    | other ->
      print_endline
        (Printf.sprintf
           "%s json %S"
           item.id
           (Jsonaf.to_string (Res.Item.jsonaf_of_t other))));
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

let%expect_test "handle_tool_result emits item_appended for canonical tool-output items" =
  let moderator =
    moderator_of_source
      {|
        type state = { count : int }
        type event =
          [ `Item_appended(item)
          | `Pre_tool_call(tool_call)
          | `Post_tool_response(tool_result)
          ]

        let initial_state = { count = 0 }

        let first_text : string array -> string =
          fun parts ->
            if Array.length(parts) == 0 then "" else Array.get(parts, 0)

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Item_appended(item) ->
              let summary = Item.id(item) ++ ":" ++ first_text(Item.text_parts(item)) in
              Task.bind
                (Turn.append_item(Item.output_text_message("seen-" ++ to_string(st.count), summary)),
                 fun ignored_turn ->
                 Task.pure({ count = st.count + 1 }))
            | `Pre_tool_call(call) ->
              Task.bind(Tool.approve(), fun ignored_tool ->
              Task.pure(st))
            | `Post_tool_response(result) ->
              Task.pure(st)
      |}
  in
  let output_item =
    Chat_response.Tool_call.output_item
      ~kind:Chat_response.Tool_call.Kind.Function
      ~call_id:"call-1"
      ~output:(Res.Tool_output.Output.Text "tool-result")
  in
  ignore
    (ok_or_fail
       (Stream.handle_tool_result
          ~moderator:(Some moderator)
          ~available_tools:[]
          ~now_ms:2
          ~history:[ output_item ]
          ~name:"search"
          ~kind:Chat_response.Tool_call.Kind.Function
          ~item:output_item)
     : Moderation.Runtime_request.t list);
  print_effective_overlay_items moderator;
  [%expect
    {|
    seen-0 assistant "host-message-1:tool-result"
    |}]
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

let%expect_test "finish_turn surfaces request_turn emitted during turn_end" =
  let moderator =
    moderator_of_source
      {|
        type state = { seen : int }
        type event = [ `Turn_end ]

        let initial_state = { seen = 0 }

        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Turn_end ->
              Task.bind(Runtime.request_turn(), fun ignored ->
              Task.pure({ seen = 1 }))
      |}
  in
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ]
  in
  let requests =
    ok_or_fail
      (Stream.finish_turn
         ~moderator:(Some moderator)
         ~available_tools:[]
         ~now_ms:1
         ~history)
  in
  print_runtime_requests requests;
  [%expect {| (Request_turn) |}]
;;

let%expect_test "raw stream observers are isolated independently" =
  let seen = Queue.create () in
  Stream.For_testing.notify_each
    [ (fun value ->
        Queue.enqueue seen ("first " ^ value);
        failwith "observer")
    ; (fun value -> Queue.enqueue seen ("second " ^ value))
    ]
    "event";
  Queue.iter seen ~f:print_endline;
  [%expect
    {|
    first event
    second event
    |}]
;;

let%expect_test "raw stream observer cancellation propagates" =
  (try
     Stream.For_testing.notify_each
       [ (fun () -> raise (Eio.Cancel.Cancelled (Failure "cancelled"))) ]
       ()
   with
   | Eio.Cancel.Cancelled _ -> print_endline "cancelled");
  [%expect {| cancelled |}]
;;

let%expect_test "canonical tool output callback failures propagate" =
  let check kind ~on_fn_out ~on_tool_out =
    try
      Stream.For_testing.emit_tool_output
        ~on_fn_out
        ~on_tool_out
        ~kind
        ~call_id:"call"
        ~result:(Res.Tool_output.Output.Text "result")
      |> ignore;
      print_endline "unexpected success"
    with
    | Failure message -> print_endline message
  in
  check `Function ~on_fn_out:(fun _ -> failwith "on_fn_out") ~on_tool_out:ignore;
  check `Function ~on_fn_out:ignore ~on_tool_out:(fun _ -> failwith "on_tool_out");
  check `Custom ~on_fn_out:ignore ~on_tool_out:(fun _ -> failwith "custom on_tool_out");
  [%expect
    {|
    on_fn_out
    on_tool_out
    custom on_tool_out
    |}]
;;

let%expect_test "stream identity survives event order and finalization" =
  Eio_main.run
  @@ fun env ->
  let tool_tbl : (string, Ochat_function.runner) Hashtbl.t =
    Hashtbl.create (module String)
  in
  let history_events = Queue.create () in
  let message_events = stream_message ~output_index:0 ~item_id:"message-1" "done" in
  let reordered =
    match message_events with
    | [ added; delta; done_ ] -> [ delta; done_; added; done_ ]
    | _ -> assert false
  in
  let allocator, history =
    run_entry_stream
      env
      ~namespace:"message-order"
      ~responses:[ reordered ]
      ~tool_tbl
      ~on_history_event:(Queue.enqueue history_events)
      ()
  in
  let event_ids =
    Queue.to_list history_events
    |> List.map ~f:(fun event -> History_entry.Id.sequence event.entry_id)
  in
  let final_message =
    List.find_exn history ~f:(fun entry -> String.equal (entry_kind entry) "message")
  in
  print_s
    [%sexp
      (event_ids : int list)
    , (History_entry.Id.sequence (History_entry.id final_message) : int)
    , (List.map history ~f:entry_kind : string list)
    , (History_entry.Allocator.next_sequence allocator : int)];
  [%expect {| ((1 1 1 1) 1 (input message) 2) |}]
;;

let%expect_test "tool completion before added executes once with stable identities" =
  Eio_main.run
  @@ fun env ->
  let executions = ref 0 in
  let tool_tbl : (string, Ochat_function.runner) Hashtbl.t =
    Hashtbl.create (module String)
  in
  Hashtbl.set tool_tbl ~key:"echo" ~data:(fun ~invocation:_ payload ->
    Int.incr executions;
    Res.Tool_output.Output.Text payload);
  let added, done_ =
    stream_function_call
      ~output_index:0
      ~item_id:"function-1"
      ~call_id:"call-1"
      ~arguments:{|{"x":1}|}
  in
  let callbacks = Queue.create () in
  let _, history =
    run_entry_stream
      env
      ~namespace:"early-tool"
      ~responses:
        [ [ done_; added; done_; added ]
        ; stream_message ~output_index:0 ~item_id:"message-2" "complete"
        ]
      ~tool_tbl
      ~on_history_tool_out:(Queue.enqueue callbacks)
      ()
  in
  let call =
    List.find_exn history ~f:(fun entry ->
      String.equal (entry_kind entry) "function-call")
  in
  let output =
    List.find_exn history ~f:(fun entry ->
      String.equal (entry_kind entry) "function-output")
  in
  let callback = Queue.dequeue_exn callbacks in
  let call_id item =
    match item with
    | Res.Item.Function_call call -> call.call_id
    | Function_call_output output -> output.call_id
    | _ -> failwith "Expected function call occurrence"
  in
  print_s
    [%sexp
      (!executions : int)
    , (List.map history ~f:entry_kind : string list)
    , (History_entry.Id.equal (History_entry.id call) (History_entry.id output) : bool)
    , (String.equal
         (call_id (History_entry.item call))
         (call_id (History_entry.item output))
       : bool)
    , (History_entry.Id.equal (History_entry.id callback) (History_entry.id output)
       : bool)];
  [%expect {| (1 (input function-call function-output message) false true true) |}]
;;

let%expect_test "parallel mixed tools keep schedule order and distinct identities" =
  Eio_main.run
  @@ fun env ->
  let tool_tbl : (string, Ochat_function.runner) Hashtbl.t =
    Hashtbl.create (module String)
  in
  Hashtbl.set tool_tbl ~key:"echo" ~data:(fun ~invocation:_ payload ->
    Res.Tool_output.Output.Text payload);
  let function_added, function_done =
    stream_function_call
      ~output_index:0
      ~item_id:"function-1"
      ~call_id:"call-f"
      ~arguments:"function"
  in
  let custom_added, custom_done =
    stream_custom_call
      ~output_index:1
      ~item_id:"custom-1"
      ~call_id:"call-c"
      ~input:"custom"
  in
  let callback_ids = Queue.create () in
  let _, history =
    run_entry_stream
      env
      ~namespace:"mixed-tools"
      ~responses:
        [ [ function_added; custom_added; function_done; custom_done ]
        ; stream_message ~output_index:0 ~item_id:"message-3" "complete"
        ]
      ~tool_tbl
      ~parallel_tool_calls:true
      ~on_history_tool_out:(fun entry ->
        Queue.enqueue callback_ids (History_entry.Id.sequence (History_entry.id entry)))
      ()
  in
  let ids =
    List.map history ~f:(fun entry -> History_entry.Id.sequence (History_entry.id entry))
  in
  print_s
    [%sexp
      (List.map history ~f:entry_kind : string list)
    , (ids : int list)
    , (Queue.to_list callback_ids : int list)];
  [%expect
    {|
    ((input function-call custom-call function-output custom-output message)
     (0 1 2 3 4 5) (3 4))
    |}]
;;

let%expect_test "queued user entry follows tool output in the next request" =
  Eio_main.run
  @@ fun env ->
  let allocator =
    History_entry.Allocator.create ~namespace:"mid-turn-user" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let initial = input_entry allocator in
  let queued_user =
    History_entry.create
      ~allocator
      (Res.Item.Input_message
         { role = User
         ; content = [ input_text "steer the next completion" ]
         ; _type = "message"
         })
    |> Result.ok_or_failwith
  in
  let pending = ref [ queued_user ] in
  let safe_point_input =
    Stream.Safe_point_input.
      { consume_entries =
          (fun () ->
            let entries = !pending in
            pending := [];
            entries)
      ; consume_compatibility_text = (fun () -> None)
      }
  in
  let tool_tbl : (string, Ochat_function.runner) Hashtbl.t =
    Hashtbl.create (module String)
  in
  Hashtbl.set tool_tbl ~key:"echo" ~data:(fun ~invocation:_ payload ->
    Res.Tool_output.Output.Text payload);
  let added, done_ =
    stream_function_call
      ~output_index:0
      ~item_id:"mid-turn-call"
      ~call_id:"mid-turn-call-id"
      ~arguments:"tool result"
  in
  let request_kinds = Queue.create () in
  let attempt = ref 0 in
  let post_stream ~sw:_ ~inputs =
    Int.incr attempt;
    Queue.enqueue
      request_kinds
      (List.map inputs ~f:(function
         | Res.Item.Input_message { role = User; content = Text { text; _ } :: _; _ } ->
           "user:" ^ text
         | Function_call _ -> "function-call"
         | Function_call_output _ -> "function-output"
         | _ -> "other"));
    if Int.equal !attempt 1
    then Stdlib.List.to_seq [ added; done_ ]
    else
      stream_message ~output_index:0 ~item_id:"mid-turn-final" "finished"
      |> Stdlib.List.to_seq
  in
  let history =
    Stream.run_completion_stream_in_memory_entries
      ~env
      ~allocator
      ~history:[ initial ]
      ~safe_point_input
      ~tools:(Some [])
      ~tool_tbl
      ~post_stream
      ()
  in
  print_s
    [%sexp
      (Queue.to_list request_kinds : string list list)
    , (List.map history ~f:entry_kind : string list)
    , (List.exists history ~f:(fun entry ->
         History_entry.Id.equal (History_entry.id entry) (History_entry.id queued_user))
       : bool)];
  [%expect
    {|
    (((user:hello)
      (user:hello function-call function-output "user:steer the next completion"))
     (input function-call function-output input message) true)
    |}]
;;

let%expect_test "stream retry discards failed attempt before identity publication" =
  Eio_main.run
  @@ fun env ->
  let allocator =
    History_entry.Allocator.create ~namespace:"retry-stream" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let attempts = ref 0 in
  let observed = Queue.create () in
  let post_stream ~sw:_ ~inputs:_ =
    Int.incr attempts;
    if Int.equal !attempts 1
    then
      fun () ->
        raise
          (Res.Response_stream_parsing_error
             (`Object [], Failure "invalid stream response"))
    else
      stream_message ~output_index:0 ~item_id:"message-retry" "done" |> Stdlib.List.to_seq
  in
  let history =
    Stream.run_completion_stream_in_memory_entries
      ~env
      ~allocator
      ~history:[ input_entry allocator ]
      ~on_history_event:(Queue.enqueue observed)
      ~tools:(Some [])
      ~tool_tbl:(Hashtbl.create (module String))
      ~post_stream
      ()
  in
  print_s
    [%sexp
      (!attempts : int)
    , (Queue.length observed : int)
    , (List.map history ~f:entry_kind : string list)
    , (List.map history ~f:(fun entry ->
         History_entry.Id.sequence (History_entry.id entry))
       : int list)];
  [%expect {| (2 3 (input message) (0 1)) |}]
;;

let%expect_test "stream callbacks publish before the response tail is requested" =
  Eio_main.run
  @@ fun env ->
  let allocator =
    History_entry.Allocator.create ~namespace:"live-stream" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let observed = ref 0 in
  let events = stream_message ~output_index:0 ~item_id:"live-message" "live" in
  let first = List.hd_exn events in
  let rest = List.tl_exn events in
  let post_stream ~sw:_ ~inputs:_ =
    let first_pending = ref true in
    let rec stream () =
      if !first_pending
      then (
        first_pending := false;
        Seq.Cons (first, stream))
      else (
        assert (!observed > 0);
        Stdlib.List.to_seq rest ())
    in
    stream
  in
  let history =
    Stream.run_completion_stream_in_memory_entries
      ~env
      ~allocator
      ~history:[ input_entry allocator ]
      ~on_event:(fun _ -> Int.incr observed)
      ~tools:(Some [])
      ~tool_tbl:(Hashtbl.create (module String))
      ~post_stream
      ()
  in
  print_s [%sexp (!observed : int), (List.map history ~f:entry_kind : string list)];
  [%expect {| (3 (input message)) |}]
;;

let%expect_test "parsing failure after publication propagates without replay" =
  Eio_main.run
  @@ fun env ->
  let allocator =
    History_entry.Allocator.create ~namespace:"published-failure" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let attempts = ref 0 in
  let observed = ref 0 in
  let post_stream ~sw:_ ~inputs:_ =
    Int.incr attempts;
    let first =
      List.hd_exn (stream_message ~output_index:0 ~item_id:"partial-message" "partial")
    in
    let emitted = ref false in
    fun () ->
      if not !emitted
      then (
        emitted := true;
        Seq.Cons
          ( first
          , fun () ->
              raise
                (Res.Response_stream_parsing_error
                   (`Object [], Failure "invalid streamed tail")) ))
      else Seq.Nil
  in
  (match
     Stream.run_completion_stream_in_memory_entries
       ~env
       ~allocator
       ~history:[ input_entry allocator ]
       ~on_event:(fun _ -> Int.incr observed)
       ~tools:(Some [])
       ~tool_tbl:(Hashtbl.create (module String))
       ~post_stream
       ()
   with
   | exception Res.Response_stream_parsing_error _ -> ()
   | _ -> failwith "Expected streamed parsing failure");
  print_s [%sexp (!attempts : int), (!observed : int)];
  [%expect {| (1 1) |}]
;;

let%expect_test "conflicting completion payload fails before duplicate execution" =
  Eio_main.run
  @@ fun env ->
  let tool_tbl : (string, Ochat_function.runner) Hashtbl.t =
    Hashtbl.create (module String)
  in
  Hashtbl.set tool_tbl ~key:"echo" ~data:(fun ~invocation:_ payload ->
    Res.Tool_output.Output.Text payload);
  let added, first =
    stream_function_call
      ~output_index:0
      ~item_id:"conflict-1"
      ~call_id:"call-conflict"
      ~arguments:"one"
  in
  let _, second =
    stream_function_call
      ~output_index:0
      ~item_id:"conflict-1"
      ~call_id:"call-conflict"
      ~arguments:"two"
  in
  (try
     ignore
       (run_entry_stream
          env
          ~namespace:"tool-conflict"
          ~responses:[ [ added; first; second ] ]
          ~tool_tbl
          ()
        : History_entry.Allocator.t * History_entry.t list);
     print_endline "unexpected success"
   with
   | Failure message -> print_endline message);
  [%expect {| Conflicting completion for streamed tool item conflict-1 |}]
;;

let%expect_test "file-backed stream exposes final canonical message identity" =
  let temp_dir = "ochat-stream-identity-" ^ Int.to_string (Random.int 1_000_000) in
  Core_unix.mkdir_p temp_dir;
  let output_file = Filename.concat temp_dir "conversation.chatmd" in
  Out_channel.write_all output_file ~data:"<user>hello</user>\n";
  Eio_main.run
  @@ fun env ->
  let history_events = Queue.create () in
  let final_history = ref None in
  let post_stream ~sw:_ ~inputs:_ =
    let events = stream_message ~output_index:0 ~item_id:"file-message" "done" in
    let first = List.hd_exn events in
    let rest = List.tl_exn events in
    let first_pending = ref true in
    let rec stream () =
      if !first_pending
      then (
        first_pending := false;
        Seq.Cons (first, stream))
      else (
        assert (not (Queue.is_empty history_events));
        Stdlib.List.to_seq rest ())
    in
    stream
  in
  Chat_response.Driver.run_completion_stream
    ~env
    ~output_file
    ~post_stream
    ~on_history_event:(Queue.enqueue history_events)
    ~on_final_history:(fun history -> final_history := Some history)
    ();
  let history = Option.value_exn !final_history in
  let message =
    List.find_exn history ~f:(fun entry -> String.equal (entry_kind entry) "message")
  in
  let message_id = History_entry.id message in
  print_s
    [%sexp
      (Queue.length history_events : int)
    , (Queue.for_all history_events ~f:(fun event ->
         History_entry.Id.equal event.entry_id message_id)
       : bool)
    , (List.map history ~f:entry_kind : string list)];
  [%expect {| (3 true (input message)) |}]
;;
