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
    "ask_choice "
    ^ prompt
    ^ " ["
    ^ String.concat ~sep:", " (Array.to_list choices)
    ^ "]"
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
    ok_or_fail (Manager.Registry.compile_script ~surface Manager.Registry.empty script) |> snd
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
    { consume =
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
        ; _type = "message"
        }
    ]
  in
  let items =
    ok_or_fail
      (Stream.prepare_turn_inputs ~moderator:None ~available_tools:[] ~now_ms:1 ~history ())
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
    { Chat_response.Runtime_semantics.default_budget_policy with max_self_triggered_turns = 2 }
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
      { Chat_response.Runtime_semantics.default_budget_policy with max_internal_event_drain = 2 }
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
    (Manager.enqueue_internal_event moderator.manager (Lang.VVariant ("Queued", [ Lang.VString "one" ])));
  ok_or_fail
    (Manager.enqueue_internal_event moderator.manager (Lang.VVariant ("Queued", [ Lang.VString "two" ])));
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
    |> fun snapshot -> List.length snapshot.Session.Moderator_snapshot.queued_internal_events
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
    "approval suspension blocks turn-driver progression, keeps queued work pending, and is not persisted"
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
    moderator_of_source
      ~surface:Chatml.Chatml_builtin_surface.ui_moderator_surface
      source
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
  let pending_snapshot = ok_or_fail (Manager.snapshot moderator.manager) in
  print_s [%sexp (pending_snapshot.current_state : Session.Snapshot.t)];
  print_endline
    (Printf.sprintf
       "pending_snapshot_queue=%d"
       (List.length pending_snapshot.queued_internal_events));
  ok_or_fail
    (Manager.enqueue_internal_event
       moderator.manager
       (Lang.VVariant ("Queued", [ Lang.VString "host" ])));
  let queued_while_pending = ok_or_fail (Manager.snapshot moderator.manager) in
  print_endline
    (Printf.sprintf
       "queued_while_pending=%d"
       (List.length queued_while_pending.queued_internal_events));
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
  let restored =
    let script =
      CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
    in
    let artifact =
      ok_or_fail
        (Manager.Registry.compile_script
           ~surface:Chatml.Chatml_builtin_surface.ui_moderator_surface
           Manager.Registry.empty
           script)
      |> snd
    in
    ok_or_fail
      (Manager.create
         ~artifact
         ~capabilities:Chat_response.Moderation.Capabilities.default
         ~snapshot:pending_snapshot
         ())
  in
  print_endline
    ("restored_pending=" ^ show_pending_ui_request (Manager.pending_ui_request restored));
  let restored_snapshot = ok_or_fail (Manager.snapshot restored) in
  print_s [%sexp (restored_snapshot.current_state : Session.Snapshot.t)];
  print_endline
    (Printf.sprintf
       "restored_queue=%d"
       (List.length restored_snapshot.queued_internal_events));
  [%expect
    {|
    Session is waiting for UI input.
    pending=ask_text continue?
    (Record ((approved (String ""))))
    pending_snapshot_queue=0
    queued_while_pending=1
    Session is waiting for UI input.
    1
    (Record ((approved (String approved))))
    queued_after_resume=2
    queued-host assistant "host"
    queued-buffered assistant "buffered"
    restored_pending=none
    (Record ((approved (String ""))))
    restored_queue=0
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
      { consume =
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
