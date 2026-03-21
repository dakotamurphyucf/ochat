open Core
module Lang = Chatml.Chatml_lang
module Moderation = Chat_response.Moderation
module Res = Openai.Responses
module Runtime = Chatml_moderator_runtime
module Builtin_modules = Chatml_builtin_modules

let show_value = Builtin_modules.value_to_string

let show_values (values : Lang.value list) : string =
  values |> List.map ~f:show_value |> String.concat ~sep:"; " |> Printf.sprintf "[%s]"
;;

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let input_text (text : string) : Res.Input_message.content_item =
  Res.Input_message.Text { text; _type = "input_text" }
;;

let output_text (text : string) : Res.Output_message.content =
  { Res.Output_message.annotations = []; text; _type = "output_text" }
;;

let test_context ~(phase : string) : Lang.value =
  let session_meta = `Null |> Chatml.Chatml_value_codec.jsonaf_to_value in
  Lang.VRecord
    (Map.of_alist_exn
       (module String)
       [ "session_id", Lang.VString "session-1"
       ; "now_ms", Lang.VInt 123
       ; "phase", Lang.VString phase
       ; "messages", Lang.VArray [||]
       ; "available_tools", Lang.VArray [||]
       ; "session_meta", session_meta
       ])
;;

let compile_session ~handlers (source : string) : Runtime.session =
  let compiled = ok_or_fail (Runtime.compile_script ~source ()) in
  let config = Runtime.default_runtime_config ~handlers () in
  ok_or_fail
    (Runtime.instantiate_session
       config
       compiled
       ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" })
;;

let print_messages (messages : Moderation.Message.t list) =
  List.iter messages ~f:(fun message ->
    print_endline (Printf.sprintf "%s %s %S" message.id message.role message.content))
;;

let%expect_test "projection preserves stable host ids" =
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ; Res.Item.Function_call
        { name = "search"
        ; arguments = {|{"q":"ocaml"}|}
        ; call_id = "call-1"
        ; _type = "function_call"
        ; id = None
        ; status = None
        }
    ; Res.Item.Function_call_output
        { output = Res.Tool_output.Output.Text {|{"ok":true}|}
        ; call_id = "call-1"
        ; _type = "function_call_output"
        ; id = None
        ; status = None
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
  let projection, messages =
    Moderation.Projection.project_history Moderation.Projection.empty history
  in
  print_s [%sexp (projection : Moderation.Projection.t)];
  print_messages messages;
  let history =
    history
    @ [ Res.Item.Input_message
          { role = Res.Input_message.User
          ; content = [ input_text "Next" ]
          ; _type = "message"
          }
      ]
  in
  let projection, messages = Moderation.Projection.project_history projection history in
  print_s [%sexp (projection : Moderation.Projection.t)];
  print_messages messages;
  let _, context =
    Moderation.Projection.project_context
      ~projection
      ~session_id:"session-1"
      ~now_ms:456
      ~phase:Moderation.Phase.Pre_tool_call
      ~history
      ~available_tools:
        [ Res.Request.Tool.Function
            { name = "search"
            ; description = Some "search docs"
            ; parameters = `Object [ "type", `String "object" ]
            ; strict = false
            ; type_ = "function"
            }
        ; Res.Request.Tool.Custom_function
            { name = "exec"
            ; description = Some "run command"
            ; format = `String "string"
            ; type_ = "custom"
            }
        ]
      ~session_meta:`Null
  in
  print_endline (Moderation.Phase.to_string context.phase);
  print_s
    [%sexp
      (List.map context.available_tools ~f:(fun tool -> tool.name, tool.description)
       : (string * string) list)];
  let call =
    history |> List.find_map ~f:Moderation.Tool_call.of_response_item |> Option.value_exn
  in
  print_endline
    (show_value (Moderation.Event.to_value (Moderation.Event.Pre_tool_call call)));
  [%expect
    {|
    ((item_ids (host-message-1 host-message-2 host-message-3 msg-1))
     (next_generated_id 4))
    host-message-1 user "Hello"
    host-message-2 assistant "search {\"q\":\"ocaml\"}"
    host-message-3 tool "{\"ok\":true}"
    msg-1 assistant "Done"
    ((item_ids
      (host-message-1 host-message-2 host-message-3 msg-1 host-message-4))
     (next_generated_id 5))
    host-message-1 user "Hello"
    host-message-2 assistant "search {\"q\":\"ocaml\"}"
    host-message-3 tool "{\"ok\":true}"
    msg-1 assistant "Done"
    host-message-4 user "Next"
    pre_tool_call
    ((search "search docs") (exec "run command"))
    `Pre_tool_call({ args = `Object([|{ key = q; value = `String(ocaml) }|]); id = call-1; name = search })
    |}]
;;

let%expect_test "decode committed local effects into structured moderation outcome" =
  let appended_message =
    Moderation.Message.create
      ~id:"host-synth-1"
      ~role:"assistant"
      ~content:"Synthetic"
      ~meta:`Null
  in
  let effects =
    [ { Lang.op = "Turn.prepend_system"; args = [ Lang.VString "policy" ] }
    ; { op = "Turn.append_message"
      ; args = [ Moderation.Message.to_value appended_message ]
      }
    ; { op = "Tool.rewrite_args"
      ; args =
          [ Chatml.Chatml_value_codec.jsonaf_to_value (`Object [ "mode", `String "safe" ])
          ]
      }
    ; { op = "Runtime.request_compaction"; args = [] }
    ; { op = "Runtime.emit"; args = [ Lang.VVariant ("Ping", []) ] }
    ]
  in
  let decoded = ok_or_fail (Runtime.decode_local_effects effects) in
  let outcome = ok_or_fail (Moderation.Outcome.of_runtime_effects decoded) in
  print_s [%sexp (outcome.overlay_ops : Moderation.Overlay.op list)];
  print_s [%sexp (outcome.tool_moderation : Moderation.Tool_moderation.t option)];
  print_s [%sexp (outcome.runtime_requests : Moderation.Runtime_request.t list)];
  print_endline ("emitted=" ^ show_values outcome.emitted_events);
  let conflict =
    Runtime.decode_local_effects
      [ { op = "Tool.approve"; args = [] }
      ; { op = "Tool.reject"; args = [ Lang.VString "denied" ] }
      ]
    |> Result.bind ~f:Moderation.Outcome.of_runtime_effects
  in
  (match conflict with
   | Ok _ -> print_endline "unexpected success"
   | Error msg -> print_endline msg);
  [%expect
    {|
    ((Prepend_system policy)
     (Append_message
      ((id host-synth-1) (role assistant) (content Synthetic) (meta Null))))
    ((Rewrite_args (Object ((mode (String safe))))))
    (Request_compaction)
    emitted=[`Ping]
    Expected at most one tool moderation action for a single host event.
    |}]
;;

let%expect_test "capability registry adapts named model recipes" =
  let script =
    {|
      type event = [ `Tick ]
      type state = { seen : string array }

      let initial_state = { seen = Array.make(0, "") }

      let on_event : context -> state -> event -> state task =
        fun ctx st ev ->
          Task.bind(Model.call("summarize", `String("payload")), fun call_result ->
          Task.bind(Model.spawn("summarize", `String("payload")), fun job_id ->
          Task.pure({ seen = [ to_string(call_result), job_id ] })))
    |}
  in
  let recipe : Moderation.Capabilities.model_recipe =
    { call =
        (fun ~payload ->
          Ok
            (Moderation.Capabilities.Model_ok
               (`Object [ "summary", payload; "source", `String "moderation-test" ])))
    ; spawn = (fun ~payload:_ -> Ok "job-1")
    }
  in
  let handlers =
    Moderation.Capabilities.runtime_handlers
      { Moderation.Capabilities.default with
        model_recipes = Map.of_alist_exn (module String) [ "summarize", recipe ]
      }
  in
  let session = compile_session ~handlers script in
  (match
     Runtime.handle_event
       session
       ~context:(test_context ~phase:"turn_start")
       ~event:(Lang.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "ok"
   | Error msg -> print_endline msg);
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  [%expect
    {|
    ok
    state={ seen = [|`Ok(`Object([|{ key = summary; value = `String(payload) }, { key = source; value = `String(moderation-test) }|])), job-1|] }
    |}]
;;
