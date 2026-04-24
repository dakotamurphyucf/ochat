open Core
open Chatml
module L = Chatml_lang
module Runtime = Chatml_moderator_runtime
module Builtin_surface = Chatml_builtin_surface
module Builtin_modules = Chatml_builtin_modules

let show_value = Builtin_modules.value_to_string

let show_values (values : L.value list) : string =
  values |> List.map ~f:show_value |> String.concat ~sep:"; " |> Printf.sprintf "[%s]"
;;

let show_effects (effects : L.eff list) : string =
  let show_effect (eff : L.eff) =
    let rendered_args = eff.args |> List.map ~f:show_value |> String.concat ~sep:", " in
    if String.is_empty rendered_args
    then Printf.sprintf "%s()" eff.op
    else Printf.sprintf "%s(%s)" eff.op rendered_args
  in
  effects |> List.map ~f:show_effect |> String.concat ~sep:"; " |> Printf.sprintf "[%s]"
;;

let show_pending_ui_request = function
  | None -> "none"
  | Some (Runtime.Ask_text { prompt }) -> "ask_text " ^ prompt
  | Some (Runtime.Ask_choice { prompt; choices }) ->
    "ask_choice "
    ^ prompt
    ^ " ["
    ^ String.concat ~sep:", " (Array.to_list choices)
    ^ "]"
;;

let item_id_of_value = function
  | L.VRecord fields ->
    (match Map.find fields "id" with
     | Some (L.VString id) -> id
     | _ -> "<invalid-item>")
  | _ -> "<invalid-item>"
;;

let show_local_effects (effects : L.eff list) : string =
  let show_local_effect = function
    | Runtime.Turn_effect (Runtime.Prepend_system text) -> "prepend_system " ^ text
    | Runtime.Turn_effect (Runtime.Append_message item) ->
      "append_item " ^ item_id_of_value item
    | Runtime.Turn_effect (Runtime.Replace_message (target_id, item)) ->
      "replace_item " ^ target_id ^ "->" ^ item_id_of_value item
    | Runtime.Turn_effect (Runtime.Delete_message id) -> "delete_item " ^ id
    | Runtime.Turn_effect (Runtime.Halt reason) -> "halt " ^ reason
    | Runtime.Tool_moderation_effect _ -> "tool_moderation"
    | Runtime.Ui_notification message -> "ui_notify " ^ message
    | Runtime.Emit_internal_event event -> "emit " ^ show_value event
    | Runtime.Request_compaction -> "request_compaction"
    | Runtime.Request_turn -> "request_turn"
    | Runtime.End_session reason -> "end_session " ^ reason
  in
  match Runtime.decode_local_effects effects with
  | Ok local_effects ->
    local_effects
    |> List.map ~f:show_local_effect
    |> String.concat ~sep:"; "
    |> Printf.sprintf "[%s]"
  | Error msg -> "error: " ^ msg
;;

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let json_null = L.VVariant ("Null", [])

let json_string (value : string) : L.value = L.VVariant ("String", [ L.VString value ])

let json_entry ~(key : string) ~(value : L.value) : L.value =
  L.VRecord (Map.of_alist_exn (module String) [ "key", L.VString key; "value", value ])
;;

let json_object (fields : (string * L.value) list) : L.value =
  fields
  |> List.map ~f:(fun (key, value) -> json_entry ~key ~value)
  |> Array.of_list
  |> fun entries -> L.VVariant ("Object", [ L.VArray entries ])
;;

let item ~(id : string) ~(value : L.value) : L.value =
  L.VRecord (Map.of_alist_exn (module String) [ "id", L.VString id; "value", value ])
;;

let tool_desc ~(name : string) ~(description : string) : L.value =
  L.VRecord
    (Map.of_alist_exn
       (module String)
       [ "name", L.VString name
       ; "description", L.VString description
       ; "input_schema", json_null
       ])
;;

let message_item_value ~(role : string) : L.value =
  json_object [ "type", json_string "message"; "role", json_string role ]
;;

let context
      ?(items = [||])
      ?(available_tools = [||])
      ?(session_meta = json_null)
      ~(phase : string)
      ()
  : L.value
  =
  L.VRecord
    (Map.of_alist_exn
       (module String)
       [ "session_id", L.VString "session-1"
       ; "now_ms", L.VInt 123
       ; "phase", L.VString phase
       ; "items", L.VArray items
       ; "available_tools", L.VArray available_tools
       ; "session_meta", session_meta
       ])
;;

let compile_session ?(handlers = Runtime.default_handlers) (source : string)
  : Runtime.session
  =
  let compiled = ok_or_fail (Runtime.compile_script ~source ()) in
  let config = Runtime.default_runtime_config ~handlers () in
  ok_or_fail
    (Runtime.instantiate_session
       config
       compiled
       ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" })
;;

let compile_session_with_surface
      ?(handlers = Runtime.default_handlers)
      ~(surface : Builtin_surface.surface)
      (source : string)
  : Runtime.session
  =
  let compiled = ok_or_fail (Runtime.compile_script ~surface ~source ()) in
  let config = Runtime.default_runtime_config ~surface ~handlers () in
  ok_or_fail
    (Runtime.instantiate_session
       config
       compiled
       ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" })
;;

let local_ops_script =
  {|
    type state = { count : int }
    type event = [ `Tick | `InternalDone(string) ]

    let initial_state = { count = 0 }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          Task.bind(Log.info("starting"), fun ignored_log ->
          Task.bind(Turn.prepend_system("be concise"), fun ignored_turn ->
          Task.bind(Runtime.request_compaction(), fun ignored_compaction ->
          Task.bind(Runtime.emit(`InternalDone("queued")), fun ignored_emit ->
          Task.bind(Runtime.end_session("finished"), fun ignored_end ->
          Task.pure({ count = st.count + 1 }))))))
        | `InternalDone(_) ->
          Task.pure(st)
  |}
;;

let external_ops_script =
  {|
    type state = { last : string array }
    type event = [ `Tick ]

    let initial_state = { last = Array.make(0, "") }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          Task.bind(Tool.call("echo", `String("payload")), fun tool_result ->
          Task.bind(Model.call("classify", `String("payload")), fun model_result ->
          Task.bind(Tool.spawn("tool-bg", `Null), fun tool_job ->
          Task.bind(Model.spawn("model-bg", `Null), fun model_job ->
          Task.bind(Schedule.after_ms(50, `Tick), fun timer_id ->
          Task.bind(Schedule.cancel(timer_id), fun ignored_cancel ->
          Task.bind(Process.run("cat", ["default.md"]), fun ignored_cancel ->
          Task.pure
            ({ last =
                 [ to_string(tool_result)
                 , to_string(model_result)
                 , tool_job
                 , model_job
                 , timer_id
                 ]
             }))))))))
  |}
;;

let missing_external_handler_script =
  {|
    type event = [ `Tick ]

    let initial_state = 0

    let on_event : context -> int -> event -> int task =
      fun ctx st ev ->
        Task.bind(Tool.call("echo", `Null), fun ignored_tool ->
        Task.pure(st + 1))
  |}
;;

let helper_builtins_script =
  {|
    type state =
      { model_text : string
      ; model_json : string
      ; spawn_id : string
      }

    type event = [ `Tick ]

    let initial_state = { model_text = ""; model_json = ""; spawn_id = "" }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          Task.bind(
            Turn.replace_or_append(
              Option.some("msg-1"),
              Item.output_text_message("msg-1", "rewritten")),
            fun ignored_replace ->
          Task.bind(
            Turn.replace_or_append(
              Option.none(),
              Item.output_text_message("msg-2", "added")),
            fun ignored_append ->
          Task.bind(Turn.append_notice("watch this"), fun ignored_notice ->
          Task.bind(Model.call_text("classify", "payload"), fun model_text ->
          Task.bind(Model.call_json("classify", `Null), fun model_json ->
          Task.bind(Model.spawn_text("classify", "later"), fun spawn_id ->
          Task.pure
            ({ model_text = to_string(model_text)
             ; model_json = to_string(model_json)
             ; spawn_id = spawn_id
             })))))))
  |}
;;

let item_helper_parity_script =
  {|
    type state =
      { assistant : bool
      ; assistant_text : string
      ; notice_id : string
      ; notice_text : string
      ; system : bool
      ; system_text : string
      ; tool_call : bool
      ; tool_result : bool
      ; user : bool
      ; user_text : string
      }

    type event = [ `Tick ]

    let initial_state =
      { assistant = false
      ; assistant_text = ""
      ; notice_id = ""
      ; notice_text = ""
      ; system = false
      ; system_text = ""
      ; tool_call = false
      ; tool_result = false
      ; user = false
      ; user_text = ""
      }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          let user_item = Item.user_text("user-1", "hello") in
          let assistant_item = Item.assistant_text("assistant-1", "reply") in
          let system_item = Item.system_text("system-1", "policy") in
          let notice_item = Item.notice("notice-1", "watch this") in
          let tool_call_item =
            Item.create("call-1", Json.parse("{\"type\":\"function_call\"}")) in
          let tool_result_item =
            Item.create("result-1", Json.parse("{\"type\":\"function_call_output\"}")) in
          Task.bind(Turn.append_item(user_item), fun ignored_user ->
          Task.bind(Turn.append_item(assistant_item), fun ignored_assistant ->
          Task.bind(Turn.append_item(system_item), fun ignored_system ->
          Task.bind(Turn.append_item(notice_item), fun ignored_notice ->
          Task.pure
            ({ assistant = Item.is_assistant(assistant_item)
             ; assistant_text = Option.get_or(Item.text(assistant_item), "")
             ; notice_id = Item.id(notice_item)
             ; notice_text = Option.get_or(Item.text(notice_item), "")
             ; system = Item.is_system(system_item)
             ; system_text = Option.get_or(Item.text(system_item), "")
             ; tool_call = Item.is_tool_call(tool_call_item)
             ; tool_result = Item.is_tool_result(tool_result_item)
             ; user = Item.is_user(user_item)
             ; user_text = Option.get_or(Item.text(user_item), "")
             })))))
  |}
;;

let ui_notify_script =
  {|
    type event = [ `Tick ]

    let initial_state = 0

    let on_event : context -> int -> event -> int task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          Task.bind(Ui.notify("watch this"), fun ignored_notify ->
          Task.pure(st + 1))
  |}
;;

let approval_suspend_script =
  {|
    type state = { approved : string; count : int }
    type event = [ `Tick | `Queued(string) ]

    let initial_state = { approved = ""; count = 0 }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          Task.bind(Turn.prepend_system("before"), fun ignored_turn ->
          Task.bind(Runtime.emit(`Queued("buffered")), fun ignored_emit ->
          Task.bind(Approval.ask_text("continue?"), fun answer ->
          Task.pure({ approved = answer; count = st.count + 1 }))))
        | `Queued(_) -> Task.pure(st)
  |}
;;

let nested_approval_script =
  {|
    type event = [ `Tick ]

    let initial_state = 0

    let on_event : context -> int -> event -> int task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          Task.bind(Approval.ask_text("first"), fun first ->
          Task.bind(Approval.ask_text("second"), fun second ->
          Task.pure(st + 1)))
  |}
;;

let tool_call_helper_parity_script =
  {|
    type state =
      { arg_array_len : int
      ; arg_bool : bool
      ; arg_present : bool
      ; arg_string : string
      ; missing_arg : bool
      ; named : bool
      ; one_of : bool
      ; wrong_array : bool
      ; wrong_string : bool
      }

    type event = [ `Tick ]

    let initial_state =
      { arg_array_len = 0
      ; arg_bool = false
      ; arg_present = false
      ; arg_string = ""
      ; missing_arg = false
      ; named = false
      ; one_of = false
      ; wrong_array = false
      ; wrong_string = false
      }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          let call : tool_call =
            { id = "call-1"
            ; name = "search"
            ; args =
                Json.parse(
                  "{\"query\":\"cats\",\"stream\":true,\"tags\":[\"news\",\"tech\"]}")
            } in
          Task.pure
            ({ arg_array_len =
                 Array.length(Option.get_or(Tool_call.arg_array(call, "tags"), Array.make(0, `Null)))
             ; arg_bool = Option.get_or(Tool_call.arg_bool(call, "stream"), false)
             ; arg_present = Option.is_some(Tool_call.arg(call, "query"))
             ; arg_string = Option.get_or(Tool_call.arg_string(call, "query"), "")
             ; missing_arg = Option.is_none(Tool_call.arg(call, "missing"))
             ; named = Tool_call.is_named(call, "search")
             ; one_of = Tool_call.is_one_of(call, ["search"])
             ; wrong_array = Option.is_none(Tool_call.arg_array(call, "query"))
             ; wrong_string = Option.is_none(Tool_call.arg_string(call, "stream"))
             })
  |}
;;

let context_helper_parity_script =
  {|
    type state =
      { assistant_count : int
      ; found_item_id : string
      ; found_tool : bool
      ; has_tool : bool
      ; last_assistant_id : string
      ; last_item_id : string
      ; last_system_id : string
      ; last_tool_call_id : string
      ; last_tool_result_id : string
      ; last_user_id : string
      ; since_assistant_len : int
      ; since_user_len : int
      }

    type event = [ `Tick ]

    let initial_state =
      { assistant_count = 0
      ; found_item_id = ""
      ; found_tool = false
      ; has_tool = false
      ; last_assistant_id = ""
      ; last_item_id = ""
      ; last_system_id = ""
      ; last_tool_call_id = ""
      ; last_tool_result_id = ""
      ; last_user_id = ""
      ; since_assistant_len = 0
      ; since_user_len = 0
      }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Tick ->
          let missing_item = Item.create("missing", `Null) in
          Task.pure
            ({ assistant_count = Array.length(Context.items_by_role(ctx, "assistant"))
             ; found_item_id =
                 Item.id(Option.get_or(Context.find_item(ctx, "assistant-1"), missing_item))
             ; found_tool = Option.is_some(Context.find_tool(ctx, "search"))
             ; has_tool = Context.has_tool(ctx, "browse")
             ; last_assistant_id =
                 Item.id(Option.get_or(Context.last_assistant_item(ctx), missing_item))
             ; last_item_id = Item.id(Option.get_or(Context.last_item(ctx), missing_item))
             ; last_system_id =
                 Item.id(Option.get_or(Context.last_system_item(ctx), missing_item))
             ; last_tool_call_id =
                 Item.id(Option.get_or(Context.last_tool_call(ctx), missing_item))
             ; last_tool_result_id =
                 Item.id(Option.get_or(Context.last_tool_result(ctx), missing_item))
             ; last_user_id = Item.id(Option.get_or(Context.last_user_item(ctx), missing_item))
             ; since_assistant_len =
                 Array.length(Context.items_since_last_assistant_turn(ctx))
             ; since_user_len = Array.length(Context.items_since_last_user_turn(ctx))
             })
  |}
;;

let%expect_test "moderator runtime default local operations" =
  let logs = ref [] in
  let handlers =
    { Runtime.default_handlers with
      on_log =
        (fun _session ~level ~message ->
          logs
          := !logs
             @ [ Printf.sprintf "%s:%s" (Runtime.string_of_log_level level) message ];
          Ok ())
    }
  in
  let session = compile_session ~handlers local_ops_script in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model" ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "ok"
   | Error msg -> print_endline ("error: " ^ msg));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  print_endline ("queue=" ^ show_values (Runtime.queued_events session));
  print_endline ("effects=" ^ show_effects (Runtime.committed_local_effects session));
  print_endline ("halted=" ^ Bool.to_string (Runtime.is_halted session));
  print_endline ("logs=[" ^ String.concat ~sep:"; " !logs ^ "]");
  [%expect
    {|
    ok
    state={ count = 1 }
    queue=[`InternalDone(queued)]
    effects=[Turn.prepend_system(be concise); Runtime.request_compaction(); Runtime.emit(`InternalDone(queued)); Runtime.end_session(finished)]
    halted=true
    logs=[info:starting]
    |}]
;;

let%expect_test "moderator runtime default external handlers" =
  let actions = ref [] in
  let handlers =
    { Runtime.default_handlers with
      on_tool_call =
        (fun _session ~name ~args ->
          actions := !actions @ [ "tool_call " ^ name ^ " " ^ show_value args ];
          Ok (L.VVariant ("Ok", [ args ])))
    ; on_model_call =
        (fun _session ~recipe ~payload ->
          actions := !actions @ [ "model_call " ^ recipe ^ " " ^ show_value payload ];
          Ok (L.VVariant ("Refused", [ L.VString "policy" ])))
    ; on_tool_spawn =
        (fun _session ~name ~args ->
          actions := !actions @ [ "tool_spawn " ^ name ^ " " ^ show_value args ];
          Ok "tool-job-1")
    ; on_model_spawn =
        (fun _session ~recipe ~payload ->
          actions := !actions @ [ "model_spawn " ^ recipe ^ " " ^ show_value payload ];
          Ok "model-job-1")
    ; on_process_run =
        (fun _session ~command ~args ->
          actions := !actions @ [ "process_run " ^ command ^ " " ^ show_value args ];
          Ok "process-job-1")
    ; on_schedule_after_ms =
        (fun _session ~delay_ms ~payload ->
          actions
          := !actions
             @ [ Printf.sprintf "schedule_after_ms %d %s" delay_ms (show_value payload) ];
          Ok "timer-1")
    ; on_schedule_cancel =
        (fun _session ~id ->
          actions := !actions @ [ "schedule_cancel " ^ id ];
          Ok ())
    }
  in
  let session = compile_session ~handlers external_ops_script in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model" ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "ok"
   | Error msg -> print_endline ("error: " ^ msg));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  print_endline ("queue=" ^ show_values (Runtime.queued_events session));
  print_endline ("effects=" ^ show_effects (Runtime.committed_local_effects session));
  print_endline ("halted=" ^ Bool.to_string (Runtime.is_halted session));
  print_endline ("actions=[" ^ String.concat ~sep:"; " !actions ^ "]");
  [%expect
    {|
    ok
    state={ last = [|`Ok(`String(payload)), `Refused(policy), tool-job-1, model-job-1, timer-1|] }
    queue=[]
    effects=[]
    halted=false
    actions=[tool_call echo `String(payload); model_call classify `String(payload); tool_spawn tool-bg `Null; model_spawn model-bg `Null; schedule_after_ms 50 `Tick; schedule_cancel timer-1; process_run cat [|default.md|]]
    |}]
;;

let%expect_test "moderator runtime helper builtins reuse existing operations" =
  let actions = ref [] in
  let handlers =
    { Runtime.default_handlers with
      on_model_call =
        (fun _session ~recipe ~payload ->
          actions := !actions @ [ "model_call " ^ recipe ^ " " ^ show_value payload ];
          Ok (L.VVariant ("Ok", [ payload ])))
    ; on_model_spawn =
        (fun _session ~recipe ~payload ->
          actions := !actions @ [ "model_spawn " ^ recipe ^ " " ^ show_value payload ];
          Ok "model-job-text")
    }
  in
  let session = compile_session ~handlers helper_builtins_script in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model" ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "ok"
   | Error msg -> print_endline ("error: " ^ msg));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  print_endline ("effects=" ^ show_local_effects (Runtime.committed_local_effects session));
  print_endline ("actions=[" ^ String.concat ~sep:"; " !actions ^ "]");
  [%expect
    {|
    ok
    state={ model_json = `Ok(`Null); model_text = `Ok(`String(payload)); spawn_id = model-job-text }
    effects=[replace_item msg-1->msg-1; append_item msg-2; append_item system:watch this]
    actions=[model_call classify `String(payload); model_call classify `Null; model_spawn classify `String(later)]
    |}]
;;

let%expect_test "moderator runtime Ui.notify is available only on the UI surface" =
  let actions = ref [] in
  let handlers =
    { Runtime.default_handlers with
      on_ui_notify =
        (fun _session ~message ->
          actions := !actions @ [ "ui_notify " ^ message ];
          Ok ())
    }
  in
  let compiled =
    ok_or_fail
      (Runtime.compile_script
         ~surface:Chatml_builtin_surface.ui_moderator_surface
         ~source:ui_notify_script
         ())
  in
  let config =
    Runtime.default_runtime_config
      ~surface:Chatml_builtin_surface.ui_moderator_surface
      ~handlers
      ()
  in
  let session =
    ok_or_fail
      (Runtime.instantiate_session
         config
         compiled
         ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" })
  in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model" ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "ok"
   | Error msg -> print_endline ("error: " ^ msg));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  print_endline ("effects=" ^ show_local_effects (Runtime.committed_local_effects session));
  print_endline ("actions=[" ^ String.concat ~sep:"; " !actions ^ "]");
  (match Runtime.compile_script ~source:ui_notify_script () with
   | Ok _ -> print_endline "unexpected default-surface success"
   | Error msg -> print_endline msg);
  [%expect
    {|
    ok
    state=1
    effects=[ui_notify watch this]
    actions=[ui_notify watch this]
    line 10, characters 20-22:
    10|    Ui
          ^^

    Type error: Unknown variable 'Ui'
    |}]
;;

let%expect_test "moderator runtime suspends approval, blocks handle_event, and commits on resume" =
  let session =
    compile_session_with_surface
      ~surface:Chatml_builtin_surface.ui_moderator_surface
      approval_suspend_script
  in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"turn_start" ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "suspended"
   | Error msg -> print_endline ("error: " ^ msg));
  print_endline
    ("pending=" ^ show_pending_ui_request (Runtime.pending_ui_request session));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  print_endline ("queue=" ^ show_values (Runtime.queued_events session));
  print_endline ("effects=" ^ show_effects (Runtime.committed_local_effects session));
  ignore
    (ok_or_fail
       (Runtime.enqueue_internal_event
          session
          (L.VVariant ("Queued", [ L.VString "host" ])))
     : unit);
  print_endline ("queue_after_host_enqueue=" ^ show_values (Runtime.queued_events session));
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"internal_event" ())
       ~event:(L.VVariant ("Queued", [ L.VString "late" ]))
   with
   | Ok () -> print_endline "unexpected handle_event success"
   | Error msg -> print_endline msg);
  (match Runtime.resume_ui_request session ~response:"  approved  " with
   | Ok () -> print_endline "resumed"
   | Error msg -> print_endline ("resume error: " ^ msg));
  print_endline
    ("pending_after_resume=" ^ show_pending_ui_request (Runtime.pending_ui_request session));
  print_endline ("state_after_resume=" ^ show_value (Runtime.current_state session));
  print_endline ("queue_after_resume=" ^ show_values (Runtime.queued_events session));
  print_endline
    ("effects_after_resume=" ^ show_local_effects (Runtime.committed_local_effects session));
  (match Runtime.resume_ui_request session ~response:"again" with
   | Ok () -> print_endline "unexpected second resume success"
   | Error msg -> print_endline msg);
  [%expect
    {|
    suspended
    pending=ask_text continue?
    state={ approved = ; count = 0 }
    queue=[]
    effects=[]
    queue_after_host_enqueue=[`Queued(host)]
    Session is waiting for UI input.
    resumed
    pending_after_resume=none
    state_after_resume={ approved = approved; count = 1 }
    queue_after_resume=[`Queued(host); `Queued(buffered)]
    effects_after_resume=[prepend_system before; emit `Queued(buffered)]
    Session is not waiting for UI input.
    |}]
;;

let%expect_test "moderator runtime rejects nested approval during resume" =
  let session =
    compile_session_with_surface
      ~surface:Chatml_builtin_surface.ui_moderator_surface
      nested_approval_script
  in
  ignore
    (ok_or_fail
       (Runtime.handle_event
          session
          ~context:(context ~phase:"turn_start" ())
          ~event:(L.VVariant ("Tick", [])))
     : unit);
  print_endline
    ("pending=" ^ show_pending_ui_request (Runtime.pending_ui_request session));
  (match Runtime.resume_ui_request session ~response:"first" with
   | Ok () -> print_endline "unexpected resume success"
   | Error msg -> print_endline msg);
  print_endline
    ("pending_after_error=" ^ show_pending_ui_request (Runtime.pending_ui_request session));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  print_endline ("effects=" ^ show_effects (Runtime.committed_local_effects session));
  [%expect
    {|
    pending=ask_text first
    Nested UI approval is not allowed.
    pending_after_error=none
    state=0
    effects=[]
    |}]
;;

let%expect_test "moderator runtime Item helper builtins are available on the default surface" =
  let session = compile_session item_helper_parity_script in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model" ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "ok"
   | Error msg -> print_endline ("error: " ^ msg));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  print_endline ("effects=" ^ show_local_effects (Runtime.committed_local_effects session));
  [%expect
    {|
    ok
    state={ assistant = true; assistant_text = reply; notice_id = notice-1; notice_text = watch this; system = true; system_text = policy; tool_call = true; tool_result = true; user = true; user_text = hello }
    effects=[append_item user-1; append_item assistant-1; append_item system-1; append_item notice-1]
    |}]
;;

let%expect_test "moderator runtime Tool_call helper builtins are available on the default surface" =
  let session = compile_session tool_call_helper_parity_script in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model" ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "ok"
   | Error msg -> print_endline ("error: " ^ msg));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  [%expect
    {|
    ok
    state={ arg_array_len = 2; arg_bool = true; arg_present = true; arg_string = cats; missing_arg = true; named = true; one_of = true; wrong_array = true; wrong_string = true }
    |}]
;;

let%expect_test "moderator runtime Context helper builtins are available on the default surface" =
  let items =
    [| item ~id:"system-0" ~value:(message_item_value ~role:"system")
     ; item ~id:"user-1" ~value:(message_item_value ~role:"user")
     ; item
         ~id:"call-1"
         ~value:(json_object [ "type", json_string "function_call"; "name", json_string "search" ])
     ; item ~id:"assistant-1" ~value:(message_item_value ~role:"assistant")
     ; item
         ~id:"result-1"
         ~value:(json_object [ "type", json_string "function_call_output" ])
    |]
  in
  let available_tools =
    [| tool_desc ~name:"search" ~description:"Search the web"
     ; tool_desc ~name:"browse" ~description:"Open a page"
    |]
  in
  let session = compile_session context_helper_parity_script in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model" ~items ~available_tools ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "ok"
   | Error msg -> print_endline ("error: " ^ msg));
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  [%expect
    {|
    ok
    state={ assistant_count = 1; found_item_id = assistant-1; found_tool = true; has_tool = true; last_assistant_id = assistant-1; last_item_id = result-1; last_system_id = system-0; last_tool_call_id = call-1; last_tool_result_id = result-1; last_user_id = user-1; since_assistant_len = 2; since_user_len = 4 }
    |}]
;;

let%expect_test "moderator runtime default unconfigured external operation fails clearly" =
  let session = compile_session missing_external_handler_script in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model" ())
       ~event:(L.VVariant ("Tick", []))
   with
   | Ok () -> print_endline "unexpected success"
   | Error msg -> print_endline msg);
  print_endline ("state=" ^ show_value (Runtime.current_state session));
  print_endline ("queue=" ^ show_values (Runtime.queued_events session));
  print_endline ("effects=" ^ show_effects (Runtime.committed_local_effects session));
  [%expect
    {|
    Tool.call is not configured
    state=0
    queue=[]
    effects=[]
    |}]
;;
