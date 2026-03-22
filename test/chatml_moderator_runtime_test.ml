open Core
open Chatml
module L = Chatml_lang
module Runtime = Chatml_moderator_runtime
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

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let context ~(phase : string) : L.value =
  let json_null = L.VVariant ("Null", []) in
  L.VRecord
    (Map.of_alist_exn
       (module String)
       [ "session_id", L.VString "session-1"
       ; "now_ms", L.VInt 123
       ; "phase", L.VString phase
       ; "items", L.VArray [||]
       ; "available_tools", L.VArray [||]
       ; "session_meta", json_null
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
       ~context:(context ~phase:"before_model")
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
       ~context:(context ~phase:"before_model")
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

let%expect_test "moderator runtime default unconfigured external operation fails clearly" =
  let session = compile_session missing_external_handler_script in
  (match
     Runtime.handle_event
       session
       ~context:(context ~phase:"before_model")
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
