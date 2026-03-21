open Core
module Lang = Chatml.Chatml_lang
module Parse = Chatml.Chatml_parse
module Typechecker = Chatml_typechecker
module Resolver = Chatml_resolver
module Eval = Chatml.Chatml_eval
module Builtin_surface = Chatml.Chatml_builtin_surface
module Builtin_modules = Chatml_builtin_modules
module Value_codec = Chatml.Chatml_value_codec

type compiled_script =
  { surface : Builtin_surface.surface
  ; program : Lang.program
  ; checked : Typechecker.checked_program
  ; resolved : Lang.resolved_program
  ; source_text : string
  }

type log_level =
  | Debug
  | Info
  | Warn
  | Error_level

type turn_effect =
  | Prepend_system of string
  | Append_message of Lang.value
  | Replace_message of string * Lang.value
  | Delete_message of string
  | Halt of string

type tool_moderation =
  | Approve
  | Reject of string
  | Rewrite_args of Lang.value
  | Redirect of string * Lang.value

type local_effect =
  | Turn_effect of turn_effect
  | Tool_moderation_effect of tool_moderation
  | Emit_internal_event of Lang.value
  | Request_compaction
  | End_session of string

type op_kind =
  | Local_transactional
  | External_sync
  | External_async
  | Diagnostic

type op_def =
  { name : string
  ; kind : op_kind
  ; perform : session -> Lang.value list -> (Lang.value, string) result
  ; phase_check : string -> (unit, string) result
  }

and runtime_config =
  { surface : Builtin_surface.surface
  ; operations : op_def list
  }

and compiled_entrypoints =
  { initial_state_name : string
  ; on_event_name : string
  }

and default_handlers =
  { on_log : session -> level:log_level -> message:string -> (unit, string) result
  ; on_turn_effect : session -> turn_effect -> (unit, string) result
  ; on_tool_moderation : session -> tool_moderation -> (unit, string) result
  ; on_tool_call :
      session -> name:string -> args:Lang.value -> (Lang.value, string) result
  ; on_tool_spawn : session -> name:string -> args:Lang.value -> (string, string) result
  ; on_model_call :
      session -> recipe:string -> payload:Lang.value -> (Lang.value, string) result
  ; on_model_spawn :
      session -> recipe:string -> payload:Lang.value -> (string, string) result
  ; on_schedule_after_ms :
      session -> delay_ms:int -> payload:Lang.value -> (string, string) result
  ; on_schedule_cancel : session -> id:string -> (unit, string) result
  ; on_request_compaction : session -> (unit, string) result
  ; on_end_session : session -> reason:string -> (unit, string) result
  }

and exec_ctx =
  { phase : string
  ; mutable local_effects_rev : Lang.eff list
  ; mutable emitted_rev : Lang.value list
  ; mutable end_session_requested : string option
  }

and session =
  { env : Lang.env
  ; mutable state : Lang.value
  ; on_event : Lang.value
  ; queue : Lang.value Queue.t
  ; operations : op_def String.Map.t
  ; source_text : string
  ; mutable current_exec : exec_ctx option
  ; mutable committed_local_effects_rev : Lang.eff list
  ; mutable halted : bool
  }

let format_runtime_error (session : session) (err : Lang.runtime_error) : string =
  Lang.format_runtime_error session.source_text err
;;

let duplicate_names (ops : op_def list) : string list =
  let counts =
    List.fold ops ~init:String.Map.empty ~f:(fun acc op ->
      Map.update acc op.name ~f:(function
        | None -> 1
        | Some n -> n + 1))
  in
  counts
  |> Map.to_alist
  |> List.filter_map ~f:(fun (name, count) -> if count > 1 then Some name else None)
;;

let operations_map (ops : op_def list) : (op_def String.Map.t, string) result =
  match duplicate_names ops with
  | [] ->
    Ok
      (List.fold ops ~init:String.Map.empty ~f:(fun acc op ->
         Map.set acc ~key:op.name ~data:op))
  | dups ->
    Error
      (Printf.sprintf
         "Duplicate moderator runtime operations: %s"
         (String.concat ~sep:", " dups))
;;

let string_of_log_level (level : log_level) : string =
  match level with
  | Debug -> "debug"
  | Info -> "info"
  | Warn -> "warn"
  | Error_level -> "error"
;;

let allow_all_phases (_phase : string) : (unit, string) result = Ok ()

let require_phases (allowed_phases : string list) (phase : string) : (unit, string) result
  =
  if List.mem allowed_phases phase ~equal:String.equal
  then Ok ()
  else
    Error (Printf.sprintf "expected one of [%s]" (String.concat ~sep:", " allowed_phases))
;;

let not_configured (name : string) : (unit, string) result =
  Error (Printf.sprintf "%s is not configured" name)
;;

let default_handlers : default_handlers =
  { on_log = (fun _session ~level:_ ~message:_ -> Ok ())
  ; on_turn_effect = (fun _session _effect -> Ok ())
  ; on_tool_moderation = (fun _session _action -> Ok ())
  ; on_tool_call = (fun _session ~name:_ ~args:_ -> Error "Tool.call is not configured")
  ; on_tool_spawn = (fun _session ~name:_ ~args:_ -> Error "Tool.spawn is not configured")
  ; on_model_call =
      (fun _session ~recipe:_ ~payload:_ -> Error "Model.call is not configured")
  ; on_model_spawn =
      (fun _session ~recipe:_ ~payload:_ -> Error "Model.spawn is not configured")
  ; on_schedule_after_ms =
      (fun _session ~delay_ms:_ ~payload:_ -> Error "Schedule.after_ms is not configured")
  ; on_schedule_cancel = (fun _session ~id:_ -> not_configured "Schedule.cancel")
  ; on_request_compaction = (fun _session -> Ok ())
  ; on_end_session = (fun _session ~reason:_ -> Ok ())
  }
;;

let expect_arity (name : string) (expected : int) (args : Lang.value list)
  : (unit, string) result
  =
  if Int.equal (List.length args) expected
  then Ok ()
  else
    Error
      (Printf.sprintf
         "%s: expected %d argument(s), got %d"
         name
         expected
         (List.length args))
;;

let expect_string_arg (name : string) (value : Lang.value) : (string, string) result =
  match value with
  | Lang.VString s -> Ok s
  | _ -> Error (Printf.sprintf "%s: expected string argument" name)
;;

let expect_int_arg (name : string) (value : Lang.value) : (int, string) result =
  match value with
  | Lang.VInt n -> Ok n
  | _ -> Error (Printf.sprintf "%s: expected int argument" name)
;;

let decode_turn_prepend_system (args : Lang.value list) : (turn_effect, string) result =
  match args with
  | [ value ] ->
    Result.map (expect_string_arg "Turn.prepend_system" value) ~f:(fun message ->
      Prepend_system message)
  | _ ->
    Error
      (Printf.sprintf
         "Turn.prepend_system: expected 1 argument(s), got %d"
         (List.length args))
;;

let decode_turn_append_message (args : Lang.value list) : (turn_effect, string) result =
  match args with
  | [ message ] -> Ok (Append_message message)
  | _ ->
    Error
      (Printf.sprintf
         "Turn.append_message: expected 1 argument(s), got %d"
         (List.length args))
;;

let decode_turn_replace_message (args : Lang.value list) : (turn_effect, string) result =
  match args with
  | [ id; message ] ->
    (match expect_string_arg "Turn.replace_message" id with
     | Error msg -> Error msg
     | Ok message_id -> Ok (Replace_message (message_id, message)))
  | _ ->
    Error
      (Printf.sprintf
         "Turn.replace_message: expected 2 argument(s), got %d"
         (List.length args))
;;

let decode_turn_delete_message (args : Lang.value list) : (turn_effect, string) result =
  match args with
  | [ id ] ->
    Result.map (expect_string_arg "Turn.delete_message" id) ~f:(fun message_id ->
      Delete_message message_id)
  | _ ->
    Error
      (Printf.sprintf
         "Turn.delete_message: expected 1 argument(s), got %d"
         (List.length args))
;;

let decode_turn_halt (args : Lang.value list) : (turn_effect, string) result =
  match args with
  | [ reason ] ->
    Result.map (expect_string_arg "Turn.halt" reason) ~f:(fun msg -> Halt msg)
  | _ ->
    Error (Printf.sprintf "Turn.halt: expected 1 argument(s), got %d" (List.length args))
;;

let decode_tool_approve (args : Lang.value list) : (tool_moderation, string) result =
  match args with
  | [] -> Ok Approve
  | _ ->
    Error
      (Printf.sprintf "Tool.approve: expected 0 argument(s), got %d" (List.length args))
;;

let decode_tool_reject (args : Lang.value list) : (tool_moderation, string) result =
  match args with
  | [ reason ] ->
    Result.map (expect_string_arg "Tool.reject" reason) ~f:(fun msg -> Reject msg)
  | _ ->
    Error
      (Printf.sprintf "Tool.reject: expected 1 argument(s), got %d" (List.length args))
;;

let decode_tool_rewrite_args (args : Lang.value list) : (tool_moderation, string) result =
  match args with
  | [ args_value ] -> Ok (Rewrite_args args_value)
  | _ ->
    Error
      (Printf.sprintf
         "Tool.rewrite_args: expected 1 argument(s), got %d"
         (List.length args))
;;

let decode_tool_redirect (args : Lang.value list) : (tool_moderation, string) result =
  match args with
  | [ name; args_value ] ->
    (match expect_string_arg "Tool.redirect" name with
     | Error msg -> Error msg
     | Ok destination -> Ok (Redirect (destination, args_value)))
  | _ ->
    Error
      (Printf.sprintf "Tool.redirect: expected 2 argument(s), got %d" (List.length args))
;;

let decode_runtime_emit (args : Lang.value list) : (local_effect, string) result =
  match args with
  | [ event ] -> Ok (Emit_internal_event event)
  | _ ->
    Error
      (Printf.sprintf "Runtime.emit: expected 1 argument(s), got %d" (List.length args))
;;

let decode_runtime_request_compaction (args : Lang.value list)
  : (local_effect, string) result
  =
  match args with
  | [] -> Ok Request_compaction
  | _ ->
    Error
      (Printf.sprintf
         "Runtime.request_compaction: expected 0 argument(s), got %d"
         (List.length args))
;;

let decode_runtime_end_session (args : Lang.value list) : (local_effect, string) result =
  match args with
  | [ reason ] ->
    Result.map (expect_string_arg "Runtime.end_session" reason) ~f:(fun msg ->
      End_session msg)
  | _ ->
    Error
      (Printf.sprintf
         "Runtime.end_session: expected 1 argument(s), got %d"
         (List.length args))
;;

let decode_local_effect (eff : Lang.eff) : (local_effect, string) result =
  match eff.op with
  | "Turn.prepend_system" ->
    Result.map (decode_turn_prepend_system eff.args) ~f:(fun turn -> Turn_effect turn)
  | "Turn.append_message" ->
    Result.map (decode_turn_append_message eff.args) ~f:(fun turn -> Turn_effect turn)
  | "Turn.replace_message" ->
    Result.map (decode_turn_replace_message eff.args) ~f:(fun turn -> Turn_effect turn)
  | "Turn.delete_message" ->
    Result.map (decode_turn_delete_message eff.args) ~f:(fun turn -> Turn_effect turn)
  | "Turn.halt" ->
    Result.map (decode_turn_halt eff.args) ~f:(fun turn -> Turn_effect turn)
  | "Tool.approve" ->
    Result.map (decode_tool_approve eff.args) ~f:(fun action ->
      Tool_moderation_effect action)
  | "Tool.reject" ->
    Result.map (decode_tool_reject eff.args) ~f:(fun action ->
      Tool_moderation_effect action)
  | "Tool.rewrite_args" ->
    Result.map (decode_tool_rewrite_args eff.args) ~f:(fun action ->
      Tool_moderation_effect action)
  | "Tool.redirect" ->
    Result.map (decode_tool_redirect eff.args) ~f:(fun action ->
      Tool_moderation_effect action)
  | "Runtime.emit" -> decode_runtime_emit eff.args
  | "Runtime.request_compaction" -> decode_runtime_request_compaction eff.args
  | "Runtime.end_session" -> decode_runtime_end_session eff.args
  | op -> Error (Printf.sprintf "Unknown moderator local effect '%s'" op)
;;

let decode_local_effects (effects : Lang.eff list) : (local_effect list, string) result =
  Result.all (List.map effects ~f:decode_local_effect)
;;

let with_nullary
      (name : string)
      (f : session -> (Lang.value, string) result)
      (session : session)
      (args : Lang.value list)
  : (Lang.value, string) result
  =
  match expect_arity name 0 args with
  | Error msg -> Error msg
  | Ok () -> f session
;;

let with_unary
      (name : string)
      (f : session -> Lang.value -> (Lang.value, string) result)
      (session : session)
      (args : Lang.value list)
  : (Lang.value, string) result
  =
  match expect_arity name 1 args with
  | Error msg -> Error msg
  | Ok () ->
    (match args with
     | [ arg ] -> f session arg
     | _ -> assert false)
;;

let with_binary
      (name : string)
      (f : session -> Lang.value -> Lang.value -> (Lang.value, string) result)
      (session : session)
      (args : Lang.value list)
  : (Lang.value, string) result
  =
  match expect_arity name 2 args with
  | Error msg -> Error msg
  | Ok () ->
    (match args with
     | [ lhs; rhs ] -> f session lhs rhs
     | _ -> assert false)
;;

let wrap_unit_result (result : (unit, string) result) : (Lang.value, string) result =
  Result.map result ~f:(fun () -> Lang.VUnit)
;;

let wrap_string_result (result : (string, string) result) : (Lang.value, string) result =
  Result.map result ~f:(fun id -> Lang.VString id)
;;

let default_operations ?(handlers = default_handlers) () : op_def list =
  let log_op (name : string) (level : log_level) : op_def =
    { name
    ; kind = Diagnostic
    ; phase_check = allow_all_phases
    ; perform =
        with_unary name (fun session message_value ->
          match expect_string_arg name message_value with
          | Error msg -> Error msg
          | Ok message -> wrap_unit_result (handlers.on_log session ~level ~message))
    }
  in
  let local_turn_op
        (name : string)
        (decode : Lang.value list -> (turn_effect, string) result)
    : op_def
    =
    { name
    ; kind = Local_transactional
    ; phase_check = allow_all_phases
    ; perform =
        (fun session args ->
          match decode args with
          | Error msg -> Error msg
          | Ok eff -> wrap_unit_result (handlers.on_turn_effect session eff))
    }
  in
  let local_tool_moderation_op
        (name : string)
        (decode : Lang.value list -> (tool_moderation, string) result)
    : op_def
    =
    { name
    ; kind = Local_transactional
    ; phase_check = allow_all_phases
    ; perform =
        (fun session args ->
          match decode args with
          | Error msg -> Error msg
          | Ok action -> wrap_unit_result (handlers.on_tool_moderation session action))
    }
  in
  [ log_op "Log.debug" Debug
  ; log_op "Log.info" Info
  ; log_op "Log.warn" Warn
  ; log_op "Log.error" Error_level
  ; local_turn_op "Turn.prepend_system" decode_turn_prepend_system
  ; local_turn_op "Turn.append_message" decode_turn_append_message
  ; local_turn_op "Turn.replace_message" decode_turn_replace_message
  ; local_turn_op "Turn.delete_message" decode_turn_delete_message
  ; local_turn_op "Turn.halt" decode_turn_halt
  ; local_tool_moderation_op "Tool.approve" decode_tool_approve
  ; local_tool_moderation_op "Tool.reject" decode_tool_reject
  ; local_tool_moderation_op "Tool.rewrite_args" decode_tool_rewrite_args
  ; local_tool_moderation_op "Tool.redirect" decode_tool_redirect
  ; { name = "Tool.call"
    ; kind = External_sync
    ; phase_check = allow_all_phases
    ; perform =
        with_binary "Tool.call" (fun session name_value args_value ->
          match expect_string_arg "Tool.call" name_value with
          | Error msg -> Error msg
          | Ok name -> handlers.on_tool_call session ~name ~args:args_value)
    }
  ; { name = "Tool.spawn"
    ; kind = External_async
    ; phase_check = allow_all_phases
    ; perform =
        with_binary "Tool.spawn" (fun session name_value args_value ->
          match expect_string_arg "Tool.spawn" name_value with
          | Error msg -> Error msg
          | Ok name ->
            wrap_string_result (handlers.on_tool_spawn session ~name ~args:args_value))
    }
  ; { name = "Model.call"
    ; kind = External_sync
    ; phase_check = allow_all_phases
    ; perform =
        with_binary "Model.call" (fun session recipe_value payload ->
          match expect_string_arg "Model.call" recipe_value with
          | Error msg -> Error msg
          | Ok recipe -> handlers.on_model_call session ~recipe ~payload)
    }
  ; { name = "Model.spawn"
    ; kind = External_async
    ; phase_check = allow_all_phases
    ; perform =
        with_binary "Model.spawn" (fun session recipe_value payload ->
          match expect_string_arg "Model.spawn" recipe_value with
          | Error msg -> Error msg
          | Ok recipe ->
            wrap_string_result (handlers.on_model_spawn session ~recipe ~payload))
    }
  ; { name = "Schedule.after_ms"
    ; kind = External_async
    ; phase_check = allow_all_phases
    ; perform =
        with_binary "Schedule.after_ms" (fun session delay_value payload ->
          match expect_int_arg "Schedule.after_ms" delay_value with
          | Error msg -> Error msg
          | Ok delay_ms ->
            if delay_ms < 0
            then Error "Schedule.after_ms: delay must be non-negative"
            else
              wrap_string_result
                (handlers.on_schedule_after_ms session ~delay_ms ~payload))
    }
  ; { name = "Schedule.cancel"
    ; kind = External_sync
    ; phase_check = allow_all_phases
    ; perform =
        with_unary "Schedule.cancel" (fun session id_value ->
          match expect_string_arg "Schedule.cancel" id_value with
          | Error msg -> Error msg
          | Ok id -> wrap_unit_result (handlers.on_schedule_cancel session ~id))
    }
  ; { name = "Runtime.emit"
    ; kind = Local_transactional
    ; phase_check = allow_all_phases
    ; perform =
        with_unary "Runtime.emit" (fun session event ->
          match session.current_exec with
          | None -> Error "Runtime.emit is only valid during active task interpretation"
          | Some exec ->
            exec.emitted_rev <- event :: exec.emitted_rev;
            Ok Lang.VUnit)
    }
  ; { name = "Runtime.request_compaction"
    ; kind = Local_transactional
    ; phase_check = allow_all_phases
    ; perform =
        with_nullary "Runtime.request_compaction" (fun session ->
          wrap_unit_result (handlers.on_request_compaction session))
    }
  ; { name = "Runtime.end_session"
    ; kind = Local_transactional
    ; phase_check = allow_all_phases
    ; perform =
        with_unary "Runtime.end_session" (fun session reason_value ->
          match expect_string_arg "Runtime.end_session" reason_value with
          | Error msg -> Error msg
          | Ok reason ->
            (match handlers.on_end_session session ~reason with
             | Error msg -> Error msg
             | Ok () ->
               (match session.current_exec with
                | None ->
                  Error
                    "Runtime.end_session is only valid during active task interpretation"
                | Some exec ->
                  exec.end_session_requested <- Some reason;
                  Ok Lang.VUnit)))
    }
  ]
;;

let default_runtime_config
      ?(surface = Builtin_surface.moderator_surface)
      ?(handlers = default_handlers)
      ()
  : runtime_config
  =
  { surface; operations = default_operations ~handlers () }
;;

let surface_name_sets (surface : Builtin_surface.surface)
  : String.Set.t * String.Set.t * String.Set.t
  =
  ( surface.globals |> List.map ~f:(fun builtin -> builtin.name) |> String.Set.of_list
  , surface.modules
    |> List.map ~f:(fun builtin_module -> builtin_module.name)
    |> String.Set.of_list
  , surface.type_aliases |> List.map ~f:(fun alias -> alias.name) |> String.Set.of_list )
;;

let ensure_surface_compatible
      ~(compiled_surface : Builtin_surface.surface)
      ~(runtime_surface : Builtin_surface.surface)
  : (unit, string) result
  =
  let compiled_globals, compiled_modules, compiled_aliases =
    surface_name_sets compiled_surface
  in
  let runtime_globals, runtime_modules, runtime_aliases =
    surface_name_sets runtime_surface
  in
  if not (Set.equal compiled_globals runtime_globals)
  then Error "Runtime surface globals do not match the surface used to compile the script"
  else if not (Set.equal compiled_modules runtime_modules)
  then Error "Runtime surface modules do not match the surface used to compile the script"
  else if not (Set.equal compiled_aliases runtime_aliases)
  then
    Error
      "Runtime surface type aliases do not match the surface used to compile the script"
  else Ok ()
;;

let expect_callable (name : string) (value : Lang.value) : (Lang.value, string) result =
  match value with
  | Lang.VClosure _ | Lang.VBuiltin _ -> Ok value
  | _ -> Error (Printf.sprintf "Entrypoint '%s' is not callable" name)
;;

let compile_script ?(surface = Builtin_surface.moderator_surface) ~(source : string) ()
  : (compiled_script, string) result
  =
  match Parse.parse_program source with
  | Error diagnostic -> Error (Parse.format_diagnostic source diagnostic)
  | Ok program ->
    (match Typechecker.check_program_with_surface surface program with
     | Error diagnostic -> Error (Typechecker.format_diagnostic source diagnostic)
     | Ok checked ->
       let resolved = Resolver.resolve_checked_program checked program in
       Ok { surface; program; checked; resolved; source_text = source })
;;

let instantiate_session
      (config : runtime_config)
      (compiled : compiled_script)
      ~(entrypoints : compiled_entrypoints)
  : (session, string) result
  =
  match
    ensure_surface_compatible
      ~compiled_surface:compiled.surface
      ~runtime_surface:config.surface
  with
  | Error msg -> Error msg
  | Ok () ->
    (match operations_map config.operations with
     | Error msg -> Error msg
     | Ok operations ->
       let env = Builtin_modules.create_env_with_surface config.surface in
       (try
          Eval.eval_program env compiled.resolved;
          match Lang.find_var env entrypoints.initial_state_name with
          | None ->
            Error
              (Printf.sprintf
                 "Missing initial_state binding '%s'"
                 entrypoints.initial_state_name)
          | Some initial_state ->
            (match Lang.find_var env entrypoints.on_event_name with
             | None ->
               Error
                 (Printf.sprintf
                    "Missing on_event binding '%s'"
                    entrypoints.on_event_name)
             | Some on_event ->
               (match expect_callable entrypoints.on_event_name on_event with
                | Error msg -> Error msg
                | Ok on_event ->
                  Ok
                    { env
                    ; state = initial_state
                    ; on_event
                    ; queue = Queue.create ()
                    ; operations
                    ; source_text = compiled.source_text
                    ; current_exec = None
                    ; committed_local_effects_rev = []
                    ; halted = false
                    }))
        with
        | Lang.Runtime_error err ->
          Error (Lang.format_runtime_error compiled.source_text err)))
;;

let current_state (session : session) : Lang.value = session.state

let current_phase (session : session) : string option =
  Option.map session.current_exec ~f:(fun exec -> exec.phase)
;;

let pending_local_effects (session : session) : Lang.eff list =
  match session.current_exec with
  | None -> []
  | Some exec -> List.rev exec.local_effects_rev
;;

let committed_local_effects (session : session) : Lang.eff list =
  List.rev session.committed_local_effects_rev
;;

let queued_events (session : session) : Lang.value list = Queue.to_list session.queue

let take_queued_event (session : session) : Lang.value option =
  Queue.dequeue session.queue
;;

let is_halted (session : session) : bool = session.halted

let restore
      (session : session)
      ~(state : Lang.value)
      ~(queued_events : Lang.value list)
      ~(halted : bool)
  : (unit, string) result
  =
  match session.current_exec with
  | Some _ -> Error "Cannot restore moderator runtime during active task interpretation"
  | None ->
    session.state <- state;
    Queue.clear session.queue;
    List.iter queued_events ~f:(fun event -> Queue.enqueue session.queue event);
    session.committed_local_effects_rev <- [];
    session.halted <- halted;
    Ok ()
;;

let with_current_exec
      (session : session)
      ~(name : string)
      ~(f : exec_ctx -> (unit, string) result)
  : (unit, string) result
  =
  match session.current_exec with
  | None ->
    Error (Printf.sprintf "%s is only valid during active task interpretation" name)
  | Some exec -> f exec
;;

let emit_internal_event (session : session) (event : Lang.value) : (unit, string) result =
  with_current_exec session ~name:"emit_internal_event" ~f:(fun exec ->
    exec.emitted_rev <- event :: exec.emitted_rev;
    Ok ())
;;

let request_session_end (session : session) ~(reason : string) : (unit, string) result =
  with_current_exec session ~name:"request_session_end" ~f:(fun exec ->
    exec.end_session_requested <- Some reason;
    Ok ())
;;

let phase_of_context (context : Lang.value) : (string, string) result =
  match context with
  | Lang.VRecord fields ->
    (match Value_codec.expect_record_field "context" fields "phase" with
     | Error msg -> Error msg
     | Ok phase_value ->
       (match Value_codec.expect_string "context.phase" phase_value with
        | Ok phase -> Ok phase
        | Error msg -> Error msg))
  | _ -> Error "context: expected record with string field 'phase'"
;;

let expect_task_value (value : Lang.value) : (Lang.task, string) result =
  match value with
  | Lang.VTask task -> Ok task
  | _ -> Error "Expected task result from on_event"
;;

let continuation_task_result
      (session : session)
      (fn : Lang.value)
      (args : Lang.value list)
  : (Lang.task, string) result
  =
  match Eval.apply_value_result fn args with
  | Ok value -> expect_task_value value
  | Error err -> Error (format_runtime_error session err)
;;

let continuation_value_result
      (session : session)
      (fn : Lang.value)
      (args : Lang.value list)
  : (Lang.value, string) result
  =
  match Eval.apply_value_result fn args with
  | Ok value -> Ok value
  | Error err -> Error (format_runtime_error session err)
;;

let find_operation (session : session) (name : string) : (op_def, string) result =
  match Map.find session.operations name with
  | Some op -> Ok op
  | None -> Error (Printf.sprintf "Unknown task operation '%s'" name)
;;

let dispatch_effect
      (session : session)
      (exec : exec_ctx)
      ~(spawned : bool)
      (eff : Lang.eff)
  : (Lang.value, string) result
  =
  match find_operation session eff.op with
  | Error msg -> Error msg
  | Ok op ->
    (match op.phase_check exec.phase with
     | Error msg ->
       Error
         (Printf.sprintf
            "Operation '%s' is invalid in phase '%s': %s"
            op.name
            exec.phase
            msg)
     | Ok () ->
       (match spawned, op.kind with
        | true, External_async -> op.perform session eff.args
        | true, _ -> Error (Printf.sprintf "Operation '%s' is not spawnable" op.name)
        | false, External_async ->
          Error (Printf.sprintf "Operation '%s' must be spawned" op.name)
        | false, Local_transactional ->
          (match op.perform session eff.args with
           | Error msg -> Error msg
           | Ok value ->
             exec.local_effects_rev <- eff :: exec.local_effects_rev;
             Ok value)
        | false, (External_sync | Diagnostic) -> op.perform session eff.args))
;;

let rec interpret_task (session : session) (exec : exec_ctx) (task : Lang.task)
  : (Lang.value, string) result
  =
  match task with
  | Lang.TPure value -> Ok value
  | Lang.TFail msg -> Error msg
  | Lang.TBind (task, k) ->
    (match interpret_task session exec task with
     | Error msg -> Error msg
     | Ok value ->
       (match continuation_task_result session k [ value ] with
        | Error msg -> Error msg
        | Ok next_task -> interpret_task session exec next_task))
  | Lang.TMap (task, f) ->
    (match interpret_task session exec task with
     | Error msg -> Error msg
     | Ok value -> continuation_value_result session f [ value ])
  | Lang.TCatch (task, h) ->
    let saved_local_effects = exec.local_effects_rev in
    let saved_emitted = exec.emitted_rev in
    let saved_end_session_requested = exec.end_session_requested in
    (match interpret_task session exec task with
     | Ok value -> Ok value
     | Error msg ->
       exec.local_effects_rev <- saved_local_effects;
       exec.emitted_rev <- saved_emitted;
       exec.end_session_requested <- saved_end_session_requested;
       (match continuation_task_result session h [ Lang.VString msg ] with
        | Error err -> Error err
        | Ok next_task -> interpret_task session exec next_task))
  | Lang.TPerform eff -> dispatch_effect session exec ~spawned:false eff
  | Lang.TSpawn eff -> dispatch_effect session exec ~spawned:true eff
;;

let handle_event (session : session) ~(context : Lang.value) ~(event : Lang.value)
  : (unit, string) result
  =
  if session.halted
  then Error "Session has ended"
  else (
    match phase_of_context context with
    | Error msg -> Error msg
    | Ok phase ->
      let exec =
        { phase; local_effects_rev = []; emitted_rev = []; end_session_requested = None }
      in
      let old_state = session.state in
      session.current_exec <- Some exec;
      let result =
        match Eval.apply_value_result session.on_event [ context; old_state; event ] with
        | Error err -> Error (format_runtime_error session err)
        | Ok value ->
          (match expect_task_value value with
           | Error msg -> Error msg
           | Ok task -> interpret_task session exec task)
      in
      session.current_exec <- None;
      (match result with
       | Error msg -> Error msg
       | Ok new_state ->
         session.state <- new_state;
         session.committed_local_effects_rev
         <- exec.local_effects_rev @ session.committed_local_effects_rev;
         List.iter (List.rev exec.emitted_rev) ~f:(fun queued_event ->
           Queue.enqueue session.queue queued_event);
         if Option.is_some exec.end_session_requested then session.halted <- true;
         Ok ()))
;;
