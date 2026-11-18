open Core
module Res = Openai.Responses

module Kind = struct
  type t =
    | Function
    | Custom
end

let is_builtin_fork implementation = phys_equal implementation Functions.fork
let fork_custom_error = "Error: [fork] cannot be invoked as a custom tool call."

let function_call_output ~call_id ~output : Res.Function_call_output.t =
  { output; call_id; _type = "function_call_output"; id = None; status = None }
;;

let custom_tool_call_output ~call_id ~output : Res.Custom_tool_call_output.t =
  { output; call_id; _type = "custom_tool_call_output"; id = None }
;;

let output_item ~kind ~call_id ~output : Res.Item.t =
  match kind with
  | Kind.Function -> Res.Item.Function_call_output (function_call_output ~call_id ~output)
  | Kind.Custom ->
    Res.Item.Custom_tool_call_output (custom_tool_call_output ~call_id ~output)
;;

let call_item ~kind ~name ~payload ~call_id ~id : Res.Item.t =
  match kind with
  | Kind.Function ->
    Res.Item.Function_call
      { name; arguments = payload; call_id; _type = "function_call"; id; status = None }
  | Kind.Custom ->
    Res.Item.Custom_tool_call
      { name; input = payload; call_id; _type = "custom_tool_call"; id }
;;

module Native = struct
  type t =
    { implementation : Ochat_function.t
    ; prepare_invocation : Ochat_function.Invocation.t -> Ochat_function.Invocation.t
    }

  let create implementation ~prepare_invocation = { implementation; prepare_invocation }

  let run_with t runner ~invocation payload =
    runner ~invocation:(t.prepare_invocation invocation) payload
  ;;

  let run t = run_with t t.implementation.run_with_progress
end

let run_tool
      ~kind
      ~name
      ~payload
      ~call_id
      ~tool_tbl
      ~on_fork
      ?native
      ?runner
      ?before_effect
      ?inference_parent
      ?on_tool_execution
      ?on_execution_event
      ()
  =
  let implementation, builtin_fork =
    match native, runner with
    | Some _, Some _ -> invalid_arg "Tool_call.run_tool: multiple selected bindings"
    | Some native, None ->
      ( native.Native.implementation.run_with_progress
      , is_builtin_fork native.implementation )
    | None, selected ->
      let runner =
        Option.value_or_thunk selected ~default:(fun () ->
          (* Standalone drivers historically own fork even without a tool-table
             entry. An explicit replacement always takes precedence. *)
          if String.equal name "fork"
          then
            Option.value
              (Hashtbl.find tool_tbl name)
              ~default:Functions.fork.run_with_progress
          else Hashtbl.find_exn tool_tbl name)
      in
      runner, phys_equal runner Functions.fork.run_with_progress
  in
  let runner =
    match kind, String.equal name "fork" && builtin_fork with
    | Kind.Custom, true ->
      fun ~invocation:_ _ -> Res.Tool_output.Output.Text fork_custom_error
    | Kind.Function, true ->
      (match on_fork with
       | None ->
         fun ~invocation:_ _ ->
           Res.Tool_output.Output.Text
             "Error: [fork] is missing a handler for function-call execution."
       | Some on_fork ->
         fun ~invocation arguments -> on_fork ~invocation ~call_id ~arguments)
    | (Kind.Function | Kind.Custom), false -> implementation
  in
  let runner =
    Option.value_map native ~default:runner ~f:(fun native ->
      Native.run_with native runner)
  in
  let kind =
    match kind with
    | Kind.Function -> `Function
    | Kind.Custom -> `Custom
  in
  Tool_executor.run
    ~kind
    ~call_id
    ~name
    ~payload
    ~runner
    ?before_effect
    ?inference_parent
    ?on_tool_execution
    ?on_execution_event
    ()
;;
