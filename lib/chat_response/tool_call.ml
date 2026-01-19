open Core
module Res = Openai.Responses

module Kind = struct
  type t =
    | Function
    | Custom
end

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

let run_tool ~kind ~name ~payload ~call_id ~tool_tbl ~on_fork =
  match kind, String.equal name "fork" with
  | Kind.Custom, true -> Res.Tool_output.Output.Text fork_custom_error
  | Kind.Function, true ->
    (match on_fork with
     | None ->
       Res.Tool_output.Output.Text
         "Error: [fork] is missing a handler for function-call execution."
     | Some on_fork -> on_fork ~call_id ~arguments:payload)
  | (Kind.Function | Kind.Custom), false ->
    let fn = Hashtbl.find_exn tool_tbl name in
    fn payload
;;
