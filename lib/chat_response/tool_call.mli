module Res = Openai.Responses

module Kind : sig
  type t =
    | Function
    | Custom
end

val fork_custom_error : string

val run_tool
  :  kind:Kind.t
  -> name:string
  -> payload:string
  -> call_id:string
  -> tool_tbl:(string, string -> Res.Tool_output.Output.t) Base.Hashtbl.t
  -> on_fork:(call_id:string -> arguments:string -> Res.Tool_output.Output.t) option
  -> Openai.Responses.Tool_output.Output.t

val call_item
  :  kind:Kind.t
  -> name:string
  -> payload:string
  -> call_id:string
  -> id:string option
  -> Res.Item.t

val function_call_output
  :  call_id:string
  -> output:Res.Tool_output.Output.t
  -> Res.Function_call_output.t

val custom_tool_call_output
  :  call_id:string
  -> output:Res.Tool_output.Output.t
  -> Res.Custom_tool_call_output.t

val output_item
  :  kind:Kind.t
  -> call_id:string
  -> output:Res.Tool_output.Output.t
  -> Res.Item.t
