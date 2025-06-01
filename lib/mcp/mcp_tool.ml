(*-----------------------------------------------------------------------
  Remote MCP tool → Gpt_function wrapper

  This helper turns a tool discovered via `tools/list` on an MCP server
  into a [Gpt_function.t] that can be exposed to the chat runtime.  It
  hides the JSON-RPC plumbing and converts the wire-level
  [Mcp_types.Tool_result.t] into a simple string result that fits the
  existing Chat_response driver.
------------------------------------------------------------------------*)

open Core
module Client = Mcp_client
module Tool = Mcp_types.Tool
module Result_ = Mcp_types.Tool_result
module Jsonaf = Jsonaf_ext

let string_of_content (c : Result_.content) : string =
  match c with
  | Result_.Text s -> s
  | Result_.Json j | Result_.Rich j -> Jsonaf.to_string j
;;

let string_of_result (r : Result_.t) : string =
  let parts = List.map r.content ~f:string_of_content in
  String.concat ~sep:"\n" parts
;;

(* Given a live [Mcp_client.t] (kept around by the caller for the
   lifetime of the wrapped function) and a [Tool.t] descriptor, produce
   a [Gpt_function.t]. *)
let gpt_function_of_remote_tool
      ~(env : < process_mgr : [> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ; .. >)
      ~(uri : string)
      (tool : Tool.t)
  : Gpt_function.t
  =
  let module Def = struct
    type input = Jsonaf.t

    let name = tool.name
    let description = tool.description
    let parameters = tool.input_schema

    (* The OpenAI function-call interface delivers the raw JSON string
       of the arguments.  We simply parse it into a Jsonaf value and
       pass it verbatim to the MCP `tools/call` method. *)
    let input_of_string s = Jsonaf.of_string s
  end
  in
  let run (args : Jsonaf.t) : string =
    (* Connect on-demand, run the tool, then close. *)
    Eio.Switch.run (fun sw ->
      let client = Client.connect ~sw ~env ~uri in
      match Client.call_tool client ~name:tool.name ~arguments:args with
      | Ok res -> string_of_result res
      | Error msg -> msg)
  in
  Gpt_function.create_function (module Def) run
;;

(*---------------------------------------------------------------------*)
