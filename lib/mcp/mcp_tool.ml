(**-----------------------------------------------------------------------
  Remote MCP tool → Ochat_function wrapper

  This helper turns a tool discovered via `tools/list` on an MCP server
  into a [Ochat_function.t] that can be exposed to the chat runtime.  It
  hides the JSON-RPC plumbing and converts the wire-level
  [Mcp_types.Tool_result.t] into a simple string result that fits the
  existing Chat_response driver.
------------------------------------------------------------------------*)

open Core
module Client = Mcp_client
module Tool = Mcp_types.Tool
module Result_ = Mcp_types.Tool_result
module Jsonaf = Jsonaf_ext

(** Convert one piece of tool output into plain text.
    * [Text]    – returned unchanged.
    * [Json]
    * [Rich]    – pretty-printed using {!Jsonaf_ext.to_string}. *)
let string_of_content (c : Result_.content) : string =
  match c with
  | Result_.Text s -> s
  | Result_.Json j | Result_.Rich j -> Jsonaf.to_string j
;;

(** Flatten a multi-part {!Mcp_types.Tool_result.t} into a single newline-
    separated string. *)
let string_of_result (r : Result_.t) : string =
  let parts = List.map r.content ~f:string_of_content in
  String.concat ~sep:"\n" parts
;;

(** [ochat_function_of_remote_tool ~sw ~client ~strict tool] converts the remote
    description [tool] into a fully-callable {!Ochat_function.t}.

    Parameters:
    • [sw] – retained for API compatibility. The owning host controls the
      connected client's lifetime and notification routing.
    • [client] – already-connected {!Mcp_client.t}.  The caller is
      responsible for keeping the client alive for at least as long as the
      returned function may be invoked.
    • [strict] – forwarded as provider metadata, not local schema validation.

    Result: a {!Ochat_function.t} whose name, description and JSON-schema are
    copied verbatim from the remote declaration and whose implementation
    performs a synchronous [`tools/call`] RPC.

    Example – wrapping the server-side *echo* tool and invoking it:

    {[
      let () =
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
          let client =
            Mcp_client.connect ~sw ~env "stdio:python3 -m mcp.reference_server"
          in
          let tools = Mcp_client.list_tools client |> Result.ok_or_failwith in
          let echo_desc = List.find_exn tools ~f:(fun t -> String.equal t.name "echo") in
          let echo_fn = ochat_function_of_remote_tool ~sw ~client ~strict:true echo_desc in
          let args = `Assoc [ "text", `String "hi" ] in
          Printf.printf "%s\n" (Ochat_function.call echo_fn args)
    ]}

    The call prints "hi". The wrapper does not consume the client's shared
    notification queue; its owner must route catalog invalidations. *)
let ochat_function_of_remote_tool ~sw:(_ : Eio.Switch.t) ~client ~strict (tool : Tool.t)
  : Ochat_function.t
  =
  let module Def = struct
    type input = Jsonaf.t

    let type_ = "function"
    let name = tool.name
    let description = tool.description
    let parameters = tool.input_schema

    (* The OpenAI function-call interface delivers the raw JSON string
       of the arguments.  We simply parse it into a Jsonaf value and
       pass it verbatim to the MCP `tools/call` method. *)
    let input_of_string s = Jsonaf.of_string s
  end
  in
  (* The function that will be called by the chat runtime. *)
  (* It takes a JSON object as input, calls the MCP server, and returns
     a string result. *)
  let run (args : Jsonaf.t) : Openai.Responses.Tool_output.Output.t =
    match Client.call_tool client ~name:tool.name ~arguments:args with
    | Ok res -> Openai.Responses.Tool_output.Output.Text (string_of_result res)
    | Error msg -> Openai.Responses.Tool_output.Output.Text msg
  in
  Ochat_function.create_function (module Def) ~strict run
;;

(*---------------------------------------------------------------------*)
