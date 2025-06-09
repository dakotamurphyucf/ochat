(*-----------------------------------------------------------------------
  Remote MCP tool → Gpt_function wrapper

  This helper turns a tool discovered via `tools/list` on an MCP server
  into a [Gpt_function.t] that can be exposed to the chat runtime.  It
  hides the JSON-RPC plumbing and converts the wire-level
  [Mcp_types.Tool_result.t] into a simple string result that fits the
  existing Chat_response driver.
------------------------------------------------------------------------*)

open Core
open Eio
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
let gpt_function_of_remote_tool ~sw ~client ~strict (tool : Tool.t) : Gpt_function.t =
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
  (* Set up a daemon that listens for notifications from the MCP server
     and prints them to stdout. This is useful for debugging and
     monitoring tool calls. *)
  (* Note: This is a simple example; in production, you might want to
     handle notifications more robustly, e.g., by logging them or
     processing them in some way. *)
  let notifications = Client.notifications client in
  Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      let json = Eio.Stream.take notifications in
      print_endline (Jsonaf.to_string @@ Mcp_types.Jsonrpc.jsonaf_of_notification json);
      loop ()
    in
    let _ = loop () in
    `Stop_daemon);
  (* The function that will be called by the chat runtime. *)
  (* It takes a JSON object as input, calls the MCP server, and returns
     a string result. *)
  let run (args : Jsonaf.t) : string =
    match Client.call_tool client ~name:tool.name ~arguments:args with
    | Ok res -> string_of_result res
    | Error msg -> msg
  in
  Gpt_function.create_function (module Def) ~strict run
;;

(*---------------------------------------------------------------------*)
