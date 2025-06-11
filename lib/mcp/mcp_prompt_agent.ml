open! Core

module CM = Prompt.Chat_markdown
module JT = Mcp_types

let input_schema : Jsonaf.t =
  Jsonaf.of_string
    {|{
  "type": "object",
  "properties": {"input": {"type": "string"}},
  "required": ["input"]
}|}

(* Build a [CM.content_item] representing a plain text user message. *)
let content_item_of_input (text : string) : CM.content_item =
  let basic : CM.basic_content_item =
    { type_ = "text"
    ; text = Some text
    ; image_url = None
    ; document_url = None
    ; is_local = false
    ; cleanup_html = false
    }
  in
  CM.Basic basic

let of_chatmd_file
    ~(env : Eio_unix.Stdenv.base)
    ~(core : Mcp_server_core.t)
    ~(path : _ Eio.Path.t)
  : JT.Tool.t * Mcp_server_core.tool_handler * Mcp_server_core.prompt
  =
  (* ------------------------------------------------------------------ *)
  let filename =
    match Eio.Path.split path with
    | None -> "prompt"
    | Some (_, base) -> base
  in
  let name = Filename.chop_extension filename in
  let prompt_xml = Eio.Path.load path in

  (* For now we don’t attempt to extract a description from the XML.  A more
     sophisticated implementation could parse the <system> or first comment
     lines. *)
  let prompt : Mcp_server_core.prompt =
    { description = None; messages = `String prompt_xml }
  in

  (* ------------------------------------------------------------------ *)
  let tool_spec : JT.Tool.t =
    { name
    ; description = Some "ChatMD agent prompt"
    ; input_schema
    }
  in

  (* Handler that executes the prompt via Chat_response.Driver.run_agent. *)
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    (* Generate a short random progress token so that clients can correlate
       progress updates. *)
    let progress_token =
      let hex = "0123456789abcdef" in
      String.init 8 ~f:(fun _ -> hex.[Random.int 16])
    in
    (* Helper to send a progress update. *)
    let send_progress ~progress ?total ?message () =
      let payload : Mcp_server_core.progress_payload =
        { progress_token; progress; total; message }
      in
      Mcp_server_core.notify_progress core payload
    in
    match args with
    | `Object kvs -> (
        match List.Assoc.find kvs ~equal:String.equal "input" with
        | Some (`String user_input) -> (
            try
              send_progress ~progress:0.0 ?total:None ?message:(Some "Starting agent") ();
              let result =
                Eio.Switch.run @@ fun _sw ->
                let cache = Chat_response.Cache.create ~max_size:256 () in
                let ctx = Chat_response.Ctx.create ~env ~dir:env#cwd ~cache in
                Chat_response.Driver.run_agent ~ctx prompt_xml [ content_item_of_input user_input ]
              in
              send_progress ~progress:1.0 ?total:None ?message:(Some "Completed") ();
              Ok (`String result)
            with exn ->
              send_progress ~progress:1.0 ?total:None ?message:(Some "Failed") ();
              Error (Printf.sprintf "Agent execution failed: %s" (Exn.to_string exn)))
        | _ -> Error "Missing field 'input' (string)")
    | _ -> Error "arguments must be object"
  in

  tool_spec, handler, prompt

