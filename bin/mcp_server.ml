open! Core

module JT = Mcp_types

(* -------------------------------------------------------------------------- *)
(* CLI flags                                                                  *)
(* -------------------------------------------------------------------------- *)

let http_port_ref : int option ref = ref None

let () =
  let speclist : (string * Arg.spec * string) list =
    [ "--http"
    , Arg.Int (fun p -> http_port_ref := Some p)
    , "Run Streamable HTTP server on the given port (instead of stdio)" ]
  in
  Arg.parse speclist (fun _ -> ()) "mcp_server [--http PORT]"


let setup_tool_echo (core : Mcp_server_core.t) : unit =
  let input_schema =
    Jsonaf.of_string
      "{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}},\"required\":[\"text\"]}"
  in
  let spec : JT.Tool.t = { name = "echo"; description = Some "Echo back text"; input_schema } in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs -> (
        match List.Assoc.find kvs ~equal:String.equal "text" with
        | Some (`String s) -> Ok (`String s)
        | _ -> Error "missing field text or not string")
    | _ -> Error "arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler

(* --------------------------------------------------------------------- *)
(* Built-in ocamlgpt tools exposed over MCP                                *)
(* --------------------------------------------------------------------- *)

let register_builtin_apply_patch (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  (* Pull metadata from the existing [Definitions] module so that we keep a
     single source of truth. *)
  let module Def = Definitions.Apply_patch in
  let spec : JT.Tool.t =
    { name = "apply_patch"; description = Def.description; input_schema = Def.parameters }
  in
  (* Re-use the already implemented helper residing in [Functions]. *)
  let gpt_fn = Functions.apply_patch ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs -> (
        match List.Assoc.find kvs ~equal:String.equal "input" with
        | Some (`String patch_text) ->
            let input_json = `Object [ "input", `String patch_text ] in
            Ok (`String (gpt_fn.run (Jsonaf.to_string input_json)))
        | _ -> Error "apply_patch expects field 'input' (string)")
    | _ -> Error "apply_patch arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler

let register_builtin_read_dir (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  let module Def = Definitions.Read_directory in
  let spec : JT.Tool.t =
    { name = "read_dir"; description = Def.description; input_schema = Def.parameters }
  in
  let gpt_fn = Functions.read_dir ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs -> (
        match List.Assoc.find kvs ~equal:String.equal "path" with
        | Some (`String path) ->
            let input_json = `Object [ "path", `String path ] in
            Ok (`String (gpt_fn.run (Jsonaf.to_string input_json)))
        | _ -> Error "read_dir expects field 'path' (string)")
    | _ -> Error "read_dir arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler

let register_builtin_get_contents (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  (* The existing definition is named [Get_contents] and its [name] is
     "read_file", but on the chatmd side the built-in is exposed under the
     friendlier name [get_contents].  We keep that external name for
     consistency while still re-using the JSON schema from the definition. *)
  let module Def = Definitions.Get_contents in
  let spec : JT.Tool.t =
    { name = "get_contents"; description = Def.description; input_schema = Def.parameters }
  in
  let gpt_fn = Functions.get_contents ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs -> (
        match List.Assoc.find kvs ~equal:String.equal "file" with
        | Some (`String file_path) ->
            let input_json = `Object [ "file", `String file_path ] in
            Ok (`String (gpt_fn.run (Jsonaf.to_string input_json)))
        | _ -> Error "get_contents expects field 'file' (string)")
    | _ -> Error "get_contents arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler

let run_stdio ~core : unit =
  let rec loop ic oc =
    match In_channel.input_line ic with
    | None -> ()
    | Some line -> (
        try
          let json = Jsonaf.of_string line in
          let responses = Mcp_server_router.handle ~core json in
          List.iter responses ~f:(fun j ->
              Out_channel.output_string oc (Jsonaf.to_string j);
              Out_channel.output_char oc '\n');
          Out_channel.flush oc;
          loop ic oc
        with exn ->
          (* On parse error just ignore line *)
          eprintf "[mcp_server] Error processing line: %s\n" (Exn.to_string exn);
          loop ic oc)
  in
  loop In_channel.stdin Out_channel.stdout

let () =
  (* For Phase-1 we ignore CLI flags.  Future milestones will add --http etc. *)
  let core = Mcp_server_core.create () in
  (* For built-in tools we need an [Eio.Path.t] representing the current
     working directory.  We therefore register them inside the main Eio
     fibre where we have access to [env#cwd]. *)
  Eio_main.run (fun env ->
      let dir = env#cwd in
      (* Register demo echo plus built-in functions. *)
      setup_tool_echo core;
      register_builtin_apply_patch core ~dir;
      register_builtin_read_dir core ~dir;
      register_builtin_get_contents core ~dir;
      (* -----------------------------------------------------------------
         Prompt folder scanning – every *.chatmd file is registered as both a
         prompt and an agent-backed tool.  The folder can be specified via the
         env var [MCP_PROMPTS_DIR].  If unset we look for "./prompts" relative
         to the current working directory and silently ignore missing dirs. *)

      let prompts_dir =
        match Sys.getenv "MCP_PROMPTS_DIR" with
        | Some p -> Eio.Path.(dir / p)
        | None ->
          let default = Eio.Path.(dir / "prompts") in
          default
      in
      (* -----------------------------------------------------------------
         Prompt scanning & lightweight hot-reload
         -----------------------------------------------------------------

         We maintain a hash-set of filenames that have already been processed.
         A background fibre re-scans the [prompts_dir] directory every few
         seconds and registers any brand-new *.chatmd files.  Re-registering
         an existing name is harmless – it merely overwrites the previous
         entry and still triggers the [list_changed] hooks so connected
         clients invalidate their cache.  We do **not** attempt to detect
         deletions at this stage; that can be added later together with a
         proper inotify/FSEvents watcher. *)

      let processed : (string, unit) Hashtbl.t = Hashtbl.create (module String) in

      let scan_prompts () =
        (try
           Eio.Path.read_dir prompts_dir
           |> List.filter ~f:(fun fname -> Filename.check_suffix fname ".chatmd")
           |> List.iter ~f:(fun fname ->
                  if not (Hashtbl.mem processed fname) then (
                    let file_path = Eio.Path.(prompts_dir / fname) in
                    match Or_error.try_with (fun () ->
                        Mcp_prompt_agent.of_chatmd_file ~env ~path:file_path)
                    with
                    | Error err ->
                        eprintf
                          "[mcp_server] Failed to load prompt %s: %s\n"
                          fname (Error.to_string_hum err)
                    | Ok (tool, handler, prompt) ->
                        Hashtbl.set processed ~key:fname ~data:();
                        Mcp_server_core.register_tool core tool handler;
                        Mcp_server_core.register_prompt core ~name:tool.name prompt))
         with _exn -> ());
        ()
      in

      (* Initial scan so the first batch of prompts is available immediately. *)
      scan_prompts ();

      (* Background hot-reload: disabled for now.
         The code above lays the groundwork by keeping a [processed] table.

         We now run a very lightweight polling fibre that re-scans the prompt
         directory every 10 seconds.  This is a pragmatic interim solution
         until Eio exposes a platform-independent file-watcher.  The work is
         cheap – we only stat the directory and `Hashtbl.mem` prevents
         duplicate work, so the overhead is negligible.  If at some point a
         real watcher becomes available we can drop this polling loop without
         touching other parts of the server. *)
      let start_polling_prompts ~sw () =
        let rec loop () =
          scan_prompts ();
          (* Wait a bit before the next scan. *)
          Eio.Time.sleep env#clock 10.0;
          loop ()
        in
        Eio.Fiber.fork ~sw loop
      in

      (* ----------------------------------------------------------------- *)
      (match !http_port_ref with
       | Some port ->
           Eio.Switch.run (fun sw ->
               (* Poller lives under the same switch so it terminates when the
                  HTTP server shuts down *)
               start_polling_prompts ~sw ();
               (* Launch Streamable HTTP server and block forever *)
               Mcp_server_http.run ~env ~core ~port)
       | None ->
           (* stdio mode – we still spawn the polling fibre so that long-lived
              sessions also benefit from newly added prompts.  Since the stdio
              loop is blocking we need a dedicated switch. *)
           Eio.Switch.run (fun sw ->
               start_polling_prompts ~sw ();
               run_stdio ~core)))

