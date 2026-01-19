(** MCP server – command-line wrapper around {!module:Mcp_server_core}.

    The binary initialises a fresh in-memory registry, registers a handful
    of *built-in* tools (see list below) and then exposes the registry either
    over **stdio** (default) or **HTTP** when the [--http] flag is supplied.

    {2 Built-in tools}

    | Name | Purpose |
    |------|---------|
    | ["echo"] | Trivial helper that returns the supplied text verbatim. |
    | ["apply_patch"] | Apply a textual V4A diff/patch to the workspace. |
    | ["read_dir"] | List the contents of a directory. |
    | ["get_contents"] | Read a file and return its contents. |
    | ["meta_refine"] | Refine a *meta-prompt* using LLM-backed heuristics. |
    | ["webpage_to_markdown"] | Download a web page and convert it to Markdown. |

    In addition, every *.chatmd* file discovered in the prompts directory is
    registered twice: once as a *prompt* (accessible via
    [`prompts/*`](#)) and once as a *tool* that exposes the same prompt under
    [`tools/call`].

    Additional *.chatmd* files located in the prompts directory are scanned
    at start-up and on a lightweight polling loop, turning each prompt into
    both a user-visible entry and an agent-backed tool.

    {1 CLI flags}

    • [--http PORT] – bind a Streamable HTTP endpoint on [PORT] instead of
      stdio.  The service supports JSON-RPC 2.0 over HTTP POST and
      Server-Sent Events for push notifications.  See
      {!module:Mcp_server_http} for details.

    • [--require-auth] – enable OAuth 2.1 Bearer-token validation.  When the
      flag is omitted the server skips authentication (useful during local
      development and automated tests).

    • [--client-id], [--client-secret] – static client credentials accepted
      by the token endpoint when [--require-auth] is active.

    {1 Usage}

    {[
      # run in stdio mode (useful when the parent process communicates over pipes)
      dune exec bin/mcp_server.exe

      # expose an HTTP API on port 8080
      dune exec bin/mcp_server.exe --http 8080
    ]}
*)

open Core

(* Seed the global PRNG once at start-up so that session identifiers and other
   random values are not predictable across server restarts.  We perform the
   call here (rather than in library code) because test runners that rely on
   deterministic output forbid [Random.self_init] inside inline tests. *)
let () = Random.self_init ()

module JT = Mcp_types

(* -------------------------------------------------------------------------- *)
(* CLI flags                                                                  *)
(* -------------------------------------------------------------------------- *)

let http_port_ref : int option ref = ref None
let require_auth_ref = ref false
let client_id_ref : string option ref = ref None
let client_secret_ref : string option ref = ref None

let () =
  let speclist : (string * Arg.spec * string) list =
    [ ( "--http"
      , Arg.Int (fun p -> http_port_ref := Some p)
      , "Run Streamable HTTP server on the given port (instead of stdio)" )
    ; ( "--require-auth"
      , Arg.Set require_auth_ref
      , "Enable minimal OAuth 2.1 auth (client-credentials)" )
    ; ( "--client-id"
      , Arg.String (fun s -> client_id_ref := Some s)
      , "Static OAuth client_id accepted by the token endpoint" )
    ; ( "--client-secret"
      , Arg.String (fun s -> client_secret_ref := Some s)
      , "Static OAuth client_secret accepted by the token endpoint" )
    ]
  in
  Arg.parse speclist (fun _ -> ()) "mcp_server [--http PORT]"
;;

(** [setup_tool_echo core] registers a trivial tool named ["echo"].

    The tool specification declares a single required parameter
    {[ {"text" : string} ]}.  At runtime the handler returns the same
    string wrapped in a JSON value.

    This helper is intended purely as a smoke-test for the
    request/response plumbing and as a reference implementation when
    creating new tools.

    @param core  Registry instance returned by
      {!Mcp_server_core.create}.  The function mutates the registry in
      place.

    @raise Invalid_argument  If a subsequent attempt is made to
      register another tool under the name ["echo"].  The exception is
      raised by {!Mcp_server_core.register_tool}.  *)
let setup_tool_echo (core : Mcp_server_core.t) : unit =
  let input_schema =
    Jsonaf.of_string
      "{\"type\":\"object\",\"properties\":{\"text\":{\"type\":\"string\"}},\"required\":[\"text\"]}"
  in
  let spec : JT.Tool.t =
    { name = "echo"; description = Some "Echo back text"; input_schema }
  in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs ->
      (match List.Assoc.find kvs ~equal:String.equal "text" with
       | Some (`String s) -> Ok (`String s)
       | _ -> Error "missing field text or not string")
    | _ -> Error "arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler
;;

(* --------------------------------------------------------------------- *)
(* Built-in ocamlochat tools exposed over MCP                                *)
(* --------------------------------------------------------------------- *)

(** [register_builtin_apply_patch core ~dir] registers the
    built-in ["apply_patch"] tool.

    The underlying implementation is provided by
    {!module:Functions.apply_patch}; this helper merely adapts the JSON-RPC
    call-site contract to the untyped text interface expected by
    [Functions.apply_patch].

    The handler accepts a single field:

    {v
      {
        "input" : "<unified-v4a-patch>"
      }
    v}

    It returns the patched contents as a plain JSON string.

    @param core Mutable registry returned by {!Mcp_server_core.create} and
      shared by all transports.
    @param dir  Capability representing the current working directory. The
      value is used by {!Functions.apply_patch} to resolve relative file
      paths in the diff hunk headers.

    @raise Invalid_argument  Propagated from
      {!Mcp_server_core.register_tool} if a tool called ["apply_patch"] was
      already present in the registry. *)
let register_builtin_apply_patch (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  (* Pull metadata from the existing [Definitions] module so that we keep a
     single source of truth. *)
  let module Def = Definitions.Apply_patch in
  let spec : JT.Tool.t =
    { name = "apply_patch"; description = Def.description; input_schema = Def.parameters }
  in
  (* Re-use the already implemented helper residing in [Functions]. *)
  let ochat_fn = Functions.apply_patch ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs ->
      (match List.Assoc.find kvs ~equal:String.equal "input" with
       | Some (`String patch_text) ->
         let input_json = `Object [ "input", `String patch_text ] in
         let res =
           match ochat_fn.run (Jsonaf.to_string input_json) with
           | Openai.Responses.Tool_output.Output.Text t -> t
           | _ -> "Unsupported output type"
         in
         Ok (`String res)
       | _ -> Error "apply_patch expects field 'input' (string)")
    | _ -> Error "apply_patch arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler
;;

(** [register_builtin_read_dir core ~dir] registers the built-in
    ["read_dir"] tool.

    The handler is powered by {!module:Functions.read_dir}.  It accepts a
    single required argument:

    {v
      {
        "path" : "/absolute/or/relative"
      }
    v}

    The function responds with a JSON array holding all entries of the
    requested directory, sorted lexicographically, with the usual ["."] and
    [".."] components omitted.

    I/O errors (permission denied, missing directory, …) are propagated as
    [`Error msg`] which the router converts into a JSON-RPC exception
    visible to the client.

    @param core Registry to mutate.
    @param dir  Capability to the cwd – required by
      {!Functions.read_dir}. *)
let register_builtin_read_dir (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  let module Def = Definitions.Read_directory in
  let spec : JT.Tool.t =
    { name = "read_dir"; description = Def.description; input_schema = Def.parameters }
  in
  let ochat_fn = Functions.read_dir ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs ->
      (match List.Assoc.find kvs ~equal:String.equal "path" with
       | Some (`String path) ->
         let input_json = `Object [ "path", `String path ] in
         let res =
           match ochat_fn.run (Jsonaf.to_string input_json) with
           | Openai.Responses.Tool_output.Output.Text t -> t
           | _ -> "Unsupported output type"
         in
         Ok (`String res)
       | _ -> Error "read_dir expects field 'path' (string)")
    | _ -> Error "read_dir arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler
;;

(** [register_builtin_get_contents core ~dir] registers the built-in
    ["get_contents"] tool (kept for backward compatibility with the legacy
    name ["read_file"].)

    The tool expects exactly one argument:

    {v
      {
        "file" : "path/to/file"
      }
    v}

    It returns the *raw* file contents wrapped in a JSON string.  Binary
    files are therefore not supported.

    @param core Registry to mutate.
    @param dir  Capability to the working directory, forwarded to
      {!Functions.get_contents} so that relative paths resolve correctly. *)
let register_builtin_get_contents (core : Mcp_server_core.t) ~(dir : _ Eio.Path.t) : unit =
  let module Def = Definitions.Get_contents in
  let spec : JT.Tool.t =
    { name = "get_contents"
    ; description = Def.description
    ; input_schema = Def.parameters
    }
  in
  let ochat_fn = Functions.get_contents ~dir in
  let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
    match args with
    | `Object kvs ->
      (match List.Assoc.find kvs ~equal:String.equal "file" with
       | Some (`String file_path) ->
         let input_json = `Object [ "file", `String file_path ] in
         let res =
           match ochat_fn.run (Jsonaf.to_string input_json) with
           | Openai.Responses.Tool_output.Output.Text t -> t
           | _ -> "Unsupported output type"
         in
         Ok (`String res)
       | _ -> Error "get_contents expects field 'file' (string)")
    | _ -> Error "get_contents arguments must be object"
  in
  Mcp_server_core.register_tool core spec handler
;;

(** [run_stdio ~core ~env] implements the *line-delimited JSON* transport.

    The function blocks indefinitely, performing the following steps in a
    tight loop:

    1. Read a single line from [stdin] and parse it as JSON;
    2. Dispatch the value to {!Mcp_server_router.handle};
    3. Serialise every response returned by the router and write it back to
       [stdout], each on its own line.

    All work happens inside the caller’s fibre – the function never
    returns unless an exception bubbles up or the surrounding switch is
    aborted.

    @param core Shared registry instance.
    @param env  Eio standard environment obtained from
      [Eio_main.run]. *)
let run_stdio ~core ~env : unit =
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  (* We use a buffered reader to read lines from stdin.  The buffer size is
     arbitrary but should be large enough to handle typical input sizes. *)
  let buf = Eio.Buf_read.of_flow stdin ~initial_size:10000 ~max_size:Int.max_value in
  let rec loop stdin stdout =
    let line = Eio.Buf_read.line buf in
    let json = Jsonaf.of_string line in
    let responses = Mcp_server_router.handle ~core ~env json in
    List.iter responses ~f:(fun j ->
      Eio.Flow.copy_string (Jsonaf.to_string j) stdout;
      Eio.Flow.copy_string "\n" stdout);
    loop stdin stdout
  in
  loop stdin stdout
;;

let () =
  (* For Phase-1 we ignore CLI flags.  Future milestones will add --http etc. *)
  let core = Mcp_server_core.create () in
  (* For built-in tools we need an [Eio.Path.t] representing the current
     working directory.  We therefore register them inside the main Eio
     fibre where we have access to [env#cwd]. *)
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    (* Register demo echo plus built-in functions. *)
    setup_tool_echo core;
    register_builtin_apply_patch core ~dir;
    register_builtin_read_dir core ~dir;
    register_builtin_get_contents core ~dir;
    (* Meta-prompting refinement tool ----------------------------------- *)
    let register_builtin_meta_refine (core : Mcp_server_core.t) : unit =
      let module Def = Definitions.Meta_refine in
      let spec : JT.Tool.t =
        { name = Def.name; description = Def.description; input_schema = Def.parameters }
      in
      let ochat_fn = Functions.meta_refine ~env in
      let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
        match args with
        | `Object kvs ->
          (match List.Assoc.find kvs ~equal:String.equal "prompt" with
           | Some (`String prompt) ->
             let input_json = `Object [ "prompt", `String prompt; "task", `String "" ] in
             let res =
               match ochat_fn.run (Jsonaf.to_string input_json) with
               | Openai.Responses.Tool_output.Output.Text t -> t
               | _ -> "Unsupported output type"
             in
             Ok (`String res)
           | _ -> Error "meta_refine expects field 'prompt' (string)")
        | _ -> Error "meta_refine arguments must be object"
      in
      Mcp_server_core.register_tool core spec handler
    in
    register_builtin_meta_refine core;
    (* Webpage → Markdown tool --------------------------------------- *)
    let module Def = Definitions.Webpage_to_markdown in
    let spec : JT.Tool.t =
      { name = Def.name; description = Def.description; input_schema = Def.parameters }
    in
    let ochat_fn = Functions.webpage_to_markdown ~dir ~net:(Eio.Stdenv.net env) in
    let handler (args : Jsonaf.t) : (Jsonaf.t, string) Result.t =
      match args with
      | `Object kvs ->
        (match List.Assoc.find kvs ~equal:String.equal "url" with
         | Some (`String url) ->
           let input_json = `Object [ "url", `String url ] in
           let res =
             match (ochat_fn ~env).run (Jsonaf.to_string input_json) with
             | Openai.Responses.Tool_output.Output.Text t -> t
             | _ -> "Unsupported output type"
           in
           Ok (`String res)
         | _ -> Error "webpage_to_markdown expects field 'url' (string)")
      | _ -> Error "arguments must be object"
    in
    Mcp_server_core.register_tool core spec handler;
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
           if not (Hashtbl.mem processed fname)
           then (
             let file_path = Eio.Path.(prompts_dir / fname) in
             match
               Or_error.try_with (fun () ->
                 Mcp_prompt_agent.of_chatmd_file ~env ~core ~path:file_path)
             with
             | Error err ->
               eprintf
                 "[mcp_server] Failed to load prompt %s: %s\n"
                 fname
                 (Error.to_string_hum err)
             | Ok (tool, handler, prompt) ->
               Hashtbl.set processed ~key:fname ~data:();
               Mcp_server_core.register_tool core tool handler;
               Mcp_server_core.register_prompt core ~name:tool.name prompt))
       with
       | _exn -> ());
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
        Eio.Time.sleep (Eio.Stdenv.clock env) 10.0;
        loop ()
      in
      Eio.Fiber.fork ~sw loop
    in
    (* --------------------------------------------------------------- *)
    (* Lightweight polling for resource list changes.                  *)
    (* --------------------------------------------------------------- *)
    let start_polling_resources ~sw () =
      let previous_listing = ref String.Set.empty in
      let cwd_string = Stdlib.Sys.getcwd () in
      let scan_resources () =
        let current_listing =
          match Or_error.try_with (fun () -> Stdlib.Sys.readdir cwd_string) with
          | Error _ -> String.Set.empty
          | Ok arr ->
            Array.to_list arr
            |> List.filter ~f:(fun fname ->
              let path = Filename.concat cwd_string fname in
              Stdlib.Sys.file_exists path && not (Stdlib.Sys.is_directory path))
            |> String.Set.of_list
        in
        if not (Set.equal !previous_listing current_listing)
        then (
          previous_listing := current_listing;
          Mcp_server_core.notify_resources_changed core)
      in
      let rec loop () =
        scan_resources ();
        (* initial / periodic scan *)
        Eio.Time.sleep (Eio.Stdenv.clock env) 10.0;
        loop ()
      in
      Eio.Fiber.fork ~sw loop
    in
    (* ----------------------------------------------------------------- *)
    match !http_port_ref with
    | Some port ->
      Eio.Switch.run (fun sw ->
        (* Poller lives under the same switch so it terminates when the
                  HTTP server shuts down *)
        start_polling_prompts ~sw ();
        start_polling_resources ~sw ();
        (* Launch Streamable HTTP server and block forever *)
        Mcp_server_http.run ~require_auth:true ~env ~core ~port)
    | None ->
      (* stdio mode – we still spawn the polling fibre so that long-lived
              sessions also benefit from newly added prompts.  Since the stdio
              loop is blocking we need a dedicated switch. *)
      Eio.Switch.run (fun sw ->
        start_polling_prompts ~sw ();
        start_polling_resources ~sw ();
        run_stdio ~core ~env))
;;
