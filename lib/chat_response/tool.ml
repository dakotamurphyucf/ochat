(*********************************************************************
     Helpers for tool creation and conversion.

     This gathers the previously scattered [convert_tools], [custom_fn]
     and [agent_fn] helpers into one cohesive namespace.  The module is
     intentionally kept local to this file for now; future work will
     move it to its own compilation unit.
  *********************************************************************)

open Core
module CM = Prompt.Chat_markdown
module Res = Openai.Responses

(*------------------------------------------------------------------*)
(* 5. Remote MCP tool metadata cache                                *)
(*------------------------------------------------------------------*)

(* We keep a small TTL-based LRU that maps an MCP server URI to the
   list of tools it exposes. This avoids re-running the expensive
   `tools/list` handshake for every `<tool mcp_server=...>`
   declaration inside a prompt. *)

module String_key = struct
  type t = string [@@deriving sexp, compare, hash]

  (* The cache key is just the server URI – no internal invariants. *)
  let invariant (_ : t) = ()
end

module Tool_cache = Ttl_lru_cache.Make (String_key)

let tool_cache : Mcp_types.Tool.t list Tool_cache.t = Tool_cache.create ~max_size:32 ()
let cache_ttl = Time_ns.Span.of_int_sec 300

(* When a given MCP server notifies that its tool list has changed we
   simply drop the cached entry for that URI so that the next lookup
   forces a fresh `tools/list` request.  The helper below registers a
   lightweight daemon (at most one per client/URI pair) that listens
   for such notifications and performs the invalidation.            *)

let register_invalidation_listener ~sw ~mcp_server ~client =
  (* We attach the listener on a background fibre so it does not block the
     normal execution flow.  The fibre terminates automatically when the
     underlying stream closes (e.g. connection lost) or the switch is
     torn down. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      match
        try Some (Eio.Stream.take (Mcp_client.notifications client)) with
        | End_of_file -> None
      with
      | None -> `Stop_daemon
      | Some notification ->
        (match notification.method_ with
         | "notifications/tools/list_changed" ->
           ignore (Tool_cache.remove tool_cache mcp_server : _)
         | _ -> ());
        loop ()
    in
    loop ())
;;

(*--- 4-a.  OpenAI → Responses tool conversion ----------------------*)

let convert_tools (ts : Openai.Completions.tool list) : Res.Request.Tool.t list =
  List.map ts ~f:(fun { type_; function_ = { name; description; parameters; strict } } ->
    Res.Request.Tool.Function { name; description; parameters; strict; type_ })
;;

(*--- 4-b.  Custom shell command tool --------------------------------*)

let custom_fn ~env (c : CM.custom_tool) : Gpt_function.t =
  let CM.{ name; description; command } = c in
  let module M : Gpt_function.Def with type input = string list = struct
    type input = string list

    let name = name

    let description : string option =
      match description with
      | Some desc ->
        Some
          (String.concat
             [ "Run a "
             ; command
             ; " shell command with arguments, and returns its output.\n"
             ; desc
             ])
      | None ->
        Some
          (String.concat
             [ "Run a "
             ; command
             ; " shell command with arguments, and returns its output"
             ])
    ;;

    let parameters : Jsonaf.t =
      `Object
        [ "type", `String "object"
        ; ( "properties"
          , `Object
              [ ( "arguments"
                , `Object
                    [ "type", `String "array"
                    ; "items", `Object [ "type", `String "string" ]
                    ] )
              ] )
        ; "required", `Array [ `String "arguments" ]
        ; "additionalProperties", `False
        ]
    ;;

    let input_of_string s : input =
      let j = Jsonaf.of_string s in
      j
      |> Jsonaf.member_exn "arguments"
      |> Jsonaf.list_exn
      |> List.map ~f:Jsonaf.string_exn
    ;;
  end
  in
  let fp (params : string list) : string =
    let proc_mgr = Eio.Stdenv.process_mgr env in
    Eio.Switch.run
    @@ fun sw ->
    (* 1.  Pipe for capturing stdout & stderr. *)
    let r, w = Eio.Process.pipe ~sw proc_mgr in
    let cmdline = command |> String.substr_replace_all ~pattern:"%20" ~with_:" " in
    (* Split on whitespace – rudimentary, but sufficient for Phase-1. *)
    let cmd_list =
      if String.is_empty cmdline
      then invalid_arg "custom_fn: empty command line"
      else
        String.split_on_chars ~on:[ ' '; Char.of_int_exn 32 ] cmdline
        |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    (* 2.  Check that the command is not empty. *)
    (* 2.  Spawn the child process with the provided command and parameters. *)
    (* Note: we use [Eio.Process.spawn] to run the command, which captures
       stdout and stderr into the pipe [w]. *)
    (* Note: we use [Eio.Buf_read.parse_exn] to read the output from the pipe. *)
    match
      Eio.Process.spawn ~sw proc_mgr ~stdout:w ~stderr:w (List.append cmd_list params)
    with
    | exception ex ->
      let err_msg = Fmt.str "error running %s command: %a" command Eio.Exn.pp ex in
      Eio.Flow.close w;
      err_msg
    | _child ->
      Eio.Flow.close w;
      (match Eio.Buf_read.parse_exn ~max_size:1_000_000 Eio.Buf_read.take_all r with
       | res ->
         let max_len = 100000 in
         let res =
           if String.length res > max_len
           then String.append (String.sub res ~pos:0 ~len:max_len) " ...truncated"
           else res
         in
         res
       | exception ex -> Fmt.str "error running %s command: %a" command Eio.Exn.pp ex)
  in
  (* timeout functioin eio *)
  let fp x =
    try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60.0 (fun () -> fp x) with
    | Eio.Time.Timeout ->
      Printf.sprintf "timeout running command %s" (String.concat ~sep:" " x)
  in
  (* Create the Gpt_function.t using the module M and the function fp. *)
  (* Note: we use [Gpt_function.create_function] to create the function. *)
  (* Note: we use [module M] to specify the module type for the function. *)
  Gpt_function.create_function (module M) fp
;;

(*--- 4-c.  Agent tool → Gpt_function.t ------------------------------*)

let agent_fn ~(ctx : _ Ctx.t) ~run_agent (agent_spec : CM.agent_tool) : Gpt_function.t =
  let CM.{ name; description; agent; is_local } = agent_spec in
  (* pull components from the shared context *)
  let _net_unused = Ctx.net ctx in
  (* Interface definition for the agent tool – expects an object with a
       single string field "input". *)
  let module M : Gpt_function.Def with type input = string = struct
    type input = string

    let name = name

    let description : string option =
      Option.first_some
        description
        (Some
           (Printf.sprintf
              "Run agent prompt located at %s and return its final answer."
              agent))
    ;;

    let parameters : Jsonaf.t =
      `Object
        [ "type", `String "object"
        ; "properties", `Object [ "input", `Object [ "type", `String "string" ] ]
        ; "required", `Array [ `String "input" ]
        ; "additionalProperties", `False
        ]
    ;;

    let input_of_string s : input =
      match Jsonaf.(of_string s |> member_exn "input") with
      | `String str -> str
      | _ -> failwith "Expected {\"input\": string} for agent tool input"
    ;;
  end
  in
  let run (user_msg : string) : string =
    (* Build a basic content item from the provided user input. *)
    let basic_item : CM.basic_content_item =
      { type_ = "text"
      ; text = Some user_msg
      ; image_url = None
      ; document_url = None
      ; is_local = false
      ; cleanup_html = false
      }
    in
    (* Fetch the agent prompt (local or remote) *)
    let prompt_xml = Fetch.get ~ctx agent ~is_local in
    (* Delegate the heavy lifting to the provided [run_agent] callback. *)
    run_agent ~ctx prompt_xml [ CM.Basic basic_item ]
  in
  Gpt_function.create_function (module M) run
;;

let mcp_tool
      ~sw
      ~ctx
      CM.{ names; description = _; mcp_server; strict; client_id_env; client_secret_env }
  =
  (* Inject per-server credentials via URI query params if attribute specifies
     environment variable names and the variables are present. *)
  let mcp_server_uri =
    let uri = Uri.of_string mcp_server in
    let add_param_if_some uri (name, opt_var) =
      match opt_var with
      | None -> uri
      | Some env_var ->
        (match Sys.getenv env_var with
         | Some v when not (String.is_empty v) -> Uri.add_query_param' uri (name, v)
         | _ -> uri)
    in
    let uri = add_param_if_some uri ("client_id", client_id_env) in
    let uri = add_param_if_some uri ("client_secret", client_secret_env) in
    Uri.to_string uri
  in
  let client = Mcp_client.connect ~sw ~env:(Ctx.env ctx) mcp_server_uri in
  (* Ensure cache invalidation for this server is wired up exactly
     once.  We conservatively register a listener each time – the
     underlying [Tool_cache.remove] operation is idempotent and cheap,
     so occasional duplicates are harmless. *)
  register_invalidation_listener ~sw ~mcp_server ~client;
  let get_tool name =
    let tools_for_server =
      Tool_cache.find_or_add tool_cache mcp_server ~ttl:cache_ttl ~default:(fun () ->
        match Mcp_client.list_tools client with
        | Ok lst -> lst
        | Error msg -> failwithf "Failed to list tools from %s: %s" mcp_server msg ())
    in
    let tool_meta =
      match List.find tools_for_server ~f:(fun t -> String.equal t.name name) with
      | Some t -> t
      | None ->
        (* Cache might be stale – refresh once before giving up. *)
        let tools =
          match Mcp_client.list_tools client with
          | Ok lst ->
            (* Update cache and continue. *)
            Tool_cache.set_with_ttl tool_cache ~key:mcp_server ~data:lst ~ttl:cache_ttl;
            lst
          | Error msg -> failwithf "Failed to list tools from %s: %s" mcp_server msg ()
        in
        (match List.find tools ~f:(fun t -> String.equal t.name name) with
         | Some t -> t
         | None ->
           failwithf
             "MCP server %s does not expose tool %s (after refresh)"
             mcp_server
             name
             ())
    in
    Mcp_tool.gpt_function_of_remote_tool ~sw ~client ~strict tool_meta
  in
  match names with
  | Some names -> List.map names ~f:get_tool
  | None ->
    let tools_for_server =
      Tool_cache.find_or_add tool_cache mcp_server ~ttl:cache_ttl ~default:(fun () ->
        match Mcp_client.list_tools client with
        | Ok lst -> lst
        | Error msg -> failwithf "Failed to list tools from %s: %s" mcp_server msg ())
    in
    List.map tools_for_server ~f:(fun t ->
      Mcp_tool.gpt_function_of_remote_tool ~sw ~client ~strict t)
;;

(*--- 4-d.  Unified declaration → function mapping ------------------*)

let of_declaration ~sw ~(ctx : _ Ctx.t) ~run_agent (decl : CM.tool) : Gpt_function.t list =
  match decl with
  | CM.Builtin name ->
    (match name with
     | "apply_patch" -> [ Functions.apply_patch ~dir:(Ctx.dir ctx) ]
     | "read_dir" -> [ Functions.read_dir ~dir:(Ctx.dir ctx) ]
     | "get_contents" -> [ Functions.get_contents ~dir:(Ctx.dir ctx) ]
     | "webpage_to_markdown" ->
       [ Functions.webpage_to_markdown ~dir:(Ctx.dir ctx) ~net:(Ctx.net ctx) ]
     | "fork" -> [ Functions.fork ]
     | other -> failwithf "Unknown built-in tool: %s" other ())
  | CM.Custom c -> [ custom_fn ~env:(Ctx.env ctx) c ]
  | CM.Agent agent_spec -> [ agent_fn ~ctx ~run_agent agent_spec ]
  | CM.Mcp mcp -> mcp_tool ~sw ~ctx mcp
;;
