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
    match Eio.Process.spawn ~sw proc_mgr ~stdout:w ~stderr:w (command :: params) with
    | exception ex ->
      let err_msg = Fmt.str "error running %s command: %a" command Eio.Exn.pp ex in
      Eio.Flow.close w;
      err_msg
    | _child ->
      Eio.Flow.close w;
      (match Eio.Buf_read.parse_exn ~max_size:1_000_000 Eio.Buf_read.take_all r with
       | res -> res
       | exception ex -> Fmt.str "error running %s command: %a" command Eio.Exn.pp ex)
  in
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

(*--- 4-d.  Unified declaration → function mapping ------------------*)

let of_declaration ~(ctx : _ Ctx.t) ~run_agent (decl : CM.tool) : Gpt_function.t =
  match decl with
  | CM.Builtin name ->
    (match name with
     | "apply_patch" -> Functions.apply_patch ~dir:(Ctx.dir ctx)
     | "read_dir" -> Functions.read_dir ~dir:(Ctx.dir ctx)
     | "get_contents" -> Functions.get_contents ~dir:(Ctx.dir ctx)
     | other -> failwithf "Unknown built-in tool: %s" other ())
  | CM.Custom c -> custom_fn ~env:(Ctx.env ctx) c
  | CM.Agent agent_spec -> agent_fn ~ctx ~run_agent agent_spec
  | CM.Mcp { name; description = _; mcp_server } ->
    (* Retrieve (and cache) the list of tools for this server. *)
    let tools_for_server =
      Tool_cache.find_or_add tool_cache mcp_server ~ttl:cache_ttl ~default:(fun () ->
        Eio.Switch.run (fun sw ->
          let client = Mcp_client.connect ~sw ~env:(Ctx.env ctx) ~uri:mcp_server in
          match Mcp_client.list_tools client with
          | Ok lst -> lst
          | Error msg -> failwithf "Failed to list tools from %s: %s" mcp_server msg ()))
    in
    let tool_meta =
      match List.find tools_for_server ~f:(fun t -> String.equal t.name name) with
      | Some t -> t
      | None ->
        (* Cache might be stale – refresh once before giving up. *)
        let tools =
          Eio.Switch.run (fun sw ->
            let client = Mcp_client.connect ~sw ~env:(Ctx.env ctx) ~uri:mcp_server in
            match Mcp_client.list_tools client with
            | Ok lst ->
              (* Update cache and continue. *)
              Tool_cache.set_with_ttl tool_cache ~key:mcp_server ~data:lst ~ttl:cache_ttl;
              lst
            | Error msg -> failwithf "Failed to list tools from %s: %s" mcp_server msg ())
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
    Mcp_tool.gpt_function_of_remote_tool ~env:(Ctx.env ctx) ~uri:mcp_server tool_meta
;;
