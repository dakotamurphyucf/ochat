(** Tool helper utilities.

    This module turns a ChatMarkdown [`<tool …/>`] declaration into a
    runtime {!Ochat_function.t} that can be submitted to the *OpenAI
    function-calling API*.  The helper covers **five** independent
    back-ends:

    1. {b Built-ins} – OCaml functions hard-coded in {!Functions}
       (e.g. ["apply_patch"], ["fork"], …).
    2. {b Scoped file readers} – configured [`read_file`] roots resolved
       through the live host filesystem capabilities.
    3. {b Custom shell commands} – `{<tool command="grep" …/>}` wrappers
       that spawn an arbitrary process inside the Eio sandbox.
    4. {b Agent prompts} – nested ChatMarkdown agents executed through
       the same driver stack.
    5. {b Remote MCP tools} – functions discovered dynamically over the
       Model-Context-Protocol network.

    The public surface is intentionally small – only the dispatcher
    {!of_declaration} and a (de)serialisation helper {!convert_tools}
    are meant to be consumed by other modules.  Everything else is
    private glue code.

    {1 Example}

    Converting a list of ChatMarkdown declarations into the request
    payload expected by {!Openai.Responses}:

    {[
      let ochat_functions =
        List.concat_map declarations ~f:(Tool.of_declaration ~sw ~ctx ~run_agent)

      let comp_tools, _tbl = Ochat_function.functions ochat_functions in
      let request_tools  = Tool.convert_tools comp_tools in
      (* … pass [request_tools] to [Openai.Responses.post_response] … *)
    ]}

    {1 Warning}

    The {e custom shell} backend executes arbitrary commands provided by
    the prompt author.  Enable it only in a {b trusted} environment and
    consider using dedicated OCaml helpers or remote MCP tools for
    production workloads. *)

open Core
module CM = Prompt.Chat_markdown

let agent_page_classification (decl : CM.tool) =
  match decl with
  | CM.Agent { name; _ } -> Some (name, Tool_execution_event.Subagent)
  | CM.Builtin "fork" -> Some ("fork", Tool_execution_event.Subagent)
  | CM.Custom { name; _ } -> Some (name, Tool_execution_event.Shell_script)
  | CM.Shell { name; _ } -> Some (name, Tool_execution_event.Shell_script)
  | CM.Builtin _ | CM.Read_file _ | CM.Mcp _ | CM.Extension _ | CM.Inherited _ -> None
;;

module Res = Openai.Responses

let register_invalidation_listener ~sw ~cache ~client =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      match
        try Some (Eio.Stream.take (Mcp_client.notifications client)) with
        | End_of_file -> None
      with
      | None -> `Stop_daemon
      | Some notification ->
        if String.equal notification.method_ "notifications/tools/list_changed"
        then Mcp_discovery_cache.invalidate cache;
        loop ()
    in
    loop ())
;;

(*--- 4-a.  OpenAI → Responses tool conversion ----------------------*)

(** [convert_tools ts] converts a list of [`Openai.Completions.tool`]
    descriptors – the minimal structure returned by the [openai] SDK –
    into the richer {!Openai.Responses.Request.Tool.t} representation
    expected by the *chat/completions* endpoint.

    The transformation is a {i pure}, field-by-field copy.  It exists
    only to prevent callers from having to depend on both modules at
    once.

    Complexity: O(n) where [n = List.length ts]. *)
let convert_tools (ts : Openai.Completions.tool list) : Res.Request.Tool.t list =
  List.map ts ~f:(fun { type_; function_ = { name; description; parameters; strict } } ->
    match type_ with
    | "custom" ->
      Res.Request.Tool.Custom_function { name; description; format = parameters; type_ }
    | _ -> Res.Request.Tool.Function { name; description; parameters; strict; type_ })
;;

(*--- 4-c.  Agent tool → Ochat_function.t ------------------------------*)

(** [agent_fn ~ctx ~run_agent spec] wraps a nested ChatMarkdown
    {e agent} into a {!Ochat_function.t}.  Calling the resulting function
    is equivalent to starting a brand-new ChatMarkdown driver on the
    referenced `*.chatmd` file.

    Expected input
    {[
      { "input" : string }   (* Message forwarded to the agent *)
    ]}

    The helper runs [run_agent] – a higher-order callback supplied by
    the caller – to avoid creating a circular dependency with
    {!Chat_response.Driver}.  The child conversation inherits the
    parent context [ctx] but not its message history.

    Typical use-case: breaking down a complex user request into
    multiple self-contained sub-tasks handled by specialised prompts. *)
let agent_fn ~(ctx : _ Ctx.t) ~run_agent (agent_spec : CM.agent_tool) : Ochat_function.t =
  let CM.{ name; description; agent; is_local } = agent_spec in
  (* pull components from the shared context *)
  let _net_unused = Ctx.net ctx in
  (* Interface definition for the agent tool – expects an object with a
       single string field "input". *)
  let module M : Ochat_function.Def with type input = string = struct
    type input = string

    let name = name
    let type_ = "function"

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
  let run ~source ?observer (user_msg : string) : string =
    (* Build a basic content item from the provided user input. *)
    let basic_item : CM.basic_content_item =
      { type_ = "text"
      ; text = Some user_msg
      ; image_url = None
      ; document_url = None
      ; is_local = false
      ; cleanup_html = false
      ; markdown = false
      }
    in
    (* Fetch the agent prompt (local or remote) *)
    let prompt_xml = Fetch.get ~ctx agent ~is_local in
    let prompt_dir = if is_local then Fetch.resolve_local_dir ~ctx agent else None in
    (* Delegate the heavy lifting to the provided [run_agent] callback. *)
    run_agent
      ?prompt_dir
      ?session_id:(Some agent)
      ?observer
      ~source
      ~ctx
      prompt_xml
      [ CM.Basic basic_item ]
  in
  Ochat_function.create_streaming_function
    (module M)
    (fun ~invocation args ->
       let source = Fork.Invocation_id.create () |> Fork.Invocation_id.to_string in
       let observer =
         if Ochat_function.Invocation.is_observed invocation
         then (
           let trace =
             Agent_trace.create
               ~emit:(Ochat_function.Invocation.emit invocation)
               ~emit_trace:(Ochat_function.Invocation.emit_trace invocation)
           in
           Some
             Agent_response_loop.
               { on_event = Agent_trace.on_event trace
               ; on_tool_execution = Agent_trace.on_tool_execution trace
               })
         else None
       in
       Res.Tool_output.Output.Text (run ~source ?observer args))
;;

(** [mcp_tool ~sw ~ctx decl] resolves a `{<tool mcp_server="…"/>}`
      declaration.  It returns one {!Ochat_function.t} per advertised
      remote function.

      Implementation details:
      – Remote metadata are fetched through {!Mcp_client.list_tools}.
      – A five-minute cache belongs to this connected declaration only.
      – The helper registers a background fibre listening for
        `notifications/tools/list_changed` and invalidates the cache on
        demand.

      When [decl.names] is [`Some list`] only the named tools are wrapped;
      otherwise the full catalog is exposed. *)
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
  let cache =
    Mcp_discovery_cache.create
      ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock (Ctx.env ctx)))
      ~load:(fun () ->
        match Mcp_client.list_tools client with
        | Ok tools -> tools
        | Error _ -> failwith "MCP tool discovery failed")
  in
  register_invalidation_listener ~sw ~cache ~client;
  let wrap = Mcp_tool.ochat_function_of_remote_tool ~sw ~client ~strict in
  let get_tool name =
    let find () =
      List.find (Mcp_discovery_cache.get cache) ~f:(fun tool ->
        String.equal tool.Mcp_types.Tool.name name)
    in
    match find () with
    | Some tool -> wrap tool
    | None ->
      Mcp_discovery_cache.invalidate cache;
      (match find () with
       | Some tool -> wrap tool
       | None -> failwithf "MCP tool %s is unavailable" name ())
  in
  match names with
  | Some names -> List.map names ~f:get_tool
  | None -> List.map (Mcp_discovery_cache.get cache) ~f:wrap
;;

(*--- 4-d.  Unified declaration → function mapping ------------------*)
(** [of_declaration ~sw ~ctx ~run_agent decl] dispatches a single
    ChatMarkdown [`<tool …/>`] declaration to its runtime
    implementation.

    The helper inspects the variant constructor of [decl] and returns
    a list of {!type:Ochat_function.t}.  A single declaration can map to
    several functions – for example an [`<tool mcp_server=…/>`]
    element expands to the complete set of remote tools exposed by the
    referenced MCP server.  The resulting list is therefore suitable
    for direct consumption by {!Ochat_function.functions}.

    Input invariants
    • [sw] – parent {!Eio.Switch.t}.  Child fibres (e.g. MCP cache
      listeners) are attached to this switch so that they terminate
      cleanly when the caller’s scope ends.
    • [ctx] – shared execution context.  Directory paths, network and
      environment handles are forwarded to the lower-level helpers.
    • [run_agent] – callback used to start a nested ChatMarkdown agent
      when handling [`CM.Agent _`] declarations.  Passing the function
      as an argument avoids a circular dependency with
      {!module:Chat_response.Driver}.

    Complexity: O(1) except for the MCP branch which may perform a
    network round-trip when the server metadata is not cached.

    @raise Failure if the declaration references an unknown built-in
           tool name.
*)
let read_file_root host source (root : Chatmd_read_file_spec.Root.t) =
  match Shell_runtime.Host.resolve_existing_directory host ~source root.path with
  | Ok path -> Functions.read_file_root ~id:root.id ~path ?description:root.description ()
  | Error error -> failwithf "[%s] %s" error.code error.message ()
;;

let configured_read_file host ctx (specification : Chatmd_read_file_spec.t) =
  let roots =
    List.map specification.roots ~f:(read_file_root host specification.source)
  in
  Functions.get_contents_scoped
    ~fs:(Eio.Stdenv.fs (Ctx.env ctx))
    ~dir:(Ctx.tool_dir ctx)
    ~roots
    ?description:specification.description
    ()
;;

let default_read_file ctx =
  let root =
    Functions.read_file_root
      ~id:"cwd"
      ~path:(Ctx.tool_dir ctx)
      ~description:"ochat launch directory"
      ()
  in
  Functions.get_contents_scoped
    ~fs:(Eio.Stdenv.fs (Ctx.env ctx))
    ~dir:(Ctx.tool_dir ctx)
    ~roots:[ root ]
    ()
;;

let of_declaration ?shell_registry ?host ~sw ~(ctx : _ Ctx.t) ~run_agent (decl : CM.tool)
  : Ochat_function.t list
  =
  match decl with
  | CM.Inherited _ ->
    failwith
      "capability.inheritance_required: inherited tool references require an admitted \
       parent binding"
  | CM.Extension _ ->
    failwith
      "chatml.extension_unavailable: extension tool execution is not yet enabled on this \
       host"
  | CM.Builtin name ->
    (match name with
     | "apply_patch" -> [ Functions.apply_patch ~dir:(Ctx.tool_dir ctx) ]
     | "read_dir" -> [ Functions.read_dir ~dir:(Ctx.tool_dir ctx) ]
     | "append_to_file" -> [ Functions.append_to_file ~dir:(Ctx.tool_dir ctx) ]
     | "find_and_replace" -> [ Functions.find_and_replace ~dir:(Ctx.tool_dir ctx) ]
     | "get_contents" | "read_file" -> [ default_read_file ctx ]
     | "webpage_to_markdown" ->
       [ Functions.webpage_to_markdown
           ~env:(Ctx.env ctx)
           ~dir:(Ctx.tool_dir ctx)
           ~net:(Ctx.net ctx)
       ]
     | "fork" -> [ Functions.fork ]
     | "odoc_search" ->
       [ Functions.odoc_search ~dir:(Ctx.tool_dir ctx) ~net:(Ctx.net ctx) ]
     | "index_markdown_docs" ->
       [ Functions.index_markdown_docs ~env:(Ctx.env ctx) ~dir:(Ctx.tool_dir ctx) ]
     | "markdown_search" ->
       [ Functions.markdown_search ~dir:(Ctx.tool_dir ctx) ~net:(Ctx.net ctx) ]
     | "query_vector_db" ->
       [ Functions.query_vector_db ~dir:(Ctx.tool_dir ctx) ~net:(Ctx.net ctx) ]
     | "index_ocaml_code" ->
       [ Functions.index_ocaml_code
           ~env:(Ctx.env ctx)
           ~dir:(Ctx.tool_dir ctx)
           ~net:(Ctx.net ctx)
       ]
     | "import_image" -> [ Functions.import_image ~dir:(Ctx.tool_dir ctx) ]
     | "meta_refine" -> [ Functions.meta_refine ~env:(Ctx.env ctx) ]
     | other -> failwithf "Unknown built-in tool: %s" other ())
  | CM.Read_file specification ->
    let host =
      Option.value_or_thunk host ~default:(fun () ->
        failwith
          "Configured read_file declarations require a live shell host for path \
           resolution")
    in
    [ configured_read_file host ctx specification ]
  | CM.Custom tool ->
    failwithf
      "Legacy shell tool %S must be constructed through Agent_runtime"
      tool.name
      ()
  | CM.Shell tool ->
    (match shell_registry with
     | None ->
       failwithf
         "Shell tool %S references runtime %S, but the shell runtime registry is not \
          instantiated"
         tool.name
         tool.runtime
         ()
     | Some registry ->
       (match Shell_tool.create registry tool with
        | Ok tool -> [ tool ]
        | Error error -> failwithf "%s: %s" error.code error.message ()))
  | CM.Agent agent_spec -> [ agent_fn ~ctx ~run_agent agent_spec ]
  | CM.Mcp mcp -> mcp_tool ~sw ~ctx mcp
;;
