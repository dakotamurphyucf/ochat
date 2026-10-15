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
      (* Tool descriptors are lowered by the explicit inference adapter. *)
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
  | CM.Persistent_agent ({ name; _ }, _) -> Some (name, Tool_execution_event.Subagent)
  | CM.Builtin "fork" -> Some ("fork", Tool_execution_event.Subagent)
  | CM.Custom { name; _ } -> Some (name, Tool_execution_event.Shell_script)
  | CM.Shell { name; _ } -> Some (name, Tool_execution_event.Shell_script)
  | CM.Builtin _ | CM.Read_file _ | CM.Mcp _ | CM.Extension _ | CM.Inherited _ -> None
;;

module Res = Openai.Responses

module Agent_source_runner = struct
  type t =
    { revision : Chatml_source.Digest.t
    ; prepare :
        (declaration:CM.agent_tool
         -> launch:Source_launch.t
         -> checkpoint:(unit -> unit)
         -> (Background_request.Agent_artifact.t, Agent_protocol.Error.t) Result.t)
          option
    ; run :
        implementation:Ochat_function.t option
        -> declaration:CM.agent_tool
        -> ctx:Eio_unix.Stdenv.base Ctx.t
        -> source:string
        -> observer:Agent_response_loop.observer option
        -> CM.content_item list
        -> string
    ; run_content :
        declaration:CM.agent_content -> ctx:Eio_unix.Stdenv.base Ctx.t -> string
    }
end

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

module Agent_definition (Spec : sig
    val declaration : CM.agent_tool
  end) : Ochat_function.Def with type input = string = struct
  type input = string

  let CM.{ name; description; agent; is_local = _ } = Spec.declaration
  let name = name
  let type_ = "function"

  let description =
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

  let input_of_string input =
    match Jsonaf.(of_string input |> member_exn "input") with
    | `String value -> value
    | _ -> failwith "Expected {\"input\": string} for agent tool input"
  ;;
end

let agent_input_items input : CM.content_item list =
  [ Basic
      { type_ = "text"
      ; text = Some input
      ; image_url = None
      ; document_url = None
      ; is_local = false
      ; cleanup_html = false
      ; markdown = false
      }
  ]
;;

let agent_observer invocation =
  if Ochat_function.Invocation.is_observed invocation
  then (
    let trace =
      Agent_trace.create
        ~emit:(Ochat_function.Invocation.emit invocation)
        ~emit_trace:(Ochat_function.Invocation.emit_trace invocation)
    in
    Some
      Agent_response_loop.
        { on_event = Agent_trace.on_transcript_event trace
        ; on_tool_execution = Agent_trace.on_tool_execution trace
        })
  else None
;;

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
let agent_fn ?agent_source_runner ~(ctx : _ Ctx.t) ~run_agent (agent_spec : CM.agent_tool)
  : Ochat_function.t
  =
  let CM.{ name = _; description = _; agent; is_local } = agent_spec in
  (* pull components from the shared context *)
  let _net_unused = Ctx.net ctx in
  let module M =
    Agent_definition (struct
      let declaration = agent_spec
    end)
  in
  (* Construction is synchronous and cannot invoke its callback. Initialize this
     private identity cell before returning the actual function for registration. *)
  let implementation = ref None in
  let run ~ctx ~source ?observer (user_msg : string) : string =
    let items = agent_input_items user_msg in
    match agent_source_runner with
    | Some runner ->
      runner.Agent_source_runner.run
        ~implementation:(Some (Option.value_exn !implementation))
        ~declaration:agent_spec
        ~ctx
        ~source
        ~observer
        items
    | None ->
      let prompt_xml = Fetch.get ~ctx agent ~is_local in
      let prompt_dir = if is_local then Fetch.resolve_local_dir ~ctx agent else None in
      run_agent
        ?prompt_dir
        ?session_id:(Some agent)
        ?observer
        ~source
        ~ctx
        prompt_xml
        items
  in
  let function_ =
    Ochat_function.create_streaming_function
      (module M)
      (fun ~invocation args ->
         let source = Fork.Invocation_id.create () |> Fork.Invocation_id.to_string in
         let observer = agent_observer invocation in
         let ctx =
           Option.value_map
             (Ochat_function.Invocation.inference_parent invocation)
             ~default:ctx
             ~f:(fun parent -> Ctx.with_inference_parent ctx ~parent)
         in
         Res.Tool_output.Output.Text (run ~ctx ~source ?observer args))
  in
  implementation := Some function_;
  function_
;;

module Deferred_agent = struct
  type t = { implementation : Ochat_function.t }

  let implementation t = t.implementation

  let create ~declaration:(agent_spec : CM.agent_tool) ~runner ~with_context =
    let module M =
      Agent_definition (struct
        let declaration = agent_spec
      end)
    in
    let implementation = ref None in
    let function_ =
      Ochat_function.create_streaming_function
        (module M)
        (fun ~invocation input ->
           let output =
             with_context
               ~implementation:(Option.value_exn !implementation)
               ~invocation
               (fun ctx ->
                  let ctx =
                    Option.value_map
                      (Ochat_function.Invocation.inference_parent invocation)
                      ~default:ctx
                      ~f:(fun parent -> Ctx.with_inference_parent ctx ~parent)
                  in
                  let observer = agent_observer invocation in
                  let source =
                    Fork.Invocation_id.create () |> Fork.Invocation_id.to_string
                  in
                  runner.Agent_source_runner.run
                    ~implementation:(Some (Option.value_exn !implementation))
                    ~declaration:agent_spec
                    ~ctx
                    ~source
                    ~observer
                    (agent_input_items input))
           in
           Res.Tool_output.Output.Text output)
    in
    implementation := Some function_;
    { implementation = function_ }
  ;;
end

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
      ~env
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
  let client = Mcp_client.connect ~sw ~env mcp_server_uri in
  let cache =
    Mcp_discovery_cache.create
      ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock env))
      ~load:(fun () ->
        match Mcp_client.list_tools client with
        | Ok tools -> tools
        | Error _ -> failwith "MCP tool discovery failed")
  in
  register_invalidation_listener ~sw ~cache ~client;
  let wrap (captured : Mcp_types.Tool.t) =
    let function_ = Mcp_tool.ochat_function_of_remote_tool ~sw ~client ~strict captured in
    let check_catalog () =
      match
        List.filter (Mcp_discovery_cache.get cache) ~f:(fun current ->
          String.equal current.Mcp_types.Tool.name captured.name)
      with
      | [ current ]
        when Option.equal String.equal current.description captured.description
             && Jsonaf.exactly_equal current.input_schema captured.input_schema -> ()
      | _ -> failwith "mcp.catalog_changed: reload the tool definition before calling it"
    in
    { function_ with
      run =
        (fun input ->
          check_catalog ();
          function_.run input)
    ; run_with_progress =
        (fun ~invocation input ->
          check_catalog ();
          function_.run_with_progress ~invocation input)
    }
  in
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
  Shell_runtime.Host.resolve_existing_directory host ~source root.path
  |> Result.map ~f:(fun path ->
    Functions.read_file_root ~id:root.id ~path ?description:root.description ())
;;

let resolve_read_roots host (specification : Chatmd_read_file_spec.t) =
  List.map specification.roots ~f:(read_file_root host specification.source) |> Result.all
;;

let validate_read_roots host = function
  | CM.Read_file specification ->
    resolve_read_roots host specification |> Result.map ~f:ignore
  | CM.Builtin _
  | Custom _
  | Shell _
  | Agent _
  | Mcp _
  | Extension _
  | Inherited _
  | Persistent_agent _ -> Ok ()
;;

let configured_read_file host ~env ~tool_dir (specification : Chatmd_read_file_spec.t) =
  let roots =
    match resolve_read_roots host specification with
    | Ok roots -> roots
    | Error error -> failwithf "[%s] %s" error.code error.message ()
  in
  Functions.get_contents_scoped
    ~fs:(Eio.Stdenv.fs env)
    ~dir:tool_dir
    ~roots
    ?description:specification.description
    ()
;;

let default_read_file ~env ~tool_dir =
  let root =
    Functions.read_file_root
      ~id:"cwd"
      ~path:tool_dir
      ~description:"ochat launch directory"
      ()
  in
  Functions.get_contents_scoped ~fs:(Eio.Stdenv.fs env) ~dir:tool_dir ~roots:[ root ] ()
;;

module Native_error = struct
  type t =
    { code : string
    ; message : string
    }
  [@@deriving sexp, compare, equal]
end

let inference_required message =
  Error Native_error.{ code = "agent.inference_required"; message }
;;

let of_declaration_with
      ?shell_registry
      ?host
      ?agent_source_runner
      ~sw
      ~env
      ~tool_dir
      ~agent
      (decl : CM.tool)
  : (Ochat_function.t list, Native_error.t) result
  =
  match decl with
  | CM.Persistent_agent _ ->
    failwith
      "agent.persistence_unavailable: authored persistent tools require an admitted \
       session adapter"
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
     | "apply_patch" -> Ok [ Functions.apply_patch ~dir:tool_dir ]
     | "read_dir" -> Ok [ Functions.read_dir ~dir:tool_dir ]
     | "append_to_file" -> Ok [ Functions.append_to_file ~dir:tool_dir ]
     | "find_and_replace" -> Ok [ Functions.find_and_replace ~dir:tool_dir ]
     | "get_contents" | "read_file" -> Ok [ default_read_file ~env ~tool_dir ]
     | "webpage_to_markdown" ->
       Ok [ Functions.webpage_to_markdown ~env ~dir:tool_dir ~net:(Eio.Stdenv.net env) ]
     | "fork" -> Ok [ Functions.fork ]
     | "odoc_search" ->
       Ok [ Functions.odoc_search ~dir:tool_dir ~net:(Eio.Stdenv.net env) ]
     | "index_markdown_docs" -> Ok [ Functions.index_markdown_docs ~env ~dir:tool_dir ]
     | "markdown_search" ->
       Ok [ Functions.markdown_search ~dir:tool_dir ~net:(Eio.Stdenv.net env) ]
     | "query_vector_db" ->
       Ok [ Functions.query_vector_db ~dir:tool_dir ~net:(Eio.Stdenv.net env) ]
     | "index_ocaml_code" ->
       Ok [ Functions.index_ocaml_code ~env ~dir:tool_dir ~net:(Eio.Stdenv.net env) ]
     | "import_image" -> Ok [ Functions.import_image ~dir:tool_dir ]
     | "meta_refine" ->
       (match agent with
        | Some (ctx, _) ->
          Ok [ Functions.meta_refine ~env ~inference:(Ctx.inference_execution ctx) () ]
        | None -> inference_required "meta_refine requires admitted inference")
     | other -> failwithf "Unknown built-in tool: %s" other ())
  | CM.Read_file specification ->
    let host =
      Option.value_or_thunk host ~default:(fun () ->
        failwith
          "Configured read_file declarations require a live shell host for path \
           resolution")
    in
    Ok [ configured_read_file host ~env ~tool_dir specification ]
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
        | Ok tool -> Ok [ tool ]
        | Error error -> failwithf "%s: %s" error.code error.message ()))
  | CM.Agent agent_spec ->
    (match agent with
     | Some (ctx, run_agent) ->
       Ok [ agent_fn ?agent_source_runner ~ctx ~run_agent agent_spec ]
     | None -> inference_required "Agent requires admitted inference")
  | CM.Mcp mcp -> Ok (mcp_tool ~sw ~env mcp)
;;

let of_declaration
      ?shell_registry
      ?host
      ?agent_source_runner
      ~sw
      ~(ctx : _ Ctx.t)
      ~run_agent
      (decl : CM.tool)
  =
  match
    of_declaration_with
      ?shell_registry
      ?host
      ?agent_source_runner
      ~sw
      ~env:(Ctx.env ctx)
      ~tool_dir:(Ctx.tool_dir ctx)
      ~agent:(Some (ctx, run_agent))
      decl
  with
  | Ok functions -> functions
  | Error error -> failwithf "%s: %s" error.Native_error.code error.message ()
;;

let of_declaration_without_inference ?shell_registry ~sw ~host decl =
  of_declaration_with
    ?shell_registry
    ~host
    ~sw
    ~env:host.Shell_runtime.Host.env
    ~tool_dir:host.tool_dir
    ~agent:None
    decl
;;
