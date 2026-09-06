open Core
module CM = Prompt.Chat_markdown
module Moderation = Moderation
module Moderator_manager = Moderator_manager
module Res = Openai.Responses
module Output = Res.Tool_output.Output
module Stream_moderator = In_memory_stream

(* --------------------------------------------------------------------------- *)
(* Internal helper – record used for keeping track of running tool invocations *)
(* --------------------------------------------------------------------------- *)

type driver_pending_call_kind =
  [ `Function
  | `Custom
  ]

type driver_pending_call =
  { seq : int
  ; call_id : string
  ; kind : driver_pending_call_kind
  ; promise : Openai.Responses.Tool_output.Output.t Eio.Promise.or_exn
  }

type appended_call =
  { seq : int
  ; kind : driver_pending_call_kind
  }

let now_ms (env : Eio_unix.Stdenv.base) : int =
  Eio.Time.now (Eio.Stdenv.clock env) *. 1000. |> Int.of_float
;;

let has_script elements =
  List.exists elements ~f:(function
    | CM.Script _ -> true
    | _ -> false)
;;

let has_end_session_request requests =
  Option.is_some (Runtime_semantics.should_end_session requests)
;;

let fetch_prompt ~ctx ~prompt ~is_local =
  try
    let xml = Fetch.get ~ctx prompt ~is_local in
    let prompt_dir = if is_local then Fetch.resolve_local_dir ~ctx prompt else None in
    Ok (xml, prompt_dir)
  with
  | exn -> Error (Exn.to_string exn)
;;

let capabilities_with_model_executor
      ~(model_executor : Model_executor.t)
      ~(session_id : string)
      (capabilities : Moderation.Capabilities.t)
  : Moderation.Capabilities.t
  =
  { capabilities with
    model_recipes =
      Map.of_alist_exn
        (module String)
        [ ( Model_executor.agent_prompt_v1_name
          , Model_executor.recipe_agent_prompt_v1 model_executor ~session_id )
        ]
  }
;;

let create_moderator
      ~env
      ~session_id
      ~elements
      ~history
      ~available_tools
      ?(capabilities = Moderation.Capabilities.default)
      ?(runtime_policy = Runtime_semantics.default_policy)
      ?on_process_run
      ()
  : (Stream_moderator.moderator option, string) result
  =
  let open Result.Let_syntax in
  let%bind _, artifact =
    Moderator_manager.Registry.of_elements Moderator_manager.Registry.empty elements
  in
  match artifact with
  | None -> Ok None
  | Some artifact ->
    let%bind manager =
      Moderator_manager.create ~artifact ~capabilities ?on_process_run ()
    in
    let moderator =
      Stream_moderator.{ manager; session_id; session_meta = `Null; runtime_policy }
    in
    let now_ms = now_ms env in
    let%bind outcome =
      Moderator_manager.handle_event
        manager
        ~session_id
        ~now_ms
        ~history
        ~available_tools
        ~session_meta:`Null
        ~event:Moderation.Event.Session_start
    in
    let%bind drained =
      if has_end_session_request outcome.runtime_requests
      then Ok []
      else
        Moderator_manager.drain_internal_events
          manager
          ~session_id
          ~now_ms
          ~history
          ~available_tools
          ~session_meta:`Null
    in
    let requests =
      List.concat_map (outcome :: drained) ~f:(fun outcome -> outcome.runtime_requests)
    in
    List.iter requests ~f:(fun request ->
      Log.emit
        `Info
        (Printf.sprintf
           "Ignoring moderator runtime request in shared driver: %s"
           (Sexp.to_string_hum ([%sexp_of: Moderation.Runtime_request.t] request))));
    Ok (Some moderator)
;;

let create_moderator_entries
      ~env
      ~session_id
      ~elements
      ~allocator
      ~history
      ~available_tools
      ?(capabilities = Moderation.Capabilities.default)
      ?(runtime_policy = Runtime_semantics.default_policy)
      ?on_process_run
      ()
  =
  let open Result.Let_syntax in
  let%bind _, artifact =
    Moderator_manager.Registry.of_elements Moderator_manager.Registry.empty elements
  in
  match artifact with
  | None -> Ok None
  | Some artifact ->
    let%bind manager =
      Moderator_manager.create_entries
        ~artifact
        ~capabilities
        ~allocator
        ?on_process_run
        ()
    in
    let moderator =
      Stream_moderator.{ manager; session_id; session_meta = `Null; runtime_policy }
    in
    let now_ms = now_ms env in
    let%bind outcome =
      Moderator_manager.handle_event_entries
        manager
        ~session_id
        ~now_ms
        ~history
        ~available_tools
        ~session_meta:`Null
        ~event:Moderation.Event.Session_start
    in
    let%bind drained =
      if has_end_session_request outcome.runtime_requests
      then Ok []
      else
        Moderator_manager.drain_internal_events_entries
          manager
          ~session_id
          ~now_ms
          ~history
          ~available_tools
          ~session_meta:`Null
    in
    let requests =
      List.concat_map (outcome :: drained) ~f:(fun outcome -> outcome.runtime_requests)
    in
    List.iter requests ~f:(fun request ->
      Log.emit
        `Info
        (Printf.sprintf
           "Ignoring moderator runtime request in shared driver: %s"
           (Sexp.to_string_hum ([%sexp_of: Moderation.Runtime_request.t] request))));
    Ok (Some moderator)
;;

let render_tool_output_text (output : Res.Tool_output.Output.t) =
  match output with
  | Text text -> text
  | Content parts ->
    parts
    |> List.map ~f:(function
      | Res.Tool_output.Output_part.Input_text { text } -> text
      | Input_image { image_url; _ } -> Printf.sprintf "<image src=\"%s\" />" image_url)
    |> String.concat ~sep:"\n"
;;

let append_output_message ~append (o : Res.Output_message.t) =
  let phase_input =
    match o.phase with
    | None -> ""
    | Some p -> Printf.sprintf " phase=\"%s\"" p
  in
  append
    (Printf.sprintf
       "<assistant id=\"%s\" status=\"%s\"%s>\n\t%s|\n\t\t%s\n\t|%s\n</assistant>\n"
       o.id
       o.status
       phase_input
       "RAW"
       (Fetch.tab_on_newline
          (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" "))
       "RAW")
;;

let append_reasoning ~append (r : Res.Reasoning.t) =
  let summaries =
    List.map r.summary ~f:(fun s ->
      Printf.sprintf "\n<summary>\n\t\t%s\n</summary>\n" (Fetch.tab_on_newline s.text))
    |> String.concat ~sep:""
  in
  append
    (Printf.sprintf
       "\n<reasoning id=\"%s\">\n%s\n</reasoning>\n"
       r.id
       (Fetch.tab_on_newline summaries))
;;

let append_tool_call
      ~append
      ~save_doc
      ~show_tool_call
      ~(seq : int)
      ~(kind : driver_pending_call_kind)
      ~(name : string)
      ~(payload : string)
      ~(call_id : string)
      ~(item_id : string option)
  =
  let item_id = Option.value item_id ~default:call_id in
  let tool_call_url id = Printf.sprintf "%i.tool-call.%s.json" seq id in
  let type_attr =
    match kind with
    | `Function -> ""
    | `Custom -> " type=\"custom_tool_call\""
  in
  if show_tool_call
  then
    append
      (Printf.sprintf
         "\n\
          <tool_call%s tool_call_id=\"%s\" function_name=\"%s\" id=\"%s\">\n\
          \t%s|\n\
          \t\t%s\n\
          \t|%s\n\
          </tool_call>\n"
         type_attr
         call_id
         name
         item_id
         "RAW"
         (Fetch.tab_on_newline payload)
         "RAW")
  else (
    let content =
      Printf.sprintf "<doc src=\"./.chatmd/%s\" local>" (tool_call_url call_id)
    in
    append
      (Printf.sprintf
         "\n\
          <tool_call%s tool_call_id=\"%s\" function_name=\"%s\" id=\"%s\">\n\
          \t%s\n\
          </tool_call>\n"
         type_attr
         call_id
         name
         item_id
         (Fetch.tab_on_newline content));
    save_doc (tool_call_url call_id) payload)
;;

let append_tool_output
      ~append
      ~save_doc
      ~show_tool_call
      ~(seq : int)
      ~(kind : driver_pending_call_kind)
      ~(call_id : string)
      ~(output : Res.Tool_output.Output.t)
  =
  let result = render_tool_output_text output in
  let tool_call_result_url id = Printf.sprintf "%i.tool-call-result.%s.json" seq id in
  let type_attr =
    match kind with
    | `Function -> ""
    | `Custom -> " type=\"custom_tool_call\""
  in
  if show_tool_call
  then
    append
      (Printf.sprintf
         "\n\
          <tool_response%s tool_call_id=\"%s\" id=\"%d\">\n\
          \t%s|\n\
          \t\t%s\n\
          \t|%s\n\
          </tool_response>\n"
         type_attr
         call_id
         seq
         "RAW"
         (Fetch.tab_on_newline result)
         "RAW")
  else (
    let content =
      Printf.sprintf "<doc src=\"./.chatmd/%s\" local>" (tool_call_result_url call_id)
    in
    append
      (Printf.sprintf
         "\n<tool_response%s tool_call_id=\"%s\" id=\"%d\">\n\t%s\n</tool_response>\n"
         type_attr
         call_id
         seq
         (Fetch.tab_on_newline content));
    save_doc (tool_call_result_url call_id) result)
;;

let append_generated_items ~append ~save_doc ~show_tool_call items =
  let calls = Hashtbl.create (module String) in
  let next_seq = ref 0 in
  List.iter items ~f:(function
    | Res.Item.Output_message o -> append_output_message ~append o
    | Res.Item.Reasoning r -> append_reasoning ~append r
    | Res.Item.Function_call fc ->
      append_tool_call
        ~append
        ~save_doc
        ~show_tool_call
        ~seq:!next_seq
        ~kind:`Function
        ~name:fc.name
        ~payload:fc.arguments
        ~call_id:fc.call_id
        ~item_id:fc.id;
      Hashtbl.set calls ~key:fc.call_id ~data:{ seq = !next_seq; kind = `Function };
      Int.incr next_seq
    | Res.Item.Custom_tool_call tc ->
      append_tool_call
        ~append
        ~save_doc
        ~show_tool_call
        ~seq:!next_seq
        ~kind:`Custom
        ~name:tc.name
        ~payload:tc.input
        ~call_id:tc.call_id
        ~item_id:tc.id;
      Hashtbl.set calls ~key:tc.call_id ~data:{ seq = !next_seq; kind = `Custom };
      Int.incr next_seq
    | Res.Item.Function_call_output out ->
      Option.iter (Hashtbl.find calls out.call_id) ~f:(fun { seq; kind } ->
        append_tool_output
          ~append
          ~save_doc
          ~show_tool_call
          ~seq
          ~kind
          ~call_id:out.call_id
          ~output:out.output)
    | Res.Item.Custom_tool_call_output out ->
      Option.iter (Hashtbl.find calls out.call_id) ~f:(fun { seq; kind } ->
        append_tool_output
          ~append
          ~save_doc
          ~show_tool_call
          ~seq
          ~kind
          ~call_id:out.call_id
          ~output:out.output)
    | Res.Item.Input_message _ | Res.Item.Web_search_call _ | Res.Item.File_search_call _
      -> ())
;;

(*********************************************************************
  Driver – high-level entry points exposed to CLI & tools
  -------------------------------------------------------

  The **Driver** is the orchestration layer that turns a ChatMarkdown
  document (on disk) into successive calls to the OpenAI API.  It
  exposes two families of helpers:

  • Blocking completions – {!run_completion} (single request) and
    {!run_agent} (nested agent usage).
  • Streaming completions – {!run_completion_stream} which allows TUIs
    and web front-ends to render partial deltas in real time.

  The implementation is necessarily long because it wires together:

  * Prompt preprocessing (append template, ensure `<user>` skeleton…)
  * Configuration extraction (model, temperature, reasoning…)
  * Tool discovery – built-ins, user declared, MCP remote
  * Response loop (streaming vs blocking)
  * Rendering of assistant answers back into the `.chatmd` buffer

  Nevertheless the public API remains small and should stay backward
  compatible: customise behaviour by tweaking optional labelled
  arguments rather than editing the implementation.
**********************************************************************)

(*********************************************************************
  Driver – public API for ChatMarkdown completion
  ------------------------------------------------

  The **Driver** exposes two convenience wrappers that behave similarly
  to the OpenAI *chat completions* endpoint but accept ChatMarkdown as
  input.  They bundle parsing, tool wiring, caching and the recursive
  response loop into a single call so that CLI utilities and the TUI do
  not have to care about the underlying plumbing.

  • {!run_completion} – synchronous, single-turn helper; good for unit
    tests or scripts that only need the final assistant answer.
  • {!run_completion_stream} – streaming flavour; yields
    [`Response_stream.t`] events as they arrive and exposes incremental
    updates to the caller via the [on_event] callback.

  Both functions maintain an *output buffer* on disk (a `.chatmd` file)
  so that long-running conversations survive restarts and can be edited
  manually between turns.
**********************************************************************)

(*********************************************************************
  Driver – end-to-end helpers used by the CLI
  -------------------------------------------

  The driver is the *glue* that takes a user-editable ChatMarkdown file
  on disk (e.g. `conversation.chatmd`), feeds it to the converter, loops
  until the model has delivered its answer and finally appends the
  assistant reply back into the same file.

  There are two entry points:

  • {!run_completion} – blocking variant, returns only when the final
    assistant message has been produced.
  • {!run_completion_stream} – streaming version used by the TUI; emits
    incremental events via an `on_event` callback.

  The rest of the module is pure plumbing: reading/writing files,
  resolving configuration blocks, preparing tool tables and delegating
  to {!Response_loop.run}.
**********************************************************************)

(** Driver – interactive and batch helpers

    The *driver* is the glue between filesystem documents ( `.chatmd`
    files edited by users) and the backend components (Converter,
    Response_loop, Fork, …).  Two high-level entry points are exposed:

    • {!run_completion} – synchronous loop suitable for scripting or
      tests.
    • {!run_completion_stream} – streaming variant that produces
      incremental events consumed by the TUI.

    Both helpers keep the following invariants:

    * A persistent cache lives under `~/.chatmd/cache.bin` so that agent
      prompts and MCP metadata survive across runs.
    * All tool-generated artefacts are written next to the working
      document to make manual inspection easy.
*)

(** [run_agent ?history_compaction ~ctx prompt_xml items] evaluates a *nested agent* inside
    the currently running conversation.

    The function treats [prompt_xml] as a standalone ChatMarkdown
    document representing the agent’s system prompt and optional
    configuration blocks.  The additional [items] (typically
    user-supplied content inserted at runtime) are appended before the
    request is sent to OpenAI.

    Workflow:

    1. Merge [prompt_xml] and [items] into a single XML buffer and
       re-parse it using {!Prompt.Chat_markdown.parse_chat_inputs}.
    2. Derive configuration (model, temperature, reasoning, …) from the
       embedded `<config/>` block; defaults mirror {!run_completion}.
    3. Discover and instantiate tools declared inside the agent prompt
       (via {!Tool.of_declaration}).
    4. Convert the prompt to [`Item.t`] values with {!Converter.to_items}
       and delegate execution to {!Response_loop.run}, which resolves
       function calls recursively.
    5. Concatenate *assistant* messages produced after the initial
       request and return them as a single string.

    This helper is primarily used by the *fork* tool to let the model
    spawn sub-agents without leaving the main conversation context. *)

type agent_observer = Agent_response_loop.observer =
  { on_event : Res.Response_stream.t -> unit
  ; on_tool_execution : Tool_execution_event.t -> unit
  }

let agent_allocator_namespace =
  let next = Atomic.make 0 in
  fun session_id ->
    let sequence = Atomic.fetch_and_add next 1 in
    Printf.sprintf "%s/agent-%d" session_id sequence
;;

let create_history_entries ~allocator items =
  List.map items ~f:(History_entry.create ~allocator)
  |> Result.all
  |> Result.ok_or_failwith
;;

let extension_entries ~prefix entries =
  let prefix_length = List.length prefix in
  let retained, extension = List.split_n entries prefix_length in
  let is_same_prefix =
    List.equal
      (fun left right ->
         History_entry.Id.equal (History_entry.id left) (History_entry.id right))
      prefix
      retained
  in
  if is_same_prefix
  then extension
  else failwith "Response execution did not preserve the initial history prefix"
;;

let run_entries
      ~ctx
      ~allocator
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ?history_compaction
      ?response_dir
      ?observer
      ?on_sourced_event
      ?source
      ?parent_call_id
      ?post
      ?post_stream
      ~model
      ~tool_tbl
      history
  =
  match observer with
  | None ->
    Response_loop.run_entries
      ~ctx
      ~allocator
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ?history_compaction
      ?response_dir
      ?post
      ~model
      ~tool_tbl
      history
  | Some observer ->
    Agent_response_loop.run_entries
      ~ctx
      ~allocator
      ?temperature
      ?max_output_tokens
      ?tools
      ?reasoning
      ?history_compaction
      ?response_dir
      ?on_sourced_event
      ?source
      ?parent_call_id
      ~model
      ~tool_tbl
      ~observer
      ?post_stream
      history
;;

let rec run_agent
          ?(history_compaction = false)
          ?prompt_dir
          ?session_id
          ?response_dir
          ?observer
          ?(on_sourced_event = fun _ -> ())
          ?source
          ?parent_call_id
          ?(shell_manifest_authorizer = Shell_runtime.Manifest_authorizer.deny)
          ?(shell_approval_provider = Shell_runtime.Approval_broker.None_available)
          ~(ctx : _ Ctx.t)
          (prompt_xml : string)
          (items : CM.content_item list)
  : string
  =
  Eio.Switch.run
  @@ fun sw ->
  (* 1.  Extract individual components from the shared context *)
  let dir = Option.value prompt_dir ~default:(Ctx.dir ctx) in
  let response_dir = Option.value response_dir ~default:(Ctx.dir ctx) in
  let ctx =
    Ctx.create ~env:(Ctx.env ctx) ~dir ~tool_dir:(Ctx.tool_dir ctx) ~cache:(Ctx.cache ctx)
  in
  (* 1.  Build the full agent XML by adding any inline user items. *)
  let msg =
    CM.User
      { role = "user"
      ; content = Some (Items items)
      ; name = None
      ; id = None
      ; status = None
      ; phase = None
      ; ochat_history_id = None
      ; source_context = None
      ; function_call = None
      ; tool_call = None
      ; tool_call_id = None
      ; type_ = None
      }
  in
  (* 2.  Parse the merged document into structured elements. *)
  let prompt_source = Option.value session_id ~default:"<nested-agent>" in
  let elements = CM.parse_chat_inputs ~source:prompt_source ~dir prompt_xml in
  (* 3.  Configuration (max_tokens, model, …) *)
  let cfg = Config.of_elements elements in
  let CM.{ max_tokens; model; reasoning_effort; temperature; show_tool_call = _; id } =
    cfg
  in
  let model =
    Option.value_map model ~default:Res.Request.Gpt4 ~f:Res.Request.model_of_str_exn
  in
  let reasoning =
    Option.map reasoning_effort ~f:(fun eff ->
      Res.Request.Reasoning.
        { effort = Some (Effort.of_str_exn eff); summary = Some Summary.Detailed })
  in
  let runtime_session_id =
    Option.first_some id session_id |> Option.value ~default:"nested-agent"
  in
  let host =
    Agent_runtime.host
      ~env:(Ctx.env ctx)
      ~workspace:(Ctx.tool_dir ctx)
      ~tool_dir:(Ctx.tool_dir ctx)
      ~prompt_dir:dir
      ~session_dir:response_dir
      ~cache_dir:response_dir
      ~home:(Agent_runtime.default_home (Ctx.env ctx))
      ~session_id:runtime_session_id
      ~resource_runner:(Sys.getenv "OCHAT_SHELL_RESOURCE_RUNNER")
      ~prompt_elements:elements
    |> Result.map_error ~f:(fun diagnostics ->
      List.map diagnostics ~f:Agent_runtime.diagnostic_to_string
      |> String.concat ~sep:"\n")
    |> Result.ok_or_failwith
  in
  let agent_runtime =
    Agent_runtime.create
      ~sw
      ~ctx
      ~host
      ~platform:(Agent_runtime.platform ())
      ~prompt_elements:elements
      ~manifest_authorizer:shell_manifest_authorizer
      ~approval_provider:shell_approval_provider
      ~approval_store:(Shell_access.Approval.create_store ())
      ~run_agent:(fun ?prompt_dir ?session_id ?observer ~source ~ctx prompt items ->
        run_agent
          ~history_compaction
          ?prompt_dir
          ?session_id
          ?observer
          ~source
          ~response_dir
          ~shell_manifest_authorizer
          ~shell_approval_provider
          ~ctx
          prompt
          items)
      ()
    |> Result.map_error ~f:(fun diagnostics ->
      List.map diagnostics ~f:Agent_runtime.diagnostic_to_string
      |> String.concat ~sep:"\n")
    |> Result.ok_or_failwith
  in
  let tools = agent_runtime.functions in
  let comp_tools, tool_tbl = Ochat_function.functions tools in
  let tools_req = Tool.convert_tools comp_tools in
  (* 5.  Convert XML ‑> API items and enter the execute loop to handle function calls. *)
  let init_items =
    Converter.to_items
      ~ctx
      ~run_agent:(fun ?prompt_dir ?session_id ~ctx prompt items ->
        run_agent
          ~history_compaction
          ?prompt_dir
          ?session_id
          ~response_dir
          ~shell_manifest_authorizer
          ~shell_approval_provider
          ~ctx
          prompt
          items)
      (elements @ [ msg ])
  in
  let all_items =
    if has_script elements
    then (
      let session_id =
        Option.first_some id session_id |> Option.value ~default:"nested-agent"
      in
      let exec_context : Model_executor.exec_context =
        { ctx
        ; run_agent =
            (fun ?history_compaction ?prompt_dir ?session_id ~ctx prompt items ->
              run_agent
                ?history_compaction
                ?prompt_dir
                ?session_id
                ~response_dir
                ~shell_manifest_authorizer
                ~shell_approval_provider
                ~ctx
                prompt
                items)
        ; fetch_prompt
        }
      in
      let model_executor = Model_executor.create ~sw ~exec_context () in
      let capabilities =
        capabilities_with_model_executor
          ~model_executor
          ~session_id
          Moderation.Capabilities.default
      in
      let allocator =
        let namespace =
          match source with
          | Some source -> session_id ^ "/" ^ source
          | None -> agent_allocator_namespace session_id
        in
        History_entry.Allocator.create ~namespace ~next_sequence:0
        |> Result.ok_or_failwith
      in
      let init_entries = create_history_entries ~allocator init_items in
      let moderator =
        let on_process_run = Agent_runtime.moderator_process_handler agent_runtime in
        create_moderator_entries
          ~env:(Ctx.env ctx)
          ~session_id
          ~elements
          ~allocator
          ~history:init_entries
          ~available_tools:tools_req
          ~capabilities
          ?on_process_run
          ()
        |> Result.ok_or_failwith
      in
      Option.iter moderator ~f:(fun (m : In_memory_stream.moderator) ->
        Model_executor.register_session
          model_executor
          ~session_id:m.session_id
          ~manager:m.manager);
      In_memory_stream.run_completion_stream_in_memory_entries
        ~env:(Ctx.env ctx)
        ~datadir:response_dir
        ~allocator
        ~history:init_entries
        ~tools:(Some tools_req)
        ~tool_tbl
        ?temperature
        ?max_output_tokens:max_tokens
        ?reasoning
        ?moderator
        ?on_event:(Option.map observer ~f:(fun observer -> observer.on_event))
        ~on_sourced_event
        ?source
        ?parent_call_id
        ?on_tool_execution:
          (Option.map observer ~f:(fun observer -> observer.on_tool_execution))
        ~history_compaction
        ~parallel_tool_calls:true
        ~model
        ()
      |> History_entry.items)
    else (
      let session_id =
        Option.first_some id session_id |> Option.value ~default:"nested-agent"
      in
      let allocator =
        let namespace =
          match source with
          | Some source -> session_id ^ "/" ^ source
          | None -> agent_allocator_namespace session_id
        in
        History_entry.Allocator.create ~namespace ~next_sequence:0
        |> Result.ok_or_failwith
      in
      let init_entries = create_history_entries ~allocator init_items in
      let all_entries =
        run_entries
          ~ctx
          ~allocator
          ?temperature
          ?max_output_tokens:max_tokens
          ~tools:tools_req
          ?reasoning
          ~history_compaction
          ~response_dir
          ?observer
          ~on_sourced_event
          ?source
          ?parent_call_id
          ~model
          ~tool_tbl
          init_entries
      in
      extension_entries ~prefix:init_entries all_entries |> History_entry.items)
  in
  (* 6.  Extract assistant messages and concatenate them. *)
  (if has_script elements then List.drop all_items (List.length init_items) else all_items)
  |> List.filter_map ~f:(function
    | Res.Item.Output_message o ->
      Some (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
    | _ -> None)
  |> String.concat ~sep:"\n"
;;

(*──────────────────────── 6.  Main driver  ───────────────────────────────*)

(*──────────────────────── 7.  Public helper  ─────────────────────────────*)
(** [run_completion ~env ?prompt_file ~output_file ()] runs a complete
    ChatMarkdown turn in **blocking** mode.

    The helper:

    • Optionally prepends [prompt_file] – typically a template – to the
      ongoing conversation stored in [output_file].
    • Parses the resulting XML buffer, extracts configuration, declared
      tools and user messages.
    • Submits the conversation to OpenAI and recursively resolves any
      tool calls until the model produces a purely textual answer.
    • Appends assistant messages, reasoning blocks and tool-call
      artefacts back into [output_file], making the document
      self-contained.
    • Inserts an empty `<user>` block at the end so that the next human
      edit has a placeholder.

    Persistent state:

    • A cache is stored in `~/.chatmd/cache.bin` (created with
      {!Io.ensure_chatmd_dir}).
    • Tool-generated artefacts (JSON arguments, scraped web pages, …)
      are written next to [output_file] for easy inspection.

    Example – minimal CLI-style invocation:
    {[
      Eio_main.run @@ fun env ->
        Driver.run_completion
          ~env
          ~output_file:"conversation.chatmd"
          ()
    ]} *)
let run_completion
      ~env
      ?prompt_file
      ?(parallel_tool_calls = true)
      ?(meta_refine = false)
      ~output_file
      ()
  =
  if meta_refine then Caml_unix.putenv "OCHAT_META_REFINE" "1";
  (* [run_completion ~env ?prompt_file ~output_file ()] enters a
     read-eval-append loop on [output_file].  Each iteration:

     1. Parses the XML buffer into ChatMarkdown elements.
     2. Converts them to OpenAI items via {!Converter}.
     3. Runs {!Response_loop.run} until no pending function calls.
     4. Appends the assistant answer (and reasoning) back to
        [output_file].  *)
  (* Synchronous execution path does not yet thread the flag further –
     discard it to avoid a warning until Task 4 refactors the loop. *)
  let _parallel_tool_calls = parallel_tool_calls in
  let cwd = Eio.Stdenv.cwd env in
  (* Directory of the ChatMarkdown buffer on disk.  We use it as the base when
     resolving relative paths that originate from the prompt itself. *)
  let output_dir : _ Eio.Path.t =
    let dirname = Filename.dirname output_file in
    if Filename.is_relative dirname
    then Eio.Path.(cwd / dirname)
    else Eio.Path.(Eio.Stdenv.fs env / dirname)
  in
  (* All IO on [output_file] uses [cwd] so that relative paths behave like a
     regular shell. *)
  let dir = cwd in
  (* Ensure the hidden data directory exists and get its path. *)
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1000 () in
  (* 1 •append initial prompt file if provided *)
  Option.iter prompt_file ~f:(fun file ->
    Io.append_doc ~dir output_file (Io.load_doc ~dir file));
  (* 2 • main loop *)
  let rec loop () =
    let xml = Io.load_doc ~dir output_file in
    (* Parse ChatMarkdown with [output_dir] as the base for resolving      *)
    (* <import/> or other file-relative constructs inside the prompt.       *)
    let elements = CM.parse_chat_inputs ~dir:output_dir xml in
    (* gather config *)
    let cfg = Config.of_elements elements in
    let CM.
          { max_tokens = model_tokens
          ; model = model_opt
          ; reasoning_effort
          ; temperature
          ; id
          ; _
          }
      =
      cfg
    in
    let reasoning =
      Option.map reasoning_effort ~f:(fun eff ->
        { Res.Request.Reasoning.effort =
            Some (Res.Request.Reasoning.Effort.of_str_exn eff)
        ; summary = Some Detailed
        })
    in
    let model =
      Option.value_map model_opt ~default:Res.Request.Gpt4 ~f:Res.Request.model_of_str_exn
    in
    (* convert xml → items and fire first request *)
    let ctx = Ctx.create ~env ~dir:output_dir ~cache ~tool_dir:dir in
    (* tools / function mapping *)
    let builtin_fns =
      [ Functions.webpage_to_markdown
          ~env:(Ctx.env ctx)
          ~dir:(Ctx.tool_dir ctx)
          ~net:(Ctx.net ctx)
      ; Functions.fork
      ]
    in
    let comp_tools, tool_tbl = Ochat_function.functions builtin_fns in
    let tools = Tool.convert_tools comp_tools in
    (* Reuse earlier [ctx] for conversion to items. *)
    let init_items =
      Converter.to_items
        ~ctx
        ~run_agent:(fun ?prompt_dir ?session_id ~ctx prompt items ->
          run_agent
            ~history_compaction:false
            ?prompt_dir
            ?session_id
            ~response_dir:datadir
            ~ctx
            prompt
            items)
        elements
    in
    let append = Io.append_doc ~dir output_file in
    let save_doc name contents = Io.save_doc ~dir:datadir name contents in
    if has_script elements
    then (
      let session_id = Option.value id ~default:output_file in
      let runtime_requests = ref [] in
      let all_items =
        Eio.Switch.run
        @@ fun sw ->
        let exec_context : Model_executor.exec_context =
          { ctx
          ; run_agent =
              (fun ?history_compaction ?prompt_dir ?session_id ~ctx prompt items ->
                run_agent
                  ?history_compaction
                  ?prompt_dir
                  ?session_id
                  ~response_dir:datadir
                  ~ctx
                  prompt
                  items)
          ; fetch_prompt
          }
        in
        let model_executor = Model_executor.create ~sw ~exec_context () in
        let capabilities =
          capabilities_with_model_executor
            ~model_executor
            ~session_id
            Moderation.Capabilities.default
        in
        let allocator =
          History_entry.Allocator.create
            ~namespace:(session_id ^ "/blocking")
            ~next_sequence:0
          |> Result.ok_or_failwith
        in
        let init_entries = create_history_entries ~allocator init_items in
        let moderator =
          create_moderator_entries
            ~env
            ~session_id
            ~elements
            ~allocator
            ~history:init_entries
            ~available_tools:tools
            ~capabilities
            ()
          |> Result.ok_or_failwith
        in
        Option.iter moderator ~f:(fun (m : In_memory_stream.moderator) ->
          Model_executor.register_session
            model_executor
            ~session_id:m.session_id
            ~manager:m.manager);
        In_memory_stream.run_completion_stream_in_memory_entries
          ~env
          ~datadir
          ~allocator
          ~history:init_entries
          ~tools:(Some tools)
          ~tool_tbl
          ?temperature
          ?max_output_tokens:model_tokens
          ?reasoning
          ?moderator
          ~on_runtime_request:(fun request ->
            runtime_requests := request :: !runtime_requests)
          ~parallel_tool_calls
          ~meta_refine
          ~model
          ()
        |> extension_entries ~prefix:init_entries
        |> History_entry.items
      in
      append_generated_items ~append ~save_doc ~show_tool_call:true all_items;
      if not (has_end_session_request !runtime_requests) then append "\n<user>\n\n</user>")
    else (
      (* For the response loop we use a context bound to the .chatmd data folder so
         that any tool-generated artefacts land in that directory. *)
      let ctx_loop = Ctx.create ~env ~dir:datadir ~cache ~tool_dir:datadir in
      let allocator =
        History_entry.Allocator.create
          ~namespace:(Option.value id ~default:output_file ^ "/blocking")
          ~next_sequence:0
        |> Result.ok_or_failwith
      in
      let init_entries = create_history_entries ~allocator init_items in
      let all_entries =
        run_entries
          ~ctx:ctx_loop
          ~allocator
          ?temperature
          ?max_output_tokens:model_tokens
          ~tools
          ?reasoning
          ~tool_tbl
          ~model
          init_entries
      in
      let generated =
        extension_entries ~prefix:init_entries all_entries |> History_entry.items
      in
      append_generated_items ~append ~save_doc ~show_tool_call:true generated;
      append "\n<user>\n\n</user>")
  in
  loop ();
  Cache.save ~file:cache_file cache
;;

(** [run_completion_stream ~env ?prompt_file ?on_event ~output_file ()]
    streams assistant deltas and high-level events **as they arrive**.

    Compared to {!run_completion} this variant:

    • Uses the streaming OpenAI API to obtain partial tokens.
    • Invokes [?on_event] for every chunk, letting callers update a TUI
      or web UI in real time.  The default callback ignores events so
      existing scripts remain unchanged.
    • Executes tool calls as soon as they are fully parsed, then
      continues streaming the response.

    Side-effects mirror {!run_completion}: partial messages and
    reasoning summaries are appended to [output_file] immediately so
    the buffer is crash-resistant.

    Example – live rendering in the terminal:
    {[
      let on_event = function
        | Responses.Response_stream.Output_text_delta d ->
            Out_channel.output_string stdout d.delta
        | _ -> ()

      Eio_main.run @@ fun env ->
        Driver.run_completion_stream
          ~env
          ~output_file:"conversation.chatmd"
          ~on_event
          ()
    ]} *)
let run_completion_stream
      ~env
      ?prompt_file
      ?(on_event : Openai.Responses.Response_stream.t -> unit = fun _ -> ())
      ?(on_history_event : History_stream_event.t -> unit = fun _ -> ())
      ?(on_sourced_event : Sourced_response_event.t -> unit = fun _ -> ())
      ?(on_history_tool_out : History_entry.t -> unit = fun _ -> ())
      ?post_stream
      ?(on_final_history : History_entry.t list -> unit = fun _ -> ())
      ?(parallel_tool_calls = true)
      ?(meta_refine = false)
      ?(history_compaction = false)
      ?(shell_manifest_authorizer = Shell_runtime.Manifest_authorizer.deny)
      ?(shell_approval_provider = Shell_runtime.Approval_broker.None_available)
      ~output_file
      ()
  =
  if meta_refine then Caml_unix.putenv "OCHAT_META_REFINE" "1";
  Eio.Switch.run
  @@ fun sw ->
  (* ─────────────────────── 0.  setup & helpers ───────────────────────── *)
  let _ = parallel_tool_calls in
  let cwd = Eio.Stdenv.cwd env in
  (* Base directory of the ChatMarkdown buffer (prompt). *)
  let output_dir : _ Eio.Path.t =
    let dirname = Filename.dirname output_file in
    if Filename.is_relative dirname
    then Eio.Path.(cwd / dirname)
    else Eio.Path.(Eio.Stdenv.fs env / dirname)
  in
  (* [dir] is used for regular file IO relative to the user’s shell. *)
  let dir = cwd in
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let net = env#net in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1_000 () in
  let append_doc = Io.append_doc ~dir output_file in
  Option.iter prompt_file ~f:(fun file -> append_doc (Io.load_doc ~dir file));
  (* Pretty logger: every event – even if we do not act on it *)
  let log_event _ev =
    (* print_endline "STREAM EVENT:";
    print_endline (Jsonaf.to_string_hum (Res.Response_stream.jsonaf_of_t ev)) *)
    ()
  in
  let fn_id = ref 0 in
  (* 1‑A • read current prompt XML and parse *)
  let xml =
    if String.equal output_file "/dev/stdout"
    then
      Io.load_doc ~dir
      @@ Option.value_exn
           prompt_file
           ~message:"No output file specified, cannot run in streaming mode."
    else Io.load_doc ~dir output_file
  in
  (* ─────────────────────── 1.  main recursive turn ────────────────────── *)
  (* 1‑B • parse the XML into ChatMarkdown elements *)
  (* Use [output_dir] as the base for <import/> and local document paths
     inside the prompt. *)
  let elements = CM.parse_chat_inputs ~source:output_file ~dir:output_dir xml in
  (* 1‑B • current config (max_tokens, model, …) *)
  let cfg = Config.of_elements elements in
  let CM.{ max_tokens; model; reasoning_effort; temperature; show_tool_call; id } = cfg in
  let model =
    Option.value_map model ~f:Res.Request.model_of_str_exn ~default:Res.Request.Gpt4
  in
  let reasoning =
    Option.map reasoning_effort ~f:(fun eff ->
      Res.Request.Reasoning.
        { effort = Some (Effort.of_str_exn eff); summary = Some Summary.Detailed })
  in
  (* Execution context anchored at the prompt directory – ensures that any
     relative paths in <doc src="…">, <import>, or nested agent prompts are
     resolved against the folder that contains [output_file]. *)
  let ctx = Ctx.create ~env ~dir:output_dir ~cache ~tool_dir:(Eio.Stdenv.cwd env) in
  let runtime_session_id = Option.value id ~default:output_file in
  let host =
    Agent_runtime.host
      ~env
      ~workspace:cwd
      ~tool_dir:(Ctx.tool_dir ctx)
      ~prompt_dir:output_dir
      ~session_dir:datadir
      ~cache_dir:datadir
      ~home:(Agent_runtime.default_home env)
      ~session_id:runtime_session_id
      ~resource_runner:(Sys.getenv "OCHAT_SHELL_RESOURCE_RUNNER")
      ~prompt_elements:elements
    |> Result.map_error ~f:(fun diagnostics ->
      List.map diagnostics ~f:Agent_runtime.diagnostic_to_string
      |> String.concat ~sep:"\n")
    |> Result.ok_or_failwith
  in
  let agent_runtime =
    Agent_runtime.create
      ~sw
      ~ctx
      ~host
      ~platform:(Agent_runtime.platform ())
      ~prompt_elements:elements
      ~manifest_authorizer:shell_manifest_authorizer
      ~approval_provider:shell_approval_provider
      ~approval_store:(Shell_access.Approval.create_store ())
      ~run_agent:(fun ?prompt_dir ?session_id ?observer ~source ~ctx prompt items ->
        run_agent
          ~history_compaction
          ?prompt_dir
          ?session_id
          ?observer
          ~source
          ~response_dir:datadir
          ~shell_manifest_authorizer
          ~shell_approval_provider
          ~ctx
          prompt
          items)
      ()
    |> Result.map_error ~f:(fun diagnostics ->
      List.map diagnostics ~f:Agent_runtime.diagnostic_to_string
      |> String.concat ~sep:"\n")
    |> Result.ok_or_failwith
  in
  (* 1-C • tools / functions – only tools declared by user *)
  let comp_tools, tool_tbl = Ochat_function.functions agent_runtime.functions in
  let tools = Tool.convert_tools comp_tools in
  (* 1-D • initial request items *)
  let inputs =
    Converter.to_items
      ~ctx
      ~run_agent:(fun ?prompt_dir ?session_id ~ctx prompt items ->
        run_agent
          ~history_compaction
          ?prompt_dir
          ?session_id
          ~response_dir:datadir
          ~shell_manifest_authorizer
          ~shell_approval_provider
          ~ctx
          prompt
          items)
      elements
  in
  if has_script elements
  then (
    let save_doc name contents = Io.save_doc ~dir:datadir name contents in
    let session_id = Option.value id ~default:output_file in
    let exec_context : Model_executor.exec_context =
      { ctx
      ; run_agent =
          (fun ?history_compaction ?prompt_dir ?session_id ~ctx prompt items ->
            run_agent
              ?history_compaction
              ?prompt_dir
              ?session_id
              ~response_dir:datadir
              ~shell_manifest_authorizer
              ~shell_approval_provider
              ~ctx
              prompt
              items)
      ; fetch_prompt
      }
    in
    let model_executor = Model_executor.create ~sw ~exec_context () in
    let capabilities =
      capabilities_with_model_executor
        ~model_executor
        ~session_id
        Moderation.Capabilities.default
    in
    let allocator =
      History_entry.Allocator.create ~namespace:(session_id ^ "/stream") ~next_sequence:0
      |> Result.ok_or_failwith
    in
    let input_entries = create_history_entries ~allocator inputs in
    let moderator =
      let on_process_run = Agent_runtime.moderator_process_handler agent_runtime in
      create_moderator_entries
        ~env
        ~session_id
        ~elements
        ~allocator
        ~history:input_entries
        ~available_tools:tools
        ~capabilities
        ?on_process_run
        ()
      |> Result.ok_or_failwith
    in
    Option.iter moderator ~f:(fun (m : In_memory_stream.moderator) ->
      Model_executor.register_session
        model_executor
        ~session_id:m.session_id
        ~manager:m.manager);
    let runtime_requests = ref [] in
    let all_items =
      In_memory_stream.run_completion_stream_in_memory_entries
        ~env
        ~datadir
        ~allocator
        ~history:input_entries
        ~on_event:(fun ev ->
          log_event ev;
          on_event ev)
        ~on_history_event
        ~on_sourced_event
        ~on_history_tool_out
        ~tools:(Some tools)
        ~tool_tbl
        ?temperature
        ?max_output_tokens:max_tokens
        ?reasoning
        ?moderator
        ~on_runtime_request:(fun request ->
          runtime_requests := request :: !runtime_requests)
        ~history_compaction
        ~parallel_tool_calls
        ~meta_refine
        ~model
        ?post_stream
        ()
      |> History_entry.items
    in
    append_generated_items
      ~append:append_doc
      ~save_doc
      ~show_tool_call
      (List.drop all_items (List.length inputs));
    if not (has_end_session_request !runtime_requests)
    then append_doc "\n<user>\n\n</user>")
  else (
    let allocator =
      History_entry.Allocator.create
        ~namespace:(Option.value id ~default:output_file ^ "/stream")
        ~next_sequence:0
      |> Result.ok_or_failwith
    in
    let registry = History_stream_event.Registry.create ~allocator in
    let inputs = create_history_entries ~allocator inputs in
    (* ─────────────────────── 1.  main recursive turn ────────────────────── *)
    let rec turn inputs =
      let scope = History_stream_event.Registry.create_scope registry in
      (* ────────────────── 2.  streaming callback state ─────────────────── *)
      (* existing tables … *)
      let new_items : History_entry.t list ref = ref [] in
      let add_item item =
        let id =
          History_stream_event.Registry.find_item registry ~source:None item ~scope
          |> Option.value_or_thunk ~default:(fun () ->
            History_entry.Allocator.allocate allocator |> Result.ok_or_failwith)
        in
        let entry = History_entry.create_with_id ~id item in
        if
          not
            (List.exists !new_items ~f:(fun existing ->
               History_entry.Id.equal (History_entry.id existing) (History_entry.id entry)))
        then new_items := entry :: !new_items;
        entry
      in
      let opened_msgs : (string, unit) Hashtbl.t = Hashtbl.create (module String)
      and func_info : (string, string * string) Hashtbl.t = Hashtbl.create (module String)
      and function_completions : (string, string) Hashtbl.t =
        Hashtbl.create (module String)
      and custom_completions : (string, string) Hashtbl.t = Hashtbl.create (module String)
      and reasoning_state : (string, int) Hashtbl.t = Hashtbl.create (module String) in
      let run_again = ref false in
      let output_text_delta ~id txt =
        if not (Hashtbl.mem opened_msgs id)
        then (
          append_doc (Printf.sprintf "\n<assistant id=\"%s\">\n\t%s|\n\t\t" id "RAW");
          Hashtbl.set opened_msgs ~key:id ~data:());
        append_doc (Fetch.tab_on_newline txt)
      in
      let close_message id =
        if Hashtbl.mem opened_msgs id
        then (
          append_doc (Printf.sprintf "\n\t|%s\n</assistant>\n" "RAW");
          (* remove the message from the opened list *)
          Hashtbl.remove opened_msgs id)
      in
      let lt, gt = "<", ">" in
      (* avoid raw “<tag>” in the output *)
      let open_reasoning id =
        append_doc
          (Printf.sprintf "\n%sreasoning id=\"%s\"%s\n\t%ssummary%s\n\t\t" lt id gt lt gt)
      in
      let open_new_summary () =
        append_doc (Printf.sprintf "\n\t%ssummary%s\n\t\t" lt gt)
      in
      let close_summary () = append_doc (Printf.sprintf "\n\t%s/summary%s" lt gt) in
      let close_reasoning () = append_doc (Printf.sprintf "\n%s/reasoning%s\n" lt gt) in
      (* -----------------------------------------------------------------
       Parallel execution of tool calls

       When [parallel_tool_calls] is [true], each tool invocation is
       scheduled in its own fiber under [sw].  A shared semaphore
       prevents unbounded concurrency.  The resulting outputs are
       collected and later appended **in the original call order** so
       that the ChatMarkdown document remains deterministic.
       ---------------------------------------------------------------- *)
      (* Semaphore limiting concurrent invocations.  We create it lazily the
       first time a tool call is encountered to avoid the (small) cost
       in turns without any tools. *)
      let sem = lazy (Eio.Semaphore.make 8) in
      (* Accumulates promises for running tool calls. *)
      let pending_calls : driver_pending_call list ref = ref [] in
      let handle_function_done ~item_id ~arguments =
        match Hashtbl.find func_info item_id with
        | None -> () (* should not happen *)
        | Some (name, call_id)
          when List.exists !pending_calls ~f:(fun pending ->
                 String.equal pending.call_id call_id) -> ()
        | Some (name, call_id) ->
          (* Allocate a unique sequence number for deterministic ordering *)
          let seq = !fn_id in
          Int.incr fn_id;
          (* ----------------------------------------------------------------- *)
          (* 1.  Persist the tool_call request into the buffer / disk          *)
          let tool_call_url id = Printf.sprintf "%i.tool-call.%s.json" seq id in
          if show_tool_call
          then
            append_doc
              (Printf.sprintf
                 "\n\
                  <tool_call tool_call_id=\"%s\" function_name=\"%s\" id=\"%s\">\n\
                  \t%s|\n\
                  \t\t%s\n\
                  \t|%s\n\
                  </tool_call>\n"
                 call_id
                 name
                 item_id
                 "RAW"
                 (Fetch.tab_on_newline arguments)
                 "RAW")
          else (
            let content =
              Printf.sprintf "<doc src=\"./.chatmd/%s\" local>" (tool_call_url call_id)
            in
            append_doc
              (Printf.sprintf
                 "\n\
                  <tool_call tool_call_id=\"%s\" function_name=\"%s\" id=\"%s\">\n\
                  \t%s\n\
                  </tool_call>\n"
                 call_id
                 name
                 item_id
                 (Fetch.tab_on_newline content));
            Io.save_doc ~dir:datadir (tool_call_url call_id) arguments);
          (* 2.  Add the function_call item so the model sees the invocation *)
          let fn_call_item =
            Tool_call.call_item
              ~kind:Tool_call.Kind.Function
              ~name
              ~payload:arguments
              ~call_id
              ~id:(Some item_id)
          in
          ignore (add_item fn_call_item : History_entry.t);
          (* 3.  Spawn the actual tool invocation in its own fiber           *)
          let history_entries_so_far =
            if history_compaction
            then
              (* If history compaction is enabled, we only keep the latest
               version of each file read by the model. *)
              Compact_history.collapse_read_file_entries
                (List.append inputs (List.rev !new_items))
            else List.append inputs (List.rev !new_items)
          in
          let run_tool () =
            Tool_call.run_tool
              ~kind:Tool_call.Kind.Function
              ~name
              ~payload:arguments
              ~call_id
              ~tool_tbl
              ~on_fork:
                (Some
                   (fun ~invocation:_ ~call_id ~arguments ->
                     let invocation_id = Fork.Invocation_id.create () in
                     let child_allocator =
                       Fork.allocator
                         ~parent_namespace:(History_entry.Allocator.namespace allocator)
                         invocation_id
                     in
                     Res.Tool_output.Output.Text
                       (Fork.execute_entries
                          ~env
                          ~allocator:child_allocator
                          ~history:history_entries_so_far
                          ~invocation_id
                          ~call_id
                          ~arguments
                          ~tools
                          ~tool_tbl
                          ~on_event
                          ~on_sourced_event
                          ~on_fn_out:(fun _ -> ())
                          ?temperature
                          ?max_output_tokens:max_tokens
                          ?reasoning
                          ())))
              ()
          in
          let promise =
            if not parallel_tool_calls
            then (
              (* Sequential fall-back – run immediately in the current fiber *)
              let result = run_tool () in
              (* Wrap result into an already-resolved promise so code below is
               agnostic to the execution mode. *)
              let pr, resv = Eio.Promise.create () in
              Eio.Promise.resolve_ok resv result;
              pr)
            else
              (* Parallel mode – fork a new fiber and run under the semaphore *)
              Eio.Fiber.fork_promise ~sw (fun () ->
                (* Acquire permit *)
                let s = Lazy.force sem in
                Eio.Semaphore.acquire s;
                Fun.protect
                  ~finally:(fun () -> Eio.Semaphore.release s)
                  (fun () -> run_tool ()))
          in
          (* 4.  Record the pending call for later collection *)
          pending_calls := { seq; call_id; kind = `Function; promise } :: !pending_calls;
          run_again := true
      in
      let handle_custom_tool_call_done ~item_id ~input =
        match Hashtbl.find func_info item_id with
        | None -> ()
        | Some (name, call_id)
          when List.exists !pending_calls ~f:(fun pending ->
                 String.equal pending.call_id call_id) -> ()
        | Some (name, call_id) ->
          let seq = !fn_id in
          Int.incr fn_id;
          let tool_call_url id = Printf.sprintf "%i.tool-call.%s.json" seq id in
          if show_tool_call
          then
            append_doc
              (Printf.sprintf
                 "\n\
                  <tool_call type=\"custom_tool_call\" tool_call_id=\"%s\" \
                  function_name=\"%s\" id=\"%s\">\n\
                  \t%s|\n\
                  \t\t%s\n\
                  \t|%s\n\
                  </tool_call>\n"
                 call_id
                 name
                 item_id
                 "RAW"
                 (Fetch.tab_on_newline input)
                 "RAW")
          else (
            let content =
              Printf.sprintf "<doc src=\"./.chatmd/%s\" local>" (tool_call_url call_id)
            in
            append_doc
              (Printf.sprintf
                 "\n\
                  <tool_call type=\"custom_tool_call\" tool_call_id=\"%s\" \
                  function_name=\"%s\" id=\"%s\">\n\
                  \t%s\n\
                  </tool_call>\n"
                 call_id
                 name
                 item_id
                 (Fetch.tab_on_newline content));
            Io.save_doc ~dir:datadir (tool_call_url call_id) input);
          let call_item : Res.Item.t =
            Tool_call.call_item
              ~kind:Tool_call.Kind.Custom
              ~name
              ~payload:input
              ~call_id
              ~id:(Some item_id)
          in
          ignore (add_item call_item : History_entry.t);
          let run_tool () =
            Tool_call.run_tool
              ~kind:Tool_call.Kind.Custom
              ~name
              ~payload:input
              ~call_id
              ~tool_tbl
              ~on_fork:None
              ()
          in
          let promise =
            if not parallel_tool_calls
            then (
              let result = run_tool () in
              let pr, resv = Eio.Promise.create () in
              Eio.Promise.resolve_ok resv result;
              pr)
            else
              Eio.Fiber.fork_promise ~sw (fun () ->
                let s = Lazy.force sem in
                Eio.Semaphore.acquire s;
                Fun.protect
                  ~finally:(fun () -> Eio.Semaphore.release s)
                  (fun () -> run_tool ()))
          in
          pending_calls := { seq; call_id; kind = `Custom; promise } :: !pending_calls;
          run_again := true
      in
      let callback (ev : Res.Response_stream.t) =
        History_stream_event.observe registry ~scope ~source:None ev
        |> Option.iter ~f:on_history_event;
        (* For debugging purposes we still log every event. *)
        log_event ev;
        (* Internal book-keeping for writing the streamed response back into the
         conversation buffer and executing tool calls. *)
        (match ev with
         (* ───────────────────────── assistant text ────────────────────── *)
         | Res.Response_stream.Output_text_delta { item_id; delta; _ } ->
           output_text_delta ~id:item_id delta
         | Res.Response_stream.Output_item_done { item; _ } ->
           (match item with
            | Res.Response_stream.Item.Output_message om ->
              ignore (add_item (Output_message om) : History_entry.t);
              (* close an open message block, if any *)
              close_message om.id
            | Res.Response_stream.Item.Reasoning r ->
              ignore (add_item (Reasoning r) : History_entry.t);
              (* close an open reasoning block, if any *)
              (match Hashtbl.find reasoning_state r.id with
               | Some _ ->
                 close_summary ();
                 close_reasoning ();
                 Hashtbl.remove reasoning_state r.id
               | None -> ())
            | _ -> ())
         (* ─────────────────────── reasoning deltas ────────────────────── *)
         | Res.Response_stream.Reasoning_summary_text_delta
             { item_id; delta; summary_index; _ } ->
           (match Hashtbl.find reasoning_state item_id with
            | None ->
              (* first chunk for this reasoning item *)
              open_reasoning item_id;
              Hashtbl.set reasoning_state ~key:item_id ~data:summary_index
            | Some current when current = summary_index ->
              () (* same summary → continue *)
            | Some _ ->
              (* moved to the next summary *)
              close_summary ();
              open_new_summary ();
              Hashtbl.set reasoning_state ~key:item_id ~data:summary_index);
           append_doc (Fetch.tab_on_newline delta)
         (* ────────────────────── function calls etc. ──────────────────── *)
         | Res.Response_stream.Output_item_added { item; _ } ->
           (match item with
            | Res.Response_stream.Item.Function_call fc ->
              let idx = Option.value fc.id ~default:fc.call_id in
              Hashtbl.set func_info ~key:idx ~data:(fc.name, fc.call_id);
              Hashtbl.find function_completions idx
              |> Option.iter ~f:(fun arguments ->
                handle_function_done ~item_id:idx ~arguments)
            | Res.Response_stream.Item.Custom_function tc ->
              let idx = Option.value tc.id ~default:tc.call_id in
              Hashtbl.set func_info ~key:idx ~data:(tc.name, tc.call_id);
              Hashtbl.find custom_completions idx
              |> Option.iter ~f:(fun input ->
                handle_custom_tool_call_done ~item_id:idx ~input)
            | Res.Response_stream.Item.Reasoning r ->
              (* first chunk for this reasoning item *)
              open_reasoning r.id;
              Hashtbl.set reasoning_state ~key:r.id ~data:0
            | Res.Response_stream.Item.Output_message m ->
              let phase_input =
                match m.phase with
                | None -> ""
                | Some p -> Printf.sprintf " phase=\"%s\"" p
              in
              append_doc
                (Printf.sprintf
                   "\n<assistant id=\"%s\"%s>\n\t%s|\n\t\t"
                   m.id
                   phase_input
                   "RAW");
              Hashtbl.set opened_msgs ~key:m.id ~data:()
            | _ -> ())
         | Res.Response_stream.Function_call_arguments_done { item_id; arguments; _ } ->
           (match Hashtbl.find function_completions item_id with
            | Some existing when not (String.equal existing arguments) ->
              failwithf "Conflicting completion for streamed tool item %s" item_id ()
            | Some _ -> ()
            | None -> Hashtbl.set function_completions ~key:item_id ~data:arguments);
           handle_function_done ~item_id ~arguments
         | Res.Response_stream.Custom_tool_call_input_done { item_id; input; _ } ->
           (match Hashtbl.find custom_completions item_id with
            | Some existing when not (String.equal existing input) ->
              failwithf "Conflicting completion for streamed tool item %s" item_id ()
            | Some _ -> ()
            | None -> Hashtbl.set custom_completions ~key:item_id ~data:input);
           handle_custom_tool_call_done ~item_id ~input
         | _ -> ());
        on_event ev
      in
      let request_entries =
        (* If [history_compaction] is enabled, we compact the history so that
         multiple calls to the same file are replaced with a single call
         that points to the latest file content. *)
        if history_compaction
        then Compact_history.collapse_read_file_entries inputs
        else inputs
      in
      let hist = History_entry.items request_entries in
      (* ────────────────── 3.  fire request in stream mode ──────────────── *)
      let events =
        In_memory_stream.For_testing.retry_stream_start
          ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock env))
          (fun () ->
             match post_stream with
             | Some post -> post ~sw ~inputs:hist
             | None ->
               Res.post_response
                 Res.Stream
                 ?max_output_tokens:max_tokens
                 ?temperature
                 ~tools
                 ~parallel_tool_calls
                 ?reasoning
                 ~model
                 ~dir:datadir
                 net
                 ~sw
                 ~inputs:hist)
      in
      Seq.iter callback events;
      (* ----------------------------------------------------------------- *)
      (*  Collect results from any pending tool invocations.  We enforce  *)
      (*  deterministic ordering by iterating over them sorted by [seq].   *)
      (* ----------------------------------------------------------------- *)
      let sorted_calls =
        List.sort !pending_calls ~compare:(fun a b -> Int.compare a.seq b.seq)
      in
      List.iter sorted_calls ~f:(fun { seq; call_id; kind; promise } ->
        let result =
          match Eio.Promise.await_exn promise with
          | Openai.Responses.Tool_output.Output.Text t -> t
          | Content parts ->
            parts
            |> List.map ~f:(function
              | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
              | Input_image { image_url; _ } ->
                Printf.sprintf "<image src=\"%s\" />" image_url)
            |> String.concat ~sep:"\n"
        in
        let tool_call_result_url id =
          Printf.sprintf "%i.tool-call-result.%s.json" seq id
        in
        let type_attr =
          match kind with
          | `Function -> ""
          | `Custom -> " type=\"custom_tool_call\""
        in
        if show_tool_call
        then
          append_doc
            (Printf.sprintf
               "\n\
                <tool_response%s tool_call_id=\"%s\" id=\"%d\">\n\
                \t%s|\n\
                \t\t%s\n\
                \t|%s\n\
                </tool_response>\n"
               type_attr
               call_id
               seq
               "RAW"
               (Fetch.tab_on_newline result)
               "RAW")
        else (
          let content =
            Printf.sprintf
              "<doc src=\"./.chatmd/%s\" local>"
              (tool_call_result_url call_id)
          in
          append_doc
            (Printf.sprintf
               "\n\
                <tool_response%s tool_call_id=\"%s\" id=\"%d\">\n\
                \t%s\n\
                </tool_response>\n"
               type_attr
               call_id
               seq
               (Fetch.tab_on_newline content));
          Io.save_doc ~dir:datadir (tool_call_result_url call_id) result);
        let kind : Tool_call.Kind.t =
          match kind with
          | `Function -> Tool_call.Kind.Function
          | `Custom -> Tool_call.Kind.Custom
        in
        let out_item : Res.Item.t =
          Tool_call.output_item ~kind ~call_id ~output:(Output.Text result)
        in
        ignore
          (History_stream_event.Registry.tool_output registry ~scope ~source:None ~call_id
           : History_entry.Id.t);
        let entry = add_item out_item in
        on_history_tool_out entry);
      (* make sure any dangling assistant block is closed *)
      Hashtbl.iter_keys opened_msgs ~f:(fun id -> close_message id);
      (* 4 • If no function call just happened, append empty user message.   *)
      (* 4 • If a function call just happened, recurse for the next turn.   *)
      if !run_again
      then turn (List.append inputs (List.rev !new_items))
      else (
        append_doc "\n<user>\n\n</user>";
        List.append inputs (List.rev !new_items))
    in
    let history = turn inputs in
    on_final_history history);
  Cache.save ~file:cache_file cache
;;
