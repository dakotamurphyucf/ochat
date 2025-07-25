open Core
module CM = Prompt.Chat_markdown
module Res = Openai.Responses

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

(** [run_agent ~ctx prompt_xml items] evaluates a *nested agent* inside
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

let rec run_agent ~(ctx : _ Ctx.t) (prompt_xml : string) (items : CM.content_item list)
  : string
  =
  Eio.Switch.run
  @@ fun sw ->
  (* 1.  Extract individual components from the shared context *)
  let dir = Ctx.dir ctx in
  (* 1.  Build the full agent XML by adding any inline user items. *)
  let msg =
    CM.User
      { role = "user"
      ; content = Some (Items items)
      ; name = None
      ; id = None
      ; status = None
      ; function_call = None
      ; tool_call = None
      ; tool_call_id = None
      }
  in
  (* 2.  Parse the merged document into structured elements. *)
  let elements = CM.parse_chat_inputs ~dir prompt_xml in
  (* 3.  Configuration (max_tokens, model, …) *)
  let cfg = Config.of_elements elements in
  let CM.{ max_tokens; model; reasoning_effort; temperature; show_tool_call = _; id = _ } =
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
  (* 4.  Collect tool declarations inside the agent prompt. *)
  let declared_tools =
    List.filter_map elements ~f:(function
      | CM.Tool t -> Some t
      | _ -> None)
  in
  (* Convert user-declared tools → functions *)
  let tools : Ochat_function.t list =
    List.concat_map declared_tools ~f:(fun decl ->
      let ctx_for_tool =
        match decl with
        | CM.Agent _ -> { ctx with dir = Eio.Stdenv.cwd (Ctx.env ctx) }
        | _ -> { ctx with dir = Eio.Stdenv.cwd (Ctx.env ctx) }
      in
      Tool.of_declaration ~sw ~ctx:ctx_for_tool ~run_agent decl)
  in
  let comp_tools, tool_tbl = Ochat_function.functions tools in
  let tools_req = Tool.convert_tools comp_tools in
  (* 5.  Convert XML ‑> API items and enter the execute loop to handle function calls. *)
  let init_items = Converter.to_items ~ctx ~run_agent (elements @ [ msg ]) in
  let all_items =
    Response_loop.run
      ~ctx
      ?temperature
      ?max_output_tokens:max_tokens
      ~tools:tools_req
      ?reasoning
      ~model
      ~tool_tbl
      init_items
  in
  (* 6.  Extract assistant messages and concatenate them. *)
  List.drop all_items (List.length init_items)
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
      ?prompt_file (* optional template to prepend *)
      ~output_file (* evolving conversation buffer *)
      ()
  =
  (* [run_completion ~env ?prompt_file ~output_file ()] enters a
     read-eval-append loop on [output_file].  Each iteration:

     1. Parses the XML buffer into ChatMarkdown elements.
     2. Converts them to OpenAI items via {!Converter}.
     3. Runs {!Response_loop.run} until no pending function calls.
     4. Appends the assistant answer (and reasoning) back to
        [output_file].  *)
  let dir = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
  (* Ensure the hidden data directory exists and get its path. *)
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1000 () in
  (* 1 • append initial prompt file if provided *)
  Option.iter prompt_file ~f:(fun file ->
    Io.append_doc ~dir output_file (Io.load_doc ~dir file));
  (* 2 • main loop *)
  let rec loop () =
    let xml = Io.load_doc ~dir output_file in
    let elements = CM.parse_chat_inputs ~dir xml in
    (* gather config *)
    let cfg = Config.of_elements elements in
    let CM.
          { max_tokens = model_tokens
          ; model = model_opt
          ; reasoning_effort
          ; temperature
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
    (* convert xml → items and fire first request *)
    let ctx = Ctx.create ~env ~dir ~cache in
    (* tools / function mapping *)
    let builtin_fns =
      [ Functions.webpage_to_markdown
          ~env:(Ctx.env ctx)
          ~dir:(Ctx.dir ctx)
          ~net:(Ctx.net ctx)
      ; Functions.fork
      ]
    in
    let comp_tools, tool_tbl = Ochat_function.functions builtin_fns in
    let tools = Tool.convert_tools comp_tools in
    (* convert xml → items and fire first request *)
    let ctx = Ctx.create ~env ~dir ~cache in
    let init_items = Converter.to_items ~ctx ~run_agent elements in
    (* For the response loop we use a context bound to the .chatmd data folder so
       that any tool-generated artefacts land in that directory. *)
    let ctx_loop = Ctx.create ~env ~dir:datadir ~cache in
    let all_items =
      Response_loop.run
        ~ctx:ctx_loop
        ?temperature
        ?max_output_tokens:model_tokens
        ~tools
        ?reasoning
        ~tool_tbl
        ~model:
          (Option.value_map
             model_opt
             ~default:Res.Request.Gpt4
             ~f:Res.Request.model_of_str_exn)
        init_items
    in
    (* 3 • render assistant text back into XML buffer *)
    let append = Io.append_doc ~dir output_file in
    List.iter
      (List.drop all_items (List.length init_items))
      ~f:(function
        | Res.Item.Output_message o ->
          append
            (Printf.sprintf
               "<assistant id=\"%s\">\n\t%s|\n\t\t%s\n\t|%s\n</assistant>\n"
               o.id
               "RAW"
               (Fetch.tab_on_newline
                  (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" "))
               "RAW")
        | Res.Item.Reasoning r ->
          let summaries =
            List.map r.summary ~f:(fun s ->
              Printf.sprintf
                "\n<summary>\n\t\t%s\n</summary>\n"
                (Fetch.tab_on_newline s.text))
            |> String.concat ~sep:""
          in
          print_endline "summaries";
          print_endline summaries;
          append
            (Printf.sprintf
               "\n<reasoning id=\"%s\">\n%s\n</reasoning>\n"
               r.id
               (Fetch.tab_on_newline summaries))
        | Res.Item.Function_call fc ->
          append
            (Printf.sprintf
               "\n\
                <tool_call function_name=\"%s\" tool_call_id=\"%s\">\n\
                %s\n\
                </tool_call>\n"
               fc.name
               fc.call_id
               fc.arguments)
        | Res.Item.Function_call_output fco ->
          append
            (Printf.sprintf
               "\n<tool_response tool_call_id=\"%s\">%s</tool_response>\n"
               fco.call_id
               fco.output)
        | Res.Item.Input_message _
        | Res.Item.Web_search_call _
        | Res.Item.File_search_call _ -> ());
    (* stop if no new function calls were produced *)
    if
      List.exists all_items ~f:(function
        | Res.Item.Function_call _ -> true
        | _ -> false)
    then loop ()
    else append "\n<user>\n\n</user>"
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
      ?prompt_file (* optional template to prepend once          *)
      ?(on_event : Openai.Responses.Response_stream.t -> unit = fun _ -> ())
      ~output_file (* evolving conversation buffer               *)
      ()
  =
  Eio.Switch.run
  @@ fun sw ->
  (* ─────────────────────── 0.  setup & helpers ───────────────────────── *)
  let dir = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
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
  let elements = CM.parse_chat_inputs ~dir xml in
  (* 1‑B • current config (max_tokens, model, …) *)
  let cfg = Config.of_elements elements in
  let CM.{ max_tokens; model; reasoning_effort; temperature; show_tool_call; id = _ } =
    cfg
  in
  let model =
    Option.value_map model ~f:Res.Request.model_of_str_exn ~default:Res.Request.Gpt4
  in
  let reasoning =
    Option.map reasoning_effort ~f:(fun eff ->
      Res.Request.Reasoning.
        { effort = Some (Effort.of_str_exn eff); summary = Some Summary.Detailed })
  in
  let user_decl_tools =
    List.filter_map elements ~f:(function
      | CM.Tool t -> Some t
      | _ -> None)
    |> List.concat_map ~f:(fun decl ->
      let ctx_for_tool =
        match decl with
        | CM.Agent _ -> Ctx.create ~env ~dir:(Eio.Stdenv.cwd env) ~cache
        | _ -> Ctx.create ~env ~dir:(Eio.Stdenv.cwd env) ~cache
      in
      Tool.of_declaration ~sw ~ctx:ctx_for_tool ~run_agent decl)
  in
  (* 1-C • tools / functions – only tools declared by user *)
  let comp_tools, tool_tbl = Ochat_function.functions user_decl_tools in
  let tools = Tool.convert_tools comp_tools in
  (* 1‑D • initial request items *)
  let ctx = Ctx.create ~env ~dir ~cache in
  let inputs = Converter.to_items ~ctx ~run_agent elements in
  (* ─────────────────────── 1.  main recursive turn ────────────────────── *)
  let rec turn inputs =
    (* ────────────────── 2.  streaming callback state ─────────────────── *)
    (* existing tables … *)
    let new_items : Res.Item.t list ref = ref [] in
    let add_item item = new_items := item :: !new_items in
    let opened_msgs : (string, unit) Hashtbl.t = Hashtbl.create (module String)
    and func_info : (string, string * string) Hashtbl.t = Hashtbl.create (module String)
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
    let open_new_summary () = append_doc (Printf.sprintf "\n\t%ssummary%s\n\t\t" lt gt) in
    let close_summary () = append_doc (Printf.sprintf "\n\t%s/summary%s" lt gt) in
    let close_reasoning () = append_doc (Printf.sprintf "\n%s/reasoning%s\n" lt gt) in
    let handle_function_done ~item_id ~arguments =
      match Hashtbl.find func_info item_id with
      | None -> () (* should not happen *)
      | Some (name, call_id) ->
        let i = !fn_id in
        let tool_call_url id = Printf.sprintf "%i.tool-call.%s.json" i id in
        (* assistant’s tool‑call request *)
        (* if show_tool_call is true, then we will show the tool call in the output *)
        (* otherwise, we will save the tool call to a file and not show it in the output *)
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
          (* save the tool call to a file *)
          Io.save_doc ~dir:datadir (tool_call_url call_id) arguments);
        let fn_call =
          Res.Item.Function_call
            { name
            ; arguments
            ; call_id
            ; _type = "function_call"
            ; id = Some item_id
            ; status = None
            }
        in
        (* add the function call to the new items list *)
        add_item fn_call;
        (* run the tool *)
        let fn = Hashtbl.find_exn tool_tbl name in
        (* Special-case the "fork" tool so that we run the custom in-memory
           clone instead of the user-supplied stub implementation.  The
           real logic will be implemented in [Fork.execute]; for the moment
           we delegate to a placeholder to keep the build green. *)
        (* Current conversation history up to this point (including any
           items we have already queued in [new_items]).  We reverse
           [new_items] because items are accumulated in reverse order. *)
        let history_so_far = List.append inputs (List.rev !new_items) in
        let result =
          if String.equal name "fork"
          then
            Fork.execute
              ~env
              ~history:history_so_far
              ~call_id
              ~arguments
              ~tools
              ~tool_tbl
              ~on_event
              ~on_fn_out:(fun _ -> ())
              ?temperature
              ?max_output_tokens:max_tokens
              ?reasoning
              ()
          else fn arguments
        in
        let tool_call_result_url id = Printf.sprintf "%i.tool-call-result.%s.json" i id in
        if show_tool_call
        then
          append_doc
            (Printf.sprintf
               "\n\
                <tool_response tool_call_id=\"%s\" id=\"%s\">\n\
                \t%s|\n\
                \t\t%s\n\
                \t|%s\n\
                </tool_response>\n"
               call_id
               item_id
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
               "\n<tool_response tool_call_id=\"%s\" id=\"%s\">\n\t%s\n</tool_response>\n"
               call_id
               item_id
               (Fetch.tab_on_newline content));
          Io.save_doc ~dir:datadir (tool_call_result_url call_id) result);
        let fn_out =
          Res.Item.Function_call_output
            { call_id
            ; _type = "function_call_output"
            ; id = None
            ; status = None
            ; output = result
            }
        in
        (* add the function call output to the new items list *)
        add_item fn_out;
        Int.incr fn_id;
        run_again := true
    in
    let callback (ev : Res.Response_stream.t) =
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
            add_item (Output_message om);
            (* close an open message block, if any *)
            close_message om.id
          | Res.Response_stream.Item.Reasoning r ->
            add_item (Reasoning r);
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
          | Some current when current = summary_index -> () (* same summary → continue *)
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
            Hashtbl.set func_info ~key:idx ~data:(fc.name, fc.call_id)
          | Res.Response_stream.Item.Reasoning r ->
            (* first chunk for this reasoning item *)
            open_reasoning r.id;
            Hashtbl.set reasoning_state ~key:r.id ~data:0
          | _ -> ())
       | Res.Response_stream.Function_call_arguments_done { item_id; arguments; _ } ->
         handle_function_done ~item_id ~arguments
       | _ -> ());
      on_event ev
    in
    (* ────────────────── 3.  fire request in stream mode ──────────────── *)
    Res.post_response
      (Res.Stream callback)
      ?max_output_tokens:max_tokens
      ?temperature
      ~tools
      ?reasoning
      ~model
      ~dir:datadir
      net
      ~inputs;
    (* make sure any dangling assistant block is closed *)
    Hashtbl.iter_keys opened_msgs ~f:(fun id -> close_message id);
    (* 4 • If no function call just happened, append empty user message.   *)
    (* 4 • If a function call just happened, recurse for the next turn.   *)
    if !run_again
    then turn (List.append inputs (List.rev !new_items))
    else append_doc "\n<user>\n\n</user>"
  in
  turn inputs;
  Cache.save ~file:cache_file cache
;;

(** [run_completion_stream_in_memory_v1 ~env ~history ~tools ()] streams a
    ChatMarkdown conversation **held entirely in memory**.

    Compared to {!run_completion_stream} this helper:

    • Accepts an explicit [history] (list of {!Openai.Responses.Item.t})
      instead of reading a `.chatmd` file from disk.
    • Returns the *complete* history after all assistant turns and tool
      calls have been resolved.
    • Never touches the filesystem except for the persistent cache under
      `[~/.chatmd]`, making it suitable for unit-tests or server back-ends
      where direct file IO is undesirable.

    Optional callbacks mirror the streaming variant:

    • [?on_event] – invoked for each streaming event received from the
      OpenAI API (token deltas, item completions, …). Defaults to a no-op.
    • [?on_fn_out] – executed after each tool call completes, allowing the
      caller to react to side-effects without waiting for the final
      assistant answer.

    @param env      Standard Eio runtime environment.
    @param history  Initial conversation state.
    @param tools    Compile-time list of tool definitions visible to the
                    model.  Pass [[]] for none.
    @param tool_tbl Optional lookup table generated from [tools].  The
                    default builds a fresh table via
                    {!Ochat_function.functions} when omitted.
    @param temperature Temperature override forwarded the OpenAI request.
    @param max_output_tokens Hard cap on the number of tokens generated by
           the model per request.
    @param reasoning Optional reasoning settings forwarded to the API.

    @return The updated [history], i.e. the concatenation of the original
            [history] and every item produced during the streaming loop.

    @raise Any exception bubbled-up by the OpenAI client or user-supplied
           tool functions.  The function does **not** swallow errors. *)
let run_completion_stream_in_memory_v1
      ~env
      ~(history : Openai.Responses.Item.t list)
      ?(on_event : Openai.Responses.Response_stream.t -> unit = fun _ -> ())
      ?(on_fn_out : Openai.Responses.Function_call_output.t -> unit = fun _ -> ())
      ~tools
      ?tool_tbl
      ?temperature
      ?max_output_tokens
      ?reasoning
      ?(model = Openai.Responses.Request.O3)
      ()
  : Openai.Responses.Item.t list
  =
  let net = env#net in
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Io.ensure_chatmd_dir ~cwd in
  (* A tiny cache is fine for interactive use. *)
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1_000 () in
  (* Derive default [tools] / [tool_tbl] from the supplied argument, falling
     back to the empty set if none provided. *)
  let tools, tool_tbl =
    match tools, tool_tbl with
    | Some t, Some tbl -> t, tbl
    | _ ->
      let comp_tools, tbl = Ochat_function.functions [] in
      Tool.convert_tools comp_tools, tbl
  in
  (*──────────────────────── Internal recursive turn ───────────────────────*)
  let rec turn (hist : Openai.Responses.Item.t list) : Openai.Responses.Item.t list =
    (* State for this streaming request. *)
    let func_info : (string, string * string) Hashtbl.t =
      Hashtbl.create (module String)
    in
    let reasoning_state : (string, int) Hashtbl.t = Hashtbl.create (module String) in
    let new_items : Openai.Responses.Item.t list ref = ref [] in
    let add_item it = new_items := it :: !new_items in
    let run_again = ref false in
    (* Execute tool and queue follow-up items. *)
    let handle_function_done ~item_id ~arguments hist =
      match Hashtbl.find func_info item_id with
      | None ->
        Openai.Responses.Function_call_output.
          { output = ""
          ; call_id = ""
          ; _type = "function_call_output"
          ; id = None
          ; status = None
          }
      | Some (name, call_id) ->
        let fn_call : Openai.Responses.Function_call.t =
          { name
          ; arguments
          ; call_id
          ; _type = "function_call"
          ; id = Some item_id
          ; status = None
          }
        in
        let fn_call_item = Openai.Responses.Item.Function_call fn_call in
        let fn = Hashtbl.find_exn tool_tbl name in
        let result =
          if String.equal name "fork"
          then (
            let res =
              turn
              @@ Fork.history
                   ~history:(List.append hist (List.rev (fn_call_item :: !new_items)))
                   ~arguments
                   call_id
            in
            [ List.last_exn res ]
            |> List.filter_map ~f:(function
              | Res.Item.Output_message o ->
                Some (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
              | _ -> None)
            |> String.concat ~sep:"\n")
          else fn arguments
        in
        (* Queue helper items to the history so that the model sees the call
           and the result. *)
        let fn_out : Openai.Responses.Function_call_output.t =
          { output = result
          ; call_id
          ; _type = "function_call_output"
          ; id = None
          ; status = None
          }
        in
        let fn_out_item = Openai.Responses.Item.Function_call_output fn_out in
        new_items := fn_out_item :: fn_call_item :: !new_items;
        run_again := true;
        fn_out
    in
    (* Streaming callback – keep minimal book-keeping for tool-calls while
       forwarding every event to [on_event] so the caller can update the UI. *)
    let stream_cb (ev : Openai.Responses.Response_stream.t) =
      match ev with
      | Openai.Responses.Response_stream.Output_item_added { item; _ } ->
        (match item with
         | Openai.Responses.Response_stream.Item.Function_call fc ->
           let idx = Option.value fc.id ~default:fc.call_id in
           Hashtbl.set func_info ~key:idx ~data:(fc.name, fc.call_id)
         | Openai.Responses.Response_stream.Item.Reasoning r ->
           Hashtbl.set reasoning_state ~key:r.id ~data:0
         | _ -> ());
        on_event ev
      | Output_item_done { item; _ } ->
        (match item with
         | Openai.Responses.Response_stream.Item.Output_message om ->
           add_item (Openai.Responses.Item.Output_message om)
         | Openai.Responses.Response_stream.Item.Reasoning r ->
           add_item (Openai.Responses.Item.Reasoning r)
         | _ -> ());
        on_event ev
      | Function_call_arguments_done { item_id; arguments; _ } ->
        on_event ev;
        let fn_out = handle_function_done ~item_id ~arguments hist in
        on_fn_out fn_out
      | Function_call_arguments_delta _
      | Reasoning_summary_text_delta _
      | Output_text_delta _ -> on_event ev
      | _ -> ()
    in
    (* Fire the request. *)
    try
      Openai.Responses.post_response
        (Openai.Responses.Stream stream_cb)
        ?max_output_tokens
        ?temperature
        ~tools
        ~model
        ?reasoning
        ~dir:datadir
        net
        ~inputs:hist;
      let hist = List.append hist (List.rev !new_items) in
      if !run_again then turn hist else hist
    with
    | Openai.Responses.Response_stream_parsing_error (json, exn) ->
      Io.log
        ~dir:datadir
        ~file:"raw-openai-streaming-response-json-parsing-error.txt"
        (Printf.sprintf "Error parsing JSON from line: %s" (Core.Exn.to_string exn)
         ^ "\n"
         ^ Jsonaf.to_string json
         ^ "\n");
      turn hist
  in
  let full_history = turn history in
  Cache.save ~file:cache_file cache;
  full_history
;;
