open Core
module CM = Prompt.Chat_markdown
module Res = Openai.Responses
module Output = Res.Tool_output.Output

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

let rec run_agent
          ?(history_compaction = false)
          ~(ctx : _ Ctx.t)
          (prompt_xml : string)
          (items : CM.content_item list)
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
      ; type_ = None
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
  (* Convert user-declared tools → functions – use the real CWD so that any
     file references inside the tool behave like shell commands. *)
  let tools : Ochat_function.t list =
    List.concat_map declared_tools ~f:(fun decl ->
      Tool.of_declaration ~sw ~ctx ~run_agent:(run_agent ~history_compaction) decl)
  in
  let comp_tools, tool_tbl = Ochat_function.functions tools in
  let tools_req = Tool.convert_tools comp_tools in
  (* 5.  Convert XML ‑> API items and enter the execute loop to handle function calls. *)
  let init_items =
    Converter.to_items ~ctx ~run_agent:(run_agent ~history_compaction) (elements @ [ msg ])
  in
  let all_items =
    Response_loop.run
      ~ctx
      ?temperature
      ?max_output_tokens:max_tokens
      ~tools:tools_req
      ?reasoning
      ~history_compaction
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
      Converter.to_items ~ctx ~run_agent:(run_agent ~history_compaction:false) elements
    in
    (* For the response loop we use a context bound to the .chatmd data folder so
       that any tool-generated artefacts land in that directory. *)
    let ctx_loop = Ctx.create ~env ~dir:datadir ~cache ~tool_dir:datadir in
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
        | Res.Item.Custom_tool_call tc ->
          append
            (Printf.sprintf
               "\n\
                <tool_call type=\"custom_tool_call\" function_name=\"%s\" \
                tool_call_id=\"%s\">\n\
                %s\n\
                </tool_call>\n"
               tc.name
               tc.call_id
               tc.input)
        | Res.Item.Function_call_output fco ->
          let output =
            match fco.output with
            | Openai.Responses.Tool_output.Output.Text text -> text
            | Content parts ->
              parts
              |> List.map ~f:(function
                | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
                | Input_image { image_url; _ } ->
                  Printf.sprintf "<image src=\"%s\" />" image_url)
              |> String.concat ~sep:"\n"
          in
          append
            (Printf.sprintf
               "\n<tool_response tool_call_id=\"%s\">%s</tool_response>\n"
               fco.call_id
               output)
        | Res.Item.Custom_tool_call_output tco ->
          let output =
            match tco.output with
            | Openai.Responses.Tool_output.Output.Text text -> text
            | Content parts ->
              parts
              |> List.map ~f:(function
                | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
                | Input_image { image_url; _ } ->
                  Printf.sprintf "<image src=\"%s\" />" image_url)
              |> String.concat ~sep:"\n"
          in
          append
            (Printf.sprintf
               "\n\
                <tool_response type=\"custom_tool_call\" \
                tool_call_id=\"%s\">%s</tool_response>\n"
               tco.call_id
               output)
        | Res.Item.Input_message _
        | Res.Item.Web_search_call _
        | Res.Item.File_search_call _ -> ());
    (* stop if no new function calls were produced *)
    if
      List.exists all_items ~f:(function
        | Res.Item.Function_call _ -> true
        | Res.Item.Custom_tool_call _ -> true
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
      ?prompt_file
      ?(on_event : Openai.Responses.Response_stream.t -> unit = fun _ -> ())
      ?(parallel_tool_calls = true)
      ?(meta_refine = false)
      ?(history_compaction = false)
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
  let elements = CM.parse_chat_inputs ~dir:output_dir xml in
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
  (* Execution context anchored at the prompt directory – ensures that any
     relative paths in <doc src="…">, <import>, or nested agent prompts are
     resolved against the folder that contains [output_file]. *)
  let ctx = Ctx.create ~env ~dir:output_dir ~cache ~tool_dir:(Eio.Stdenv.cwd env) in
  (* Tools declared inside the prompt should behave as if executed from the
     user’s shell, therefore we build a context whose [dir] is the real CWD. *)
  let user_decl_tools =
    List.filter_map elements ~f:(function
      | CM.Tool t -> Some t
      | _ -> None)
    |> List.concat_map ~f:(fun decl ->
      Tool.of_declaration ~sw ~ctx ~run_agent:(run_agent ~history_compaction) decl)
  in
  (* 1-C • tools / functions – only tools declared by user *)
  let comp_tools, tool_tbl = Ochat_function.functions user_decl_tools in
  let tools = Tool.convert_tools comp_tools in
  (* 1-D • initial request items *)
  let inputs =
    Converter.to_items ~ctx ~run_agent:(run_agent ~history_compaction) elements
  in
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
        add_item fn_call_item;
        (* 3.  Spawn the actual tool invocation in its own fiber           *)
        let history_so_far =
          if history_compaction
          then
            (* If history compaction is enabled, we only keep the latest
               version of each file read by the model. *)
            Compact_history.collapse_read_file_history
            @@ List.append inputs (List.rev !new_items)
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
                 (fun ~call_id ~arguments ->
                   Res.Tool_output.Output.Text
                     (Fork.execute
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
                        ())))
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
        add_item call_item;
        let run_tool () =
          Tool_call.run_tool
            ~kind:Tool_call.Kind.Custom
            ~name
            ~payload:input
            ~call_id
            ~tool_tbl
            ~on_fork:None
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
          | Res.Response_stream.Item.Custom_function tc ->
            let idx = Option.value tc.id ~default:tc.call_id in
            Hashtbl.set func_info ~key:idx ~data:(tc.name, tc.call_id)
          | Res.Response_stream.Item.Reasoning r ->
            (* first chunk for this reasoning item *)
            open_reasoning r.id;
            Hashtbl.set reasoning_state ~key:r.id ~data:0
          | _ -> ())
       | Res.Response_stream.Function_call_arguments_done { item_id; arguments; _ } ->
         handle_function_done ~item_id ~arguments
       | Res.Response_stream.Custom_tool_call_input_done { item_id; input; _ } ->
         handle_custom_tool_call_done ~item_id ~input
       | _ -> ());
      on_event ev
    in
    let hist =
      (* If [history_compaction] is enabled, we compact the history so that
         multiple calls to the same file are replaced with a single call
         that points to the latest file content. *)
      if history_compaction
      then Compact_history.collapse_read_file_history inputs
      else inputs
    in
    (* ────────────────── 3.  fire request in stream mode ──────────────── *)
    Res.post_response
      (Res.Stream callback)
      ?max_output_tokens:max_tokens
      ?temperature
      ~tools
      ~parallel_tool_calls
      ?reasoning
      ~model
      ~dir:datadir
      net
      ~inputs:hist;
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
      let tool_call_result_url id = Printf.sprintf "%i.tool-call-result.%s.json" seq id in
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
          Printf.sprintf "<doc src=\"./.chatmd/%s\" local>" (tool_call_result_url call_id)
        in
        append_doc
          (Printf.sprintf
             "\n<tool_response%s tool_call_id=\"%s\" id=\"%d\">\n\t%s\n</tool_response>\n"
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
      add_item out_item);
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

    @param history_compaction If [true], the function will compact the
           history so that multiple calls to the same file are replaced with a
           single call that points to the latest file content. Outputs for older calls are replaced with a
           place holder that points to the latest call output (stale) file content removed — see newer read_file output later

    @return The updated [history], i.e. the concatenation of the original
            [history] and every item produced during the streaming loop.

    @raise Any exception bubbled-up by the OpenAI client or user-supplied
           tool functions.  The function does **not** swallow errors. *)
let run_completion_stream_in_memory_v1
      ~env
      ?datadir
      ~(history : Openai.Responses.Item.t list)
      ?(on_event : Openai.Responses.Response_stream.t -> unit = fun _ -> ())
      ?(on_fn_out : Openai.Responses.Function_call_output.t -> unit = fun _ -> ())
      ?(on_tool_out : Openai.Responses.Item.t -> unit = fun _ -> ())
      ~tools
      ?tool_tbl
      ?temperature
      ?max_output_tokens
      ?reasoning
      ?(history_compaction = false)
      ?(parallel_tool_calls = true)
      ?(meta_refine = false)
      ?system_event
      ?(model = Openai.Responses.Request.O3)
      ?prompt_cache_key
      ?prompt_cache_retention
      ()
  : Openai.Responses.Item.t list
  =
  if meta_refine then Caml_unix.putenv "OCHAT_META_REFINE" "1";
  (* Mark [system_event] as used to avoid -unused-var warnings. *)
  let _ = system_event in
  Eio.Switch.run
  @@ fun sw ->
  let _ = parallel_tool_calls in
  let net = env#net in
  let datadir =
    match datadir with
    | Some d -> d
    | None ->
      let cwd = Eio.Stdenv.cwd env in
      Io.ensure_chatmd_dir ~cwd
  in
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
    (* ------------------------------------------------------------------ *)
    (* Support parallel execution of tool calls (in-memory variant)        *)
    (* ------------------------------------------------------------------ *)
    let sem = lazy (Eio.Semaphore.make 8) in
    let pending_calls : driver_pending_call list ref = ref [] in
    let handle_function_done ~item_id ~arguments hist =
      match Hashtbl.find func_info item_id with
      | None -> ()
      | Some (name, call_id) ->
        let seq = List.length !pending_calls in
        let fn_call_item : Openai.Responses.Item.t =
          Tool_call.call_item
            ~kind:Tool_call.Kind.Function
            ~name
            ~payload:arguments
            ~call_id
            ~id:(Some item_id)
        in
        add_item fn_call_item;
        let history_so_far =
          if history_compaction
          then
            (* If history compaction is enabled, we only keep the latest
               version of each file read by the model. *)
            Compact_history.collapse_read_file_history
            @@ List.append hist (List.rev !new_items)
          else (* Otherwise we keep the full history as is. *)
            List.append hist (List.rev !new_items)
        in
        let run_fork ~call_id ~arguments =
          let res =
            turn
            @@ Fork.history ~history:(List.append history_so_far []) ~arguments call_id
          in
          let result =
            [ List.last_exn res ]
            |> List.filter_map ~f:(function
              | Res.Item.Output_message o ->
                Some (List.map o.content ~f:(fun c -> c.text) |> String.concat ~sep:" ")
              | _ -> None)
            |> String.concat ~sep:"\n"
          in
          Output.Text result
        in
        let run_tool () =
          Tool_call.run_tool
            ~kind:Tool_call.Kind.Function
            ~name
            ~payload:arguments
            ~call_id
            ~tool_tbl
            ~on_fork:(Some run_fork)
        in
        let promise =
          if not parallel_tool_calls
          then (
            let res = run_tool () in
            let p, r = Eio.Promise.create () in
            Eio.Promise.resolve_ok r res;
            p)
          else
            Eio.Fiber.fork_promise ~sw (fun () ->
              let s = Lazy.force sem in
              Eio.Semaphore.acquire s;
              Fun.protect
                ~finally:(fun () -> Eio.Semaphore.release s)
                (fun () -> run_tool ()))
        in
        pending_calls := { seq; call_id; kind = `Function; promise } :: !pending_calls;
        run_again := true
    in
    let handle_custom_tool_call_done ~item_id ~input =
      match Hashtbl.find func_info item_id with
      | None -> ()
      | Some (name, call_id) ->
        let seq = List.length !pending_calls in
        let call_item : Openai.Responses.Item.t =
          Tool_call.call_item
            ~kind:Tool_call.Kind.Custom
            ~name
            ~payload:input
            ~call_id
            ~id:(Some item_id)
        in
        add_item call_item;
        let run_tool () =
          Tool_call.run_tool
            ~kind:Tool_call.Kind.Custom
            ~name
            ~payload:input
            ~call_id
            ~tool_tbl
            ~on_fork:None
        in
        let promise =
          if not parallel_tool_calls
          then (
            let res = run_tool () in
            let p, r = Eio.Promise.create () in
            Eio.Promise.resolve_ok r res;
            p)
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
    (* Streaming callback – keep minimal book-keeping for tool-calls while
       forwarding every event to [on_event] so the caller can update the UI. *)
    let stream_cb (ev : Openai.Responses.Response_stream.t) =
      match ev with
      | Openai.Responses.Response_stream.Output_item_added { item; _ } ->
        (match item with
         | Openai.Responses.Response_stream.Item.Function_call fc ->
           let idx = Option.value fc.id ~default:fc.call_id in
           Hashtbl.set func_info ~key:idx ~data:(fc.name, fc.call_id)
         | Openai.Responses.Response_stream.Item.Custom_function tc ->
           let idx = Option.value tc.id ~default:tc.call_id in
           Hashtbl.set func_info ~key:idx ~data:(tc.name, tc.call_id)
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
        handle_function_done ~item_id ~arguments hist
      | Custom_tool_call_input_done { item_id; input; _ } ->
        on_event ev;
        handle_custom_tool_call_done ~item_id ~input
      | Function_call_arguments_delta _
      | Custom_tool_call_input_delta _
      | Reasoning_summary_text_delta _
      | Output_text_delta _ -> on_event ev
      | _ -> ()
    in
    (* Fire the request. *)
    let input_items =
      if history_compaction then Compact_history.collapse_read_file_history hist else hist
    in
    Io.log
      ~dir:datadir
      ~file:"raw-openai-streaming-response-json-parsing-error.txt"
      (Sexp.to_string_hum
         [%sexp
           (("Requesting OpenAI streaming response with inputs:", input_items)
            : string * Openai.Responses.Item.t list)]);
    try
      Openai.Responses.post_response
        (Openai.Responses.Stream stream_cb)
        ?max_output_tokens
        ?temperature
        ~tools
        ~parallel_tool_calls
        ~model
        ?reasoning
        ?prompt_cache_key
        ?prompt_cache_retention
        ~dir:datadir
        net
        ~inputs:input_items;
      (* Wait for all pending tool invocations to finish and register their
         outputs in a deterministic order. *)
      let sorted_calls =
        List.sort !pending_calls ~compare:(fun a b -> Int.compare a.seq b.seq)
      in
      (* If no tool calls were made, we can return the history immediately. *)
      List.iteri sorted_calls ~f:(fun i { seq = _; call_id; kind; promise } ->
        let result = Eio.Promise.await_exn promise in
        let system_event_msg =
          match i, system_event with
          | 0, Some stream ->
            let rec loop acc =
              match Eio.Stream.take_nonblocking stream with
              | None ->
                (* If the stream is empty, we return the accumulated string. *)
                acc
              | Some m ->
                (* The stream is not empty, we wrap the message it in a <system_event> tag. *)
                loop
                @@ acc
                ^ Printf.sprintf "\n<system-reminder>\n%s\n</system-reminder>\n" m
            in
            loop ""
          | _, None | _, Some _ -> ""
        in
        let result =
          if String.is_empty system_event_msg
          then result
          else (
            match result with
            | Output.Text t ->
              Output.Text (Printf.sprintf "%s\n\n-------------\n\n%s" t system_event_msg)
            | Content c ->
              Content
                (Input_text
                   { text =
                       Printf.sprintf
                         "%s\n\n-------------\n\n%s"
                         (String.concat
                            ~sep:"\n"
                            (List.map c ~f:(function
                               | Input_text { text } -> text
                               | Input_image { image_url; _ } ->
                                 Printf.sprintf "<image src=\"%s\" />" image_url)))
                         system_event_msg
                   }
                 :: c))
        in
        match kind with
        | `Function ->
          let fn_out = Tool_call.function_call_output ~call_id ~output:result in
          let item = Openai.Responses.Item.Function_call_output fn_out in
          add_item item;
          on_fn_out fn_out;
          on_tool_out item
        | `Custom ->
          let out = Tool_call.custom_tool_call_output ~call_id ~output:result in
          let item = Openai.Responses.Item.Custom_tool_call_output out in
          add_item item;
          on_tool_out item);
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
      Io.log
        ~dir:(Eio.Stdenv.cwd env)
        ~file:"raw-openai-streaming-response-json-parsing-error.txt"
        (Printf.sprintf "Error parsing JSON from line: %s" (Core.Exn.to_string exn)
         ^ "\n"
         ^ Jsonaf.to_string json
         ^ "\n");
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
      (* give the log time to flush *)
      failwith (Core.Exn.to_string exn)
  in
  let full_history = turn history in
  Cache.save ~file:cache_file cache;
  full_history
;;
