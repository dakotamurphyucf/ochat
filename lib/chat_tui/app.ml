(** Terminal chat application – event-loop, streaming, persistence.

    {1 Overview}

    `Chat_tui.App` glues together all building blocks of the TUI and runs the
    main event-loop.  In concrete terms it

    • initialises `Notty_eio.Term` and renders frames via
      {!Chat_tui.Renderer},
    • interprets keystrokes with {!Chat_tui.Controller},
    • maintains a mutable {!Chat_tui.Model.t} value that represents the
      current UI state,
    • streams assistant replies from the OpenAI API using
      {!Chat_response.Driver.run_completion_stream_in_memory_v1}, and
    • persists finished conversations to disk on exit.

    Placing the code in a library module (instead of the old monolithic
    [`bin/chat_tui.ml`] executable) allows

    - reuse in tests (headless integration, golden image rendering), and
    - alternative front-ends that still want to piggy-back on the same
      orchestration logic.

    The lone public entry-point is {!run_chat}; all other helpers are local
    but kept in the interface so they can be unit-tested.
*)

open Core
open Eio.Std
open Types
module Model = Model
module Renderer = Renderer
module Redraw_throttle = Redraw_throttle
module Stream_handler = Stream
module Controller = Controller
module Persistence = Persistence
module Cmd = Cmd
module Snippet = Snippet
module CM = Prompt.Chat_markdown
module Scroll_box = Notty_scroll_box
module Res = Openai.Responses
module Res_stream = Res.Response_stream
module Res_item = Res.Item
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module Cache = Chat_response.Cache
module Tool = Chat_response.Tool
module Config = Chat_response.Config
module Req = Res.Request

(** Propagated when the user presses *Esc* to cancel an ongoing streaming
    request.  The exception is caught locally and never leaves the module. *)
exception Cancelled

(* Emit a transient “(thinking…)” placeholder so the user sees immediate
      feedback after hitting submit.  The placeholder is appended via a patch so
      the mutation goes through the centralised [Model.apply_patch] function. *)

(** [add_placeholder_thinking_message model] appends a transient
    "(thinking…)" assistant message to [model] so the user gets immediate
    visual feedback after hitting ⏎.  The placeholder is replaced by the
    first streaming token once {!handle_submit} starts receiving events. *)
let add_placeholder_thinking_message (model : Model.t) : unit =
  let patch = Add_placeholder_message { role = "assistant"; text = "(thinking…)" } in
  ignore (Model.apply_patch model patch)
;;

(** [add_placeholder_stream_error model msg] inserts a system message with
    role "error" so that fatal conditions during streaming are surfaced to
    the user instead of being silently logged. *)
let add_placeholder_stream_error (model : Model.t) text : unit =
  let patch = Add_placeholder_message { role = "error"; text } in
  ignore (Model.apply_patch model patch)
;;

let add_placeholder_compact_message (model : Model.t) : unit =
  let patch = Add_placeholder_message { role = "assistant"; text = "(compacting…)" } in
  ignore (Model.apply_patch model patch)
;;

(* Persist the in-memory session snapshot to disk unconditionally.  We
     call this helper in all quit branches so that conversation history is
     never lost, even if the user aborts ChatMarkdown export. *)
let persist_snapshot env session model =
  match session with
  | None -> ()
  | Some s ->
    let updated_session =
      Session.
        { s with
          history = Model.history_items model
        ; tasks = Model.tasks model
        ; kv_store = Hashtbl.to_alist (Model.kv_store model)
        }
    in
    Session_store.save ~env updated_session
;;

(* Construct a new history item representing the user's input and append
       it to both the canonical history list and the list of renderable
       messages.  For now we keep the simple implementation that mirrors the
       previous imperative code.  A future refactor might introduce a helper
       that converts user text into a history item in a single place. *)
let get_user_message_item text =
  let open Openai.Responses in
  Item.Input_message
    { Input_message.role = Input_message.User
    ; content = [ Input_message.Text { text; _type = "input_text" } ]
    ; _type = "message"
    }
;;

let stream_in_flight (model : Model.t) = Option.is_some (Model.fetch_sw model)

(** [apply_local_submit_effects ~dir ~env ~cache ~model ~ev_stream ~term]
    performs {b synchronous} updates that take effect immediately after the
    user submits the draft but {i before} the OpenAI request is sent.  In
    particular it

    - copies the prompt into the history as a user message, handling both
      plain text and the *Raw XML* tool-invocation dialect,
    - resets the draft buffer and caret position,
    - scrolls the viewport so the newest message is visible, and
    - pushes a redraw request onto [ev_stream] so the renderer can refresh
      the screen.

    The heavy lifting (network call, token streaming) is delegated to
    {!handle_submit}. *)
let apply_local_submit_effects ~dir ~env ~cache ~model ~ev_stream ~term =
  (* ------------------------------------------------------------------ *)
  (* 1. Retrieve original draft and run meta-refine                      *)
  (* ------------------------------------------------------------------ *)
  let orig_msg = String.strip (Model.input_line model) in
  let refined_msg =
    if String.is_empty orig_msg || true
    then orig_msg
    else (
      (* Build prompt and invoke Recursive_mp.refine.  Any exception     *)
      (* falls back to the original draft so we never block submission.  *)
      try
        let open Meta_prompting in
        let prompt_t = Prompt_intf.make ~body:orig_msg () in
        let refined_t = Recursive_mp.refine prompt_t in
        Prompt_intf.to_string refined_t
      with
      | ex ->
        (* Log but continue with original prompt *)
        print_endline (Printf.sprintf "[meta_refine] fallback – %s" (Exn.to_string ex));
        orig_msg)
  in
  let user_msg = refined_msg in
  (* ------------------------------------------------------------------ *)
  (* 2.  Optionally render a diff placeholder message for transparency   *)
  (* ------------------------------------------------------------------ *)
  (match String.equal orig_msg refined_msg with
   | true -> ()
   | false ->
     let diff_text =
       Printf.sprintf
         "[meta_refine] applied changes:\n--- original\n%s\n--- refined\n%s"
         orig_msg
         refined_msg
     in
     ignore
       (Model.apply_patch
          model
          (Add_placeholder_message { role = "meta_refine"; text = diff_text })));
  (* ------------------------------------------------------------------ *)
  (*  Inline helper commands ("/wrap", "/count", …)                  *)
  (* ------------------------------------------------------------------ *)
  (* ---------------- Submit to assistant ------------------------ *)
  (* Reset draft *)
  Model.set_input_line model "";
  (* Clear selection anchor, if any. *)
  Model.set_cursor_pos model 0;
  if not (String.is_empty user_msg)
  then (
    (* Decide between plain-text and raw-XML submission based on draft mode. *)
    match Model.draft_mode model with
    | Model.Plain ->
      ignore (Model.apply_patch model (Add_user_message { text = user_msg }));
      ignore @@ Model.add_history_item model (get_user_message_item user_msg)
    | Model.Raw_xml ->
      let module CM = Prompt.Chat_markdown in
      let xml =
        if String.is_prefix ~prefix:"<" user_msg
        then user_msg
        else Printf.sprintf "<user>\n%s\n</user>" user_msg
      in
      let elements =
        try CM.parse_chat_inputs ~dir xml with
        | exn ->
          Log.emit `Error (Printf.sprintf "XML parse error: %s" (Exn.to_string exn));
          []
      in
      Log.emit `Debug (Printf.sprintf "Parsed %d XML elements" (List.length elements));
      (* Extract the original user message from the parsed elements. *)
      let user_msg =
        List.find_map_exn elements ~f:(function
          | CM.User m ->
            let ctx = Ctx.create ~env ~dir ~cache ~tool_dir:dir in
            Some
              (Converter.convert_user_msg
                 ~ctx
                 ~run_agent:(Chat_response.Driver.run_agent ~history_compaction:true)
                 m)
          | _ -> None)
      in
      let user_msg_txt =
        match user_msg with
        | Input_message msg ->
          List.fold msg.content ~init:None ~f:(fun acc user_msg ->
            match acc with
            | Some txt ->
              (match user_msg with
               | Text t -> Some (txt ^ "\n" ^ t.text)
               | Image img ->
                 Some (txt ^ "\n" ^ Printf.sprintf "<image src=\"%s\"/>" img.image_url))
            | None -> None)
        | _ ->
          failwith
          @@ Printf.sprintf
               "Expected user message, got: %s"
               (Res_item.jsonaf_of_t user_msg |> Jsonaf.to_string)
      in
      let txt = Option.value user_msg_txt ~default:(Util.sanitize xml) in
      ignore (Model.apply_patch model (Add_user_message { text = txt }));
      ignore (Model.add_history_item model user_msg);
      Model.set_draft_mode model Model.Plain);
  Model.set_auto_follow model true;
  let _, h = Notty_eio.Term.size term in
  let input_h =
    match String.split_lines (Model.input_line model) with
    | [] -> 1
    | ls -> List.length ls
  in
  Scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:(h - input_h);
  (* Visual feedback while waiting for the first streaming tokens. *)
  add_placeholder_thinking_message model;
  Eio.Stream.add ev_stream `Redraw
;;

(** Runtime artefacts derived from the chat prompt. *)
type prompt_context =
  { cfg : Config.t (** Behavioural settings such as temperature, model, … *)
  ; tools : Req.Tool.t list (** Tools exposed to the assistant at runtime. *)
  ; tool_tbl : (string, string -> string) Hashtbl.t
    (** Mapping *tool-name → implementation*.  The assistant returns a
            JSON payload that is looked-up here and executed. *)
  }

(* ────────────────────────────────────────────────────────────────────────── *)
(*  Main event handler for submitting the draft to the assistant             *)
(* ────────────────────────────────────────────────────────────────────────── *)

(** [handle_submit ~env ~model ~ev_stream ~prompt_ctx] launches the
    *asynchronous* OpenAI completion request in a fresh switch and wires the
    streaming callbacks back into the UI.  The function never blocks the UI
    thread – it schedules fibres that push events into [ev_stream], which
    are then folded into the model by the main loop.

    On success the returned tokens replace the temporary "thinking…"
    placeholder and are persisted in {!Model.history_items}.  On failure the
    conversation is {i rolled-back} to the state before the request started
    and an error message is shown. *)
let handle_submit
      ~env
      ~model
      ~ev_stream
      ~system_event
      ~prompt_ctx
      ~datadir
      ~parallel_tool_calls
      ~history_compaction
  =
  (* Kick off the OpenAI streaming request via the [Cmd] interpreter. *)
  try
    Switch.run
    @@ fun streaming_sw ->
    Model.set_fetch_sw model (Some streaming_sw);
    let stream = Eio.Stream.create Int.max_value in
    (* Add a placeholder stream event to the model so the UI can display it. *)
    let on_event ev = Eio.Stream.add ev_stream (`Stream ev) in
    let on_event_batch evs = Eio.Stream.add ev_stream (`Stream_batch evs) in
    (* let on_event_batch evs = Eio.Stream.add ev_stream (`Stream_batch evs) in
    let on_event = throttle streaming_sw env on_event on_event_batch in *)
    let on_fn_out ev = Eio.Stream.add ev_stream (`Function_output ev) in
    (* Fork a daemon fiber to batch events using a short time window. *)
    Eio.Fiber.fork_daemon ~sw:streaming_sw (fun () ->
      let clock = Eio.Stdenv.clock env in
      let batch_ms =
        match Sys.getenv "OCHAT_STREAM_BATCH_MS" with
        | Some s ->
          (try Float.min 50. (Float.max 1. (Float.of_string s)) with
           | _ -> 12.)
        | None -> 12.
      in
      let dt = batch_ms /. 1000. in
      let rec loop acc window_open =
        match Eio.Stream.take stream with
        | `Stream ev ->
          let acc = ev :: acc in
          if not window_open
          then (
            Fiber.fork ~sw:streaming_sw (fun () ->
              Eio.Time.sleep clock dt;
              Eio.Stream.add stream `Flush);
            loop acc true)
          else loop acc true
        | `Function_output ev ->
          (match acc with
           | [] -> ()
           | [ ev1 ] -> on_event ev1
           | _ -> on_event_batch (List.rev acc));
          on_fn_out ev;
          loop [] false
        | `Flush ->
          (match acc with
           | [] -> ()
           | [ ev1 ] -> on_event ev1
           | _ -> on_event_batch (List.rev acc));
          loop [] false
      in
      loop [] false);
    (* openapi stream events -------->  *)
    let on_fn_out ev =
      (* Function call output events are sent to the UI for rendering. *)
      Eio.Stream.add stream (`Function_output ev)
    in
    let on_event ev =
      (* OpenAI stream events are sent to the UI for rendering. *)
      Eio.Stream.add stream (`Stream ev)
    in
    let items =
      Chat_response.Driver.run_completion_stream_in_memory_v1
        ~env
        ~datadir
        ~history:(Model.history_items model)
        ~tools:(Some prompt_ctx.tools)
        ~tool_tbl:prompt_ctx.tool_tbl
        ~system_event
        ?temperature:prompt_ctx.cfg.temperature
        ?max_output_tokens:prompt_ctx.cfg.max_tokens
        ?reasoning:
          (Option.map prompt_ctx.cfg.reasoning_effort ~f:(fun eff ->
             Req.Reasoning.
               { effort = Some (Req.Reasoning.Effort.of_str_exn eff)
               ; summary = Some Req.Reasoning.Summary.Detailed
               }))
        ~history_compaction
        ?model:(Option.map prompt_ctx.cfg.model ~f:Req.model_of_str_exn)
        ~on_event
        ~on_fn_out
        ~parallel_tool_calls
        ()
    in
    Model.set_fetch_sw model None;
    Eio.Stream.add ev_stream (`Replace_history items)
  with
  | ex ->
    Model.set_fetch_sw model None;
    (* ---------------- Cleanup on streaming error ---------------- *)
    (match Model.fork_start_index model with
     | Some idx ->
       let hist_prefix = List.take (Model.history_items model) idx in
       Model.set_history_items model hist_prefix;
       Model.set_messages model (Conversation.of_history hist_prefix);
       Model.set_active_fork model None;
       Model.set_fork_start_index model None
     | None -> ());
    (* Remove dangling reasoning or incomplete function calls at the tail *)
    let prune_trailing history =
      let module Item = Openai.Responses.Item in
      let rec loop rev_items acc state =
        match rev_items with
        | [] -> acc
        | item :: rest ->
          (match state with
           | `Keep -> loop rest (item :: acc) `Keep
           | `Looking ->
             (match item with
              | Item.Output_message _ -> loop rest (item :: acc) `Keep
              | Item.Function_call_output fo ->
                loop rest (item :: acc) (`Await_call fo.call_id)
              | Item.Reasoning _ -> loop rest acc `Looking
              | _ -> loop rest (item :: acc) `Looking)
           | `Await_call cid ->
             (match item with
              | Item.Function_call fc when String.equal fc.call_id cid ->
                loop rest (item :: acc) `Keep
              | Item.Reasoning _ -> loop rest acc (`Await_call cid)
              | _ -> loop rest (item :: acc) (`Await_call cid)))
      in
      loop (List.rev history) [] `Looking
    in
    let pruned = prune_trailing (Model.history_items model) in
    Model.set_history_items model pruned;
    Model.set_messages model (Conversation.of_history pruned);
    let error_msg = Printf.sprintf "Error during streaming: %s" (Exn.to_string ex) in
    print_endline error_msg;
    (* Add an error message to the model so the UI can display it. *)
    add_placeholder_stream_error model error_msg;
    Eio.Stream.add ev_stream `Redraw
;;

(* ------------------------------------------------------------------------ *)
(*  Main entry-point                                                         *)
(* ------------------------------------------------------------------------ *)

(** [run_chat ~env ~prompt_file ()] is the {b only} public entry-point of
    the module.  Call it from your executable to start an interactive chat
    session.  The function never returns – it blocks until the user quits
    the TUI.

    Parameters:
    {ul
    {- [env] – the standard environment passed by {!Eio_main.run}.}
    {- [prompt_file] – path to a *.chatmd* document that seeds the history,
       declares tools and provides default settings.}}

    Typical usage:
    {[
      let () =
        Eio_main.run @@ fun env ->
        Chat_tui.App.run_chat ~env ~prompt_file:"prompt.chatmd" ()
    ]} *)
type persist_mode =
  [ `Always
  | `Never
  | `Ask
  ]

let run_chat
      ~env
      ~prompt_file
      ?session
      ?export_file
      ?(persist_mode = `Ask)
      ?(parallel_tool_calls = true)
      ()
  =
  Switch.run
  @@ fun ui_sw ->
  (* Event queue shared between the Notty IO thread and the pure event loop. *)
  let ev_stream
    : ([ `Resize
       | `Redraw
       | Notty.Unescape.event
       | `Stream of Res.Response_stream.t
       | `Stream_batch of Res.Response_stream.t list
       | `Replace_history of Res_item.t list
         (* Update history events are used to update the model's history items. *)
       | (* Function call output events are sent to the UI for rendering. *)
         `Function_output of Res.Function_call_output.t
       ]
       as
       'ev)
        Eio.Stream.t
    =
    Eio.Stream.create 256
  in
  let system_event = Eio.Stream.create 10 in
  (* Load the chat prompt and initialise the model. *)
  let cwd = Eio.Stdenv.cwd env in
  (* Determine the directory used to store runtime artefacts (cache,
     tool outputs, etc.).  When running inside a session we place the
     hidden [.chatmd] folder {i inside} the session directory so that
     each session has an isolated cache.  Falling back to the process
     [cwd] preserves the previous behaviour for ad-hoc one-off chats
     where no session is active. *)
  let datadir : _ Eio.Path.t =
    match session with
    | Some (s : Session.t) ->
      let session_dir = Session_store.path ~env s.id in
      let ( / ) = Eio.Path.( / ) in
      let chatmd_dir = session_dir / ".chatmd" in
      (match Eio.Path.is_directory chatmd_dir with
       | true -> ()
       | false -> Eio.Path.mkdirs ~perm:0o700 chatmd_dir);
      chatmd_dir
    | None -> Io.ensure_chatmd_dir ~cwd
  in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1000 () in
  (* Base directory of the prompt file – used for resolving relative paths in
     <import/> and <doc src="…"> tags. *)
  let prompt_dir : _ Eio.Path.t =
    let dirname = Filename.dirname prompt_file in
    if Filename.is_relative dirname
    then Eio.Path.(cwd / dirname)
    else Eio.Path.(Eio.Stdenv.fs env / dirname)
  in
  (* Load the prompt file and parse it into a list of elements. *)
  let prompt_xml =
    match Io.load_doc ~dir:(Eio.Stdenv.fs env) prompt_file with
    | s -> s
    | exception e ->
      raise
        (Failure
           (Printf.sprintf
              "Failed to load prompt file %s: %s"
              prompt_file
              (Exn.to_string e)))
  in
  let prompt_elements = CM.parse_chat_inputs ~dir:prompt_dir prompt_xml in
  let cfg = Config.of_elements prompt_elements in
  let ctx = Ctx.create ~env ~dir:prompt_dir ~tool_dir:cwd ~cache in
  let declared_tools =
    List.filter_map prompt_elements ~f:(function
      | CM.Tool t -> Some t
      | _ -> None)
  in
  let tools, tool_tbl =
    (* Tools should execute relative to user’s current working directory *)
    let user_fns =
      List.concat_map declared_tools ~f:(fun decl ->
        Tool.of_declaration
          ~sw:ui_sw
          ~ctx
          ~run_agent:(Chat_response.Driver.run_agent ~history_compaction:true)
          decl)
    in
    let comp_tools, tbl = Ochat_function.functions user_fns in
    Tool.convert_tools comp_tools, tbl
  in
  (* Convert prompt → initial history items extracted from the static prompt. *)
  let history_items_prompt =
    Converter.to_items
      ~ctx
      ~run_agent:(Chat_response.Driver.run_agent ~history_compaction:true)
      prompt_elements
  in
  (* If a persisted [session] was passed in, prefer its history – otherwise
     start from the prompt defaults. *)
  let history_items =
    match session with
    | Some s when not (List.is_empty (s : Session.t).history) -> (s : Session.t).history
    | _ -> history_items_prompt
  in
  let messages =
    let initial = Conversation.of_history history_items in
    initial
  in
  (* The original prompt items are already included at the top of the
     exported ChatMarkdown file.  We now export the *entire* runtime
     history (Task #26) instead of dropping the first
     [initial_msg_count] entries.  Keep the computed value (prefixed
     with [_] to silence unused-var warnings) in case future code
     paths still need it. *)
  let initial_msg_count = List.length history_items_prompt in
  let quit_via_esc = ref false in
  (* Load persisted draft, if exists, now that [cursor_pos] is available *)
  let model : Model.t =
    Model.create
      ~history_items
      ~messages
      ~input_line:""
      ~auto_follow:true
      ~msg_buffers:(Hashtbl.create (module String))
      ~function_name_by_id:(Hashtbl.create (module String))
      ~reasoning_idx_by_id:(Hashtbl.create (module String))
      ~tool_output_by_index:(Hashtbl.create (module Int))
      ~tasks:
        (match session with
         | Some s -> s.tasks
         | None -> [])
      ~kv_store:
        (let tbl = Hashtbl.create (module String) in
         (match session with
          | Some s ->
            List.iter s.kv_store ~f:(fun (k, v) -> Hashtbl.set tbl ~key:k ~data:v)
          | None -> ());
         tbl)
      ~fetch_sw:None
      ~scroll_box:(Scroll_box.create Notty.I.empty)
      ~cursor_pos:0
      ~selection_anchor:None
      ~mode:Insert
      ~draft_mode:Plain
      ~selected_msg:None
      ~undo_stack:[]
      ~redo_stack:[]
      ~cmdline:""
      ~cmdline_cursor:0
  in
  Model.rebuild_tool_output_index model;
  (* Start the Notty terminal – its [on_event] callback just pushes events
        into [ev_stream] so the UI stays single-threaded. *)
  Notty_eio.Term.run ~input:env#stdin ~output:env#stdout ~mouse:false ~on_event:(fun ev ->
    Eio.Stream.add
      ev_stream
      (ev
        :> [ `Resize
           | `Redraw
           | Notty.Unescape.event
           | `Stream of Res_stream.t
           | `Stream_batch of Res.Response_stream.t list
           | `Replace_history of Res_item.t list
           | `Function_output of Res.Function_call_output.t
           ]))
  @@ fun term ->
  let redraw () =
    let size = Notty_eio.Term.size term in
    let img, (cx, cy) = Renderer.render_full ~size ~model in
    Notty_eio.Term.image term img;
    Notty_eio.Term.cursor term (Some (cx, cy))
  in
  let fps =
    match Sys.getenv "OCHAT_TUI_FPS" with
    | Some s ->
      (try Float.max 1. (Float.of_string s) with
       | _ -> 30.)
    | None -> 30.
  in
  let throttler =
    Redraw_throttle.create ~fps ~enqueue_redraw:(fun () ->
      Eio.Stream.add ev_stream `Redraw)
  in
  let redraw_immediate () = Redraw_throttle.redraw_immediate throttler ~draw:redraw in
  redraw ();
  (* Start the periodic scheduler to coalesce frequent updates. *)
  Redraw_throttle.spawn throttler ~sw:ui_sw ~sleep:(fun dt ->
    Eio.Time.sleep (Eio.Stdenv.clock env) dt);
  let rec handle_cancel_or_quit () =
    match Model.fetch_sw model with
    | Some sw ->
      let cancel () = Switch.fail sw Cancelled in
      Cmd.run (Cancel_streaming cancel);
      main_loop ()
    | None ->
      (* No streaming in flight – quit requested via ESC.  Remember this so
            we can prompt for export once the UI has shut down. *)
      quit_via_esc := true
  (* Main event loop – single recursive function so local recursive calls are
        possible without a [ref]. *)
  and main_loop () =
    let ev = Eio.Stream.take ev_stream in
    match ev with
    | #Notty.Unescape.event as ev -> handle_key ev
    | `Resize ->
      redraw_immediate ();
      main_loop ()
    | `Redraw ->
      Redraw_throttle.on_redraw_handled throttler;
      redraw ();
      main_loop ()
    | `Stream ev ->
      (match not (stream_in_flight model) with
       | true -> main_loop ()
       | false ->
         let patches = Stream_handler.handle_event ~model ev in
         ignore (Model.apply_patches model patches);
         (match ev with
          | Openai.Responses.Response_stream.Output_item_done { item; _ } ->
            (match item with
             | Openai.Responses.Response_stream.Item.Output_message om ->
               ignore
               @@ Model.add_history_item model (Openai.Responses.Item.Output_message om)
             | Openai.Responses.Response_stream.Item.Reasoning r ->
               ignore @@ Model.add_history_item model (Openai.Responses.Item.Reasoning r)
             | Openai.Responses.Response_stream.Item.Function_call fc ->
               ignore
               @@ Model.add_history_item model (Openai.Responses.Item.Function_call fc)
             | _ -> ())
          | _ -> ());
         (* Defer redraw to scheduler to avoid per-token redraws. *)
         Redraw_throttle.request_redraw throttler;
         main_loop ())
    | `Stream_batch items ->
      (match not (stream_in_flight model) with
       | true -> main_loop ()
       | false ->
         (* Collect patches for the whole batch, then coalesce adjacent text appends. *)
         let patches =
           List.concat_map items ~f:(fun item -> Stream_handler.handle_event ~model item)
         in
         let weight = function
           | Types.Ensure_buffer _ -> 0
           | Types.Set_function_name _ -> 1
           | Types.Update_reasoning_idx _ -> 1
           | Types.Append_text _ -> 2
           | _ -> 3
         in
         let stable_sorted =
           List.mapi patches ~f:(fun i p -> i, p)
           |> List.stable_sort ~compare:(fun (i1, p1) (i2, p2) ->
             match Int.compare (weight p1) (weight p2) with
             | 0 -> Int.compare i1 i2
             | c -> c)
           |> List.map ~f:snd
         in
         let rec coalesce acc = function
           | [] -> List.rev acc
           | Types.Append_text a1 :: Types.Append_text a2 :: rest
             when String.equal a1.id a2.id && String.equal a1.role a2.role ->
             let merged = Types.Append_text { a1 with text = a1.text ^ a2.text } in
             coalesce acc (merged :: rest)
           | p :: rest -> coalesce (p :: acc) rest
         in
         let patches = coalesce [] stable_sorted in
         ignore (Model.apply_patches model patches);
         (* Update history for completed items after applying the batch. *)
         List.iter items ~f:(fun item ->
           match item with
           | Openai.Responses.Response_stream.Output_item_done { item; _ } ->
             (match item with
              | Openai.Responses.Response_stream.Item.Output_message om ->
                ignore
                @@ Model.add_history_item model (Openai.Responses.Item.Output_message om)
              | Openai.Responses.Response_stream.Item.Reasoning r ->
                ignore @@ Model.add_history_item model (Openai.Responses.Item.Reasoning r)
              | Openai.Responses.Response_stream.Item.Function_call fc ->
                ignore
                @@ Model.add_history_item model (Openai.Responses.Item.Function_call fc)
              | _ -> ())
           | _ -> ());
         (* Defer redraw to scheduler to avoid per-batch burst redraws. *)
         Redraw_throttle.request_redraw throttler;
         main_loop ())
    | `Function_output out ->
      (match not (stream_in_flight model) with
       | true -> main_loop ()
       | false ->
         let patches = Stream_handler.handle_fn_out ~model out in
         ignore (Model.apply_patches model patches);
         ignore (Model.add_history_item model (Res_item.Function_call_output out));
         (* Defer redraw to scheduler to coalesce frequent updates. *)
         Redraw_throttle.request_redraw throttler;
         main_loop ())
    | `Replace_history items ->
      (* Replace the model's history items with the new ones. *)
      Model.set_history_items model items;
      Model.set_messages model (Conversation.of_history (Model.history_items model));
      Model.rebuild_tool_output_index model;
      (* Update the UI to reflect the new function output. *)
      redraw_immediate ();
      main_loop ()
  (* Defer to controller for local key handling first. *)
  and handle_key (ev : Notty.Unescape.event) =
    match Controller.handle_key ~model ~term ev with
    | Controller.Redraw ->
      Redraw_throttle.request_redraw throttler;
      main_loop ()
    | Controller.Submit_input ->
      (match Model.fetch_sw model with
       | Some _ ->
         let user_msg = String.strip (Model.input_line model) in
         let msg = sprintf "This is a Note From the User:\n%s" user_msg in
         Model.set_input_line model "";
         Model.set_cursor_pos model 0;
         Eio.Stream.add system_event msg;
         Redraw_throttle.request_redraw throttler;
         main_loop ()
       | None ->
         apply_local_submit_effects ~dir:cwd ~env ~cache ~model ~ev_stream ~term;
         Fiber.fork ~sw:ui_sw (fun () ->
           handle_submit
             ~env
             ~model
             ~ev_stream
             ~system_event
             ~prompt_ctx:{ cfg; tools; tool_tbl }
             ~datadir
             ~parallel_tool_calls
             ~history_compaction:true);
         main_loop ())
    | Controller.Cancel_or_quit -> handle_cancel_or_quit ()
    | Controller.Compact_context ->
      (* Do nothing if a streaming request is in flight – compaction acts on
         a {i stable} history snapshot.  Showing a terse system message keeps
         the user informed without disrupting the UI. *)
      (match Model.fetch_sw model with
       | Some _ ->
         add_placeholder_stream_error model "Cannot compact while streaming.";
         Redraw_throttle.request_redraw throttler;
         main_loop ()
       | None ->
         add_placeholder_compact_message model;
         (* Fork a new switch to run the compaction in the background.  This
            allows the user to continue interacting with the UI while the
            compaction runs. *)
         (* Note: we do not use [Switch.run] here because it would block the
            UI thread until compaction finishes. *)
         Log.emit `Info "Compacting history items…";
         (* Start a new switch for the compaction operation. *)
         Fiber.fork ~sw:ui_sw (fun () ->
           try
             Switch.run
             @@ fun streaming_sw ->
             Model.set_fetch_sw model (Some streaming_sw);
             (* Compact the history items in the model.  This is a pure
            operation that does not block the UI thread. *)
             (match session with
              | None -> ()
              | Some s ->
                persist_snapshot env session model;
                Session_store.reset_session ~env ~id:s.id ~keep_history:false ());
             let history' =
               Context_compaction.Compactor.compact_history
                 ~env:(Some env)
                 ~history:(Model.history_items model)
             in
             (* Replace model state. *)
             Model.set_history_items model history';
             Model.set_messages model (Conversation.of_history history');
             Model.rebuild_tool_output_index model;
             (* After compaction the selection is cleared and viewport resets. *)
             Model.select_message model None;
             Model.set_auto_follow model true;
             Redraw_throttle.request_redraw throttler;
             Model.set_fetch_sw model None
             (* Show a success message. *)
           with
           | _ ->
             Model.set_fetch_sw model None;
             add_placeholder_stream_error model "Compaction failed.";
             Redraw_throttle.request_redraw throttler);
         (* Continue the main loop after compaction. *)
         Redraw_throttle.request_redraw throttler;
         main_loop ())
    | Controller.Quit -> ()
    | Controller.Unhandled ->
      (match ev with
       | `Paste `End ->
         Log.emit `Warn "Unhandled paste event – this is a bug.";
         main_loop ()
       | `Paste `Start ->
         Log.emit `Warn "Unhandled paste start event – this is a bug.";
         main_loop ()
       | _ -> main_loop ())
  in
  main_loop ();
  (* Helper: export conversation to ChatMarkdown at [target_path]. *)
  let do_export ~target_path () =
    Export.archive ~env ~model ~prompt_file ~target_path ~cfg ~initial_msg_count ~session
  in
  (* ------------------------------------------------------------------ *)
  (* Shutdown & persistence                                              *)
  (* ------------------------------------------------------------------ *)

  (* Decide whether to export the conversation as ChatMarkdown.  When the
     user quits via ESC we prompt for confirmation; otherwise we keep the
     previous automatic export behaviour.  Snapshot persistence happens
     independently via [persist_snapshot]. *)
  let export_session ~target_path () =
    let cmd : Types.cmd = Persist_session (fun () -> do_export ~target_path ()) in
    Cmd.run cmd
  in
  (* Release the Notty terminal before printing any messages to stdout so
     that they appear correctly in the user’s shell.  We do this once and
     only once – further calls are benign but avoided for clarity. *)
  Notty_eio.Term.release term;
  (match !quit_via_esc with
   | false ->
     (* Quit triggered via other means (Ctrl-C / q) – export automatically. *)
     let target = Option.value export_file ~default:prompt_file in
     export_session ~target_path:target ()
   | true ->
     Out_channel.(output_string stdout "Export conversation to promptmd file? [y/N] ");
     Out_channel.flush stdout;
     (match In_channel.input_line In_channel.stdin with
      | Some ans
        when List.mem
               [ "y"; "yes" ]
               (String.lowercase (String.strip ans))
               ~equal:String.equal ->
        (* Determine target path *)
        let target_path =
          match export_file with
          | Some p -> p
          | None ->
            (* Prompt for filename *)
            Out_channel.output_string
              stdout
              (Printf.sprintf "Enter output file path [default: %s]: " prompt_file);
            Out_channel.flush stdout;
            (match In_channel.input_line In_channel.stdin with
             | Some line when not (String.is_empty (String.strip line)) ->
               String.strip line
             | _ -> prompt_file)
        in
        export_session ~target_path ()
      | _ -> ()));
  (* Decide whether to persist the snapshot. *)
  let should_persist =
    match persist_mode with
    | `Always -> true
    | `Never -> false
    | `Ask ->
      (* Ask the user – default to yes. *)
      (match session with
       | None -> false
       | Some _ ->
         Out_channel.output_string stdout "Save session snapshot? [Y/n] ";
         Out_channel.flush stdout;
         (match In_channel.input_line In_channel.stdin with
          | Some ans ->
            let ans = String.lowercase (String.strip ans) in
            not (List.mem [ "n"; "no" ] ans ~equal:String.equal)
          | None -> true))
  in
  if should_persist
  then persist_snapshot env session model
  else Log.emit `Info "Skipping session persistence as per user request."
;;
(* Exit the program. *)
