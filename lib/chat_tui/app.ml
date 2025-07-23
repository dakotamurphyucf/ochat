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

(* ────────────────────────────────────────────────────────────────────────── *)
(*  Local helpers                                                            *)
(* ────────────────────────────────────────────────────────────────────────── *)

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
  let user_msg = String.strip (Model.input_line model) in
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
      ignore (Model.apply_patch model (Add_user_message { text = user_msg }))
    | Model.Raw_xml ->
      let module CM = Prompt.Chat_markdown in
      let xml =
        if String.is_prefix ~prefix:"<" user_msg
        then user_msg
        else Printf.sprintf "<user>\n%s\n</user>" user_msg
      in
      let elements =
        try CM.parse_chat_inputs ~dir xml with
        | _ -> []
      in
      let user_msg =
        List.find_map_exn elements ~f:(function
          | CM.User m ->
            let ctx = Ctx.create ~env ~dir ~cache in
            Some
              (Converter.convert_user_msg
                 ~ctx
                 ~run_agent:Chat_response.Driver.run_agent
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

(* [handle_submit] is the main entry-point for processing user input.
      It handles inline commands, submits the draft to the assistant, and
      manages the UI updates accordingly. *)

(* let debounce_duration = 0.016 *)

(* [throttle] is a debounced version of a function that processes user input.
   It ensures that the function is called at most once every [debounce_duration]
   seconds, even if it is called multiple times in quick succession. *)
(* Note: This implementation uses a mutex to ensure thread-safety when accessing
   the buffer and last call time. *)
(* let throttle sw env f f_batch =
  let buffer = ref [] in
  let mutex = Eio.Mutex.create () in
  (* Use a mutex to ensure that only one fiber can access the buffer at a time. *)
  let clock = Eio.Stdenv.mono_clock env in
  let last_call = ref (Mtime.of_uint64_ns Int64.zero) in
  fun a ->
    let now = Eio.Time.Mono.now clock in
    let diff = Mtime.span now !last_call |> Mtime.Span.to_float_ns in
    let open Float in
    (* If the time since the last call is greater than the debounce duration,
       call the function and update the last call time. Otherwise, do nothing. *)
    Eio.Mutex.lock mutex;
    if diff >= debounce_duration
    then (
      last_call := now;
      let items = !buffer in
      buffer := [];
      match items with
      | [] -> f a
      | _ -> f_batch (List.rev (a :: items)))
    else (
      match !buffer with
      | [] ->
        buffer := [ a ];
        let timeout = Eio.Time.Timeout.seconds clock debounce_duration in
        Fiber.fork ~sw (fun () ->
          (* Wait for the debounce duration before calling the function. *)
          Eio.Time.Timeout.sleep timeout;
          (* Call the function with the buffered items. *)
          (* Note: This assumes that [f] can handle a list of items. *)
          Eio.Mutex.lock mutex;
          (* Only call the unction if the buffer is not empty. *)
          let items = !buffer in
          buffer := [];
          f_batch (List.rev items);
          Eio.Mutex.unlock mutex
          (* Unlock the mutex after processing the buffer. *))
      | _ -> buffer := a :: !buffer);
    Eio.Mutex.unlock mutex;
    ()
;; *)

(** [handle_submit ~env ~model ~ev_stream ~prompt_ctx] launches the
    *asynchronous* OpenAI completion request in a fresh switch and wires the
    streaming callbacks back into the UI.  The function never blocks the UI
    thread – it schedules fibres that push events into [ev_stream], which
    are then folded into the model by the main loop.

    On success the returned tokens replace the temporary "thinking…"
    placeholder and are persisted in {!Model.history_items}.  On failure the
    conversation is {i rolled-back} to the state before the request started
    and an error message is shown. *)
let handle_submit ~env ~model ~ev_stream ~prompt_ctx =
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
    (* Fork a daemon fiber to batch events that  *)
    Eio.Fiber.fork_daemon ~sw:streaming_sw (fun () ->
      let rec loop () =
        match Eio.Stream.take stream with
        | `Stream ev ->
          let rec inner_loop acc =
            match Eio.Stream.take_nonblocking stream with
            | None ->
              (match acc with
               | [] -> ()
               | [ ev ] -> on_event ev
               | _ ->
                 Io.log
                   ~dir:(Eio.Stdenv.cwd env)
                   ~file:"batch.txt"
                   (Sexp.to_string [%sexp "Batching events", (acc : Res_stream.t list)]
                    ^ "\n");
                 (* If we have accumulated events, send them as a batch. *)
                 on_event_batch (List.rev acc))
            | Some (`Stream ev) -> inner_loop (ev :: acc)
            | Some (`Function_output ev) ->
              if List.is_empty acc
              then on_fn_out ev
              else (
                (* If we have accumulated events, send them as a batch. *)
                on_event_batch (List.rev acc);
                on_fn_out ev);
              inner_loop []
          in
          inner_loop [ ev ];
          loop ()
        | `Function_output ev ->
          on_fn_out ev;
          loop ()
      in
      loop ());
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
        ~history:(Model.history_items model)
        ~tools:(Some prompt_ctx.tools)
        ~tool_tbl:prompt_ctx.tool_tbl
        ?temperature:prompt_ctx.cfg.temperature
        ?max_output_tokens:prompt_ctx.cfg.max_tokens
        ?reasoning:
          (Option.map prompt_ctx.cfg.reasoning_effort ~f:(fun eff ->
             Req.Reasoning.
               { effort = Some (Req.Reasoning.Effort.of_str_exn eff)
               ; summary = Some Req.Reasoning.Summary.Detailed
               }))
        ?model:(Option.map prompt_ctx.cfg.model ~f:Req.model_of_str_exn)
        ~on_event
        ~on_fn_out
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
        | [] -> List.rev acc
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
let run_chat ~env ~prompt_file () =
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
    Eio.Stream.create 10
  in
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1000 () in
  let dir = Eio.Stdenv.fs env in
  let prompt_xml =
    match Io.load_doc ~dir prompt_file with
    | s -> s
    | exception _ -> ""
  in
  let prompt_elements = CM.parse_chat_inputs ~dir prompt_xml in
  let cfg = Config.of_elements prompt_elements in
  let declared_tools =
    List.filter_map prompt_elements ~f:(function
      | CM.Tool t -> Some t
      | _ -> None)
  in
  let tools, tool_tbl =
    let ctx_for_tool decl =
      let dir = Eio.Stdenv.cwd env in
      match decl with
      | CM.Agent _ -> Ctx.create ~env ~dir ~cache
      | _ -> Ctx.create ~env ~dir ~cache
    in
    let user_fns =
      List.concat_map declared_tools ~f:(fun decl ->
        let ctx_tool = ctx_for_tool decl in
        Tool.of_declaration
          ~sw:ui_sw
          ~ctx:ctx_tool
          ~run_agent:Chat_response.Driver.run_agent
          decl)
    in
    let comp_tools, tbl = Gpt_function.functions user_fns in
    Tool.convert_tools comp_tools, tbl
  in
  (* Convert prompt → initial history items. *)
  let ctx_prompt = Ctx.create ~env ~dir ~cache in
  let history_items =
    Converter.to_items
      ~ctx:ctx_prompt
      ~run_agent:Chat_response.Driver.run_agent
      prompt_elements
  in
  let messages =
    let initial = Conversation.of_history history_items in
    initial
  in
  let initial_msg_count = List.length history_items in
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
  redraw ();
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
      redraw ();
      main_loop ()
    | `Redraw ->
      redraw ();
      main_loop ()
    | `Stream ev ->
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
      redraw ();
      main_loop ()
    | `Stream_batch items ->
      List.iter items ~f:(fun item ->
        let patches = Stream_handler.handle_event ~model item in
        ignore (Model.apply_patches model patches);
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
      redraw ();
      main_loop ()
    | `Function_output out ->
      let patches = Stream_handler.handle_fn_out ~model out in
      ignore (Model.apply_patches model patches);
      ignore (Model.add_history_item model (Res_item.Function_call_output out));
      (* Update the UI to reflect the new function output. *)
      redraw ();
      main_loop ()
    | `Replace_history items ->
      (* Replace the model's history items with the new ones. *)
      Model.set_history_items model items;
      Model.set_messages model (Conversation.of_history (Model.history_items model));
      (* Update the UI to reflect the new function output. *)
      redraw ();
      main_loop ()
  (* Defer to controller for local key handling first. *)
  and handle_key (ev : Notty.Unescape.event) =
    match Controller.handle_key ~model ~term ev with
    | Controller.Redraw ->
      redraw ();
      main_loop ()
    | Controller.Submit_input ->
      (match Model.fetch_sw model with
       | Some _ -> main_loop ()
       | None ->
         apply_local_submit_effects ~dir:cwd ~env ~cache ~model ~ev_stream ~term;
         Fiber.fork ~sw:ui_sw (fun () ->
           handle_submit ~env ~model ~ev_stream ~prompt_ctx:{ cfg; tools; tool_tbl });
         main_loop ())
    | Controller.Cancel_or_quit -> handle_cancel_or_quit ()
    | Controller.Quit -> ()
    | Controller.Unhandled -> main_loop ()
  in
  main_loop ();
  (* On shutdown, decide whether to persist the session.  We ask for
        confirmation when the user pressed ESC to quit, otherwise keep the
        previous auto-export behaviour. *)
  let export_session () =
    let cmd : Types.cmd =
      Persist_session
        (fun () ->
          Persistence.persist_session
            ~dir
            ~prompt_file
            ~datadir
            ~cfg
            ~initial_msg_count
            ~history_items:(Model.history_items model))
    in
    Cmd.run cmd
  in
  match !quit_via_esc with
  | false ->
    (* Quit triggered via other means (Ctrl-C / q) – preserve previous
           behaviour and export automatically. *)
    export_session ()
  | true ->
    Notty_eio.Term.release term;
    Out_channel.(output_string stdout "Export conversation to promptmd file? [y/N] ");
    Out_channel.flush stdout;
    (match In_channel.input_line In_channel.stdin with
     | Some ans ->
       let ans = String.lowercase (String.strip ans) in
       if List.mem [ "y"; "yes" ] ans ~equal:String.equal then export_session ()
     | None -> ())
;;
