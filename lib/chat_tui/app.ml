(* The full implementation of the interactive Chat-TUI application.

   This module was extracted from the former [bin/chat_tui.ml] monolith as
   part of the ongoing refactor.  It contains the entire orchestration logic
   that wires together

   • Notty_eio for terminal IO,
   • Chat_tui.{Model,Renderer,Controller,Stream,Cmd}, and
   • Chat_response.Driver for OpenAI streaming.

   Having all of this code inside [lib/] means that other entry-points (e.g.
   tests or future GUI variants) can launch the TUI without duplicating the
   logic.  The executable in [bin/] now merely parses command-line flags and
   delegates to [run_chat]. *)

open Core
open Eio.Std
open Types
module Model = Model
module Renderer = Renderer
module Stream_handler = Stream
module Controller = Controller
module Persistence = Persistence
module Cmd = Cmd
module CM = Prompt_template.Chat_markdown
module Scroll_box = Notty_scroll_box
module Res = Openai.Responses
module Res_stream = Res.Response_stream

(* Removed unused alias [Item_stream].  [Res_item] and [Converter] are still
   required further below. *)
module Res_item = Res.Item
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module Cache = Chat_response.Cache
module Tool = Chat_response.Tool
module Config = Chat_response.Config
module Req = Res.Request

exception Cancelled

(* ────────────────────────────────────────────────────────────────────────── *)
(*  Local helpers                                                            *)
(* ────────────────────────────────────────────────────────────────────────── *)

(* Emit a transient “(thinking…)” placeholder so the user sees immediate
   feedback after hitting submit.  The placeholder is appended via a patch so
   the mutation goes through the centralised [Model.apply_patch] function. *)
let add_placeholder_thinking_message (model : Model.t) : unit =
  let patch = Add_placeholder_message { role = "assistant"; text = "(thinking…)" } in
  ignore (Model.apply_patch model patch)
;;

(* ------------------------------------------------------------------------ *)
(*  Main entry-point                                                         *)
(* ------------------------------------------------------------------------ *)

let run_chat ~env ~prompt_file () =
  (* We wrap the whole UI inside its own switch so that helper fibres get
     cancelled automatically once the UI exits. *)
  Switch.run
  @@ fun ui_sw ->
  (* Event queue shared between the Notty IO thread and the pure event loop. *)
  let ev_stream
    : ([ `Resize
       | `Redraw
       | Notty.Unescape.event
       | `Stream of Res.Response_stream.t
       | `Function_output of Res.Function_call_output.t
       ]
       as
       'ev)
        Eio.Stream.t
    =
    Eio.Stream.create 0
  in
  (* Shared cache – used for agent runs & fetches. *)
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1000 () in
  (* ──────────────────────── Initial prompt parsing ─────────────────────── *)
  let dir = Eio.Stdenv.fs env in
  let prompt_xml =
    match Io.load_doc ~dir prompt_file with
    | s -> s
    | exception _ -> ""
  in
  let prompt_elements = CM.parse_chat_inputs ~dir prompt_xml in
  let cfg = Config.of_elements prompt_elements in
  (* Build the tool declarations and lookup table. *)
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
    let fns =
      List.map declared_tools ~f:(fun decl ->
        let ctx_tool = ctx_for_tool decl in
        Tool.of_declaration ~ctx:ctx_tool ~run_agent:Chat_response.Driver.run_agent decl)
    in
    let comp_tools, tbl = Gpt_function.functions fns in
    Tool.convert_tools comp_tools, tbl
  in
  (* Convert prompt → initial history items. *)
  let ctx_prompt = Ctx.create ~env ~dir ~cache in
  let history_items =
    ref
      (Converter.to_items
         ~ctx:ctx_prompt
         ~run_agent:Chat_response.Driver.run_agent
         prompt_elements)
  in
  let messages =
    let initial = Conversation.of_history !history_items in
    ref initial
  in
  let initial_msg_count = List.length !messages in
  (* Mutable pieces that form the [Model.t]. *)
  let input_line = ref "" in
  let auto_follow = ref true in
  let msg_buffers : (string, msg_buffer) Hashtbl.t = Hashtbl.create (module String) in
  let function_name_by_id : (string, string) Hashtbl.t = Hashtbl.create (module String) in
  let reasoning_idx_by_id : (string, int ref) Hashtbl.t =
    Hashtbl.create (module String)
  in
  let fetch_sw : Switch.t option ref = ref None in
  let scroll_box = Scroll_box.create Notty.I.empty in
  let cursor_pos = ref 0 in
  let first_draw = ref true in
  let model : Model.t =
    Model.create
      ~history_items
      ~messages
      ~input_line
      ~auto_follow
      ~msg_buffers
      ~function_name_by_id
      ~reasoning_idx_by_id
      ~fetch_sw
      ~scroll_box
      ~cursor_pos
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
           | `Function_output of Res.Function_call_output.t
           ]))
  @@ fun term ->
  (* Helper – render the entire UI and refresh the terminal. *)
  let redraw () =
    let size = Notty_eio.Term.size term in
    let img, (cx, cy) = Renderer.render_full ~size ~model in
    Notty_eio.Term.image term img;
    Notty_eio.Term.cursor term (Some (cx, cy));
    first_draw := false
  in
  redraw ();
  (* --------------------------------------------------------------------- *)
  (*  High-level UI actions                                                *)
  (* --------------------------------------------------------------------- *)

  (* Submitting the current input buffer to the assistant. *)
  let rec handle_submit () =
    let user_msg = String.strip !input_line in
    input_line := "";
    cursor_pos := 0;
    if not (String.is_empty user_msg)
    then (
      let patch = Add_user_message { text = user_msg } in
      ignore (Model.apply_patch model patch));
    auto_follow := true;
    let _, h = Notty_eio.Term.size term in
    let input_h =
      match String.split_lines !input_line with
      | [] -> 1
      | ls -> List.length ls
    in
    Scroll_box.scroll_to_bottom scroll_box ~height:(h - input_h);
    (* Visual feedback while waiting for the first streaming tokens. *)
    add_placeholder_thinking_message model;
    redraw ();
    (* Kick off the OpenAI streaming request via the [Cmd] interpreter. *)
    let start_streaming () =
      Fiber.fork ~sw:ui_sw (fun () ->
        try
          Switch.run
          @@ fun streaming_sw ->
          fetch_sw := Some streaming_sw;
          let on_event ev = Eio.Stream.add ev_stream (`Stream ev) in
          let on_fn_out ev = Eio.Stream.add ev_stream (`Function_output ev) in
          history_items
          := Chat_response.Driver.run_completion_stream_in_memory_v1
               ~env
               ~history:!history_items
               ~tools:(Some tools)
               ~tool_tbl
               ?temperature:cfg.temperature
               ?max_output_tokens:cfg.max_tokens
               ?reasoning:
                 (Option.map cfg.reasoning_effort ~f:(fun eff ->
                    Req.Reasoning.
                      { effort = Some (Req.Reasoning.Effort.of_str_exn eff)
                      ; summary = Some Req.Reasoning.Summary.Detailed
                      }))
               ?model:(Option.map cfg.model ~f:Req.model_of_str_exn)
               ~on_event
               ~on_fn_out
               ();
          messages := Conversation.of_history !history_items;
          Eio.Stream.add ev_stream `Redraw;
          fetch_sw := None
        with
        | ex ->
          fetch_sw := None;
          prerr_endline (Printf.sprintf "Error during streaming: %s" (Exn.to_string ex));
          Eio.Stream.add ev_stream `Redraw)
    in
    Cmd.run (Start_streaming start_streaming);
    main_loop ()
  (* ESC – cancel running request if any, otherwise quit. *)
  and handle_cancel_or_quit () =
    match !fetch_sw with
    | Some sw ->
      let cancel () = Switch.fail sw Cancelled in
      Cmd.run (Cancel_streaming cancel);
      main_loop ()
    | None -> ()
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
      redraw ();
      main_loop ()
    | `Function_output out ->
      let patches = Stream_handler.handle_fn_out ~model out in
      ignore (Model.apply_patches model patches);
      redraw ();
      main_loop ()
  (* Defer to controller for local key handling first. *)
  and handle_key (ev : Notty.Unescape.event) =
    match Controller.handle_key ~model ~term ev with
    | Controller.Redraw ->
      redraw ();
      main_loop ()
    | Controller.Submit_input -> handle_submit ()
    | Controller.Cancel_or_quit -> handle_cancel_or_quit ()
    | Controller.Quit -> ()
    | Controller.Unhandled -> main_loop ()
  in
  main_loop ();
  (* On shutdown, persist new messages added during the session. *)
  let cmd : Types.cmd =
    Persist_session
      (fun () ->
        Persistence.persist_session
          ~dir
          ~prompt_file
          ~datadir
          ~cfg
          ~initial_msg_count
          ~history_items:!history_items)
  in
  Cmd.run cmd
;;

(* [run_chat] *)
