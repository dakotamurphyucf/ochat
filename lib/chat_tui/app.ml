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
module Snippet = Snippet
module CM = Prompt.Chat_markdown
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

(* let run_command ~env ~model () =
  let user_msg = String.strip (Model.input_line model) in
  (* -------------------------------------------------------------- *)
  (*  /format [lang] – run external formatter on draft               *)
  (*     Currently supports only "ocaml" via ocamlformat.           *)
  (* -------------------------------------------------------------- *)
  if String.is_prefix user_msg ~prefix:"/format"
  then (
    let lang =
      match String.split (String.strip user_msg) ~on:' ' with
      | [ _ ] -> "ocaml" (* default *)
      | _ :: l :: _ -> String.lowercase l
      | _ -> "ocaml"
    in
    match lang with
    | "ocaml" ->
      (* Extract the draft minus the /format command line *)
      let all_lines = String.split_lines (Model.input_line model) in
      let body_lines, has_command_line =
        match List.last all_lines with
        | Some last when String.is_prefix (String.strip last) ~prefix:"/format" ->
          List.drop_last_exn (List.tl_exn all_lines), true
        | _ -> all_lines, false
      in
      if not has_command_line
      then (
        let txt = "Place '/format' on its own line at the end of the draft." in
        let patch = Add_placeholder_message { role = "system"; text = txt } in
        ignore (Model.apply_patch model patch);
        true)
      else (
        let code = String.concat ~sep:"\n" body_lines in
        (* ----------------------------------------------------------- *)
        (*  Helper – run ocamlformat with a watchdog timeout           *)
        (* ----------------------------------------------------------- *)
        let run_ocamlformat ~env code : (string, string) result =
          (* We wrap the invocation in [Eio.Time.with_timeout] so a
               wedged formatter process cannot block the UI forever. *)
          let clock = Eio.Stdenv.clock env in
          match
            Eio.Time.with_timeout clock 5.0 (fun () ->
              (* Run the formatter in its own switch so that a timeout
                     cancels the whole subtree – this cleans up the child
                     process and any open fds automatically. *)
              Eio.Switch.run
              @@ fun sw ->
              let proc_mgr = Eio.Stdenv.process_mgr env in
              let stdin_r, stdin_w = Eio.Process.pipe ~sw proc_mgr in
              let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
              let stderr_r, stderr_w = Eio.Process.pipe ~sw proc_mgr in
              (* Spawn ocamlformat, reading from [stdin_r]. *)
              let _child =
                Eio.Process.spawn
                  ~sw
                  proc_mgr
                  ~stdin:stdin_r
                  ~stdout:stdout_w
                  ~stderr:stderr_w
                  [ "ocamlformat"; "--impl"; "-" ]
              in
              (* Parent writes code to child's stdin and closes it so the
                     formatter sees EOF. *)
              Eio.Flow.copy_string code stdin_w;
              Eio.Flow.close stdin_w;
              (* Close our copies of the write ends to avoid leaks. *)
              Eio.Flow.close stdout_w;
              Eio.Flow.close stderr_w;
              (* Read stdout and stderr completely. *)
              let read_all r =
                try
                  Eio.Buf_read.parse_exn ~max_size:5_000_000 Eio.Buf_read.take_all r
                with
                | ex -> Fmt.str "(error reading formatter output: %a)" Eio.Exn.pp ex
              in
              let stdout_str = read_all stdout_r in
              let stderr_str = read_all stderr_r in
              if String.is_empty stderr_str then Ok stdout_str else failwith stderr_str)
          with
          | Ok formatted -> Ok formatted
          | Error `Timeout -> Error "ocamlformat timed out after 5s"
        in
        let formatted_or_error =
          try run_ocamlformat ~env code with
          | ex -> Error (Fmt.str "Failed to run ocamlformat: %a" Eio.Exn.pp ex)
        in
        match formatted_or_error with
        | Ok formatted ->
          Model.set_input_line model formatted;
          Model.set_cursor_pos model (String.length formatted);
          Model.clear_selection model;
          true
        | Error msg ->
          let txt = "ocamlformat error:\n" ^ msg in
          let patch = Add_placeholder_message { role = "system"; text = txt } in
          ignore (Model.apply_patch model patch);
          true)
    | _ ->
      let txt = "Unsupported /format language. Only 'ocaml' is supported." in
      let patch = Add_placeholder_message { role = "system"; text = txt } in
      ignore (Model.apply_patch model patch);
      true
    (* -------------------------------------------------------------- *)
    (*  /expand NAME – insert predefined snippet                      *)
    (* -------------------------------------------------------------- *))
  else if String.is_prefix user_msg ~prefix:"/expand "
  then (
    let name = String.strip (String.drop_prefix user_msg 8) |> String.lowercase in
    match Snippet.find name with
    | None ->
      let available = String.concat ~sep:", " (Snippet.available ()) in
      let txt =
        Printf.sprintf "Unknown snippet '%s'.  Available snippets: %s" name available
      in
      let patch = Add_placeholder_message { role = "system"; text = txt } in
      ignore (Model.apply_patch model patch);
      true
    | Some snippet_text ->
      (* Replace the '/expand NAME' command line with the snippet. *)
      let all_lines = String.split_lines (Model.input_line model) in
      let rec drop_last_if_cmd acc = function
        | [] -> List.rev acc, false
        | [ last ] ->
          let is_cmd = String.is_prefix (String.strip last) ~prefix:"/expand " in
          if is_cmd then List.rev acc, true else List.rev (last :: acc), false
        | hd :: tl -> drop_last_if_cmd (hd :: acc) tl
      in
      let before_lines, has_cmd = drop_last_if_cmd [] all_lines in
      if not has_cmd
      then
        (* Command not on its own line – don't expand, treat as normal submit. *)
        false
      else (
        let new_lines = before_lines @ [ snippet_text ] in
        let new_text = String.concat ~sep:"\n" new_lines in
        Model.set_input_line model new_text;
        Model.set_cursor_pos model (String.length new_text);
        Model.clear_selection model;
        true))
  else false
;; *)

let apply_local_submit_effects ~model ~ev_stream ~term =
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
    (* Add to visible conversation and canonical history *)
    let patch = Add_user_message { text = user_msg } in
    ignore (Model.apply_patch model patch));
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

type prompt_context =
  { cfg : Config.t (* Configuration parsed from the prompt file. *)
  ; tools : Req.Tool.t list (* List of tools declared in the prompt file. *)
  ; tool_tbl : (string, string -> string) Hashtbl.t (* Lookup table for tools by name. *)
  }

(* ────────────────────────────────────────────────────────────────────────── *)
(*  Main event handler for submitting the draft to the assistant             *)
(* ────────────────────────────────────────────────────────────────────────── *)

(* [handle_submit] is the main entry-point for processing user input.
      It handles inline commands, submits the draft to the assistant, and
      manages the UI updates accordingly. *)

let handle_submit ~env ~ui_sw ~model ~ev_stream ~prompt_ctx =
  (* Kick off the OpenAI streaming request via the [Cmd] interpreter. *)
  let start_streaming () =
    Fiber.fork ~sw:ui_sw (fun () ->
      try
        Switch.run
        @@ fun streaming_sw ->
        Model.set_fetch_sw model (Some streaming_sw);
        let on_event ev = Eio.Stream.add ev_stream (`Stream ev) in
        let on_fn_out ev = Eio.Stream.add ev_stream (`Function_output ev) in
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
        Model.set_history_items model items;
        Model.set_messages model (Conversation.of_history (Model.history_items model));
        Eio.Stream.add ev_stream `Redraw;
        Model.set_fetch_sw model None
      with
      | ex ->
        Model.set_fetch_sw model None;
        prerr_endline (Printf.sprintf "Error during streaming: %s" (Exn.to_string ex));
        Eio.Stream.add ev_stream `Redraw)
  in
  Cmd.run (Start_streaming start_streaming)
;;

(* ------------------------------------------------------------------------ *)
(*  Main entry-point                                                         *)
(* ------------------------------------------------------------------------ *)

let run_chat ~env ~prompt_file () =
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
    Eio.Stream.create 64
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
    | Controller.Submit_input ->
      (match Model.fetch_sw model with
       | Some _ -> main_loop ()
       | None ->
         apply_local_submit_effects ~model ~ev_stream ~term;
         Fiber.fork ~sw:ui_sw (fun () ->
           handle_submit
             ~env
             ~ui_sw
             ~model
             ~ev_stream
             ~prompt_ctx:{ cfg; tools; tool_tbl });
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
