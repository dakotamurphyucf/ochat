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

  (* ───────────────────── Draft autosave / restore ────────────────────── *)
  let draft_filename = "draft.txt" in
  let last_saved_draft : string option ref = ref None in
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
  (* Flag indicating whether the session ended via the ESC key (idle state).
     We use this to decide whether to prompt for exporting the conversation.*)
  let quit_via_esc = ref false in
  (* Load persisted draft, if exists, now that [cursor_pos] is available *)
  (match Io.load_doc ~dir:datadir draft_filename with
   | s when not (String.is_empty s) ->
     input_line := s;
     cursor_pos := String.length s;
     last_saved_draft := Some s
   | exception _ -> ()
   | _ -> ());
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
      ~draft_history:(ref [])
      ~draft_history_pos:(ref 0)
      ~selection_anchor:(ref None)
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
    first_draw := false;
    (* Autosave draft if modified *)
    let current_draft = !input_line in
    let needs_save =
      match !last_saved_draft with
      | Some prev when String.equal prev current_draft -> false
      | _ -> true
    in
    if needs_save then (
      Io.save_doc ~dir:datadir draft_filename current_draft;
      last_saved_draft := Some current_draft)
  in
  redraw ();
  (* --------------------------------------------------------------------- *)
  (*  High-level UI actions                                                *)
  (* --------------------------------------------------------------------- *)

  (* Submitting the current input buffer to the assistant. *)
  let rec handle_submit () =
    let user_msg = String.strip !input_line in
    (* ------------------------------------------------------------------ *)
    (*  Inline helper commands ("/wrap", "/count", …)                  *)
    (* ------------------------------------------------------------------ *)
    let command_handled =
      (* -------------------------------------------------------------- *)
      (*  /wrap N – reflow the current draft to N columns                *)
      (* -------------------------------------------------------------- *)
      if String.is_prefix user_msg ~prefix:"/wrap "
      then (
        match Int.of_string_opt (String.strip (String.drop_prefix user_msg 6)) with
        | None ->
          let txt = "Usage: /wrap N  – where N is a positive integer." in
          let patch = Add_placeholder_message { role = "system"; text = txt } in
          ignore (Model.apply_patch model patch);
          true
        | Some width when width <= 0 ->
          let txt = "Width must be > 0." in
          let patch = Add_placeholder_message { role = "system"; text = txt } in
          ignore (Model.apply_patch model patch);
          true
        | Some width ->
          (* Split the *current* draft into lines, remove the /wrap command
             itself, then re-flow every paragraph (separated by blank lines).
             Definition of paragraph: consecutive non-empty lines. *)
          let all_lines = String.split_lines !input_line in
          let body_lines, has_command_line =
            match List.last all_lines with
            | Some last when String.is_prefix (String.strip last) ~prefix:"/wrap " ->
              List.drop_last_exn all_lines, true
            | _ -> all_lines, false
          in
          if not has_command_line
          then (
            (* Edge-case: user typed only `/wrap N` and nothing else. *)
            let txt = "Nothing to wrap – draft is empty." in
            let patch = Add_placeholder_message { role = "system"; text = txt } in
            ignore (Model.apply_patch model patch);
            true)
          else (
            let rec paragraphs acc current = function
              | [] -> List.rev (List.rev current :: acc)
              | l :: ls when String.(strip l = "") ->
                let acc = List.rev current :: acc in
                paragraphs acc [] ls
              | l :: ls -> paragraphs acc (l :: current) ls
            in
            let paras =
              if List.is_empty body_lines
              then []
              else (
                let paras = paragraphs [] [] body_lines in
                (* Remove possible empty leading paragraph due to algorithm *)
                List.filter paras ~f:(fun p -> not (List.is_empty p)))
            in
            let rewrapped_paras =
              List.map paras ~f:(fun lines ->
                let joined = String.concat ~sep:" " (List.map lines ~f:String.strip) in
                Util.wrap_line ~limit:width joined)
            in
            let new_body_lines = List.concat rewrapped_paras in
            let new_text = String.concat ~sep:"\n" new_body_lines in
            (* Update draft (input_line) & cursor position. *)
            input_line := new_text;
            cursor_pos := String.length new_text;
            (* Clear selection & auto-follow irrelevant here. *)
            Model.clear_selection model;
            true)
          (* -------------------------------------------------------------- *)
          (*  /count – show character + line count of draft                  *)
          (* -------------------------------------------------------------- *))
      else if String.equal user_msg "/count"
      then (
        let draft = String.strip !input_line in
        let char_count = String.length draft in
        let line_count =
          match String.split_lines draft with
          | [] -> 0
          | ls -> List.length ls
        in
        let txt =
          Printf.sprintf "Draft statistics: %d chars, %d lines." char_count line_count
        in
        let patch = Add_placeholder_message { role = "system"; text = txt } in
        ignore (Model.apply_patch model patch);
        true
        (* -------------------------------------------------------------- *)
        (*  /format [lang] – run external formatter on draft               *)
        (*     Currently supports only "ocaml" via ocamlformat.           *)
        (* -------------------------------------------------------------- *))
      else if String.is_prefix user_msg ~prefix:"/format"
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
          let all_lines = String.split_lines !input_line in
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
            let proc_mgr = Eio.Stdenv.process_mgr env in
            let formatted_or_error =
              (* Run ocamlformat in a separate switch *)
              try
                Eio.Switch.run
                @@ fun sw ->
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
                (* Parent writes code to child's stdin *)
                Eio.Flow.copy_string code stdin_w;
                Eio.Flow.close stdin_w;
                (* Close write ends so child can finish *)
                Eio.Flow.close stdout_w;
                Eio.Flow.close stderr_w;
                (* Read stdout and stderr *)
                let read_all r =
                  try
                    Eio.Buf_read.parse_exn ~max_size:5_000_000 Eio.Buf_read.take_all r
                  with
                  | ex -> Fmt.str "(error reading formatter output: %a)" Eio.Exn.pp ex
                in
                let stdout_str = read_all stdout_r in
                let stderr_str = read_all stderr_r in
                if String.is_empty stderr_str then Ok stdout_str else Error stderr_str
              with
              | ex -> Error (Fmt.str "Failed to run ocamlformat: %a" Eio.Exn.pp ex)
            in
            match formatted_or_error with
            | Ok formatted ->
              input_line := formatted;
              cursor_pos := String.length formatted;
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
          true)
      (* -------------------------------------------------------------- *)
      (*  /expand NAME – insert predefined snippet                      *)
      (* -------------------------------------------------------------- *)
      else if String.is_prefix user_msg ~prefix:"/expand "
      then (
        let name = String.strip (String.drop_prefix user_msg 8) |> String.lowercase in
        match Snippet.find name with
        | None ->
          let available = String.concat ~sep:", " (Snippet.available ()) in
          let txt =
            Printf.sprintf
              "Unknown snippet '%s'.  Available snippets: %s"
              name
              available
          in
          let patch = Add_placeholder_message { role = "system"; text = txt } in
          ignore (Model.apply_patch model patch);
          true
        | Some snippet_text ->
          (* Replace the '/expand NAME' command line with the snippet. *)
          let all_lines = String.split_lines !input_line in
          let rec drop_last_if_cmd acc = function
            | [] -> List.rev acc, false
            | [ last ] ->
              let is_cmd = String.is_prefix (String.strip last) ~prefix:"/expand " in
              if is_cmd then List.rev acc, true else List.rev (last :: acc), false
            | hd :: tl -> drop_last_if_cmd (hd :: acc) tl
          in
          let before_lines, has_cmd = drop_last_if_cmd [] all_lines in
          if not has_cmd
          then (
            (* Command not on its own line – don't expand, treat as normal submit. *)
            false)
          else (
            let new_lines = before_lines @ [ snippet_text ] in
            let new_text = String.concat ~sep:"\n" new_lines in
            input_line := new_text;
            cursor_pos := String.length new_text;
            Model.clear_selection model;
            true)
      )
      else false
    in
    if command_handled
    then (
      (* Command processed – keep draft as-is and simply redraw UI.        *)
      redraw ();
      main_loop ())
    else (
      (* ---------------- Submit to assistant ------------------------ *)
      (* Reset draft *)
      input_line := "";
      cursor_pos := 0;
      (try Io.delete_doc ~dir:datadir draft_filename with _ -> ());
      last_saved_draft := Some "";
      if not (String.is_empty user_msg)
      then (
        (* Add to visible conversation and canonical history *)
        let patch = Add_user_message { text = user_msg } in
        ignore (Model.apply_patch model patch);
        (* Append to draft history for later reuse *)
        let dh = Model.draft_history model in
        dh := !dh @ [ user_msg ];
        let dh_pos = Model.draft_history_pos model in
        dh_pos := List.length !dh);
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
      main_loop ())
  (* ESC – cancel running request if any, otherwise quit. *)
  and handle_cancel_or_quit () =
    match !fetch_sw with
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
    | Controller.Submit_input -> handle_submit ()
    | Controller.Cancel_or_quit -> handle_cancel_or_quit ()
    | Controller.Quit -> ()
    | Controller.Unhandled -> main_loop ()
  in
  main_loop ();
  (* On shutdown, decide whether to persist the session.  We ask for
     confirmation when the user pressed ESC to quit, otherwise keep the
     previous auto-export behaviour. *)

  (* Helper – run the actual export. *)
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
            ~history_items:!history_items)
    in
    Cmd.run cmd
  in

  (* [quit_via_esc] is set in [handle_cancel_or_quit] when the user hits
     ESC while no request is in flight.  In that case we interactively ask
     whether the conversation should be exported to the prompt-markdown
     file.  Any answer other than an explicit “y”/“yes” skips the export. *)
  (match !quit_via_esc with
   | false ->
     (* Quit triggered via other means (Ctrl-C / q) – preserve previous
        behaviour and export automatically. *)
     export_session ()
   | true ->
     (* Ask the user.  The terminal has been restored to normal mode once
        we reach this point so standard I/O works as usual. *)
     Out_channel.(output_string stdout "Export conversation to promptmd file? [y/N] ");
     Out_channel.flush stdout;
     (match In_channel.input_line In_channel.stdin with
      | Some ans ->
        let ans = String.lowercase (String.strip ans) in
        if List.mem ["y"; "yes"] ans ~equal:String.equal then export_session ()
      | None -> ()))
;;

(* [run_chat] *)
