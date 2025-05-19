(* A very small Notty / Eio based TUI that lets the user have an interactive
   conversation with the OpenAI LLM.

   The implementation is deliberately minimal.  It keeps the whole
   conversation inside the chat-markdown file [conversation.md].  When the
   user hits <ENTER>, the current input line is appended as a new
   <msg role="user"> … </msg> block and we invoke
   [Chat_response.Driver.run_completion_stream] which performs the
   streaming request and appends the assistant’s response back into the
   same file.

   Once the function returns, we reload the file and refresh the screen.
   While the request is in-flight we display a small “(thinking…)” marker.

   The goal of this module is to demonstrate how to combine:
     • Notty + Eio                           – interactive terminal UI
     • Chat_response.Driver.run_completion_stream – OpenAI streaming

   The UI code is heavily inspired by the Gemini demo included in the
   repository, but stripped down to just the essential mechanics needed
   for chatting.                                                    *)

open Core
open Eio.Std
module CM = Prompt_template.Chat_markdown
module Scroll_box = Notty_scroll_box
module Res_stream = Openai.Responses.Response_stream
module Item = Openai.Responses.Response_stream.Item
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module Cache = Chat_response.Cache

exception Cancelled
(* -------------------------------------------------------------------------- *)
(*  Styling helpers                                                           *)
(* -------------------------------------------------------------------------- *)

let attr_of_role = function
  | "assistant" -> Notty.A.(fg lightcyan)
  | "user" -> Notty.A.(fg yellow)
  | "developer" -> Notty.A.(fg red)
  | "tool" -> Notty.A.(fg lightmagenta)
  | "reasoning" -> Notty.A.(fg lightblue)
  | _ -> Notty.A.empty
;;

(* -------------------------------------------------------------------------- *)
(*  Types shared across the module                                           *)
(* -------------------------------------------------------------------------- *)

(* Buffer used while streaming: we accumulate partial content in [text] and
   remember the [role] (for colouring) as well as the position in the
   rendered [messages] list. *)

type msg_buffer =
  { text : string ref
  ; index : int
  }

(* -------------------------------------------------------------------------- *)
(*  Helpers                                                                   *)

(* Extract a list of (role * text) from a chat-markdown document.  Only the
   {<msg>} tags are considered.  Content that is either plain text or a list
   of basic items is converted to a printable string; everything else is
   replaced with a short placeholder.                                           *)

(*──────────────────────────────────────────────────────────────────────────┐
  Helper: convert Chat_markdown content items to printable strings.         │
  We purposefully keep this very light-weight:                              │
  •  <img> becomes an inline “<img src=…/>” tag (no attempt to display)     │
  •  <doc> becomes “<doc src=…/>”.                                          │
  •  <agent> is shown as its original tag with the expanded inner items.    │
  For everything else we fall back to raw text, after sanitising control    │
  characters so the terminal is not upset.                                  │
 ──────────────────────────────────────────────────────────────────────────*)

let is_ctrl_non_nl c =
  let code = Char.to_int c in
  (code < 32 || code = 127) && not (Char.equal c '\n')
;;

let sanitize s =
  String.map s ~f:(fun c -> if is_ctrl_non_nl c then ' ' else c) |> String.strip
;;

let rec string_of_item (ci : CM.content_item) : string =
  let open CM in
  match ci with
  | Basic { text; image_url; document_url; is_local; _ } ->
    (match image_url, document_url with
     | Some { url }, _ ->
       if is_local
       then Printf.sprintf "<img src=\"%s\" local=\"true\"/>" url
       else Printf.sprintf "<img src=\"%s\"/>" url
     | _, Some src ->
       if is_local
       then Printf.sprintf "<doc src=\"%s\" local=\"true\"/>" src
       else Printf.sprintf "<doc src=\"%s\"/>" src
     | _ -> Option.value ~default:"" text)
  | Agent { url; is_local; items } ->
    let inner = List.map items ~f:string_of_item |> String.concat ~sep:" " in
    if is_local
    then Printf.sprintf "<agent src=\"%s\" local=\"true\">%s</agent>" url inner
    else Printf.sprintf "<agent src=\"%s\">%s</agent>" url inner
;;

let string_of_msg ?(render = false) ~ctx (m : CM.msg) : string =
  match m.content with
  | None -> ""
  | Some (CM.Text t) -> sanitize t
  | Some (CM.Items items) ->
    (match render with
     | false ->
       let items = List.map items ~f:(fun i -> string_of_item i) in
       String.concat ~sep:"" items |> sanitize
     | true ->
       Converter.string_of_items ~ctx ~run_agent:Chat_response.Driver.run_agent items
       |> sanitize)
;;

let messages_of_xml ~env ~dir ~cache xml : (string * string) list =
  let ctx = Ctx.create ~env ~dir ~cache in
  let elements = CM.parse_chat_inputs ~dir xml in
  List.filter_map elements ~f:(function
    | CM.Msg m ->
      let body =
        (* Special-case tool calls to make them more readable. *)
        if String.equal (String.lowercase m.role) "tool"
        then (
          match m.tool_call with
          | Some { function_ = { name; _ }; _ } ->
            Printf.sprintf "%s(%s)" name (string_of_msg ~render:true ~ctx m)
          | None -> string_of_msg ~ctx m)
        else string_of_msg ~ctx m
      in
      Some (String.lowercase m.role, body)
    | CM.Reasoning r ->
      let txt =
        List.map r.summary ~f:(fun s -> s.text) |> String.concat ~sep:" " |> sanitize
      in
      Some ("reasoning", txt)
    | _ -> None)
;;

(* Render the whole conversation plus the current input buffer as a Notty
   image, fitted to the current terminal size.                                 *)

let render ~term ~input_line msgs =
  let open Notty in
  let open Notty.Infix in
  let w, h = Notty_eio.Term.size term in
  (* Build the chat history. *)
  let history_img =
    let msg_to_image (role, text) =
      let role_attr = attr_of_role role in
      let lines = String.split_lines text in
      match lines with
      | [] -> Notty.I.empty
      | first :: rest ->
        let first_img = Notty.I.string role_attr (role ^ ": " ^ first) in
        let rest_img =
          match rest with
          | [] -> Notty.I.empty
          | _ ->
            rest
            |> List.map ~f:(fun l ->
              Notty.I.string role_attr (String.make (String.length role + 2) ' ' ^ l))
            |> Notty.I.vcat
        in
        (match Notty.I.height rest_img with
         | 0 -> first_img
         | _ -> first_img <-> rest_img)
    in
    msgs
    |> List.map ~f:msg_to_image
    |> Notty.I.vcat
    |> I.vsnap ~align:`Bottom (h - 2)
    |> I.hsnap ~align:`Left w
  in
  (* Input prompt line. *)
  let input_img =
    let prefix = "> " in
    let txt = prefix ^ input_line in
    I.string Notty.A.empty txt |> I.hsnap ~align:`Left w
  in
  history_img <-> input_img
;;

(* Write a user message to the conversation buffer – replacing the trailing
   empty <msg role="user"> … </msg> added by the previous turn.             *)

let write_user_message ~dir ~file message =
  let xml = Io.load_doc ~dir file in
  (* Trim any trailing whitespace to make the regex simpler. *)
  let xml = String.rstrip xml in
  let user_open = "<msg role=\"user\">" in
  let user_close = "</msg>" in
  let new_msg = Printf.sprintf "%s\n%s\n%s\n" user_open message user_close in
  (* Strategy: if the document ends with an empty user msg, replace it;
     otherwise just append the new block.                                      *)
  let updated_xml =
    if String.is_suffix xml ~suffix:(user_open ^ "\n\n" ^ user_close)
    then (
      let base =
        String.drop_suffix xml (String.length user_open + String.length user_close + 2)
      in
      base ^ new_msg)
    else xml ^ "\n" ^ new_msg
  in
  Io.save_doc ~dir file updated_xml
;;

(* Ensure the conversation buffer exists and contains an empty user prompt
   ready for the first turn.                                                   *)

let ensure_conversation_file ~dir file =
  (* If the file doesn’t exist yet we create it with an empty user message so
     that the driver can append assistant replies straight away. *)
  try ignore (Io.load_doc ~dir file : string) with
  | _ -> Io.save_doc ~dir file "<msg role=\"user\">\n\n</msg>\n"
;;

let run_chat ~env ~conversation_file () =
  let dir = Eio.Stdenv.fs env in
  ensure_conversation_file ~dir conversation_file;
  (* We wrap the whole UI inside its own switch so that we can spawn helper
     fibres (for the OpenAI request as well as file-watching) that live for
     the duration of the UI. *)
  Switch.run
  @@ fun ui_sw ->
  (* A stream used to forward Notty input events, resize notifications and
     custom redraw requests to the main event loop. *)
  let ev_stream
    : ([ `Resize
       | `Redraw
       | Notty.Unescape.event
       | `Stream of Openai.Responses.Response_stream.t
       ]
       as
       'ev)
        Eio.Stream.t
    =
    Eio.Stream.create 0
  in
  (* Prepare shared cache loaded like Driver does *)
  let cwd = Eio.Stdenv.cwd env in
  let datadir = Io.ensure_chatmd_dir ~cwd in
  let cache_file = Eio.Path.(datadir / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:1000 () in
  (* A helper to reload the conversation and rebuild the message list. *)
  let load_messages () =
    let xml = Io.load_doc ~dir conversation_file in
    messages_of_xml ~env ~dir ~cache xml
  in
  (* Mutable state. *)
  let input_line = ref "" in
  let messages = ref (load_messages ()) in
  (* When true, viewport auto-scrolls to bottom on redraw; disabled as soon as user
     manually scrolls. *)
  let auto_follow = ref true in
  (* -------------------------------------------------------------------------- *)
  (*  Streaming state                                                          *)
  (* -------------------------------------------------------------------------- *)

  (* For every streaming item (identified by [item_id]) we keep a buffer that
     accumulates the text we have received so far together with meta data that
     allows us to update the UI incrementally. *)

  (* item_id -> msg_buffer *)
  let msg_buffers : (string, msg_buffer) Hashtbl.t = Hashtbl.create (module String) in
  (* item_id -> function name (populated via [Output_item_added (Function_call …)]) *)
  let function_name_by_id : (string, string) Hashtbl.t = Hashtbl.create (module String) in
  (* item_id -> last seen summary_index (for reasoning deltas) *)
  let reasoning_idx_by_id : (string, int ref) Hashtbl.t =
    Hashtbl.create (module String)
  in
  (* Helper functions for the streaming UI state.                            *)
  let rec update_message_text index new_txt =
    messages
    := List.mapi !messages ~f:(fun idx (role, txt) ->
         if idx = index then role, new_txt else role, txt)
  and ensure_buffer ~id ~role : msg_buffer =
    match Hashtbl.find msg_buffers id with
    | Some b -> b
    | None ->
      let index = List.length !messages in
      let b = { text = ref ""; index } in
      Hashtbl.set msg_buffers ~key:id ~data:b;
      messages := !messages @ [ role, "" ];
      b
  and append_text ~id ~role ~delta =
    let buf = ensure_buffer ~id ~role in
    buf.text := !(buf.text) ^ delta;
    update_message_text buf.index !(buf.text)
  in
  (* Switch for the currently running OpenAI streaming request (if any). *)
  let fetch_sw : Switch.t option ref = ref None in
  (* Scroll box to display the chat history. *)
  let scroll_box = Scroll_box.create Notty.I.empty in
  let first_draw = ref true in
  Notty_eio.Term.run ~input:env#stdin ~output:env#stdout ~mouse:false ~on_event:(fun ev ->
    Eio.Stream.add
      ev_stream
      (ev :> [ `Resize | `Redraw | Notty.Unescape.event | `Stream of Res_stream.t ]))
  @@ fun term ->
  let redraw () =
    let w, h = Notty_eio.Term.size term in
    (* Determine input editor height (#lines). *)
    let input_lines =
      match String.split ~on:'\n' !input_line with
      | [] -> [ "" ]
      | ls -> ls
    in
    let input_height = List.length input_lines in
    let history_height = Int.max 1 (h - input_height) in
    (* Rebuild the history image and update the scroll box. *)
    let history_image =
      (* Build the chat history image with wrapping and prefix alignment. *)
      let msg_to_image (role, text) =
        let attr = attr_of_role role in
        let prefix = role ^ ": " in
        let indent = String.make (String.length prefix) ' ' in
        let max_width = w in
        (*---------------------------------------------------------------------------
          Word-wrap a list of words so that no produced line (except possibly lines
          containing a single over-long word) exceeds [limit] characters.

          The previous implementation could loop forever when the first word of a
          fresh line was already longer than [limit]: it would repeatedly start a
          "new line" with the same word without consuming it.
        ---------------------------------------------------------------------------*)
        let wrap_words words limit =
          let flush acc current_line =
            match List.rev current_line with
            | [] -> acc
            | xs -> String.concat ~sep:" " xs :: acc
          in
          let rec loop acc current_line current_len = function
            | [] -> List.rev (flush acc current_line)
            | w :: ws ->
              let wlen = String.length w in
              if current_len = 0
              then
                (* Start of a new line: accept [w] even if it exceeds [limit]. *)
                loop acc [ w ] wlen ws
              else if current_len + 1 + wlen <= limit
              then
                (* Word fits on the current line. *)
                loop acc (w :: current_line) (current_len + 1 + wlen) ws
              else
                (* Word doesn’t fit – flush current line and start a new one. *)
                loop (flush acc current_line) [] 0 (w :: ws)
          in
          loop [] [] 0 words
        in
        let paragraphs = String.split_lines text in
        let first_limit = Int.max 1 (max_width - String.length prefix) in
        let sub_limit = Int.max 1 (max_width - String.length indent) in
        let images =
          let open Notty.I in
          let rec process_paras idx acc = function
            | [] -> List.rev acc
            | para :: rest ->
              let words =
                String.split ~on:' ' para
                |> List.filter ~f:(fun s -> not (String.is_empty s))
              in
              let limit = if idx = 0 then first_limit else sub_limit in
              let lines = wrap_words words limit in
              let images_for_para =
                List.mapi lines ~f:(fun line_idx line ->
                  if idx = 0 && line_idx = 0
                  then string attr (prefix ^ line)
                  else string attr (indent ^ line))
              in
              (* Add blank line between paragraphs if not first and paragraph not first *)
              let acc =
                if idx > 0
                then string attr indent :: acc (* blank line respecting indent *)
                else acc
              in
              process_paras (idx + 1) (List.rev_append images_for_para acc) rest
          in
          vcat (process_paras 0 [] paragraphs)
        in
        images
      in
      !messages |> List.map ~f:msg_to_image |> Notty.I.vcat
    in
    Scroll_box.set_content scroll_box history_image;
    if !auto_follow then Scroll_box.scroll_to_bottom scroll_box ~height:history_height;
    (* Render history through scroll box. *)
    let history_view = Scroll_box.render scroll_box ~width:w ~height:history_height in
    (* Multi-line input prompt. *)
    let input_img =
      let open Notty.I in
      let prefix = "> " in
      let indent = String.make (String.length prefix) ' ' in
      let imgs =
        List.mapi input_lines ~f:(fun idx line ->
          let txt = if idx = 0 then prefix ^ line else indent ^ line in
          string Notty.A.empty txt |> hsnap ~align:`Left w)
      in
      vcat imgs
    in
    let full_img = Notty.Infix.(history_view <-> input_img) in
    Notty_eio.Term.image term full_img;
    (* Position cursor at logical end of the current input. We derive the
       coordinates from the length of [input_line] – no separate mutable
       [cursor_pos] is required. *)
    let total_index = String.length !input_line in
    let rec row_col lines offset row =
      match lines with
      | [] -> row, 0
      | l :: ls ->
        let len = String.length l in
        if total_index <= offset + len
        then row, total_index - offset
        else row_col ls (offset + len + 1) (row + 1)
    in
    let row, col_in_line = row_col input_lines 0 0 in
    let prefix_len =
      2
      (* length of " > " prefix *)
    in
    let cursor_x = prefix_len + col_in_line in
    let cursor_y = history_height + row in
    Notty_eio.Term.cursor term (Some (cursor_x, cursor_y));
    first_draw := false
  in
  redraw ();
  (* ---------------------------------------------------------------------- *)
  (*  Streaming event handling                                              *)
  (* ---------------------------------------------------------------------- *)
  let handle_stream_event (ev : Res_stream.t) =
    match ev with
    (* Assistant text *)
    | Res_stream.Output_text_delta { item_id; delta; _ } ->
      append_text ~id:item_id ~role:"assistant" ~delta
    (* Assistant triggers a function / tool call.  We remember the mapping
       item_id → function-name so that subsequent argument deltas can start
       with the proper prefix. *)
    | Res_stream.Output_item_added { item; _ } ->
      (match item with
       | Item.Function_call fc ->
         let idx = Option.value fc.id ~default:fc.call_id in
         Hashtbl.set function_name_by_id ~key:idx ~data:fc.name
       | Item.Reasoning r -> ignore (ensure_buffer ~id:r.id ~role:"reasoning")
       | _ -> ())
    (* Reasoning summaries *)
    | Res_stream.Reasoning_summary_text_delta { item_id; delta; summary_index; _ } ->
      let buf = ensure_buffer ~id:item_id ~role:"reasoning" in
      (match Hashtbl.find reasoning_idx_by_id item_id with
       | Some idx_ref when !idx_ref = summary_index -> ()
       | Some idx_ref ->
         idx_ref := summary_index;
         if not (String.is_empty !(buf.text)) then buf.text := !(buf.text) ^ "\n"
       | None -> Hashtbl.set reasoning_idx_by_id ~key:item_id ~data:(ref summary_index));
      buf.text := !(buf.text) ^ delta;
      update_message_text buf.index !(buf.text)
    (* Function call argument streaming *)
    | Res_stream.Function_call_arguments_delta { item_id; delta; _ } ->
      let buf = ensure_buffer ~id:item_id ~role:"tool" in
      if String.is_empty !(buf.text)
      then (
        let fn_name =
          Option.value (Hashtbl.find function_name_by_id item_id) ~default:"tool"
        in
        buf.text := fn_name ^ "(");
      buf.text := !(buf.text) ^ delta;
      update_message_text buf.index !(buf.text)
    | Res_stream.Function_call_arguments_done { item_id; arguments; _ } ->
      let buf = ensure_buffer ~id:item_id ~role:"tool" in
      if String.is_empty !(buf.text)
      then (
        let fn_name =
          Option.value (Hashtbl.find function_name_by_id item_id) ~default:"tool"
        in
        buf.text := fn_name ^ "(");
      buf.text := !(buf.text) ^ arguments ^ ")";
      update_message_text buf.index !(buf.text);
      messages := load_messages ()
    | _ -> ()
  in
  let rec main_loop () =
    let ev = Eio.Stream.take ev_stream in
    match ev with
    | #Notty.Unescape.event as ev -> handle_key ev
    | `Resize ->
      redraw ();
      main_loop ()
    | `Redraw ->
      redraw ();
      (* simple redraw request *)
      main_loop ()
    | `Stream ev ->
      handle_stream_event ev;
      (* update the scroll box content with the new assistant message *)
      (* update last message in list *)
      (* after updating, always keep view at bottom and redraw *)
      (* redraw already scrolls to bottom, so just call redraw *)
      redraw ();
      main_loop ()
  and handle_key : Notty.Unescape.event -> unit = function
    | `Key (`ASCII c, _) ->
      input_line := !input_line ^ String.of_char c;
      redraw ();
      main_loop ()
    | `Key (`Backspace, _) ->
      if String.length !input_line > 0
      then (
        (* drop last scalar value *)
        let len = String.length !input_line in
        input_line := String.sub !input_line ~pos:0 ~len:(len - 1));
      redraw ();
      main_loop ()
    | `Key (`Arrow `Up, _) ->
      auto_follow := false;
      let _, h = Notty_eio.Term.size term in
      (* Approximate current input height as number of lines in input_line *)
      let input_h =
        match String.split_lines !input_line with
        | [] -> 1
        | ls -> List.length ls
      in
      Scroll_box.scroll_by scroll_box ~height:(h - input_h) (-1);
      redraw ();
      main_loop ()
    | `Key (`Arrow `Down, _) ->
      auto_follow := false;
      let _, h = Notty_eio.Term.size term in
      let input_h =
        match String.split_lines !input_line with
        | [] -> 1
        | ls -> List.length ls
      in
      Scroll_box.scroll_by scroll_box ~height:(h - input_h) 1;
      redraw ();
      main_loop ()
    | `Key (`Page `Up, _) ->
      auto_follow := false;
      let _, h = Notty_eio.Term.size term in
      let input_h =
        match String.split_lines !input_line with
        | [] -> 1
        | ls -> List.length ls
      in
      Scroll_box.scroll_by scroll_box ~height:(h - input_h) (-(h - input_h));
      redraw ();
      main_loop ()
    | `Key (`Page `Down, _) ->
      auto_follow := false;
      let _, h = Notty_eio.Term.size term in
      let input_h =
        match String.split_lines !input_line with
        | [] -> 1
        | ls -> List.length ls
      in
      Scroll_box.scroll_by scroll_box ~height:(h - input_h) (h - input_h);
      redraw ();
      main_loop ()
    | `Key (`Home, _) ->
      auto_follow := false;
      Scroll_box.scroll_to_top scroll_box;
      redraw ();
      main_loop ()
    | `Key (`End, _) ->
      auto_follow := true;
      let _, h = Notty_eio.Term.size term in
      let input_h = 1 in
      Scroll_box.scroll_to_bottom scroll_box ~height:(h - input_h);
      redraw ();
      main_loop ()
    | `Key (`Enter, []) ->
      (* Insert literal newline *)
      input_line := !input_line ^ "\n";
      redraw ();
      main_loop ()
    | `Key (`Enter, [ `Meta ]) ->
      let user_msg = String.strip !input_line in
      input_line := "";
      if String.is_empty user_msg
      then (
        redraw ();
        main_loop ())
      else (
        (* 1. Write user message to the conversation buffer. *)
        write_user_message ~dir ~file:conversation_file user_msg;
        messages := load_messages ();
        auto_follow := true;
        let _, h = Notty_eio.Term.size term in
        let input_h =
          match String.split_lines !input_line with
          | [] -> 1
          | ls -> List.length ls
        in
        Scroll_box.scroll_to_bottom scroll_box ~height:(h - input_h);
        redraw ();
        (* 2. Display a temporary thinking marker. *)
        Notty_eio.Term.image
          term
          (render ~term ~input_line:"" (!messages @ [ "assistant", "(thinking…)" ]));
        (* 3. Fire off the streaming request in a separate fibre. *)
        Fiber.fork ~sw:ui_sw (fun () ->
          try
            Switch.run
            @@ fun streaming_sw ->
            (* Save switch so that we can cancel via ESC. *)
            fetch_sw := Some streaming_sw;
            let on_event ev = Eio.Stream.add ev_stream (`Stream ev) in
            (* Run the actual OpenAI streaming call (blocking in this fibre). *)
            Chat_response.Driver.run_completion_stream
              ~env
              ~output_file:conversation_file
              ~on_event
              ();
            (* After completion, force a final reload and redraw. *)
            messages := load_messages ();
            Eio.Stream.add ev_stream `Redraw;
            fetch_sw := None
          with
          | ex ->
            fetch_sw := None;
            prerr_endline (Printf.sprintf "Error during streaming: %s" (Exn.to_string ex));
            Eio.Stream.add ev_stream `Redraw);
        main_loop ())
    | `Key (`Escape, _) ->
      (match !fetch_sw with
       | Some sw ->
         (* Cancel current request and continue running *)
         Switch.fail sw Cancelled;
         main_loop ()
       | None -> () (* exit the application *))
    | _ -> main_loop ()
  in
  main_loop ()
;;

let () =
  let open Command.Let_syntax in
  let command =
    Command.basic
      ~summary:"Interactive ChatGPT TUI"
      [%map_open
        let conversation_file =
          flag
            "-file"
            (optional_with_default "./prompts/interactive.md" string)
            ~doc:"FILE Conversation buffer path (default: ./prompts/interactive.md)"
        in
        fun () -> Io.run_main (fun env -> run_chat ~env ~conversation_file ())]
  in
  Command_unix.run command
;;
