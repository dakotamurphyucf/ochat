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

exception Cancelled

(* -------------------------------------------------------------------------- *)
(*  Helpers                                                                   *)

(* Extract a list of (role * text) from a chat-markdown document.  Only the
   {<msg>} tags are considered.  Content that is either plain text or a list
   of basic items is converted to a printable string; everything else is
   replaced with a short placeholder.                                           *)

let messages_of_xml ~dir xml : (string * string) list =
  let is_ctrl_non_nl c =
    let code = Char.to_int c in
    (code < 32 || code = 127) && not (Char.equal c '\n')
  in
  let sanitize s =
    String.map s ~f:(fun c -> if is_ctrl_non_nl c then ' ' else c) |> String.strip
  in
  let elements = CM.parse_chat_inputs ~dir xml in
  List.filter_map elements ~f:(function
    | CM.Msg m ->
      let raw =
        match m.CM.content with
        | None -> ""
        | Some (CM.Text t) -> t
        | Some (CM.Items items) ->
          List.filter_map items ~f:(function
            | CM.Basic { text = Some t; _ } -> Some t
            | _ -> None)
          |> String.concat ~sep:" "
      in
      let txt = sanitize raw in
      Some (m.role, txt)
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
      let role_attr =
        match role with
        | "assistant" -> Notty.A.(fg lightcyan)
        | "user" -> Notty.A.(fg yellow)
        | _ -> Notty.A.empty
      in
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
  (* A helper to reload the conversation and rebuild the message list. *)
  let load_messages () =
    let xml = Io.load_doc ~dir conversation_file in
    messages_of_xml ~dir xml
  in
  (* Mutable state. *)
  let input_line = ref "" in
  let messages = ref (load_messages ()) in
  (* When true, viewport auto-scrolls to bottom on redraw; disabled as soon as user
     manually scrolls. *)
  let auto_follow = ref true in
  (* Buffers for currently streaming assistant messages: id -> buffer ref and index *)
  let assistant_buffers : (string, string ref) Hashtbl.t =
    Hashtbl.create (module String)
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
    (* Height available for the history (leave 1 line for prompt). *)
    let history_height = h - 1 in
    (* Rebuild the history image and update the scroll box. *)
    let history_image =
      (* Build the chat history image with wrapping and prefix alignment. *)
      let msg_to_image (role, text) =
        let attr =
          match role with
          | "assistant" -> Notty.A.(fg lightcyan)
          | "user" -> Notty.A.(fg yellow)
          | _ -> Notty.A.empty
        in
        let prefix = role ^ ": " in
        let indent = String.make (String.length prefix) ' ' in
        let max_width = w in
        (* Utility: wrap a list of words into lines with given limit *)
        let wrap_words words limit =
          let rec loop acc current_line_len current_line words =
            match words with
            | [] -> List.rev (current_line :: acc)
            | w :: ws ->
              let wlen = String.length w in
              if current_line_len = 0
              then loop acc wlen w ws
              else if current_line_len + 1 + wlen <= limit
              then (
                let new_len = current_line_len + 1 + wlen in
                loop acc new_len (current_line ^ " " ^ w) ws)
              else loop (current_line :: acc) 0 "" words (* start new line *)
          in
          match words with
          | [] -> [ "" ]
          | _ -> loop [] 0 "" words
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
    (* Input prompt line. *)
    let input_img =
      let open Notty in
      let prefix = "> " in
      let txt = prefix ^ !input_line in
      I.string A.empty txt |> I.hsnap ~align:`Left w
    in
    let full_img = Notty.Infix.(history_view <-> input_img) in
    Notty_eio.Term.image term full_img;
    first_draw := false
  in
  redraw ();
  (* Update in-memory conversation based on streaming events *)
  let append_assistant_text ~id ~delta =
    let buf_ref =
      match Hashtbl.find assistant_buffers id with
      | Some r -> r
      | None ->
        let r = ref "" in
        Hashtbl.set assistant_buffers ~key:id ~data:r;
        (* create new assistant entry with empty content *)
        messages := !messages @ [ "assistant", "" ];
        r
    in
    buf_ref := !buf_ref ^ delta;
    (* Update the scroll box content with the new assistant message *)
    (* Update last message in list *)
    messages
    := match List.rev !messages with
       | (role, _) :: rest_rev -> List.rev ((role, !buf_ref) :: rest_rev)
       | [] -> []
  in
  let handle_stream_event (ev : Res_stream.t) =
    match ev with
    | Res_stream.Output_text_delta { item_id; delta; _ } ->
      append_assistant_text ~id:item_id ~delta
    (* | Res_stream.Output_item_added { item = Item.Output_message om; _ } ->
      let txt = List.map om.content ~f:(fun c -> c.text) |> String.concat ~sep:""
      in
      append_assistant_text ~id:om.id ~delta:txt
    | Res_stream.Output_text_done { item_id; text; _ } ->
      append_assistant_text ~id:item_id ~delta:text
    | Res_stream.Content_part_added { item_id; part; _ } ->
      (match part with
       | Res_stream.Part.Output_text p -> append_assistant_text ~id:item_id ~delta:p.text
       | _ -> ())
    | Res_stream.Content_part_done { item_id; part; _ } ->
      (match part with
       | Res_stream.Part.Output_text p -> append_assistant_text ~id:item_id ~delta:p.text
       | _ -> ())
    | Res_stream.Output_item_done { item = Item.Output_message _om; _ } -> () *)
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
      if String.length !input_line > 0 then input_line := String.drop_suffix !input_line 1;
      redraw ();
      main_loop ()
    | `Key (`Arrow `Up, _) ->
      auto_follow := false;
      let _, h = Notty_eio.Term.size term in
      Scroll_box.scroll_by scroll_box ~height:(h - 1) (-1);
      redraw ();
      main_loop ()
    | `Key (`Arrow `Down, _) ->
      auto_follow := false;
      let _, h = Notty_eio.Term.size term in
      Scroll_box.scroll_by scroll_box ~height:(h - 1) 1;
      redraw ();
      main_loop ()
    | `Key (`Page `Up, _) ->
      auto_follow := false;
      let _, h = Notty_eio.Term.size term in
      Scroll_box.scroll_by scroll_box ~height:(h - 1) (-(h - 1));
      redraw ();
      main_loop ()
    | `Key (`Page `Down, _) ->
      auto_follow := false;
      let _, h = Notty_eio.Term.size term in
      Scroll_box.scroll_by scroll_box ~height:(h - 1) (h - 1);
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
      Scroll_box.scroll_to_bottom scroll_box ~height:(h - 1);
      redraw ();
      main_loop ()
    | `Key (`Enter, _) ->
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
        Scroll_box.scroll_to_bottom scroll_box ~height:(h - 1);
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
