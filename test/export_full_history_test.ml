open Core

(* -------------------------------------------------------------------------- *)
(*  Export – ensure *all* history items are exported (Task #26)                *)
(* -------------------------------------------------------------------------- *)

let%expect_test "export includes previous history items" =
  (* Prepare a temporary workspace. *)
  let tmp_dir =
    Filename.concat
      Filename.temp_dir_name
      ("export_hist_test_" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp_dir;
  (* 1. Create a minimal prompt file with a single <system> tag. *)
  let prompt_path = Filename.concat tmp_dir "prompt.chatmd" in
  let prompt_content = "<system>Hello!</system>\n" in
  Out_channel.write_all prompt_path ~data:prompt_content;
  (* 2. Craft a fake conversation history that predates the current run. *)
  let module Res = Openai.Responses in
  let module IM = Res.Input_message in
  let module OM = Res.Output_message in
  let user_msg : IM.t =
    { role = IM.User
    ; content = [ IM.Text { text = "Hi"; _type = "input_text" } ]
    ; _type = "message"
    }
  in
  let assistant_content : OM.content =
    { annotations = []; text = "Hello back!"; _type = "text" }
  in
  let assistant_msg : OM.t =
    { role = OM.Assistant
    ; id = "assistant-1"
    ; content = [ assistant_content ]
    ; status = "complete"
    ; _type = "message"
    }
  in
  let history_items : Res.Item.t list =
    [ Res.Item.Input_message user_msg; Res.Item.Output_message assistant_msg ]
  in
  let allocator =
    History_entry.Allocator.create ~namespace:"export-full-history" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let history =
    List.map history_items ~f:(History_entry.create ~allocator)
    |> Result.all
    |> Result.ok_or_failwith
  in
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let out_dir = Eio.Path.(fs / tmp_dir) in
  (* Ensure destination exists and has a .chatmd directory. *)
  (* Copy prompt to export file. *)
  let export_file = "export.chatmd" in
  Io.save_doc ~dir:out_dir export_file prompt_content;
  Chat_tui.Persistence.persist_entries
    ~dir:out_dir
    ~prompt_file:export_file
    ~checkpoint:(Chat_tui.Persistence.Checkpoint.empty ())
    ~moderator_snapshot:None
    ~history;
  let exported = Io.load_doc ~dir:out_dir export_file in
  (* The assistant reply should appear in the exported ChatMarkdown. *)
  let contains_assistant = String.is_substring exported ~substring:"Hello back!" in
  print_s [%sexp (contains_assistant : bool)];
  [%expect {| true |}]
;;
