open Core

(* -------------------------------------------------------------------------- *)
(*  Export – ensure original prompt is preserved                               *)
(* -------------------------------------------------------------------------- *)

let%expect_test "export copies original prompt" =
  (* Prepare a temporary workspace *)
  let tmp_dir =
    Filename.concat
      Filename.temp_dir_name
      ("export_test_" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp_dir;
  let prompt_path = Filename.concat tmp_dir "prompt.chatmd" in
  let prompt_content = "<system>hello</system>\n" in
  Out_channel.write_all prompt_path ~data:prompt_content;
  (* Run the minimal export routine – mimic the helper logic: copy prompt then
     persist session (with empty history so nothing is appended). *)
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_eio = Eio.Path.(fs / tmp_dir) in
  (* Ensure output directory exists *)
  (match Eio.Path.is_directory tmp_eio with
   | true -> ()
   | false -> Eio.Path.mkdirs ~perm:0o700 tmp_eio);
  let out_dir = tmp_eio in
  let file_name = "export.chatmd" in
  (* Copy prompt *)
  let original = Io.load_doc ~dir:fs prompt_path in
  Io.save_doc ~dir:out_dir file_name original;
  (* Persist nothing – history empty *)
  let datadir = Io.ensure_chatmd_dir ~cwd:out_dir in
  let module Config = Chat_response.Config in
  Chat_tui.Persistence.persist_session
    ~dir:out_dir
    ~prompt_file:file_name
    ~datadir
    ~cfg:Config.default
    ~initial_msg_count:0
    ~history_items:[];
  (* Check that export file starts with the original prompt string *)
  let exported = Io.load_doc ~dir:out_dir file_name in
  let preserved = String.is_prefix exported ~prefix:prompt_content in
  print_s [%sexp (preserved : bool)];
  [%expect {| true |}]
;;
