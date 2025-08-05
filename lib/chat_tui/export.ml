open Core

(** Archive the current conversation to a ChatMarkdown file.

    The helper mirrors the original logic embedded in
    [Chat_tui.App.run_chat] prior to Task 13.  No behaviour has been
    changed – the code was merely moved into its own compilation unit so
    that it can be reused from different entry-points (CLI, TUI, tests).

    Parameters:
    {ul
    {- [env] – {!Eio_unix.Stdenv.base} used for file IO.}
    {- [model] – in-memory UI state; only [history_items] are read.}
    {- [prompt_file] – original *.chatmd* file that seeded the session.  It
       is copied verbatim to the destination directory so that the export
       remains self-contained.}
    {- [target_path] – path of the output *.chatmd* file (may be relative
       or absolute).  Intermediate directories are created as needed.}
    {- [cfg] – runtime configuration parsed from the prompt; required by
       {!Chat_tui.Persistence.persist_session}.}
    {- [initial_msg_count] – number of static prompt messages so the
       persistence helper can avoid duplicating them.}
    {- [session] – optional active session (for attachment discovery).}}

    The function performs the following high-level steps:

    {ol
    {- Ensure the parent directory of [target_path] exists.}
    {- Copy [prompt_file] into the destination directory.}
    {- Copy all attachment directories ([.chatmd]) that may reside next
       to the prompt, inside the current working directory, or the session
       directory.}
    {- Persist the runtime conversation beyond the static prompt using
       {!Chat_tui.Persistence.persist_session}.}}

    The implementation is mostly lifted as-is from the original [do_export]
    closure in [app.ml] with minor refactorings for parameterisation. *)

let archive
      ~env
      ~(model : Model.t)
      ~(prompt_file : string)
      ~(target_path : string)
      ~(cfg : Chat_response.Config.t)
      ~(initial_msg_count : int)
      ~(session : Session.t option)
  : unit
  =
  (* -------------------------------- Path preparation -------------------------------- *)
  let dir_str = Filename.dirname target_path in
  let file_name = Filename.basename target_path in
  let fs = Eio.Stdenv.fs env in
  let out_dir = Eio.Path.(fs / dir_str) in
  (match Eio.Path.is_directory out_dir with
   | true -> ()
   | false -> Eio.Path.mkdirs ~perm:0o700 out_dir);
  let ( / ) = Eio.Path.( / ) in
  let dest_path = out_dir / file_name in
  (* -------------------------- Prompt overwrite confirmation -------------------------- *)
  let proceed =
    if Eio.Path.is_file dest_path
    then (
      Out_channel.output_string
        stdout
        (Printf.sprintf "File %s exists. Overwrite? [y/N] " target_path);
      Out_channel.flush stdout;
      match In_channel.input_line In_channel.stdin with
      | Some ans
        when List.mem
               [ "y"; "yes" ]
               (String.lowercase (String.strip ans))
               ~equal:String.equal -> true
      | _ ->
        Core.printf "Aborted.\n";
        false)
    else true
  in
  if not proceed
  then ()
  else (
    (* Destination directory that will hold runtime artefacts such as images. *)
    let cwd_export = out_dir in
    let datadir_export = Io.ensure_chatmd_dir ~cwd:cwd_export in
    (* ---------------------- 0. Copy original prompt content ---------------------- *)
    (* Resolve the prompt path: absolute paths are loaded from [fs]; relative ones
       from the current working directory so that "./prompt.chatmd" works regardless
       of where the binary was launched. *)
    let prompt_content =
      let dir_for_prompt =
        if Filename.is_absolute prompt_file then fs else Eio.Stdenv.cwd env
      in
      Option.value
        (Option.try_with (fun () -> Io.load_doc ~dir:dir_for_prompt prompt_file))
        ~default:""
    in
    Io.save_doc ~dir:cwd_export file_name prompt_content;
    (* 0.b Copy existing attachment directories so that <doc src="./.chatmd" …> references
       remain valid in the exported file. *)
    let prompt_parent_dir =
      let base_dir =
        if Filename.is_absolute prompt_file then fs else Eio.Stdenv.cwd env
      in
      Eio.Path.(base_dir / Filename.dirname prompt_file)
    in
    let session_dir =
      match session with
      | Some s -> Session_store.path ~env s.id
      | None -> Eio.Stdenv.cwd env
    in
    Attachments.copy_all
      ~prompt_dir:prompt_parent_dir
      ~cwd:(Eio.Stdenv.cwd env)
      ~session_dir
      ~dst:datadir_export;
    (* ---------------------- 1. Persist ChatMarkdown conversation ------------------ *)
    Persistence.persist_session
      ~dir:cwd_export
      ~prompt_file:file_name
      ~datadir:datadir_export
      ~cfg
      ~initial_msg_count
      ~history_items:(Model.history_items model))
;;
