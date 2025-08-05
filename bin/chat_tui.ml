(** Terminal user-interface for the **Ochat** assistant.

    This module backs the public executable {{:https://github.com/zshipko/ochat} [chat-tui]},
    a curses-like client built on top of {{!module:Notty}} and {{!module:Eio}}.
    The binary is essentially a *thin* wrapper that:

    1. Parses a rich set of command-line flags (session management, export,
       persistence, …).
    2. Delegates all heavy-lifting to {!Chat_tui.App.run_chat} once the flags are
       validated and normalised.

    The implementation lives in a regular [.ml] file because the executable has no
    public interface of its own.  Nevertheless we keep a complete odoc comment so
    that users browsing the library documentation understand which knobs are
    available from the CLI.

    {1 Usage}

    {v
      chat-tui [-file FILE]
               [--list-sessions]
               [--session NAME | --new-session]
               [--session-info NAME]
               [--export-session NAME --out FILE]
               [--export-file FILE]
               [--reset-session NAME [--prompt-file FILE] [--keep-history]]
               [--rebuild-from-prompt NAME]
               [--parallel-tool-calls | --no-parallel-tool-calls]
               [--auto-persist | --no-persist]
    v}

    Flags (grouped by category):

    • *Prompt*:  ▸ [-file FILE] – ChatMarkdown / Markdown document that seeds the
      conversation buffer and declares callable tools.  Defaults to
      {!val:default_prompt_file}.

    • *Session selection* (mutually exclusive):
      – [--list-sessions] · enumerate existing session identifiers.
      – [--session NAME] · resume the given session.
      – [--new-session]   · force creation of a brand-new session even when a
                           deterministic one already exists for the prompt.

    • *Session inspection* (exclusive with the above):
      – [--session-info NAME]         · print metadata (history length, prompt
                                        path, timestamps, …).
      – [--reset-session NAME]        · archive the snapshot and start over,
                                        optionally keeping the chat history
                                        ([--keep-history]) or switching to a
                                        different prompt ([--prompt-file]).
      – [--rebuild-from-prompt NAME]  · rebuild the snapshot from the stored
                                        prompt file.

    • *Export*:
      – [--export-session NAME --out FILE] · convert a snapshot to a standalone
        *.chatmd* file and exit.
      – [--export-file FILE]               · after the interactive session
        finishes, save the full transcript to the given file.

    • *Runtime behaviour*:
      – [--parallel-tool-calls] / [--no-parallel-tool-calls] · toggle parallel
        execution of function-callable tools.
      – [--auto-persist] / [--no-persist] · control whether the snapshot is
        saved on exit without prompting.

    Invalid flag combinations are detected early and reported with a helpful
    diagnostic before the process terminates.
*)

open Core

let default_prompt_file = "./prompts/interactive.md"

(** [run ?session_id ?new_session ?export_file ?persist_mode
       ?parallel_tool_calls ~prompt_file ()] launches the Notty-based
    interactive chat loop.

    The function is a *re-export* of {!Chat_tui.App.run_chat} with a few
    extra responsibilities – namely resolving the appropriate session
    snapshot and applying user-selected run-time options.  It is useful
    for embedding the TUI inside another OCaml program.

    Parameters (mirroring the CLI flags):

    • [?session_id] – identifier of the session snapshot to resume.  If
      omitted a deterministic ID derived from [prompt_file] is used.

    • [?new_session] (default: [false]) – create a fresh session even
      when a snapshot bearing the deterministic ID already exists.

    • [?export_file] – when set, export the full conversation to the
      given file on normal termination (same format as
      [--export-file]).

    • [?persist_mode] – automatic save mode: `\`Ask` (default), `\`Always`, or
      `\`Never`.  See {!type:Chat_tui.App.persist_mode}.

    • [?parallel_tool_calls] (default: [true]) – whether to allow
      concurrent execution of function-callable tools.

    • [~prompt_file] – ChatMarkdown / Markdown document used to seed the
      conversation buffer and declare tools.

    The function blocks until the user quits the interface (e.g. `/quit`
    or *Ctrl-c* ).
*)
let run
      ?session_id
      ?(new_session = false)
      ?export_file
      ?(persist_mode : Chat_tui.App.persist_mode = `Ask)
      ?(parallel_tool_calls = true)
      ~prompt_file
      ()
  =
  Io.run_main (fun env ->
    let session =
      Session_store.load_or_create ~env ~prompt_file ?id:session_id ~new_session ()
    in
    Chat_tui.App.run_chat
      ~env
      ~prompt_file
      ~session
      ?export_file
      ~persist_mode
      ~parallel_tool_calls
      ())
;;

let () =
  let open Command.Let_syntax in
  let command =
    Command.basic
      ~summary:"Interactive Ochat TUI"
      [%map_open
        let conversation_file =
          flag
            "-file"
            (optional_with_default default_prompt_file string)
            ~doc:"FILE Conversation buffer path (default: ./prompts/interactive.md)"
        and list_sessions =
          flag
            "--list-sessions"
            no_arg
            ~doc:
              "List all existing sessions and exit (incompatible with other session \
               flags)"
        and session_id =
          flag
            "--session"
            (optional string)
            ~doc:"NAME Resume an existing session identified by NAME"
        and new_session =
          flag
            "--new-session"
            no_arg
            ~doc:"Create a new session instead of resuming an existing one"
        (* Export a session to ChatMarkdown *)
        and export_session_id =
          flag
            "--export-session"
            (optional string)
            ~doc:
              "NAME Export the specified session to ChatMarkdown and exit (incompatible \
               with other session flags)"
        and export_out_file =
          flag
            "--out"
            (optional string)
            ~doc:
              "FILE Output path for --export-session (required when using \
               --export-session)"
        (* Destination file for interactive export on exit *)
        and export_file =
          flag
            "--export-file"
            (optional string)
            ~doc:"FILE ChatMarkdown destination when exporting on exit (interactive mode)"
        (* Display session metadata and exit *)
        and session_info =
          flag
            "--session-info"
            (optional string)
            ~doc:
              "NAME Display metadata for session NAME and exit (incompatible with other \
               session flags)"
        (* Reset / archive session *)
        and reset_session_id =
          flag
            "--reset-session"
            (optional string)
            ~doc:
              "NAME Archive current snapshot and reset session NAME (incompatible with \
               other session flags)"
        and reset_prompt_file =
          flag
            "--prompt-file"
            (optional string)
            ~doc:
              "FILE New prompt file to use when resetting the session (optional with \
               --reset-session)"
        and reset_keep_history =
          flag
            "--keep-history"
            no_arg
            ~doc:
              "When used with --reset-session, retain conversation history and cache \
               instead of clearing them"
        (* Parallel tool call toggle *)
        and parallel_tool_calls =
          flag
            "--parallel-tool-calls"
            no_arg
            ~doc:"Enable parallel execution of tool calls (default: enabled)"
        and no_parallel_tool_calls =
          flag
            "--no-parallel-tool-calls"
            no_arg
            ~doc:"Disable parallel execution of tool calls (forces sequential evaluation)"
        and no_persist =
          flag
            "--no-persist"
            no_arg
            ~doc:"Do not persist session snapshot on exit (interactive mode)"
        and auto_persist =
          flag
            "--auto-persist"
            no_arg
            ~doc:"Always persist session snapshot on exit without asking"
        (* Rebuild snapshot from (edited) prompt *)
        and rebuild_session_id =
          flag
            "--rebuild-from-prompt"
            (optional string)
            ~doc:
              "NAME Rebuild session NAME from its prompt.chatmd copy and exit \
               (incompatible with other session flags)"
        in
        (* Validate mutually exclusive flags. *)
        let () =
          (* --list-sessions cannot be combined with other session-manipulating flags *)
          (match list_sessions, session_id, new_session, session_info with
           | true, Some _, _, _ | true, _, true, _ | true, _, _, Some _ ->
             Core.eprintf
               "Error: --list-sessions cannot be combined with --session, \
                --session-info, or --new-session.\n";
             exit 1
           | _ -> ());
          (* --parallel-tool-calls and --no-parallel-tool-calls are mutually exclusive *)
          (match parallel_tool_calls, no_parallel_tool_calls with
           | true, true ->
             Core.eprintf
               "Error: --parallel-tool-calls and --no-parallel-tool-calls cannot be used \
                together.\n";
             exit 1
           | _ -> ());
          (* --keep-history requires --reset-session *)
          (match reset_keep_history, reset_session_id with
           | true, None ->
             Core.eprintf "Error: --keep-history can only be used with --reset-session.\n";
             exit 1
           | _ -> ());
          (* --session and --new-session are mutually exclusive *)
          (match session_id, new_session with
           | Some _, true ->
             Core.eprintf "Error: --session and --new-session are mutually exclusive.\n";
             exit 1
           | _ -> ());
          (* --no-persist and --auto-persist are mutually exclusive *)
          (match no_persist, auto_persist with
           | true, true ->
             Core.eprintf
               "Error: --no-persist and --auto-persist are mutually exclusive.\n";
             exit 1
           | _ -> ());
          (* --session-info cannot be combined with --session or --new-session *)
          (match session_info, session_id, new_session with
           | Some _, Some _, _ | Some _, _, true ->
             Core.eprintf
               "Error: --session-info cannot be combined with --session or --new-session.\n";
             exit 1
           | _ -> ());
          (* --reset-session validation *)
          (match reset_session_id with
           | None -> ()
           | Some _ ->
             (* Ensure incompatible flags with other mutually exclusive operations *)
             (match
                list_sessions, session_id, new_session, session_info, export_session_id
              with
              | true, _, _, _, _
              | _, Some _, _, _, _
              | _, _, true, _, _
              | _, _, _, Some _, _
              | _, _, _, _, Some _ ->
                Core.eprintf
                  "Error: --reset-session is incompatible with --list-sessions, \
                   --session, --new-session, --session-info, and --export-session.\n";
                exit 1
              | _ -> ()));
          (* --export-session validation *)
          (match export_session_id with
           | None -> ()
           | Some _ ->
             (* Require --out flag *)
             (match export_out_file with
              | None ->
                Core.eprintf
                  "Error: --out must be provided when using --export-session.\n";
                exit 1
              | Some _ -> ());
             (* Ensure incompatible flags are not set *)
             (match list_sessions, session_id, new_session, session_info with
              | true, _, _, _ | _, Some _, _, _ | _, _, true, _ | _, _, _, Some _ ->
                Core.eprintf
                  "Error: --export-session is incompatible with --list-sessions, \
                   --session, --new-session, and --session-info.\n";
                exit 1
              | _ -> ()));
          (* --export-file should not be combined with --export-session, --list-sessions, --session-info, or --reset-session *)
          match
            export_file, export_session_id, list_sessions, session_info, reset_session_id
          with
          | Some _, Some _, _, _, _
          | Some _, _, true, _, _
          | Some _, _, _, Some _, _
          | Some _, _, _, _, Some _ ->
            Core.eprintf
              "Error: --export-file cannot be combined with --export-session, \
               --list-sessions, --session-info, or --reset-session.\n";
            exit 1
          | _ ->
            ();
            (* --rebuild-from-prompt validation *)
            (match rebuild_session_id with
             | None -> ()
             | Some _ ->
               (match
                  ( list_sessions
                  , session_id
                  , new_session
                  , session_info
                  , export_session_id
                  , reset_session_id )
                with
                | true, _, _, _, _, _
                | _, Some _, _, _, _, _
                | _, _, true, _, _, _
                | _, _, _, Some _, _, _
                | _, _, _, _, Some _, _
                | _, _, _, _, _, Some _ ->
                  Core.eprintf
                    "Error: --rebuild-from-prompt is incompatible with other session \
                     flags.\n";
                  exit 1
                | _ -> ()))
        in
        fun () ->
          if list_sessions
          then
            Io.run_main (fun env ->
              let sessions = Session_store.list ~env in
              List.iter sessions ~f:(fun (id, prompt) -> Core.printf "%s\t%s\n" id prompt))
          else if Option.is_some session_info
          then (
            let info_id = Option.value_exn session_info in
            Io.run_main (fun env ->
              let dir = Session_store.path ~env info_id in
              let ( / ) = Eio.Path.( / ) in
              let snapshot = dir / "snapshot.bin" in
              if not (Eio.Path.is_file snapshot)
              then (
                Core.eprintf "Error: session '%s' not found.\n" info_id;
                exit 1)
              else (
                let stats = Eio.Path.stat ~follow:true snapshot in
                let session = Session.Io.File.read snapshot in
                let format_time secs =
                  let open Core_unix in
                  let tm = localtime secs in
                  Printf.sprintf
                    "%04d-%02d-%02d %02d:%02d:%02d"
                    (tm.tm_year + 1900)
                    (tm.tm_mon + 1)
                    tm.tm_mday
                    tm.tm_hour
                    tm.tm_min
                    tm.tm_sec
                in
                Core.printf "Session: %s\n" info_id;
                Core.printf "Prompt file: %s\n" session.prompt_file;
                Core.printf "Last modified: %s\n" (format_time stats.mtime);
                Core.printf "History items: %d\n" (List.length session.history);
                Core.printf "Tasks: %d\n" (List.length session.tasks))))
          else if Option.is_some reset_session_id
          then (
            let sid = Option.value_exn reset_session_id in
            Io.run_main (fun env ->
              Session_store.reset_session
                ~env
                ~id:sid
                ~keep_history:reset_keep_history
                ?prompt_file:reset_prompt_file
                ()))
          else if Option.is_some export_session_id
          then (
            let sid = Option.value_exn export_session_id in
            let outfile = Option.value_exn export_out_file in
            Io.run_main (fun env ->
              let sdir = Session_store.path ~env sid in
              let ( / ) = Eio.Path.( / ) in
              let snapshot = sdir / "snapshot.bin" in
              if not (Eio.Path.is_file snapshot)
              then (
                Core.eprintf "Error: session '%s' not found.\n" sid;
                exit 1);
              (* Acquire a simple advisory lock to avoid reading a snapshot
                 that is concurrently being modified.  We reuse the same
                 [snapshot.bin.lock] convention used by [Session_store.save]. *)
              let lock_file = Eio.Path.(sdir / "snapshot.bin.lock") in
              let acquired_lock =
                try
                  Eio.Path.save ~create:(`Exclusive 0o600) lock_file "";
                  true
                with
                | _ -> false
              in
              if not acquired_lock
              then (
                Core.eprintf
                  "Error: session '%s' is currently locked by another process.\n"
                  sid;
                exit 1);
              let session =
                protectx
                  ~finally:(fun () ->
                    try Eio.Path.unlink lock_file with
                    | _ -> ())
                  ()
                  ~f:(fun () -> Session.Io.File.read snapshot)
              in
              let dir_str = Filename.dirname outfile in
              let file_name = Filename.basename outfile in
              let fs = Eio.Stdenv.fs env in
              let out_dir = Eio.Path.(fs / dir_str) in
              (* create directory if missing *)
              (* Create the output directory if it does not already exist *)
              (match Eio.Path.is_directory out_dir with
               | true -> ()
               | false -> Eio.Path.mkdirs ~perm:0o700 out_dir);
              let dest_path = out_dir / file_name in
              (* Confirm overwrite *)
              if Eio.Path.is_file dest_path
              then (
                Out_channel.output_string
                  stdout
                  (Printf.sprintf "File %s exists. Overwrite? [y/N] " outfile);
                Out_channel.flush stdout;
                match In_channel.input_line In_channel.stdin with
                | Some ans
                  when List.mem
                         [ "y"; "yes" ]
                         (String.lowercase (String.strip ans))
                         ~equal:String.equal -> ()
                | _ ->
                  Core.printf "Aborted.\n";
                  (* Propagate cancellation by simply returning without exporting *)
                  ());
              let cwd = out_dir in
              let datadir = Io.ensure_chatmd_dir ~cwd in
              (* ------------------------------------------------------------------ *)
              (* 0. Copy original prompt content                                   *)
              (* ------------------------------------------------------------------ *)
              (* Resolve prompt path (absolute vs relative) from the
                   perspective of the session’s original working dir – here
                   we assume [session.prompt_file] is stored exactly as first
                   provided. *)
              let prompt_contents =
                let dir_for_prompt =
                  if Filename.is_absolute session.prompt_file
                  then fs
                  else Eio.Stdenv.cwd env
                in
                Option.value
                  (Option.try_with (fun () ->
                     Io.load_doc ~dir:dir_for_prompt session.prompt_file))
                  ~default:""
              in
              (* Save the initial prompt content with restrictive permissions. *)
              Eio.Path.save ~create:(`Or_truncate 0o600) dest_path prompt_contents;
              (* ------------------------------------------------------------------ *)
              (* Attachments – reuse shared helper                                        *)
              (* ------------------------------------------------------------------ *)
              let prompt_parent_dir =
                let base_dir =
                  if Filename.is_absolute session.prompt_file
                  then fs
                  else Eio.Stdenv.cwd env
                in
                Eio.Path.(base_dir / Filename.dirname session.prompt_file)
              in
              Chat_tui.Attachments.copy_all
                ~prompt_dir:prompt_parent_dir
                ~cwd:(Eio.Stdenv.cwd env)
                ~session_dir:sdir
                ~dst:datadir;
              (* Task #26: export the full conversation history.  We no
                 longer drop the first [initial_msg_count] items that
                 correspond to the static prompt. *)
              let module Config = Chat_response.Config in
              Chat_tui.Persistence.persist_session
                ~dir:cwd
                ~prompt_file:file_name
                ~datadir
                ~cfg:Config.default
                ~initial_msg_count:0
                ~history_items:session.history;
              Core.printf "Session '%s' exported to %s\n" sid outfile))
          else if Option.is_some rebuild_session_id
          then (
            let sid = Option.value_exn rebuild_session_id in
            Io.run_main (fun env -> Session_store.rebuild_session ~env ~id:sid ()))
          else (
            let persist_mode =
              match no_persist, auto_persist with
              | true, _ -> `Never
              | _, true -> `Always
              | _ -> `Ask
            in
            let parallel_tool_calls_value =
              match parallel_tool_calls, no_parallel_tool_calls with
              | true, false -> true
              | false, true -> false
              | false, false -> true
              | true, true -> (* Already validated earlier. *) true
            in
            run
              ?session_id
              ~new_session
              ?export_file
              ~persist_mode
              ~parallel_tool_calls:parallel_tool_calls_value
              ~prompt_file:conversation_file
              ())]
  in
  Command_unix.run command
;;
