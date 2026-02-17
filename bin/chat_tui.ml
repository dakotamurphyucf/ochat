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

module Env = struct
  let with_env f = Io.run_main (fun env -> f env)
end

let help_output_texts_prompt =
  {|
  You are a helpful assistant answering queries about an interactive terminal UI for Ochat. You are tasked with
  answering questions about the interactive terminal UI for Ochat using the provided help text ouput below:

  <help-text>

  Interactive terminal UI for Ochat (with session management and export modes)

    chat-tui

  chat-tui is an interactive terminal UI for Ochat.

  The program has one interactive mode (default) plus several "one-shot" modes
  that perform an operation and exit:

    • --list-sessions
    • --session-info NAME
    • --reset-session NAME
    • --rebuild-from-prompt NAME
    • --export-session NAME --out FILE

  Sessions are stored under:

    $HOME/.ochat/sessions/   (or ./.ochat/sessions if $HOME is unset)

  Interactive mode chooses the session to use as follows:

    1) --session NAME (resume NAME)
    2) --new-session (create a fresh UUID-named session)
    3) otherwise: a deterministic ID derived from the prompt file path

  Common examples:

    chat-tui
    chat-tui -file ./prompts/interactive.md
    chat-tui --session my-session
    chat-tui --list-sessions
    chat-tui --export-session my-session --out /tmp/out.chatmd

  Session subcommands (optional, more discoverable than flags):

    chat-tui sessions -help
    chat-tui sessions list [--json]
    chat-tui sessions info NAME [--json]
    chat-tui sessions export NAME --out FILE
    chat-tui sessions reset NAME [--keep-history] [--prompt-file FILE] [--dry-run]
    chat-tui sessions rebuild-from-prompt NAME [--dry-run]

  Ask AI subcommand (ask ai questions about using chat-tui):

    chat-tui ask-ai -query QUERY

  Notes:

    • Use -help / --help for full flag documentation.
    • To set persistent defaults, you can use a config file:
        - default: $XDG_CONFIG_HOME/ochat/chat-tui.args
          (or ~/.config/ochat/chat-tui.args if XDG_CONFIG_HOME is unset)
        - disable: --no-config
        - override: --config FILE
        - debug: --print-effective-args
      The file is parsed as whitespace-separated arguments (one or more per line).
    • Some flags are mode-specific:
        - --export-file only applies to interactive mode.
        - --prompt-file only applies to --reset-session.
        - --parallel-tool-calls / --no-parallel-tool-calls and
          --auto-persist / --no-persist only apply to interactive mode.
    • For scripting, --list-sessions and --session-info support JSON output
      via --format json (or --json).
    • --dry-run prints a prompt preview; control size with -prompt-preview-max N
      (0 = unlimited).

  === flags ===

    [--auto-persist]           . In interactive mode: always persist the session
                                 snapshot on exit without prompting.
    [--dry-run]                . Print what would happen and exit (supported with
                                 --reset-session and --rebuild-from-prompt).
    [--export-file FILE]       . After you quit the interactive UI, export the
                                 full transcript to FILE in ChatMarkdown format.
                                 (interactive mode only)
    [--export-session NAME]    . Export session NAME to a standalone .chatmd file
                                 and exit. Requires --out. Incompatible with other
                                 one-shot modes.
    [--format FORMAT]          . Output format for --list-sessions /
                                 --session-info (human|tsv|json).
    [--help-short]             . Print a short usage summary and exit.
    [--json]                   . Alias for --format json (for --list-sessions /
                                 --session-info).
    [--keep-history]           . When used with --reset-session: retain
                                 conversation history and cached data instead of
                                 clearing them.
    [--list-sessions]          . List known sessions (from $HOME/.ochat/sessions)
                                 and exit. Incompatible with other one-shot modes.
    [--new-session]            . Force creation of a brand-new session (UUID) even
                                 if a prompt-derived session already exists.
                                 Incompatible with --session.
    [--no-parallel-tool-calls] . Disable parallel execution of callable tools
                                 during interactive runs (forces sequential
                                 evaluation).
    [--no-persist]             . In interactive mode: never persist the session
                                 snapshot on exit (no save).
    [--out FILE]               . Output path for --export-session. If FILE exists,
                                 you will be prompted before overwriting.
    [--parallel-tool-calls]    . Enable parallel execution of callable tools
                                 during interactive runs. (default: enabled)
    [--prompt-file FILE]       . When used with --reset-session: set a new prompt
                                 file for the reset session.
    [--rebuild-from-prompt NAME]
                               . Rebuild session NAME from its stored
                                 prompt.chatmd copy and exit.
    [--reset-session NAME]     . Archive the current snapshot and reset session
                                 NAME, optionally keeping history (--keep-history)
                                 and/or replacing the prompt (--prompt-file).
    [--session NAME]           . Resume session NAME (a directory name under
                                 $HOME/.ochat/sessions). Incompatible with
                                 --new-session.
    [--session-info NAME]      . Display metadata for session NAME (prompt path,
                                 timestamps, history length, …) and exit.
    [-file FILE]               . Prompt file (ChatMarkdown/Markdown) used to seed
                                 the interactive session. Also used to derive the
                                 default session ID when neither --session nor
                                 --new-session is provided. (default:
                                 ./prompts/interactive.md)
    [-prompt-preview-max N]    . Max chars of prompt preview for --dry-run (0 =
                                 unlimited).
    [-build-info]              . print info about this build and exit
    [-version]                 . print the version of this build and exit
    [-help], -?                . print this help text and exit





  Session management commands

    chat-tui sessions SUBCOMMAND

  === subcommands ===

    export                     . Export a session snapshot to a standalone .chatmd
                                 file
    info                       . Show session metadata
    list                       . List sessions
    rebuild-from-prompt        . Rebuild a session from its stored prompt.chatmd
    reset                      . Archive the current snapshot and reset a session
    version                    . print version information
    help                       . explain a given subcommand (perhaps recursively)

  Export a session snapshot to a standalone .chatmd file

    chat-tui sessions export NAME

  === flags ===

    --out FILE                 . Destination file (will prompt before
                                 overwriting).
    [-help], -?                . print this help text and exit

  Show session metadata

    chat-tui sessions info NAME

  === flags ===

    [--format FORMAT]          . Output format (human|tsv|json).
    [--json]                   . Alias for --format json.
    [-help], -?                . print this help text and exit

  List sessions

    chat-tui sessions list

  === flags ===

    [--format FORMAT]          . Output format (tsv|json).
    [--json]                   . Alias for --format json.
    [-help], -?                . print this help text and exit

  Rebuild a session from its stored prompt.chatmd

    chat-tui sessions rebuild-from-prompt NAME

  === flags ===

    [--dry-run]                . Print what would happen and exit.
    [-prompt-preview-max N]    . Max chars of prompt preview for --dry-run (0 =
                                 unlimited).
    [-help], -?                . print this help text and exit

  Archive the current snapshot and reset a session

    chat-tui sessions reset NAME

  === flags ===

    [--dry-run]                . Print what would happen and exit.
    [--keep-history]           . Keep history when resetting.
    [--prompt-file FILE]       . New prompt file to use after reset.
    [-prompt-preview-max N]    . Max chars of prompt preview for --dry-run (0 =
                                 unlimited).
    [-help], -?                . print this help text and exit


  Ask ai a question about chat tui cli

    chat-tui ask-ai

  === flags ===

    -query query               . to ask ai
    [-build-info]              . print info about this build and exit
    [-version]                 . print the version of this build and exit
    [-help], -?                . print this help text and exit
  </help-text>

  Use the help text above to answer questions about the interactive terminal UI for Ochat.

  Formatting:
  Format output so that it is optimized for readability and clarity in a modern terminal. So dont use Markdown, try to color the output using ANSI escape codes, and use emojis to enhance the experience.
  |}
;;

let ask_ai input env =
  let open Openai.Responses in
  let system_prompt = help_output_texts_prompt in
  let dir = Eio.Stdenv.fs env in
  let net = Eio.Stdenv.net env in
  let open Input_message in
  let text_item text : content_item = Text { text; _type = "input_text" } in
  let mk_input role text : Item.t =
    let role =
      match role with
      | "user" -> User
      | "assistant" -> Assistant
      | "system" -> System
      | "developer" -> Developer
      | _ -> System
    in
    let msg : Input_message.t =
      { role; content = [ text_item text ]; _type = "message" }
    in
    Item.Input_message msg
  in
  let inputs = [ mk_input "system" system_prompt; mk_input "user" input ] in
  try
    let response =
      post_response
        Default
        ~max_output_tokens:100000
        ~temperature:0.3
        ~model:(Request.Unknown "gpt-5.2")
        ~dir
        net
        ~inputs
    in
    let ({ Response.output; _ } : Response.t) = response in
    (* Extract assistant text from first Output_message. *)
    let rec find_text = function
      | [] -> None
      | Item.Output_message om :: _ ->
        (match om.Output_message.content with
         | { text; _ } :: _ -> Some text
         | _ -> None)
      | _ :: tl -> find_text tl
    in
    match find_text output with
    | Some text -> Ok text
    | None -> Error "error no response"
  with
  | exn ->
    eprintf "Summarizer.summarise: %s\n%!" (Exn.to_string exn);
    Io.log ~dir ~file:"Summarizer.summarise.error-log.txt" (Exn.to_string exn);
    Error (Exn.to_string exn)
;;

let default_prompt_file = "./prompts/interactive.md"

let readme_text =
  {|
chat-tui is an interactive terminal UI for Ochat.

The program has one interactive mode (default) plus several "one-shot" modes
that perform an operation and exit:

  • --list-sessions
  • --session-info NAME
  • --reset-session NAME
  • --rebuild-from-prompt NAME
  • --export-session NAME --out FILE

Sessions are stored under:

  $HOME/.ochat/sessions/   (or ./.ochat/sessions if $HOME is unset)

Interactive mode chooses the session to use as follows:

  1) --session NAME (resume NAME)
  2) --new-session (create a fresh UUID-named session)
  3) otherwise: a deterministic ID derived from the prompt file path

Common examples:

  chat-tui
  chat-tui -file ./prompts/interactive.md
  chat-tui --session my-session
  chat-tui --list-sessions
  chat-tui --export-session my-session --out /tmp/out.chatmd

Session subcommands (optional, more discoverable than flags):

  chat-tui sessions -help
  chat-tui sessions list [--json]
  chat-tui sessions info NAME [--json]
  chat-tui sessions export NAME --out FILE
  chat-tui sessions reset NAME [--keep-history] [--prompt-file FILE] [--dry-run]
  chat-tui sessions rebuild-from-prompt NAME [--dry-run]

Ask AI subcommand (ask ai questions about using chat-tui):

  chat-tui ask-ai -query QUERY

Notes:

  • Use -help / --help for full flag documentation.
  • To set persistent defaults, you can use a config file:
      - default: $XDG_CONFIG_HOME/ochat/chat-tui.args
        (or ~/.config/ochat/chat-tui.args if XDG_CONFIG_HOME is unset)
      - disable: --no-config
      - override: --config FILE
      - debug: --print-effective-args
    The file is parsed as whitespace-separated arguments (one or more per line).
  • Some flags are mode-specific:
      - --export-file only applies to interactive mode.
      - --prompt-file only applies to --reset-session.
      - --parallel-tool-calls / --no-parallel-tool-calls and
        --auto-persist / --no-persist only apply to interactive mode.
  • For scripting, --list-sessions and --session-info support JSON output
    via --format json (or --json).
  • --dry-run prints a prompt preview; control size with -prompt-preview-max N
    (0 = unlimited).
|}
;;

let readme () = readme_text

let help_short_text =
  {|
chat-tui — interactive terminal UI for Ochat

Common one-shot modes:
  chat-tui --list-sessions
  chat-tui --session-info NAME
  chat-tui --reset-session NAME [--keep-history] [--prompt-file FILE]
  chat-tui --rebuild-from-prompt NAME
  chat-tui --export-session NAME --out FILE

Session subcommands:
  chat-tui sessions -help
  chat-tui sessions list [--json]
  chat-tui sessions info NAME [--json]

Ask AI subcommand (ask ai questions about using chat-tui):

  chat-tui ask-ai -query QUERY

Interactive mode:
  chat-tui [-file FILE] [--session NAME | --new-session]

Run with --help for full flag documentation.
|}
;;

let print_help_short () = printf "%s\n" help_short_text

let load_session ~env ~prompt_file ?id ~new_session () =
  Session_store.load_or_create ~env ~prompt_file ?id ~new_session ()
;;

let run_in_env
      ~env
      ~prompt_file
      ?session_id
      ~new_session
      ?export_file
      ~persist_mode
      ~parallel_tool_calls
      ()
  =
  let session = load_session ~env ~prompt_file ?id:session_id ~new_session () in
  Chat_tui.App.run_chat
    ~env
    ~prompt_file
    ~session
    ?export_file
    ~persist_mode
    ~parallel_tool_calls
    ()
;;

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
  Env.with_env (fun env ->
    run_in_env
      ~env
      ~prompt_file
      ?session_id
      ~new_session
      ?export_file
      ~persist_mode
      ~parallel_tool_calls
      ())
;;

module Time = struct
  let format_localtime secs =
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
  ;;
end

module Handlers = struct
  module Output_format = struct
    type t =
      | Human
      | Tsv
      | Json
  end

  let require_snapshot ~id snapshot =
    if not (Eio.Path.is_file snapshot)
    then (
      eprintf "Error: session '%s' not found.\n" id;
      exit 1)
  ;;

  let print_json json = printf "%s\n" (Jsonaf.to_string_hum json)

  let sessions_to_json sessions =
    `Array
      (List.map sessions ~f:(fun (id, prompt_file) ->
         `Object [ "id", `String id; "prompt_file", `String prompt_file ]))
  ;;

  let handle_list_sessions ~env ~format =
    let sessions = Session_store.list ~env in
    match format with
    | Output_format.Tsv ->
      List.iter sessions ~f:(fun (id, prompt) -> printf "%s\t%s\n" id prompt)
    | Output_format.Json -> print_json (sessions_to_json sessions)
    | Output_format.Human -> print_json (sessions_to_json sessions)
  ;;

  let session_info_to_json ~id ~mtime_secs ~(session : Session.t) =
    `Object
      [ "id", `String id
      ; "prompt_file", `String session.prompt_file
      ; "last_modified", `String (Time.format_localtime mtime_secs)
      ; "last_modified_epoch", `Number (Float.to_string mtime_secs)
      ; "history_items", `Number (Int.to_string (List.length session.history))
      ; "tasks", `Number (Int.to_string (List.length session.tasks))
      ]
  ;;

  let handle_session_info ~env ~id ~format =
    let dir = Session_store.path ~env id in
    let snapshot = Eio.Path.(dir / "snapshot.bin") in
    require_snapshot ~id snapshot;
    let stats = Eio.Path.stat ~follow:true snapshot in
    let session = Session.Io.File.read snapshot in
    match format with
    | Output_format.Human ->
      printf "Session: %s\n" id;
      printf "Prompt file: %s\n" session.prompt_file;
      printf "Last modified: %s\n" (Time.format_localtime stats.mtime);
      printf "History items: %d\n" (List.length session.history);
      printf "Tasks: %d\n" (List.length session.tasks)
    | Output_format.Json ->
      print_json (session_info_to_json ~id ~mtime_secs:stats.mtime ~session)
    | Output_format.Tsv ->
      printf
        "%s\t%s\t%s\t%d\t%d\n"
        id
        session.prompt_file
        (Time.format_localtime stats.mtime)
        (List.length session.history)
        (List.length session.tasks)
  ;;

  module Export_session = struct
    let acquire_lock_or_exit ~id ~lock_file =
      let acquired_lock =
        try
          Eio.Path.save ~create:(`Exclusive 0o600) lock_file "";
          true
        with
        | _ -> false
      in
      if not acquired_lock
      then (
        eprintf "Error: session '%s' is currently locked by another process.\n" id;
        exit 1)
    ;;

    let read_with_lock ~lock_file ~snapshot =
      protectx
        ~finally:(fun () ->
          try Eio.Path.unlink lock_file with
          | _ -> ())
        ()
        ~f:(fun () -> Session.Io.File.read snapshot)
    ;;

    let mkdirs_if_missing dir =
      match Eio.Path.is_directory dir with
      | true -> ()
      | false -> Eio.Path.mkdirs ~perm:0o700 dir
    ;;

    let confirm_overwrite ~dest_path ~outfile =
      if not (Eio.Path.is_file dest_path)
      then true
      else (
        Out_channel.output_string
          stdout
          (Printf.sprintf "File %s exists. Overwrite? [y/N] " outfile);
        Out_channel.flush stdout;
        match In_channel.input_line In_channel.stdin with
        | Some ans
          when List.mem
                 [ "y"; "yes" ]
                 (String.lowercase (String.strip ans))
                 ~equal:String.equal -> true
        | _ ->
          printf "Aborted.\n";
          false)
    ;;

    let load_prompt_contents ~env ~fs prompt_file =
      let dir_for_prompt =
        if Filename.is_absolute prompt_file then fs else Eio.Stdenv.cwd env
      in
      Option.value
        (Option.try_with (fun () -> Io.load_doc ~dir:dir_for_prompt prompt_file))
        ~default:""
    ;;

    let prompt_parent_dir ~env ~fs prompt_file =
      let base_dir =
        if Filename.is_absolute prompt_file then fs else Eio.Stdenv.cwd env
      in
      Eio.Path.(base_dir / Filename.dirname prompt_file)
    ;;

    let persist_full_history ~cwd ~prompt_file ~datadir ~history_items =
      let module Config = Chat_response.Config in
      Chat_tui.Persistence.persist_session
        ~dir:cwd
        ~prompt_file
        ~datadir
        ~cfg:Config.default
        ~initial_msg_count:0
        ~history_items
    ;;

    let read_session ~env ~id =
      let sdir = Session_store.path ~env id in
      let snapshot = Eio.Path.(sdir / "snapshot.bin") in
      require_snapshot ~id snapshot;
      let lock_file = Eio.Path.(sdir / "snapshot.bin.lock") in
      acquire_lock_or_exit ~id ~lock_file;
      let session = read_with_lock ~lock_file ~snapshot in
      sdir, session
    ;;

    let export_paths ~env ~outfile =
      let dir_str = Filename.dirname outfile in
      let file_name = Filename.basename outfile in
      let fs = Eio.Stdenv.fs env in
      let out_dir = Eio.Path.(fs / dir_str) in
      mkdirs_if_missing out_dir;
      let dest_path = Eio.Path.(out_dir / file_name) in
      out_dir, dest_path, file_name, fs
    ;;

    let write_prompt_file ~env ~fs ~prompt_file ~dest_path =
      let prompt_contents = load_prompt_contents ~env ~fs prompt_file in
      Eio.Path.save ~create:(`Or_truncate 0o600) dest_path prompt_contents
    ;;

    let copy_attachments ~env ~fs ~prompt_file ~session_dir ~datadir =
      let prompt_dir = prompt_parent_dir ~env ~fs prompt_file in
      Chat_tui.Attachments.copy_all
        ~prompt_dir
        ~cwd:(Eio.Stdenv.cwd env)
        ~session_dir
        ~dst:datadir
    ;;

    let handle ~env ~id ~outfile =
      let sdir, session = read_session ~env ~id in
      let out_dir, dest_path, file_name, fs = export_paths ~env ~outfile in
      let proceed = confirm_overwrite ~dest_path ~outfile in
      if proceed
      then (
        let cwd = out_dir in
        let datadir = Io.ensure_chatmd_dir ~cwd in
        write_prompt_file ~env ~fs ~prompt_file:session.prompt_file ~dest_path;
        copy_attachments
          ~env
          ~fs
          ~prompt_file:session.prompt_file
          ~session_dir:sdir
          ~datadir;
        persist_full_history
          ~cwd
          ~prompt_file:file_name
          ~datadir
          ~history_items:session.history;
        printf "Session '%s' exported to %s\n" id outfile)
    ;;
  end

  let handle_export_session ~env ~id ~outfile = Export_session.handle ~env ~id ~outfile

  let timestamp_for_archive () =
    let tm = Core_unix.localtime (Core_unix.time ()) in
    Printf.sprintf
      "%04d%02d%02d-%02d%02d"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
  ;;

  let truncated ~max_len s =
    if max_len = 0
    then s
    else if String.length s <= max_len
    then s
    else String.prefix s max_len ^ "\n…"
  ;;

  let print_prompt_preview ~prompt_preview_max ~label contents =
    let contents = truncated ~max_len:prompt_preview_max contents in
    printf "%s:\n%s\n" label contents
  ;;

  let input_message_as_chatmd (im : Openai.Responses.Input_message.t) =
    let role = Openai.Responses.Input_message.role_to_string im.role in
    let content =
      List.filter_map im.content ~f:(function
        | Openai.Responses.Input_message.Text { text; _ } -> Some text
        | _ -> None)
      |> String.concat ~sep:""
    in
    match role with
    | "user" -> Printf.sprintf "<user>\n%s\n</user>\n" content
    | "assistant" -> Printf.sprintf "<assistant>\n%s\n</assistant>\n" content
    | "tool" -> Printf.sprintf "<tool_response>\n%s\n</tool_response>\n" content
    | _ -> Printf.sprintf "<msg role=\"%s\">\n%s\n</msg>\n" role content
  ;;

  let output_message_as_chatmd (om : Openai.Responses.Output_message.t) =
    let text = List.map om.content ~f:(fun c -> c.text) |> String.concat ~sep:" " in
    Printf.sprintf "\n<assistant id=\"%s\">\nRAW|\n%s\n|RAW\n</assistant>\n" om.id text
  ;;

  let reasoning_as_chatmd (r : Openai.Responses.Reasoning.t) =
    let summaries =
      List.map r.summary ~f:(fun s ->
        Printf.sprintf "\n<summary>\n%s\n</summary>\n" s.text)
      |> String.concat ~sep:""
    in
    Printf.sprintf "\n<reasoning id=\"%s\">%s\n</reasoning>\n" r.id summaries
  ;;

  let tool_output_as_chatmd_string = function
    | Openai.Responses.Tool_output.Output.Text text -> text
    | Content cont ->
      List.map cont ~f:(function
        | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
        | Input_image { image_url; _ } -> Printf.sprintf "<img src=\"%s\" />" image_url)
      |> String.concat ~sep:"\n"
  ;;

  let function_call_as_chatmd (fc : Openai.Responses.Function_call.t) =
    Printf.sprintf
      "\n\
       <tool_call tool_call_id=\"%s\" function_name=\"%s\" id=\"%s\">\n\
       %s|\n\
       %s\n\
       |%s\n\
       </tool_call>\n"
      fc.call_id
      fc.name
      (Option.value fc.id ~default:fc.call_id)
      "RAW"
      fc.arguments
      "RAW"
  ;;

  let custom_tool_call_as_chatmd (tc : Openai.Responses.Custom_tool_call.t) =
    Printf.sprintf
      "\n\
       <tool_call type=\"custom_tool_call\" tool_call_id=\"%s\" function_name=\"%s\" \
       id=\"%s\">\n\
       %s|\n\
       %s\n\
       |%s\n\
       </tool_call>\n"
      tc.call_id
      tc.name
      (Option.value tc.id ~default:tc.call_id)
      "RAW"
      tc.input
      "RAW"
  ;;

  let function_call_output_as_chatmd (fco : Openai.Responses.Function_call_output.t) =
    Printf.sprintf
      "<tool_response tool_call_id=\"%s\">\nRAW|\n%s\n|RAW\n</tool_response>\n"
      fco.call_id
      (tool_output_as_chatmd_string fco.output)
  ;;

  let custom_tool_call_output_as_chatmd (tco : Openai.Responses.Custom_tool_call_output.t)
    =
    Printf.sprintf
      "<tool_response type=\"custom_tool_call\" tool_call_id=\"%s\">\n\
       RAW|\n\
       %s\n\
       |RAW\n\
       </tool_response>\n"
      tco.call_id
      (tool_output_as_chatmd_string tco.output)
  ;;

  let other_item_as_chatmd item =
    let sexp = Sexp.to_string_hum (Openai.Responses.Item.sexp_of_t item) in
    Printf.sprintf "<item>\n%s\n</item>\n" sexp
  ;;

  let history_item_as_chatmd = function
    | Openai.Responses.Item.Input_message im -> input_message_as_chatmd im
    | Openai.Responses.Item.Output_message om -> output_message_as_chatmd om
    | Openai.Responses.Item.Function_call fc -> function_call_as_chatmd fc
    | Openai.Responses.Item.Custom_tool_call tc -> custom_tool_call_as_chatmd tc
    | Openai.Responses.Item.Function_call_output fco -> function_call_output_as_chatmd fco
    | Openai.Responses.Item.Custom_tool_call_output tco ->
      custom_tool_call_output_as_chatmd tco
    | Openai.Responses.Item.Reasoning r -> reasoning_as_chatmd r
    | item -> other_item_as_chatmd item
  ;;

  let history_as_chatmd history =
    List.map history ~f:history_item_as_chatmd |> String.concat ~sep:""
  ;;

  let print_history_preview ~prompt_preview_max history =
    printf "History items (kept): %d\n" (List.length history);
    print_prompt_preview
      ~prompt_preview_max
      ~label:"History preview (as chatmd)"
      (history_as_chatmd history)
  ;;

  let load_prompt_for_reset ~env prompt_file =
    let fs = Eio.Stdenv.fs env in
    Or_error.try_with (fun () -> Io.load_doc ~dir:fs prompt_file)
  ;;

  let load_local_prompt_copy ~dir filename =
    let path = Eio.Path.(dir / filename) in
    Or_error.try_with (fun () -> Eio.Path.load path)
  ;;

  let load_prompt_best_effort ~env prompt_file =
    let base =
      if Filename.is_absolute prompt_file then Eio.Stdenv.fs env else Eio.Stdenv.cwd env
    in
    Or_error.try_with (fun () -> Io.load_doc ~dir:base prompt_file)
  ;;

  let print_prompt_plan
        ~env
        ~dir
        ~session
        ~prompt_preview_max
        ~(new_prompt_file : string option)
    =
    match new_prompt_file with
    | Some prompt_file ->
      (match load_prompt_for_reset ~env prompt_file with
       | Ok contents ->
         printf "Prompt file: %s\n" prompt_file;
         printf
           "Would write session prompt copy: %s\n"
           (Eio.Path.native_exn Eio.Path.(dir / "prompt.chatmd"));
         print_prompt_preview ~prompt_preview_max ~label:"Prompt preview" contents
       | Error e ->
         printf "Prompt file: %s\n" prompt_file;
         printf "Could not load prompt file: %s\n" (Error.to_string_hum e))
    | None ->
      (match session.Session.local_prompt_copy with
       | None ->
         (match load_prompt_best_effort ~env session.prompt_file with
          | Ok contents ->
            printf "Prompt file: %s\n" session.prompt_file;
            print_prompt_preview ~prompt_preview_max ~label:"Prompt preview" contents
          | Error e ->
            printf "Prompt file: %s\n" session.prompt_file;
            printf "Could not load prompt file: %s\n" (Error.to_string_hum e))
       | Some filename ->
         (match load_local_prompt_copy ~dir filename with
          | Ok contents ->
            printf
              "Session prompt copy: %s\n"
              (Eio.Path.native_exn Eio.Path.(dir / filename));
            print_prompt_preview ~prompt_preview_max ~label:"Prompt preview" contents
          | Error e ->
            printf
              "Session prompt copy: %s\n"
              (Eio.Path.native_exn Eio.Path.(dir / filename));
            printf "Could not load session prompt copy: %s\n" (Error.to_string_hum e)))
  ;;

  let print_reset_plan ~env ~id ~keep_history ~prompt_file ~prompt_preview_max =
    let dir = Session_store.path ~env id in
    let snapshot = Eio.Path.(dir / "snapshot.bin") in
    require_snapshot ~id snapshot;
    let session = Session.Io.File.read snapshot in
    let archive_dir = Eio.Path.(dir / "archive") in
    let archived_snapshot =
      Eio.Path.(archive_dir / Printf.sprintf "%s.snapshot.bin" (timestamp_for_archive ()))
    in
    let lock_file = Eio.Path.(dir / "snapshot.bin.lock") in
    let chatmd_cache = Eio.Path.(dir / ".chatmd" / "cache.bin") in
    let new_prompt_file = Option.value prompt_file ~default:session.prompt_file in
    printf "Dry-run: reset session '%s'\n" id;
    printf "Would create session dir (if missing): %s\n" (Eio.Path.native_exn dir);
    printf
      "Would archive: %s -> %s\n"
      (Eio.Path.native_exn snapshot)
      (Eio.Path.native_exn archived_snapshot);
    printf "Would keep history: %b\n" keep_history;
    printf "Would set session prompt path: %s\n" new_prompt_file;
    (match keep_history with
     | true -> ()
     | false -> printf "Would delete cache: %s\n" (Eio.Path.native_exn chatmd_cache));
    printf "Would write new snapshot using lock: %s\n" (Eio.Path.native_exn lock_file);
    print_prompt_plan ~env ~dir ~session ~prompt_preview_max ~new_prompt_file:prompt_file;
    match keep_history with
    | false -> ()
    | true -> print_history_preview ~prompt_preview_max session.history
  ;;

  let print_rebuild_plan ~env ~id ~prompt_preview_max =
    let dir = Session_store.path ~env id in
    let snapshot = Eio.Path.(dir / "snapshot.bin") in
    require_snapshot ~id snapshot;
    let session = Session.Io.File.read snapshot in
    let archive_dir = Eio.Path.(dir / "archive") in
    let archived_snapshot =
      Eio.Path.(archive_dir / Printf.sprintf "%s.snapshot.bin" (timestamp_for_archive ()))
    in
    let lock_file = Eio.Path.(dir / "snapshot.bin.lock") in
    let chatmd_cache = Eio.Path.(dir / ".chatmd" / "cache.bin") in
    printf "Dry-run: rebuild session '%s' from prompt.chatmd\n" id;
    printf "Would create session dir (if missing): %s\n" (Eio.Path.native_exn dir);
    printf
      "Would archive: %s -> %s\n"
      (Eio.Path.native_exn snapshot)
      (Eio.Path.native_exn archived_snapshot);
    printf "Would delete cache: %s\n" (Eio.Path.native_exn chatmd_cache);
    printf "Would write new snapshot using lock: %s\n" (Eio.Path.native_exn lock_file);
    print_prompt_plan ~env ~dir ~session ~prompt_preview_max ~new_prompt_file:None
  ;;

  let handle_reset_session
        ~env
        ~id
        ~keep_history
        ~prompt_file
        ~dry_run
        ~prompt_preview_max
    =
    if dry_run
    then print_reset_plan ~env ~id ~keep_history ~prompt_file ~prompt_preview_max
    else Session_store.reset_session ~env ~id ~keep_history ?prompt_file ()
  ;;

  let handle_rebuild_from_prompt ~env ~id ~dry_run ~prompt_preview_max =
    if dry_run
    then print_rebuild_plan ~env ~id ~prompt_preview_max
    else Session_store.rebuild_session ~env ~id ()
  ;;

  let handle_interactive
        ~prompt_file
        ~session_id
        ~new_session
        ~export_file
        ~persist_mode
        ~parallel_tool_calls
    =
    run
      ?session_id
      ~new_session
      ?export_file
      ~persist_mode
      ~parallel_tool_calls
      ~prompt_file
      ()
  ;;
end

module Sessions_command = struct
  let parse_format format =
    match String.lowercase format with
    | "human" -> Ok Handlers.Output_format.Human
    | "tsv" -> Ok Handlers.Output_format.Tsv
    | "json" -> Ok Handlers.Output_format.Json
    | _ -> Or_error.errorf "Error: unknown --format %S (expected: human|tsv|json)" format
  ;;

  let format_of_flags ~default ~format ~json =
    match json, format with
    | true, Some f when not (String.Caseless.equal f "json") ->
      Or_error.error_string
        "Error: --json cannot be combined with --format (unless --format json)."
    | true, _ -> Ok Handlers.Output_format.Json
    | false, None -> Ok default
    | false, Some f -> parse_format f
  ;;

  let list_command =
    let open Command.Let_syntax in
    Command.basic_or_error
      ~summary:"List sessions"
      [%map_open
        let format =
          flag "--format" (optional string) ~doc:"FORMAT Output format (tsv|json)."
        and json = flag "--json" no_arg ~doc:"Alias for --format json." in
        fun () ->
          let open Or_error.Let_syntax in
          let%map format =
            format_of_flags ~default:Handlers.Output_format.Tsv ~format ~json
          in
          Env.with_env (fun env -> Handlers.handle_list_sessions ~env ~format)]
  ;;

  let info_command =
    let open Command.Let_syntax in
    Command.basic_or_error
      ~summary:"Show session metadata"
      [%map_open
        let id = anon ("NAME" %: string)
        and format =
          flag "--format" (optional string) ~doc:"FORMAT Output format (human|tsv|json)."
        and json = flag "--json" no_arg ~doc:"Alias for --format json." in
        fun () ->
          let open Or_error.Let_syntax in
          let%map format =
            format_of_flags ~default:Handlers.Output_format.Human ~format ~json
          in
          Env.with_env (fun env -> Handlers.handle_session_info ~env ~id ~format)]
  ;;

  let export_command =
    let open Command.Let_syntax in
    Command.basic
      ~summary:"Export a session snapshot to a standalone .chatmd file"
      [%map_open
        let id = anon ("NAME" %: string)
        and outfile =
          flag
            "--out"
            (required string)
            ~doc:"FILE Destination file (will prompt before overwriting)."
        in
        fun () ->
          Env.with_env (fun env -> Handlers.handle_export_session ~env ~id ~outfile)]
  ;;

  let reset_command =
    let open Command.Let_syntax in
    Command.basic_or_error
      ~summary:"Archive the current snapshot and reset a session"
      [%map_open
        let id = anon ("NAME" %: string)
        and keep_history =
          flag "--keep-history" no_arg ~doc:"Keep history when resetting."
        and prompt_file =
          flag
            "--prompt-file"
            (optional string)
            ~doc:"FILE New prompt file to use after reset."
        and dry_run = flag "--dry-run" no_arg ~doc:"Print what would happen and exit."
        and prompt_preview_max =
          flag
            "-prompt-preview-max"
            (optional_with_default 2000 int)
            ~doc:"N Max chars of prompt preview for --dry-run (0 = unlimited)."
        in
        fun () ->
          let open Or_error.Let_syntax in
          let%map () =
            if prompt_preview_max < 0
            then Or_error.error_string "Error: -prompt-preview-max must be >= 0."
            else Ok ()
          in
          Env.with_env (fun env ->
            Handlers.handle_reset_session
              ~env
              ~id
              ~keep_history
              ~prompt_file
              ~dry_run
              ~prompt_preview_max)]
  ;;

  let rebuild_command =
    let open Command.Let_syntax in
    Command.basic_or_error
      ~summary:"Rebuild a session from its stored prompt.chatmd"
      [%map_open
        let id = anon ("NAME" %: string)
        and dry_run = flag "--dry-run" no_arg ~doc:"Print what would happen and exit."
        and prompt_preview_max =
          flag
            "-prompt-preview-max"
            (optional_with_default 2000 int)
            ~doc:"N Max chars of prompt preview for --dry-run (0 = unlimited)."
        in
        fun () ->
          let open Or_error.Let_syntax in
          let%map () =
            if prompt_preview_max < 0
            then Or_error.error_string "Error: -prompt-preview-max must be >= 0."
            else Ok ()
          in
          Env.with_env (fun env ->
            Handlers.handle_rebuild_from_prompt ~env ~id ~dry_run ~prompt_preview_max)]
  ;;

  let command =
    Command.group
      ~summary:"Session management commands"
      [ "list", list_command
      ; "info", info_command
      ; "export", export_command
      ; "reset", reset_command
      ; "rebuild-from-prompt", rebuild_command
      ]
  ;;
end

module Ask_ai_command = struct
  let command =
    let open Command.Let_syntax in
    Command.basic_or_error
      ~summary:"Ask ai a question about chat tui cli"
      [%map_open
        let query = flag "-query" (required string) ~doc:"query to ask ai" in
        fun () ->
          Env.with_env (fun env ->
            let response = ask_ai query env in
            match response with
            | Ok text ->
              print_endline text;
              Ok ()
            | Error err -> Or_error.error_string err)]
  ;;
end

module Cli = struct
  type raw_flags =
    { conversation_file : string
    ; list_sessions : bool
    ; session_id : string option
    ; new_session : bool
    ; export_session_id : string option
    ; export_out_file : string option
    ; export_file : string option
    ; session_info : string option
    ; reset_session_id : string option
    ; reset_prompt_file : string option
    ; reset_keep_history : bool
    ; parallel_tool_calls : bool
    ; no_parallel_tool_calls : bool
    ; no_persist : bool
    ; auto_persist : bool
    ; rebuild_session_id : string option
    ; help_short : bool
    ; format : string option
    ; json : bool
    ; dry_run : bool
    ; prompt_preview_max : int
    }

  type action =
    | List_sessions of { format : Handlers.Output_format.t }
    | Session_info of
        { id : string
        ; format : Handlers.Output_format.t
        }
    | Reset_session of
        { id : string
        ; prompt_file : string option
        ; keep_history : bool
        ; dry_run : bool
        ; prompt_preview_max : int
        }
    | Rebuild_from_prompt of
        { id : string
        ; dry_run : bool
        ; prompt_preview_max : int
        }
    | Export_session of
        { id : string
        ; out_file : string
        }
    | Interactive of
        { session_id : string option
        ; new_session : bool
        ; prompt_file : string
        ; export_file : string option
        ; persist_mode : Chat_tui.App.persist_mode
        ; parallel_tool_calls : bool
        }

  type selector =
    | Sel_list_sessions
    | Sel_session_info of string
    | Sel_reset_session of string
    | Sel_export_session of string
    | Sel_rebuild_from_prompt of string

  let selectors t =
    List.filter_opt
      [ (if t.list_sessions then Some Sel_list_sessions else None)
      ; Option.map t.session_info ~f:(fun id -> Sel_session_info id)
      ; Option.map t.reset_session_id ~f:(fun id -> Sel_reset_session id)
      ; Option.map t.export_session_id ~f:(fun id -> Sel_export_session id)
      ; Option.map t.rebuild_session_id ~f:(fun id -> Sel_rebuild_from_prompt id)
      ]
  ;;

  let require_no_session_selection t =
    if Option.is_some t.session_id || t.new_session
    then
      Or_error.error_string
        "Error: --session/--new-session cannot be used with this mode."
    else Ok ()
  ;;

  let parse_format format =
    match String.lowercase format with
    | "human" -> Ok Handlers.Output_format.Human
    | "tsv" -> Ok Handlers.Output_format.Tsv
    | "json" -> Ok Handlers.Output_format.Json
    | _ -> Or_error.errorf "Error: unknown --format %S (expected: human|tsv|json)" format
  ;;

  let list_sessions_format t =
    match t.json, t.format with
    | true, Some f when not (String.Caseless.equal f "json") ->
      Or_error.error_string
        "Error: --json cannot be combined with --format (unless --format json)."
    | true, _ -> Ok Handlers.Output_format.Json
    | false, Some f ->
      let open Or_error.Let_syntax in
      let%map format = parse_format f in
      (match format with
       | Handlers.Output_format.Tsv | Handlers.Output_format.Json -> format
       | Handlers.Output_format.Human -> Handlers.Output_format.Tsv)
    | false, None -> Ok Handlers.Output_format.Tsv
  ;;

  let session_info_format t =
    match t.json, t.format with
    | true, Some f when not (String.Caseless.equal f "json") ->
      Or_error.error_string
        "Error: --json cannot be combined with --format (unless --format json)."
    | true, _ -> Ok Handlers.Output_format.Json
    | false, Some f ->
      let open Or_error.Let_syntax in
      let%map format = parse_format f in
      (match format with
       | Handlers.Output_format.Human
       | Handlers.Output_format.Json
       | Handlers.Output_format.Tsv -> format)
    | false, None -> Ok Handlers.Output_format.Human
  ;;

  let derive_persist_mode t =
    match t.no_persist, t.auto_persist with
    | true, true ->
      Or_error.error_string
        "Error: --no-persist and --auto-persist are mutually exclusive."
    | true, false -> Ok `Never
    | false, true -> Ok `Always
    | false, false -> Ok `Ask
  ;;

  let derive_parallel_tool_calls t =
    match t.parallel_tool_calls, t.no_parallel_tool_calls with
    | true, true ->
      Or_error.error_string
        "Error: --parallel-tool-calls and --no-parallel-tool-calls are mutually \
         exclusive."
    | _, true -> Ok false
    | _, false -> Ok true
  ;;

  let validate_global t =
    if Option.is_some t.session_id && t.new_session
    then
      Or_error.error_string "Error: --session and --new-session are mutually exclusive."
    else if t.parallel_tool_calls && t.no_parallel_tool_calls
    then
      Or_error.error_string
        "Error: --parallel-tool-calls and --no-parallel-tool-calls are mutually \
         exclusive."
    else if t.no_persist && t.auto_persist
    then
      Or_error.error_string
        "Error: --no-persist and --auto-persist are mutually exclusive."
    else if
      t.dry_run
      && Option.is_none t.reset_session_id
      && Option.is_none t.rebuild_session_id
    then
      Or_error.error_string
        "Error: --dry-run can only be used with --reset-session or --rebuild-from-prompt."
    else if t.prompt_preview_max < 0
    then Or_error.error_string "Error: -prompt-preview-max must be >= 0."
    else if t.reset_keep_history && Option.is_none t.reset_session_id
    then
      Or_error.error_string "Error: --keep-history can only be used with --reset-session."
    else Ok ()
  ;;

  let validate_export_file_usage t =
    if
      Option.is_some t.export_file
      && (Option.is_some t.export_session_id
          || t.list_sessions
          || Option.is_some t.session_info
          || Option.is_some t.reset_session_id)
    then
      Or_error.error_string
        "Error: --export-file cannot be combined with --export-session, --list-sessions, \
         --session-info, or --reset-session."
    else Ok ()
  ;;

  let normalize_interactive t =
    let open Or_error.Let_syntax in
    let%bind persist_mode = derive_persist_mode t in
    let%map parallel_tool_calls = derive_parallel_tool_calls t in
    Interactive
      { session_id = t.session_id
      ; new_session = t.new_session
      ; prompt_file = t.conversation_file
      ; export_file = t.export_file
      ; persist_mode
      ; parallel_tool_calls
      }
  ;;

  let normalize_export_session t ~id =
    match t.export_out_file with
    | None ->
      Or_error.error_string "Error: --out must be provided when using --export-session."
    | Some out_file -> Ok (Export_session { id; out_file })
  ;;

  let normalize_selected t sel =
    let open Or_error.Let_syntax in
    let%bind () = require_no_session_selection t in
    match sel with
    | Sel_list_sessions ->
      let%map format = list_sessions_format t in
      List_sessions { format }
    | Sel_session_info id ->
      let%map format = session_info_format t in
      Session_info { id; format }
    | Sel_reset_session id ->
      Ok
        (Reset_session
           { id
           ; prompt_file = t.reset_prompt_file
           ; keep_history = t.reset_keep_history
           ; dry_run = t.dry_run
           ; prompt_preview_max = t.prompt_preview_max
           })
    | Sel_export_session id -> normalize_export_session t ~id
    | Sel_rebuild_from_prompt id ->
      Ok
        (Rebuild_from_prompt
           { id; dry_run = t.dry_run; prompt_preview_max = t.prompt_preview_max })
  ;;

  let normalize_action t =
    let open Or_error.Let_syntax in
    let%bind () = validate_global t in
    let%bind () = validate_export_file_usage t in
    match selectors t with
    | [] -> normalize_interactive t
    | [ sel ] -> normalize_selected t sel
    | _ ->
      Or_error.error_string "Error: multiple session modes selected; choose only one."
  ;;
end

let run_env_action ~env (action : Cli.action) =
  match action with
  | List_sessions { format } -> Handlers.handle_list_sessions ~env ~format
  | Session_info { id; format } -> Handlers.handle_session_info ~env ~id ~format
  | Reset_session { id; prompt_file; keep_history; dry_run; prompt_preview_max } ->
    Handlers.handle_reset_session
      ~env
      ~id
      ~keep_history
      ~prompt_file
      ~dry_run
      ~prompt_preview_max
  | Rebuild_from_prompt { id; dry_run; prompt_preview_max } ->
    Handlers.handle_rebuild_from_prompt ~env ~id ~dry_run ~prompt_preview_max
  | Export_session { id; out_file } ->
    Handlers.handle_export_session ~env ~id ~outfile:out_file
  | Interactive _ -> ()
;;

let run_action (action : Cli.action) =
  match action with
  | Interactive
      { session_id
      ; new_session
      ; prompt_file
      ; export_file
      ; persist_mode
      ; parallel_tool_calls
      } ->
    Handlers.handle_interactive
      ~prompt_file
      ~session_id
      ~new_session
      ~export_file
      ~persist_mode
      ~parallel_tool_calls
  | _ -> Env.with_env (fun env -> run_env_action ~env action)
;;

let run_from_raw (raw : Cli.raw_flags) =
  if raw.help_short
  then (
    print_help_short ();
    Ok ())
  else (
    match Cli.normalize_action raw |> Or_error.tag ~tag:"Invalid flags (try --help)" with
    | Error _ as err -> err
    | Ok action ->
      run_action action;
      Ok ())
;;

let raw_flags_param =
  let open Command.Let_syntax in
  [%map_open
    let conversation_file =
      flag
        "-file"
        (optional_with_default default_prompt_file string)
        ~doc:
          "FILE Prompt file (ChatMarkdown/Markdown) used to seed the interactive \
           session. Also used to derive the default session ID when neither --session \
           nor --new-session is provided. (default: ./prompts/interactive.md)"
    and list_sessions =
      flag
        "--list-sessions"
        no_arg
        ~doc:
          "List known sessions (from $HOME/.ochat/sessions) and exit. Incompatible with \
           other one-shot modes."
    and session_id =
      flag
        "--session"
        (optional string)
        ~doc:
          "NAME Resume session NAME (a directory name under $HOME/.ochat/sessions). \
           Incompatible with --new-session."
    and new_session =
      flag
        "--new-session"
        no_arg
        ~doc:
          "Force creation of a brand-new session (UUID) even if a prompt-derived session \
           already exists. Incompatible with --session."
    and export_session_id =
      flag
        "--export-session"
        (optional string)
        ~doc:
          "NAME Export session NAME to a standalone .chatmd file and exit. Requires \
           --out. Incompatible with other one-shot modes."
    and export_out_file =
      flag
        "--out"
        (optional string)
        ~doc:
          "FILE Output path for --export-session. If FILE exists, you will be prompted \
           before overwriting."
    and export_file =
      flag
        "--export-file"
        (optional string)
        ~doc:
          "FILE After you quit the interactive UI, export the full transcript to FILE in \
           ChatMarkdown format. (interactive mode only)"
    and session_info =
      flag
        "--session-info"
        (optional string)
        ~doc:
          "NAME Display metadata for session NAME (prompt path, timestamps, history \
           length, …) and exit."
    and reset_session_id =
      flag
        "--reset-session"
        (optional string)
        ~doc:
          "NAME Archive the current snapshot and reset session NAME, optionally keeping \
           history (--keep-history) and/or replacing the prompt (--prompt-file)."
    and reset_prompt_file =
      flag
        "--prompt-file"
        (optional string)
        ~doc:
          "FILE When used with --reset-session: set a new prompt file for the reset \
           session."
    and reset_keep_history =
      flag
        "--keep-history"
        no_arg
        ~doc:
          "When used with --reset-session: retain conversation history and cached data \
           instead of clearing them."
    and parallel_tool_calls =
      flag
        "--parallel-tool-calls"
        no_arg
        ~doc:
          "Enable parallel execution of callable tools during interactive runs. \
           (default: enabled)"
    and no_parallel_tool_calls =
      flag
        "--no-parallel-tool-calls"
        no_arg
        ~doc:
          "Disable parallel execution of callable tools during interactive runs (forces \
           sequential evaluation)."
    and no_persist =
      flag
        "--no-persist"
        no_arg
        ~doc:"In interactive mode: never persist the session snapshot on exit (no save)."
    and auto_persist =
      flag
        "--auto-persist"
        no_arg
        ~doc:
          "In interactive mode: always persist the session snapshot on exit without \
           prompting."
    and rebuild_session_id =
      flag
        "--rebuild-from-prompt"
        (optional string)
        ~doc:"NAME Rebuild session NAME from its stored prompt.chatmd copy and exit."
    and help_short =
      flag "--help-short" no_arg ~doc:"Print a short usage summary and exit."
    and format =
      flag
        "--format"
        (optional string)
        ~doc:"FORMAT Output format for --list-sessions / --session-info (human|tsv|json)."
    and json =
      flag
        "--json"
        no_arg
        ~doc:"Alias for --format json (for --list-sessions / --session-info)."
    and dry_run =
      flag
        "--dry-run"
        no_arg
        ~doc:
          "Print what would happen and exit (supported with --reset-session and \
           --rebuild-from-prompt)."
    and prompt_preview_max =
      flag
        "-prompt-preview-max"
        (optional_with_default 2000 int)
        ~doc:"N Max chars of prompt preview for --dry-run (0 = unlimited)."
    in
    ({ conversation_file
     ; list_sessions
     ; session_id
     ; new_session
     ; export_session_id
     ; export_out_file
     ; export_file
     ; session_info
     ; reset_session_id
     ; reset_prompt_file
     ; reset_keep_history
     ; parallel_tool_calls
     ; no_parallel_tool_calls
     ; no_persist
     ; auto_persist
     ; rebuild_session_id
     ; help_short
     ; format
     ; json
     ; dry_run
     ; prompt_preview_max
     }
     : Cli.raw_flags)]
;;

let command =
  let open Command.Let_syntax in
  Command.basic_or_error
    ~summary:
      "Interactive terminal UI for Ochat (with session management and export modes)"
    ~readme
    [%map_open
      let raw = raw_flags_param in
      fun () -> run_from_raw raw]
;;

let normalize_help_argv argv =
  List.map argv ~f:(function
    | "--help" -> "-help"
    | "-h" -> "-help"
    | "--version" -> "-version"
    | "--build-info" -> "-build-info"
    | s -> s)
;;

module Config_file = struct
  type selection =
    | Default
    | Disabled
    | Path of string

  type resolved =
    { path : string
    ; strict : bool
    }

  let default_path () =
    match Sys.getenv "XDG_CONFIG_HOME", Sys.getenv "HOME" with
    | Some dir, _ -> Filename.concat dir "ochat/chat-tui.args"
    | None, Some home -> Filename.concat home ".config/ochat/chat-tui.args"
    | None, None -> Filename.concat "." ".config/ochat/chat-tui.args"
  ;;

  let config_path ~env path =
    let base =
      if Filename.is_absolute path then Eio.Stdenv.fs env else Eio.Stdenv.cwd env
    in
    Eio.Path.(base / path)
  ;;

  let parse_config_file contents =
    String.split_lines contents
    |> List.filter_map ~f:(fun line ->
      let line = String.strip line in
      if String.is_empty line || Char.equal line.[0] '#'
      then None
      else Some (String.split_on_chars line ~on:[ ' '; '\t' ]))
    |> List.concat
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  ;;

  let read_args_from_file ~env ~strict path =
    let path = config_path ~env path in
    match Or_error.try_with (fun () -> Eio.Path.load path) with
    | Ok contents -> Ok (parse_config_file contents)
    | Error err ->
      if strict
      then
        Error
          (Error.tag_arg
             err
             "Cannot read config file"
             (Eio.Path.native_exn path)
             [%sexp_of: string])
      else Ok []
  ;;

  let args ~env t =
    match t with
    | Disabled -> Ok []
    | Default ->
      (match Sys.getenv "OCHAT_CHAT_TUI_CONFIG" with
       | None -> read_args_from_file ~env ~strict:false (default_path ())
       | Some path -> read_args_from_file ~env ~strict:true path)
    | Path path -> read_args_from_file ~env ~strict:true path
  ;;

  let resolve t =
    match t with
    | Disabled -> None
    | Path path -> Some { path; strict = true }
    | Default ->
      (match Sys.getenv "OCHAT_CHAT_TUI_CONFIG" with
       | Some path -> Some { path; strict = true }
       | None -> Some { path = default_path (); strict = false })
  ;;

  let load_contents ~env t =
    match resolve t with
    | None -> Ok None
    | Some { path; _ } ->
      let p = config_path ~env path in
      (match Or_error.try_with (fun () -> Eio.Path.load p) with
       | Ok contents -> Ok (Some contents)
       | Error err -> Error err)
  ;;
end

let strip_config_flags argv =
  match argv with
  | [] -> Config_file.Default, []
  | prog :: rest ->
    let rec loop selection_rev acc = function
      | [] -> selection_rev, List.rev acc
      | "--no-config" :: tl -> loop Config_file.Disabled acc tl
      | ("--config" as flag) :: [] ->
        eprintf "Error: %s requires a file path\n" flag;
        exit 1
      | "--config" :: path :: tl -> loop (Config_file.Path path) acc tl
      | arg :: tl ->
        (match String.chop_prefix arg ~prefix:"--config=" with
         | Some path -> loop (Config_file.Path path) acc tl
         | None -> loop selection_rev (arg :: acc) tl)
    in
    let selection, rest = loop Config_file.Default [] rest in
    selection, prog :: rest
;;

let should_skip_config argv =
  List.exists argv ~f:(function
    | "-help" | "-?" | "-version" | "-build-info" | "--help-short" -> true
    | _ -> false)
;;

let strip_print_effective_args argv =
  let rec loop acc found = function
    | [] -> List.rev acc, found
    | "--print-effective-args" :: tl -> loop acc true tl
    | s :: tl -> loop (s :: acc) found tl
  in
  loop [] false argv
;;

let inject_config_args argv selection =
  match selection with
  | Config_file.Disabled -> argv
  | _ ->
    if should_skip_config argv
    then argv
    else (
      match argv with
      | [] -> []
      | prog :: rest ->
        let config_args =
          match Env.with_env (fun env -> Config_file.args ~env selection) with
          | Ok args -> args
          | Error e ->
            eprintf "%s\n" (Error.to_string_hum e);
            exit 1
        in
        (prog :: config_args) @ rest)
;;

let print_effective_args_and_exit ~selection ~argv ~command_kind ~apply_config =
  let skip_config = should_skip_config argv in
  printf "Command: %s\n" command_kind;
  printf "Skip config: %b\n" skip_config;
  printf "Apply config: %b\n" apply_config;
  (match Config_file.resolve selection with
   | None -> ()
   | Some { path; strict } -> printf "Resolved config path: %s (strict=%b)\n" path strict);
  (match selection with
   | Config_file.Disabled -> printf "Config: disabled\n"
   | Config_file.Default ->
     printf "Config: default";
     (match Sys.getenv "OCHAT_CHAT_TUI_CONFIG" with
      | None -> printf "\n"
      | Some p -> printf " (via OCHAT_CHAT_TUI_CONFIG=%s)\n" p)
   | Config_file.Path p -> printf "Config: --config %s\n" p);
  (match Env.with_env (fun env -> Config_file.load_contents ~env selection) with
   | Ok None -> ()
   | Ok (Some contents) ->
     printf "Config file contents (truncated):\n%s\n" (String.prefix contents 2000)
   | Error e -> printf "Could not load config file: %s\n" (Error.to_string_hum e));
  (match Env.with_env (fun env -> Config_file.args ~env selection) with
   | Ok args ->
     printf "Config args: %s\n" (Sexp.to_string_hum ([%sexp_of: string list] args))
   | Error e -> printf "Config args error: %s\n" (Error.to_string_hum e));
  let effective = if apply_config then inject_config_args argv selection else argv in
  printf "Effective argv: %s\n" (Sexp.to_string_hum ([%sexp_of: string list] effective));
  exit 0
;;

type selected_command =
  | Main of { argv : string list }
  | Sessions of { argv : string list }
  | Ask_ai of { argv : string list }

let select_command argv =
  match argv with
  | prog :: "sessions" :: rest -> Sessions { argv = (prog ^ " sessions") :: rest }
  | prog :: "ask-ai" :: rest -> Ask_ai { argv = (prog ^ " ask-ai") :: rest }
  | _ -> Main { argv }
;;

let () =
  let argv = normalize_help_argv (Sys.get_argv () |> Array.to_list) in
  let config_selection, argv = strip_config_flags argv in
  let argv, print_effective = strip_print_effective_args argv in
  match select_command argv with
  | Sessions { argv } ->
    if print_effective
    then
      print_effective_args_and_exit
        ~selection:config_selection
        ~argv
        ~command_kind:"sessions"
        ~apply_config:false
    else Command_unix.run ~argv Sessions_command.command
  | Ask_ai { argv } -> Command_unix.run ~argv Ask_ai_command.command
  | Main { argv } ->
    if print_effective
    then
      print_effective_args_and_exit
        ~selection:config_selection
        ~argv
        ~command_kind:"main"
        ~apply_config:true
    else (
      let argv = inject_config_args argv config_selection in
      Command_unix.run ~argv command)
;;
