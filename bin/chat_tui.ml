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
               [--textmate-grammar FILE]...
               [--authorize-shell-manifest]
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
      – [--textmate-grammar FILE] · load an additional TextMate grammar before
                                    starting the TUI; may be repeated.
      – [--authorize-shell-manifest] · authorize the exact canonical shell
        manifest compiled from the prompt for this process only.
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
    • Custom TextMate grammars are discovered from repeated
      --textmate-grammar flags, OCHAT_GRAMMAR_DIR, and
      $XDG_CONFIG_HOME/ochat/grammars (or ~/.config/ochat/grammars).
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
    [--textmate-grammar FILE]  . Load an additional TextMate grammar before
                                 starting the TUI. May be repeated.
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
    Eio.Switch.run
    @@ fun sw ->
    let response =
      post_response
        Default
        ~max_output_tokens:100000
        ~temperature:0.3
        ~model:(Request.Unknown "gpt-5.2")
        ~dir
        net
        ~sw
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
      - --authorize-shell-manifest only applies to interactive mode and grants
        one-process authorization to the exact compiled manifest.
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
           [--authorize-shell-manifest]

Run with --help for full flag documentation.
|}
;;

let print_help_short () = printf "%s\n" help_short_text

let load_session ~env ~prompt_file ?id ~new_session () =
  Session_store.load_or_create ~env ~prompt_file ?id ~new_session ()
;;

let run_in_env
      ~typeahead_config
      ~env
      ~prompt_file
      ?session_id
      ~new_session
      ?export_file
      ~persist_mode
      ~parallel_tool_calls
      ~textmate_grammar_files
      ~authorize_shell_manifest
      ()
  =
  let session = load_session ~env ~prompt_file ?id:session_id ~new_session () in
  let shell_manifest_authorizer =
    if authorize_shell_manifest
    then Shell_runtime.Manifest_authorizer.assume_authorized
    else Shell_runtime.Manifest_authorizer.deny
  in
  Chat_tui.App.run_chat
    ~typeahead_config
    ~env
    ~prompt_file
    ~session
    ?export_file
    ~persist_mode
    ~parallel_tool_calls
    ~textmate_grammar_files
    ~shell_manifest_authorizer
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
      ?(typeahead_config = Chat_tui.Type_ahead_config.default)
      ?session_id
      ?(new_session = false)
      ?export_file
      ?(persist_mode : Chat_tui.App.persist_mode = `Ask)
      ?(parallel_tool_calls = true)
      ?(textmate_grammar_files = [])
      ?(authorize_shell_manifest = false)
      ~prompt_file
      ()
  =
  Env.with_env (fun env ->
    run_in_env
      ~typeahead_config
      ~env
      ~prompt_file
      ?session_id
      ~new_session
      ?export_file
      ~persist_mode
      ~parallel_tool_calls
      ~textmate_grammar_files
      ~authorize_shell_manifest
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

  let read_existing_or_exit ~env ~id =
    match Session_store.read_existing ~env ~id with
    | Some session -> session
    | None ->
      eprintf "Error: session '%s' could not be read.\n" id;
      exit 1
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
    let session = read_existing_or_exit ~env ~id in
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
    type prompt_source =
      | Local_copy of string
      | Prompt_file of string

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

    let read_with_lock ~env ~id ~lock_file =
      protectx
        ~finally:(fun () ->
          try Eio.Path.unlink lock_file with
          | _ -> ())
        ()
        ~f:(fun () -> read_existing_or_exit ~env ~id)
    ;;

    let mkdirs_if_missing dir =
      match Eio.Path.is_directory dir with
      | true -> ()
      | false -> Eio.Path.mkdirs ~perm:0o700 dir
    ;;

    let confirm_overwrite ~env ~dest_path ~outfile =
      if not (Eio.Path.is_file dest_path)
      then true
      else (
        Eio.Flow.copy_string
          (Printf.sprintf "File %s exists. Overwrite? [y/N] " outfile)
          (Eio.Stdenv.stdout env);
        let input = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~max_size:1_024 in
        match Eio.Buf_read.line input with
        | ans
          when List.mem
                 [ "y"; "yes" ]
                 (String.lowercase (String.strip ans))
                 ~equal:String.equal -> true
        | exception End_of_file ->
          Eio.Flow.copy_string "Aborted.\n" (Eio.Stdenv.stdout env);
          false
        | _ ->
          Eio.Flow.copy_string "Aborted.\n" (Eio.Stdenv.stdout env);
          false)
    ;;

    let prompt_source (session : Session.t) =
      match session.local_prompt_copy with
      | Some filename -> Local_copy filename
      | None -> Prompt_file session.prompt_file
    ;;

    let prompt_contents ~env ~fs ~session_dir = function
      | Local_copy filename ->
        let path = Eio.Path.(session_dir / filename) in
        Option.value (Option.try_with (fun () -> Eio.Path.load path)) ~default:""
      | Prompt_file prompt_file ->
        let dir_for_prompt =
          if Filename.is_absolute prompt_file then fs else Eio.Stdenv.cwd env
        in
        Option.value
          (Option.try_with (fun () -> Io.load_doc ~dir:dir_for_prompt prompt_file))
          ~default:""
    ;;

    let prompt_dir ~env ~fs ~session_dir = function
      | Local_copy filename -> Eio.Path.(session_dir / Filename.dirname filename)
      | Prompt_file prompt_file ->
        let base_dir =
          if Filename.is_absolute prompt_file then fs else Eio.Stdenv.cwd env
        in
        Eio.Path.(base_dir / Filename.dirname prompt_file)
    ;;

    let initial_msg_count ~env ~prompt_dir ~prompt_xml =
      try
        let cache = Chat_response.Cache.create ~max_size:16 () in
        let ctx =
          Chat_response.Ctx.create
            ~env
            ~dir:prompt_dir
            ~tool_dir:(Eio.Stdenv.cwd env)
            ~cache
        in
        let elements =
          Prompt.Chat_markdown.parse_chat_inputs ~dir:prompt_dir prompt_xml
        in
        Chat_response.Converter.to_items
          ~ctx
          ~run_agent:(fun ?prompt_dir ?session_id ~ctx prompt items ->
            Chat_response.Driver.run_agent
              ~history_compaction:false
              ?prompt_dir
              ?session_id
              ~ctx
              prompt
              items)
          elements
        |> List.length
      with
      | exn ->
        eprintf
          "Warning: failed to compute prompt-derived history prefix for export: %s\n"
          (Exn.to_string exn);
        0
    ;;

    let persist_full_history
          ~cwd
          ~prompt_file
          ~initial_msg_count
          ~(moderator_snapshot : Session.Moderator_snapshot.t option)
          ~history
      =
      let checkpoint =
        List.take history initial_msg_count |> Chat_tui.Persistence.Checkpoint.of_entries
      in
      Chat_tui.Persistence.persist_entries
        ~dir:cwd
        ~prompt_file
        ~checkpoint
        ~moderator_snapshot
        ~history
    ;;

    let read_session ~env ~id =
      let sdir = Session_store.path ~env id in
      let snapshot = Eio.Path.(sdir / "snapshot.bin") in
      require_snapshot ~id snapshot;
      let lock_file = Eio.Path.(sdir / "snapshot.bin.lock") in
      acquire_lock_or_exit ~id ~lock_file;
      let session = read_with_lock ~env ~id ~lock_file in
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

    let write_prompt_file ~env ~fs ~session_dir ~source ~dest_path =
      let prompt_contents = prompt_contents ~env ~fs ~session_dir source in
      Eio.Path.save ~create:(`Or_truncate 0o600) dest_path prompt_contents
    ;;

    let copy_attachments ~env ~fs ~source ~session_dir ~datadir =
      let prompt_dir = prompt_dir ~env ~fs ~session_dir source in
      Chat_tui.Attachments.copy_all
        ~prompt_dir
        ~cwd:(Eio.Stdenv.cwd env)
        ~session_dir
        ~dst:datadir
    ;;

    let handle ~env ~id ~outfile =
      let sdir, session = read_session ~env ~id in
      let out_dir, dest_path, file_name, fs = export_paths ~env ~outfile in
      let proceed = confirm_overwrite ~env ~dest_path ~outfile in
      if proceed
      then (
        let source = prompt_source session in
        let prompt_xml = prompt_contents ~env ~fs ~session_dir:sdir source in
        let prompt_dir = prompt_dir ~env ~fs ~session_dir:sdir source in
        let initial_msg_count = initial_msg_count ~env ~prompt_dir ~prompt_xml in
        let cwd = out_dir in
        let datadir = Io.ensure_chatmd_dir ~cwd in
        write_prompt_file ~env ~fs ~session_dir:sdir ~source ~dest_path;
        copy_attachments ~env ~fs ~source ~session_dir:sdir ~datadir;
        persist_full_history
          ~cwd
          ~prompt_file:file_name
          ~initial_msg_count
          ~moderator_snapshot:session.moderator_state.legacy_snapshot
          ~history:session.history;
        printf "Session '%s' exported to %s\n" id outfile)
    ;;
  end

  let handle_export_session ~env ~id ~outfile = Export_session.handle ~env ~id ~outfile

  let timestamp_for_archive ~env =
    let timestamp =
      Eio.Time.now (Eio.Stdenv.clock env)
      |> Time_ns.Span.of_sec
      |> Time_ns.of_span_since_epoch
      |> Agent_protocol.Timestamp.of_time_ns
      |> Agent_protocol.Timestamp.to_string
      |> String.filter ~f:Char.is_digit
    in
    sprintf "%s-%s" (String.prefix timestamp 8) (String.sub timestamp ~pos:8 ~len:4)
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

  let print_history_preview
        ~prompt_preview_max
        ~(moderator_snapshot : Session.Moderator_snapshot.t option)
        history
    =
    printf "History items (kept): %d\n" (List.length history);
    print_prompt_preview
      ~prompt_preview_max
      ~label:"History preview (as chatmd)"
      (Chat_tui.Persistence.history_entries_as_chatmd ~moderator_snapshot ~history)
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
    let session = read_existing_or_exit ~env ~id in
    let archive_dir = Eio.Path.(dir / "archive") in
    let archived_snapshot =
      Eio.Path.(
        archive_dir / Printf.sprintf "%s.snapshot.bin" (timestamp_for_archive ~env))
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
    | true ->
      print_history_preview
        ~prompt_preview_max
        ~moderator_snapshot:session.moderator_state.legacy_snapshot
        session.history
  ;;

  let print_rebuild_plan ~env ~id ~prompt_preview_max =
    let dir = Session_store.path ~env id in
    let snapshot = Eio.Path.(dir / "snapshot.bin") in
    require_snapshot ~id snapshot;
    let session = read_existing_or_exit ~env ~id in
    let archive_dir = Eio.Path.(dir / "archive") in
    let archived_snapshot =
      Eio.Path.(
        archive_dir / Printf.sprintf "%s.snapshot.bin" (timestamp_for_archive ~env))
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
        ~typeahead_config
        ~prompt_file
        ~session_id
        ~new_session
        ~export_file
        ~persist_mode
        ~parallel_tool_calls
        ~textmate_grammar_files
        ~authorize_shell_manifest
    =
    run
      ~typeahead_config
      ?session_id
      ~new_session
      ?export_file
      ~persist_mode
      ~parallel_tool_calls
      ~textmate_grammar_files
      ~authorize_shell_manifest
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
    { typeahead_config : Chat_tui.Type_ahead_config.t Or_error.t
    ; conversation_file : string
    ; local : bool
    ; authoring_package_files : string list
    ; authoring_options : Agent_server.Authoring_options.t
    ; connect : string option
    ; bearer_token_file : string option
    ; list_sessions : bool
    ; session_id : string option
    ; new_session : bool
    ; new_daemon_session : bool
    ; daemon_prompt : string option
    ; workspace : string option
    ; detached : bool
    ; owner_bound : bool
    ; read_only : bool
    ; disconnect_grace_ms : int
    ; export_session_id : string option
    ; export_out_file : string option
    ; export_file : string option
    ; session_info : string option
    ; start_session_id : string option
    ; stop_session_id : string option
    ; stop_cancel : bool
    ; delete_session_id : string option
    ; delete_archive : bool
    ; reset_session_id : string option
    ; reset_prompt_file : string option
    ; reset_keep_history : bool
    ; parallel_tool_calls : bool
    ; no_parallel_tool_calls : bool
    ; textmate_grammar_files : string list
    ; authorize_shell_manifest : bool
    ; no_persist : bool
    ; auto_persist : bool
    ; rebuild_session_id : string option
    ; help_short : bool
    ; format : string option
    ; json : bool
    ; dry_run : bool
    ; prompt_preview_max : int
    }

  type daemon_target =
    | Attach of { session_id : string }
    | Create of
        { prompt : string
        ; workspace : string
        ; liveness : Agent_protocol.Session.liveness
        }

  type daemon_admin =
    | List of { format : Handlers.Output_format.t }
    | Info of
        { id : string
        ; format : Handlers.Output_format.t
        }
    | Reset of
        { id : string
        ; keep_history : bool
        }
    | Rebuild of { id : string }
    | Start of { id : string }
    | Stop of
        { id : string
        ; mode : Agent_protocol.Session.stop_mode
        }
    | Delete of
        { id : string
        ; policy : Agent_protocol.Session.Delete_request.policy
        }
    | Export of
        { id : string
        ; out_file : string
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
        ; textmate_grammar_files : string list
        ; authorize_shell_manifest : bool
        }
    | Daemon_interactive of
        { connect : string
        ; bearer_token_file : string option
        ; target : daemon_target
        ; mode : Agent_protocol.Session.attachment_mode
        ; textmate_grammar_files : string list
        }
    | Daemon_admin of
        { connect : string
        ; bearer_token_file : string option
        ; command : daemon_admin
        }
    | Embedded_interactive of
        { prompt_file : string
        ; authoring_package_files : string list
        ; authoring_budget : Chat_response.Authoring_validation.context_budget option
        ; textmate_grammar_files : string list
        }

  type selector =
    | Sel_list_sessions
    | Sel_session_info of string
    | Sel_start_session of string
    | Sel_stop_session of string
    | Sel_delete_session of string
    | Sel_reset_session of string
    | Sel_export_session of string
    | Sel_rebuild_from_prompt of string

  let selectors t =
    List.filter_opt
      [ (if t.list_sessions then Some Sel_list_sessions else None)
      ; Option.map t.session_info ~f:(fun id -> Sel_session_info id)
      ; Option.map t.start_session_id ~f:(fun id -> Sel_start_session id)
      ; Option.map t.stop_session_id ~f:(fun id -> Sel_stop_session id)
      ; Option.map t.delete_session_id ~f:(fun id -> Sel_delete_session id)
      ; Option.map t.reset_session_id ~f:(fun id -> Sel_reset_session id)
      ; Option.map t.export_session_id ~f:(fun id -> Sel_export_session id)
      ; Option.map t.rebuild_session_id ~f:(fun id -> Sel_rebuild_from_prompt id)
      ]
  ;;

  let require_no_local_session_selection t =
    if Option.is_some t.session_id || t.new_session || t.new_daemon_session
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
    else if t.authorize_shell_manifest && not (List.is_empty (selectors t))
    then
      Or_error.error_string
        "Error: --authorize-shell-manifest can only be used in interactive mode."
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
    else if t.stop_cancel && Option.is_none t.stop_session_id
    then Or_error.error_string "Error: --cancel can only be used with --stop-session."
    else if t.delete_archive && Option.is_none t.delete_session_id
    then Or_error.error_string "Error: --archive can only be used with --delete-session."
    else if Option.is_some t.bearer_token_file && Option.is_none t.connect
    then Or_error.error_string "Error: --bearer-token-file requires --connect."
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

  let rec normalize_interactive t =
    match t.connect with
    | Some connect -> normalize_daemon_interactive t ~connect
    | None -> normalize_local_interactive t

  and normalize_local_interactive t =
    let open Or_error.Let_syntax in
    if
      t.new_daemon_session
      || Option.is_some t.daemon_prompt
      || Option.is_some t.workspace
      || t.detached
      || t.owner_bound
      || t.read_only
    then Or_error.error_string "Error: daemon session flags require --connect."
    else (
      let legacy_requested =
        Option.is_some t.session_id
        || t.new_session
        || Option.is_some t.export_file
        || t.no_persist
        || t.auto_persist
        || t.parallel_tool_calls
        || t.no_parallel_tool_calls
        || t.authorize_shell_manifest
      in
      if t.local && legacy_requested
      then
        Or_error.error_string
          "Error: legacy session/export/runtime flags are not supported with explicit \
           --local."
      else if t.local || not legacy_requested
      then (
        let%map authoring_budget =
          Agent_server.Authoring_options.resolve t.authoring_options
        in
        Embedded_interactive
          { prompt_file = t.conversation_file
          ; authoring_package_files = t.authoring_package_files
          ; authoring_budget
          ; textmate_grammar_files = t.textmate_grammar_files
          })
      else (
        let%bind persist_mode = derive_persist_mode t in
        let%map parallel_tool_calls = derive_parallel_tool_calls t in
        Interactive
          { session_id = t.session_id
          ; new_session = t.new_session
          ; prompt_file = t.conversation_file
          ; export_file = t.export_file
          ; persist_mode
          ; parallel_tool_calls
          ; textmate_grammar_files = t.textmate_grammar_files
          ; authorize_shell_manifest = t.authorize_shell_manifest
          }))

  and normalize_daemon_interactive t ~connect =
    let open Or_error.Let_syntax in
    let%bind () =
      if t.local
      then Or_error.error_string "Error: --local and --connect are mutually exclusive."
      else if t.new_session
      then Or_error.error_string "Error: use --new-daemon-session with --connect."
      else if Option.is_some t.export_file || t.no_persist || t.auto_persist
      then
        Or_error.error_string
          "Error: local export/persistence flags cannot be used with --connect."
      else if
        t.parallel_tool_calls || t.no_parallel_tool_calls || t.authorize_shell_manifest
      then
        Or_error.error_string "Error: local runtime flags cannot be used with --connect."
      else if t.detached && t.owner_bound
      then
        Or_error.error_string
          "Error: --detached and --owner-bound are mutually exclusive."
      else if t.read_only && t.owner_bound
      then Or_error.error_string "Error: an owner-bound attachment cannot be read-only."
      else if t.disconnect_grace_ms < 0
      then Or_error.error_string "Error: --disconnect-grace-ms must be nonnegative."
      else Ok ()
    in
    let mode =
      if t.read_only
      then Agent_protocol.Session.Read_only
      else if t.owner_bound
      then Owner_read_write
      else Read_write
    in
    let%map target = daemon_target t in
    Daemon_interactive
      { connect
      ; bearer_token_file = t.bearer_token_file
      ; target
      ; mode
      ; textmate_grammar_files = t.textmate_grammar_files
      }

  and daemon_target t =
    match t.session_id, t.new_daemon_session with
    | Some session_id, false ->
      if Option.is_some t.daemon_prompt || Option.is_some t.workspace || t.detached
      then
        Or_error.error_string
          "Error: creation flags cannot be used when attaching --session."
      else Ok (Attach { session_id })
    | None, true ->
      (match t.daemon_prompt, t.workspace with
       | Some prompt, Some workspace ->
         let liveness =
           if t.owner_bound
           then
             Agent_protocol.Session.Owner_bound
               { disconnect_grace_ms = t.disconnect_grace_ms; stop_mode = Graceful }
           else Detached
         in
         Ok (Create { prompt; workspace; liveness })
       | _ ->
         Or_error.error_string
           "Error: --new-daemon-session requires --prompt and --workspace.")
    | Some _, true ->
      Or_error.error_string
        "Error: --session and --new-daemon-session are mutually exclusive."
    | None, false ->
      Or_error.error_string "Error: --connect requires --session or --new-daemon-session."
  ;;

  let normalize_export_session t ~id =
    match t.export_out_file with
    | None ->
      Or_error.error_string "Error: --out must be provided when using --export-session."
    | Some out_file -> Ok (Export_session { id; out_file })
  ;;

  let normalize_local_selected t sel =
    let open Or_error.Let_syntax in
    let%bind () = require_no_local_session_selection t in
    match sel with
    | Sel_list_sessions ->
      let%map format = list_sessions_format t in
      List_sessions { format }
    | Sel_session_info id ->
      let%map format = session_info_format t in
      Session_info { id; format }
    | Sel_start_session _ | Sel_stop_session _ | Sel_delete_session _ ->
      Or_error.error_string
        "Error: --start-session, --stop-session, and --delete-session require --connect."
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

  let validate_daemon_selected t =
    if t.local
    then Or_error.error_string "Error: --local and --connect are mutually exclusive."
    else if
      Option.is_some t.session_id
      || t.new_session
      || t.new_daemon_session
      || Option.is_some t.daemon_prompt
      || Option.is_some t.workspace
      || t.detached
      || t.owner_bound
      || t.read_only
    then Or_error.error_string "Error: interactive daemon flags cannot be used here."
    else if t.dry_run
    then
      Or_error.error_string "Error: --dry-run is not supported for daemon administration."
    else if Option.is_some t.reset_prompt_file
    then
      Or_error.error_string
        "Error: connected reset cannot replace the prompt; use prompt upgrade/rebuild."
    else Ok ()
  ;;

  let normalize_daemon_selected t ~connect sel =
    let open Or_error.Let_syntax in
    let%bind () = validate_daemon_selected t in
    let%map command =
      match sel with
      | Sel_list_sessions ->
        let%map format = list_sessions_format t in
        List { format }
      | Sel_session_info id ->
        let%map format = session_info_format t in
        Info { id; format }
      | Sel_start_session id -> Ok (Start { id })
      | Sel_stop_session id ->
        Ok (Stop { id; mode = (if t.stop_cancel then Cancel else Graceful) })
      | Sel_delete_session id ->
        Ok (Delete { id; policy = (if t.delete_archive then Archive else Remove) })
      | Sel_reset_session id -> Ok (Reset { id; keep_history = t.reset_keep_history })
      | Sel_rebuild_from_prompt id -> Ok (Rebuild { id })
      | Sel_export_session id ->
        (match t.export_out_file with
         | None ->
           Or_error.error_string
             "Error: --out must be provided when using --export-session."
         | Some out_file -> Ok (Export { id; out_file }))
    in
    Daemon_admin { connect; bearer_token_file = t.bearer_token_file; command }
  ;;

  let normalize_selected t sel =
    match t.connect with
    | None -> normalize_local_selected t sel
    | Some connect -> normalize_daemon_selected t ~connect sel
  ;;

  let normalize_action t =
    let open Or_error.Let_syntax in
    let%bind () = validate_global t in
    let%bind () = validate_export_file_usage t in
    let%bind action =
      match selectors t with
      | [] -> normalize_interactive t
      | [ sel ] -> normalize_selected t sel
      | _ ->
        Or_error.error_string "Error: multiple session modes selected; choose only one."
    in
    let local_authoring =
      (not (List.is_empty t.authoring_package_files))
      || Agent_server.Authoring_options.is_configured t.authoring_options
    in
    match local_authoring, action with
    | false, _ | _, Embedded_interactive _ -> Ok action
    | _ ->
      Or_error.error_string
        "Error: authoring package and budget flags require an embedded local session."
  ;;
end

module Daemon_connection = struct
  let protocol_error error = Error.create_s [%sexp (error : Agent_protocol.Error.t)]

  let endpoint ~env ~connect ~bearer_token_file =
    let open Result.Let_syntax in
    let%bind bearer_token =
      match bearer_token_file with
      | None -> Ok None
      | Some path ->
        Agent_transport_client.Endpoint.load_bearer_token ~env ~path
        |> Result.map ~f:Option.some
    in
    Agent_transport_client.Endpoint.create ~home:(Sys.getenv "HOME") ~bearer_token connect
  ;;

  let with_connection ~env ~connect ~bearer_token_file f =
    let open Or_error.Let_syntax in
    let%bind endpoint =
      endpoint ~env ~connect ~bearer_token_file |> Result.map_error ~f:protocol_error
    in
    Eio.Switch.run
    @@ fun sw ->
    let%bind connection =
      Agent_transport_client.Endpoint.connect
        endpoint
        ~sw
        ~env
        ~notification_capacity:4096
      |> Result.map_error ~f:protocol_error
    in
    Fun.protect
      ~finally:(fun () -> Agent_client.Connection.close connection)
      (fun () -> f sw endpoint connection)
  ;;
end

module Daemon_interactive = struct
  let protocol_error = Daemon_connection.protocol_error

  let attach ~sw ~env ~connection ~reconnect ~mode session_id =
    let open Or_error.Let_syntax in
    let%bind session_id =
      Agent_protocol.Id.Session.of_string session_id |> Result.map_error ~f:protocol_error
    in
    Chat_tui.Agent_session_client.attach
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~connection
      ~reconnect:(Some reconnect)
      ~session_id
      ~mode
      ()
    |> Result.map_error ~f:protocol_error
  ;;

  let create ~sw ~env ~connection ~reconnect ~mode ~prompt ~workspace ~liveness =
    Chat_tui.Agent_session_client.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~connection
      ~reconnect:(Some reconnect)
      { prompt
      ; workspace
      ; liveness
      ; permission_profile = None
      ; display_name = None
      ; labels = []
      ; mode
      }
    |> Result.map_error ~f:protocol_error
  ;;

  let select_client ~sw ~env ~connection ~reconnect ~mode = function
    | Cli.Attach { session_id } -> attach ~sw ~env ~connection ~reconnect ~mode session_id
    | Create { prompt; workspace; liveness } ->
      create ~sw ~env ~connection ~reconnect ~mode ~prompt ~workspace ~liveness
  ;;

  let run
        ~typeahead_config
        ~env
        ~connect
        ~bearer_token_file
        ~target
        ~mode
        ~textmate_grammar_files
    =
    Daemon_connection.with_connection
      ~env
      ~connect
      ~bearer_token_file
      (fun sw endpoint connection ->
         let reconnect () =
           Agent_transport_client.Endpoint.connect
             endpoint
             ~sw
             ~env
             ~notification_capacity:4096
         in
         let open Or_error.Let_syntax in
         let%map client = select_client ~sw ~env ~connection ~reconnect ~mode target in
         Chat_tui.App.run_agent_session
           ~typeahead_config
           ~env
           ~client
           ~textmate_grammar_files
           ())
  ;;
end

module Daemon_admin = struct
  let protocol_error = Daemon_connection.protocol_error
  let write env text = Eio.Flow.copy_string text (Eio.Stdenv.stdout env)
  let state_text sexp_of value = Sexp.to_string_mach (sexp_of value)

  let session_line (session : Agent_protocol.Session.t) =
    String.concat
      ~sep:"\t"
      [ Agent_protocol.Id.Session.to_string session.id
      ; Option.value session.spec.display_name ~default:""
      ; state_text Agent_protocol.Session.sexp_of_desired_state session.desired_state
      ; state_text Agent_protocol.Session.sexp_of_observed_state session.observed_state
      ]
  ;;

  let write_sessions env format sessions =
    match format with
    | Handlers.Output_format.Json ->
      `Array (List.map sessions ~f:Agent_protocol.Session.to_json)
      |> Jsonaf.to_string
      |> fun json -> write env (json ^ "\n")
    | Tsv | Human ->
      List.iter sessions ~f:(fun session -> write env (session_line session ^ "\n"))
  ;;

  let write_info env format (snapshot : Agent_protocol.Snapshot.t) =
    let session = snapshot.session in
    match format with
    | Handlers.Output_format.Json ->
      write env (Jsonaf.to_string (Agent_protocol.Snapshot.to_json snapshot) ^ "\n")
    | Tsv -> write env (session_line session ^ "\n")
    | Human ->
      write
        env
        (sprintf
           "Session: %s\nDesired: %s\nObserved: %s\nRevision: %Ld\nEvent sequence: %Ld\n"
           (Agent_protocol.Id.Session.to_string session.id)
           (state_text Agent_protocol.Session.sexp_of_desired_state session.desired_state)
           (state_text
              Agent_protocol.Session.sexp_of_observed_state
              session.observed_state)
           snapshot.revision
           snapshot.latest_event_sequence)
  ;;

  let session_id value =
    Agent_protocol.Id.Session.of_string value |> Result.map_error ~f:protocol_error
  ;;

  let initialize connection =
    Agent_client.Session_handle.initialize
      connection
      ~implementation_name:"chat-tui-admin"
      ~implementation_version:"dev"
    |> Result.map_error ~f:protocol_error
  ;;

  let with_handle ~env ~sw ~connection id f =
    let open Or_error.Let_syntax in
    let%bind session_id = session_id id in
    let%bind handle =
      Agent_client.Session_handle.attach
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~connection
        ~session_id
        ~mode:Read_write
        ~subscribe:false
        ()
      |> Result.map_error ~f:protocol_error
    in
    let snapshot =
      Agent_client.Session_handle.projection handle |> Agent_client.Projection.snapshot
    in
    Fun.protect
      ~finally:(fun () -> Agent_client.Session_handle.close handle)
      (fun () -> f handle snapshot)
  ;;

  let reset ~env ~sw ~connection id ~keep_history =
    with_handle ~env ~sw ~connection id (fun handle snapshot ->
      Agent_client.Session_handle.reset
        handle
        ~expected_revision:snapshot.revision
        ~keep_history
        ~keep_tasks:false
        ~keep_cache:false
        ~keep_workspace:true
        ~keep_grants:false
        ~keep_labels:true
      |> Result.map_error ~f:protocol_error
      |> Or_error.map ~f:(fun session -> write env (session_line session ^ "\n")))
  ;;

  let rebuild ~env ~sw ~connection id =
    with_handle ~env ~sw ~connection id (fun handle snapshot ->
      Agent_client.Session_handle.rebuild
        handle
        ~expected_revision:snapshot.revision
        ~prompt_choice:Pinned
      |> Result.map_error ~f:protocol_error
      |> Or_error.map ~f:(fun session -> write env (session_line session ^ "\n")))
  ;;

  let start ~env ~sw ~connection id =
    with_handle ~env ~sw ~connection id (fun handle _snapshot ->
      Agent_client.Session_handle.start handle ~queue_if_limited:true
      |> Result.map_error ~f:protocol_error
      |> Or_error.map ~f:(fun session -> write env (session_line session ^ "\n")))
  ;;

  let stop ~env ~sw ~connection id ~mode =
    with_handle ~env ~sw ~connection id (fun handle _snapshot ->
      Agent_client.Session_handle.stop handle ~mode
      |> Result.map_error ~f:protocol_error
      |> Or_error.map ~f:(fun session -> write env (session_line session ^ "\n")))
  ;;

  let delete ~env ~sw ~connection id ~policy =
    with_handle ~env ~sw ~connection id (fun handle snapshot ->
      Agent_client.Session_handle.delete
        handle
        ~expected_revision:snapshot.revision
        ~policy
        ~confirmation:id
      |> Result.map_error ~f:protocol_error
      |> Or_error.map ~f:(fun receipt ->
        write
          env
          (sprintf
             "Deleted session %s at %s\n"
             (Agent_protocol.Id.Session.to_string receipt.session_id)
             (Agent_protocol.Timestamp.to_string receipt.deleted_at))))
  ;;

  let export_format out_file =
    let _, extension = Filename.split_extension out_file in
    if Option.exists extension ~f:(String.Caseless.equal ".json")
    then Agent_protocol.Session.Export_request.Json
    else Chatmd
  ;;

  let output_path env out_file =
    let base =
      if Filename.is_absolute out_file then Eio.Stdenv.fs env else Eio.Stdenv.cwd env
    in
    Eio.Path.(base / out_file)
  ;;

  let confirm_overwrite ~env ~path ~out_file =
    if not (Eio.Path.is_file path)
    then true
    else (
      write env (sprintf "File %s exists. Overwrite? [y/N] " out_file);
      let input = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~max_size:1_024 in
      match Eio.Buf_read.line input with
      | answer ->
        List.mem
          [ "y"; "yes" ]
          (String.lowercase (String.strip answer))
          ~equal:String.equal
      | exception End_of_file -> false)
  ;;

  let download_export ~env ~handle ~blob ~out_file =
    let path = output_path env out_file in
    if not (confirm_overwrite ~env ~path ~out_file)
    then (
      write env "Aborted.\n";
      Ok ())
    else
      let open Or_error.Let_syntax in
      let%map () =
        Agent_client.Blob_download.install_atomic ~path ~download:(fun output ->
          Agent_client.Session_handle.download_blob handle ~blob ~output)
      in
      write env (sprintf "Session export written to %s\n" out_file)
  ;;

  let export ~env ~sw ~connection id ~out_file =
    with_handle ~env ~sw ~connection id (fun handle _snapshot ->
      let open Or_error.Let_syntax in
      let%bind export =
        Agent_client.Session_handle.export
          handle
          ~format:(export_format out_file)
          ~revision:None
        |> Result.map_error ~f:protocol_error
      in
      download_export ~env ~handle ~blob:export.blob ~out_file)
  ;;

  let run_command ~env ~sw ~connection = function
    | Cli.List { format } ->
      Agent_client.Admin.list_sessions connection
      |> Result.map_error ~f:protocol_error
      |> Or_error.map ~f:(write_sessions env format)
    | Info { id; format } ->
      let open Or_error.Let_syntax in
      let%bind session_id = session_id id in
      let%map snapshot =
        Agent_client.Admin.get_session connection session_id
        |> Result.map_error ~f:protocol_error
      in
      write_info env format snapshot
    | Reset { id; keep_history } -> reset ~env ~sw ~connection id ~keep_history
    | Rebuild { id } -> rebuild ~env ~sw ~connection id
    | Start { id } -> start ~env ~sw ~connection id
    | Stop { id; mode } -> stop ~env ~sw ~connection id ~mode
    | Delete { id; policy } -> delete ~env ~sw ~connection id ~policy
    | Export { id; out_file } -> export ~env ~sw ~connection id ~out_file
  ;;

  let run ~env ~connect ~bearer_token_file ~command =
    Daemon_connection.with_connection
      ~env
      ~connect
      ~bearer_token_file
      (fun sw _endpoint connection ->
         let open Or_error.Let_syntax in
         let%bind _ = initialize connection in
         run_command ~env ~sw ~connection command)
  ;;
end

module Embedded_interactive = struct
  let protocol_error error = Error.create_s [%sexp (error : Agent_protocol.Error.t)]

  let working_directory env =
    let native = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
    if Filename.is_absolute native then native else Eio_posix.Low_level.realpath native
  ;;

  let absolute_path ~cwd path =
    if Filename.is_absolute path then path else Filename.concat cwd path
  ;;

  let run
        ~typeahead_config
        ~env
        ~prompt_file
        ~textmate_grammar_files
        ~authoring_package_files
        ~authoring_budget
    =
    Eio.Switch.run
    @@ fun sw ->
    let workspace = working_directory env in
    let home = Sys.getenv "HOME" |> Option.value ~default:workspace in
    let options =
      Agent_server.Embedded.
        { prompt_file = absolute_path ~cwd:workspace prompt_file
        ; workspace
        ; tool_dir = workspace
        ; home
        ; data_root = None
        ; start_immediately = true
        ; permission_profile = default_permission_profile
        ; attachment_mode = Agent_protocol.Session.Read_write
        ; event_capacity = 4096
        }
    in
    let authoring_package_files =
      List.map authoring_package_files ~f:(absolute_path ~cwd:workspace)
    in
    Agent_server.Embedded.start
      ~sw
      ~env
      ~authoring_package_files
      ?authoring_budget
      options
    |> Result.map_error ~f:protocol_error
    |> Or_error.bind ~f:(fun host ->
      Fun.protect
        ~finally:(fun () -> Agent_server.Embedded.close host)
        (fun () ->
           let connection = Agent_server.Embedded.connect host in
           Fun.protect
             ~finally:(fun () -> Agent_client.Connection.close connection)
             (fun () ->
                Chat_tui.Agent_session_client.attach
                  ~sw
                  ~clock:(Eio.Stdenv.clock env)
                  ~connection
                  ~session_id:(Agent_server.Embedded.session_id host)
                  ~mode:Agent_protocol.Session.Read_write
                  ()
                |> Result.map_error ~f:protocol_error
                |> Or_error.map ~f:(fun client ->
                  Chat_tui.App.run_agent_session
                    ~typeahead_config
                    ~env
                    ~client
                    ~textmate_grammar_files
                    ()))))
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
  | Interactive _ | Daemon_interactive _ | Daemon_admin _ | Embedded_interactive _ -> ()
;;

let run_action ~typeahead_config (action : Cli.action) =
  match action with
  | Interactive
      { session_id
      ; new_session
      ; prompt_file
      ; export_file
      ; persist_mode
      ; parallel_tool_calls
      ; textmate_grammar_files
      ; authorize_shell_manifest
      } ->
    Handlers.handle_interactive
      ~typeahead_config
      ~prompt_file
      ~session_id
      ~new_session
      ~export_file
      ~persist_mode
      ~parallel_tool_calls
      ~textmate_grammar_files
      ~authorize_shell_manifest;
    Ok ()
  | Daemon_interactive
      { connect; bearer_token_file; target; mode; textmate_grammar_files } ->
    Env.with_env (fun env ->
      Daemon_interactive.run
        ~typeahead_config
        ~env
        ~connect
        ~bearer_token_file
        ~target
        ~mode
        ~textmate_grammar_files)
  | Daemon_admin { connect; bearer_token_file; command } ->
    Env.with_env (fun env -> Daemon_admin.run ~env ~connect ~bearer_token_file ~command)
  | Embedded_interactive
      { prompt_file; textmate_grammar_files; authoring_package_files; authoring_budget }
    ->
    Env.with_env (fun env ->
      Embedded_interactive.run
        ~typeahead_config
        ~env
        ~prompt_file
        ~textmate_grammar_files
        ~authoring_package_files
        ~authoring_budget)
  | _ ->
    Env.with_env (fun env -> run_env_action ~env action);
    Ok ()
;;

let run_from_raw (raw : Cli.raw_flags) =
  if raw.help_short
  then (
    print_help_short ();
    Ok ())
  else
    let open Or_error.Let_syntax in
    let%bind action =
      Cli.normalize_action raw |> Or_error.tag ~tag:"Invalid flags (try --help)"
    in
    let%bind typeahead_config = raw.typeahead_config in
    let%bind () =
      Chat_tui.Type_ahead_config.validate_credentials
        typeahead_config
        ~api_key:(Sys.getenv "OPENAI_API_KEY")
    in
    run_action ~typeahead_config action
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
    and typeahead =
      flag
        "--typeahead"
        (optional_with_default "off" string)
        ~doc:
          "MODE Unsent draft suggestions: off (default), manual, auto; extra provider \
           charges."
    and typeahead_model =
      flag
        "--typeahead-model"
        (optional_with_default "gpt-5.6-luna" string)
        ~doc:"MODEL Local suggestion model, independent of the agent model."
    and typeahead_history =
      flag
        "--typeahead-history-messages"
        (optional_with_default 0 int)
        ~doc:"N Opt in to sending 0–3 visible messages with the draft (default 0)."
    and typeahead_debounce =
      flag
        "--typeahead-debounce-ms"
        (optional_with_default 200 int)
        ~doc:"MS Automatic suggestion debounce, 100–5000 (default 200)."
    and typeahead_tokens =
      flag
        "--typeahead-max-output-tokens"
        (optional_with_default 200 int)
        ~doc:"N Suggestion output limit, 1–512 (default 200); not a spending cap."
    and local =
      flag
        "--local"
        no_arg
        ~doc:"Run an embedded local session instead of connecting to a daemon."
    and authoring_options = Agent_server.Authoring_options.param
    and authoring_package_files =
      flag
        "--authoring-package"
        (listed string)
        ~doc:
          "FILE Capture custom documentation for an embedded local session (repeatable)."
    and connect =
      flag
        "--connect"
        (optional string)
        ~doc:"URI Connect to an Ochat daemon using unix://, http://, or https://."
    and bearer_token_file =
      flag
        "--bearer-token-file"
        (optional string)
        ~doc:"FILE Read the HTTP daemon bearer token from FILE using Eio."
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
    and new_daemon_session =
      flag
        "--new-daemon-session"
        no_arg
        ~doc:"Create a daemon-owned session and attach to it."
    and daemon_prompt =
      flag
        "--prompt"
        (optional string)
        ~doc:"PROMPT_ID Configured prompt name for --new-daemon-session."
    and workspace =
      flag
        "--workspace"
        (optional string)
        ~doc:"WORKSPACE_ID Configured workspace name for --new-daemon-session."
    and detached =
      flag
        "--detached"
        no_arg
        ~doc:"Create a durable daemon session that outlives this TUI."
    and owner_bound =
      flag
        "--owner-bound"
        no_arg
        ~doc:"Create or attach with an owner lease whose loss starts stop grace."
    and read_only = flag "--read-only" no_arg ~doc:"Attach as a read-only observer."
    and disconnect_grace_ms =
      flag
        "--disconnect-grace-ms"
        (optional_with_default 30_000 int)
        ~doc:"MS Owner-bound disconnect grace period."
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
    and start_session_id =
      flag
        "--start-session"
        (optional string)
        ~doc:"ID Start a stopped daemon session. Requires --connect."
    and stop_session_id =
      flag
        "--stop-session"
        (optional string)
        ~doc:"ID Gracefully stop a daemon session. Requires --connect."
    and stop_cancel =
      flag "--cancel" no_arg ~doc:"Cancel active work when used with --stop-session."
    and delete_session_id =
      flag
        "--delete-session"
        (optional string)
        ~doc:"ID Delete a stopped daemon session. Requires --connect."
    and delete_archive =
      flag
        "--archive"
        no_arg
        ~doc:"Archive instead of removing when used with --delete-session."
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
    and textmate_grammar_files =
      flag
        "--textmate-grammar"
        (listed string)
        ~doc:
          "FILE Load an additional TextMate grammar before starting the TUI. May be \
           repeated. Explicit files are loaded before automatically discovered grammars."
    and authorize_shell_manifest =
      flag
        "--authorize-shell-manifest"
        no_arg
        ~doc:
          "Authorize the exact canonical shell manifest compiled from the prompt for \
           this interactive process. Without this flag, shell manifests fail closed."
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
    ({ typeahead_config =
         Chat_tui.Type_ahead_config.create
           ~mode:typeahead
           ~model:typeahead_model
           ~history_messages:typeahead_history
           ~debounce_ms:typeahead_debounce
           ~max_output_tokens:typeahead_tokens
     ; conversation_file
     ; local
     ; authoring_package_files
     ; authoring_options
     ; connect
     ; bearer_token_file
     ; list_sessions
     ; session_id
     ; new_session
     ; new_daemon_session
     ; daemon_prompt
     ; workspace
     ; detached
     ; owner_bound
     ; read_only
     ; disconnect_grace_ms
     ; export_session_id
     ; export_out_file
     ; export_file
     ; session_info
     ; start_session_id
     ; stop_session_id
     ; stop_cancel
     ; delete_session_id
     ; delete_archive
     ; reset_session_id
     ; reset_prompt_file
     ; reset_keep_history
     ; parallel_tool_calls
     ; no_parallel_tool_calls
     ; textmate_grammar_files
     ; authorize_shell_manifest
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
        let scalar_flags =
          [ "--typeahead"
          ; "--typeahead-model"
          ; "--typeahead-history-messages"
          ; "--typeahead-debounce-ms"
          ; "--typeahead-max-output-tokens"
          ]
        in
        let overridden flag =
          List.exists rest ~f:(fun arg ->
            String.equal arg flag || String.is_prefix arg ~prefix:(flag ^ "="))
        in
        let rec filter = function
          | flag :: _value :: tail
            when List.mem scalar_flags flag ~equal:String.equal && overridden flag ->
            filter tail
          | arg :: tail
            when List.exists scalar_flags ~f:(fun flag ->
                   String.is_prefix arg ~prefix:(flag ^ "=") && overridden flag) ->
            filter tail
          | arg :: tail -> arg :: filter tail
          | [] -> []
        in
        (prog :: filter config_args) @ rest)
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
