(** Terminal Ochat user-interface.

    This module provides the implementation of the public executable
    [`chat-tui`].  The binary is a very thin wrapper: it parses a
    single optional flag and then delegates to
    {!Chat_tui.App.run_chat}, which launches the full interactive
    interface.

    {1 Command-line usage}

    {v
      chat-tui [-file FILE]
    v}

    • [`-file FILE`] – path to a *markdown* / *.chatmd* document that
      seeds the conversation buffer, declares function-callable tools
      and stores default settings.  Defaults to
      {!val:default_prompt_file}.
*)

open Core

let default_prompt_file = "./prompts/interactive.md"

(** [run ~prompt_file ()] starts an interactive chat session.

    The function blocks until the user quits the TUI or an unrecoverable
    error occurs.

    @param prompt_file Path of the document used to initialise the
      session. *)
let run ~prompt_file () =
  Io.run_main (fun env -> Chat_tui.App.run_chat ~env ~prompt_file ())
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
        in
        fun () -> run ~prompt_file:conversation_file ()]
  in
  Command_unix.run command
;;
