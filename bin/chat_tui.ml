(* Minimal entry-point that delegates to [Chat_tui.App].

   All heavy-lifting (terminal orchestration, OpenAI streaming, persistence
   …) lives in [lib/chat_tui/app.ml].  Keeping this file tiny makes it trivial
   to provide alternative front-ends without touching the core logic. *)

open Core

let run ~prompt_file () =
  Io.run_main (fun env -> Chat_tui.App.run_chat ~env ~prompt_file ())

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
        fun () -> run ~prompt_file:conversation_file ()]
  in
  Command_unix.run command

