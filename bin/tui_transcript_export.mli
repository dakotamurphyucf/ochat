open! Core

(** Explicit transcript export from an attached managed TUI. The caller invokes
    this after restoring the terminal, before detaching the existing client.
    Reads one exact canonical revision, rejects incomplete disclosure, and
    renders bounded ChatMD. The output is a transcript, not an executable agent
    definition. Never substitutes the UI projection for canonical history.

    Rejects the authored definition as a destination (including resolved path
    aliases), asks before replacing other existing files, and installs complete
    bytes atomically using the existing export file owner. Cancellation is not
    converted to a recoverable error. No additional connection or attachment.
    Input document limit is 16 MiB; rendered output limit is 64 MiB. *)
val write
  :  Chat_tui.Agent_session_client.t
  -> env:Eio_unix.Stdenv.base
  -> definition_file:string
  -> output_file:string
  -> unit Or_error.t
