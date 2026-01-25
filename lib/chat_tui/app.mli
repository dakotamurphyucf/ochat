(** Terminal chat application – event-loop, streaming, export, and persistence.

     {!Chat_tui.App} is the orchestration layer that powers the Ochat terminal
     UI.

     It wires together:
     {ul
     {- {!Chat_tui.Model} for mutable UI state}
     {- {!Chat_tui.Controller} for key handling}
     {- {!Chat_tui.Renderer} for full-screen rendering}
     {- {!Notty_eio.Term} for terminal IO}
     {- {!Chat_response.Driver} for OpenAI streaming and tool execution}
     {- {!Context_compaction.Compactor} for user-triggered history compaction}}

     Use {!run_chat} to boot the UI and block until the user quits.

     Most callers should treat everything other than {!run_chat} as
     test-support: these helpers are exposed to enable white-box unit and
     integration tests of the event-loop and streaming behaviour.

     @canonical Chat_tui.App *)

(** Runtime artefacts derived from the static chat prompt. *)
type prompt_context =
  { cfg : Chat_response.Config.t (** Behavioural settings (temperature, …) *)
  ; tools : Openai.Responses.Request.Tool.t list
    (** Tools exposed to the assistant at runtime. *)
  ; tool_tbl : (string, string -> Openai.Responses.Tool_output.Output.t) Base.Hashtbl.t
    (** Mapping [tool_name -> implementation]. *)
  }

(** Persistence policy to use when the UI terminates.

    The value controls whether a {!Session.t} snapshot derived from the
    final {!Model.t} is written back to disk at the end of {!run_chat}.  The
    policy is ignored when no [session] was supplied. *)
type persist_mode =
  [ `Always
  | `Never
  | `Ask
  ]

(** Boot the TUI and block until the user terminates the program.

    Calling [run_chat ~env ~prompt_file ()] is the primary way to start an
    interactive Ochat session from an executable.  The function initialises
    a full-screen {!Notty_eio.Term}, parses the ChatMarkdown prompt, builds
    an initial {!Model.t} and then runs the main event-loop until the user
    quits.

    On shutdown the helper can:
    {ul
    {- optionally export the full conversation as ChatMarkdown (either
       automatically or after a [y/N] prompt, depending on how the user
       exited the UI and the value of [?export_file]);}
    {- optionally persist the session snapshot according to
       [?persist_mode].}}

    Parameters:
    {ul
    {- [env] – The standard environment supplied by {!Eio_main.run}.}
    {- [prompt_file] – Path to the [*.chatmd*] prompt that seeds the
       conversation, declares tools and configures default model settings.}
    {- [session] – Optional persisted session that should be resumed.
       When present, its history, tasks and key/value store take precedence
       over the defaults from [prompt_file].}
    {- [export_file] – Optional override for the ChatMarkdown export
       path.  When omitted the prompt file path is reused.}
    {- [persist_mode] – Policy controlling whether the session snapshot
       is written back on exit.  Defaults to [`Ask].}
    {- [parallel_tool_calls] – Allow multiple tool calls to run in parallel
       (default: [true]).}}
 *)
val run_chat
  :  env:Eio_unix.Stdenv.base
  -> prompt_file:string
  -> ?session:Session.t
  -> ?export_file:string
  -> ?persist_mode:persist_mode
  -> ?parallel_tool_calls:bool
  -> unit
  -> unit
