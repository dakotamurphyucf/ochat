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
  ; tool_tbl : (string, Ochat_function.runner) Base.Hashtbl.t
    (** Mapping [tool_name -> implementation]. *)
  ; moderator : Chat_response.In_memory_stream.moderator option
    (** Optional shared moderator runtime for the session. *)
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

module For_testing : sig
  val should_warm_history_before_redraw : runtime:App_runtime.t -> model:Model.t -> bool
  val cursor_for_frame : model:Model.t -> int * int -> (int * int) option
end

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

    @param env The standard environment supplied by {!Eio_main.run}.
    @param prompt_file Path to the [*.chatmd*] prompt that seeds the
           conversation, declares tools and configures default model settings.
    @param session Optional persisted session to resume.  When present, its
           history, tasks and key/value store take precedence over the
           defaults from [prompt_file].
    @param export_file Optional override for the ChatMarkdown export path.
           When omitted the prompt file path is reused.
    @param persist_mode Policy controlling whether the session snapshot is
           written back on exit.  Defaults to [`Ask].
    @param parallel_tool_calls Allow multiple tool calls to run in parallel
           (default: [true]).
    @param textmate_grammar_files Explicit TextMate grammar JSON files to load
           before terminal initialization. Directories from
           [OCHAT_GRAMMAR_DIR] and the default
           [$XDG_CONFIG_HOME/ochat/grammars] directory are scanned after the
           first frame and trigger a cache-invalidating redraw. Invalid
           explicit files stop startup; invalid discovered files produce
           warnings.
    @param shell_manifest_authorizer Authorizes the exact canonical shell
           manifest before any shell tool is exposed. The default rejects
           manifests.
    @param shell_approval_provider Supplies command-level approval decisions.
           The default uses the TUI's fiber-friendly approval broker.

    Starting the UI from an executable:
    {[
      let () =
        Eio_main.run @@ fun env ->
        Chat_tui.App.run_chat ~env ~prompt_file:"prompt.chatmd" ()
    ]}

    Resuming a saved session and exporting to a separate file:
    {[
      let () =
        Eio_main.run @@ fun env ->
        let session = Session_store.load ~env ~id:"my-session-id" in
        Chat_tui.App.run_chat
          ~env
          ~prompt_file:"prompt.chatmd"
          ~session
          ~export_file:"exported.chatmd"
          ~persist_mode:`Always
          ()
    ]}
 *)
val run_chat
  :  env:Eio_unix.Stdenv.base
  -> prompt_file:string
  -> ?session:Session.t
  -> ?export_file:string
  -> ?persist_mode:persist_mode
  -> ?parallel_tool_calls:bool
  -> ?textmate_grammar_files:string list
  -> ?shell_manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> ?shell_approval_provider:Shell_runtime.Approval_broker.provider
  -> unit
  -> unit
