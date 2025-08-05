(** Terminal chat application – event-loop, streaming and persistence.

    This is the public interface of {!Chat_tui.App}, the orchestration
    layer that powers the Ochat terminal UI.

    A single call to {!run_chat} boots the Notty terminal, parses the
    ChatMarkdown prompt, starts the main event-loop and only returns once
    the user quits the application.  All other values are exported to
    facilitate {e white-box} unit– and integration tests – regular
    consumers should ignore them.

    @canonical Chat_tui.App *)

open! Core

(** Runtime artefacts derived from the static chat prompt. *)
type prompt_context =
  { cfg : Chat_response.Config.t (** Behavioural settings (temperature, …) *)
  ; tools : Openai.Responses.Request.Tool.t list
    (** Tools exposed to the assistant at runtime. *)
  ; tool_tbl : (string, string -> string) Base.Hashtbl.t
    (** Mapping [tool-name ↦ implementation]. *)
  }

(** Persistence policy to use when the UI terminates. *)
type persist_mode =
  [ `Always
  | `Never
  | `Ask
  ]

(** [add_placeholder_thinking_message m] appends a transient
    "(thinking…)" assistant message to [m].  It is removed as soon as the
    first streaming token arrives. *)
val add_placeholder_thinking_message : Model.t -> unit

(** Insert a one-shot *error* message so fatal conditions during streaming
    surface in the conversation instead of being silently logged. *)
val add_placeholder_stream_error : Model.t -> string -> unit

(** Display a temporary "(compacting…)" assistant stub while context
    compaction is running. *)
val add_placeholder_compact_message : Model.t -> unit

(** [persist_snapshot env session model] saves [model] to the session on
    disk.  No-op when [session] is [None]. *)
val persist_snapshot : Eio_unix.Stdenv.base -> Session.t option -> Model.t -> unit

(** Immediate (synchronous) UI updates that happen right after the user
    hits ⏎ but {b before} the OpenAI request is sent.  The helper moves the
    draft into the history, clears the prompt, scrolls the viewport and
    issues a {!`Redraw} request on [ev_stream]. *)
val apply_local_submit_effects
  :  dir:Eio.Fs.dir_ty Eio.Path.t
  -> env:Eio_unix.Stdenv.base
  -> cache:Chat_response.Cache.t
  -> model:Model.t
  -> ev_stream:
       ([> `Redraw
        | `Stream of Openai.Responses.Response_stream.t
        | `Stream_batch of Openai.Responses.Response_stream.t list
        | `Replace_history of Openai.Responses.Item.t list
        | `Function_output of Openai.Responses.Function_call_output.t
        ]
        as
        'ev)
         Eio.Stream.t
  -> term:Notty_eio.Term.t
  -> unit

(** Launches the OpenAI completion stream in a new switch and shuttles the
    resulting events back to the main loop via [ev_stream].  The function
    never blocks the UI – all network IO happens in spawned fibres. *)
val handle_submit
  :  env:Eio_unix.Stdenv.base
  -> model:Model.t
  -> ev_stream:
       ([> `Redraw
        | `Stream of Openai.Responses.Response_stream.t
        | `Stream_batch of Openai.Responses.Response_stream.t list
        | `Replace_history of Openai.Responses.Item.t list
        | `Function_output of Openai.Responses.Function_call_output.t
        ]
        as
        'ev)
         Eio.Stream.t
  -> system_event:string Eio.Stream.t
  -> prompt_ctx:prompt_context
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> parallel_tool_calls:bool
  -> history_compaction:bool
  -> unit

(** Boot the TUI and block until the user terminates the program.

    @param env          The standard environment supplied by
                        {!Eio_main.run}.
    @param prompt_file  Path to the *.chatmd* prompt.
    @param session      Optional persisted session that should be resumed.
    @param export_file  Override for the ChatMarkdown export path.
    @param persist_mode Policy whether to save the session snapshot on
                        exit.  Defaults to {!`Ask}.
    @param parallel_tool_calls Allow multiple tool calls to run in
                               parallel (default: [true]).
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
