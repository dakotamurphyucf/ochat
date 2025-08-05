(** Mutable snapshot of the TUI state.

    The {!Chat_tui.Model} module concentrates every piece of information
    that the Ochat terminal UI needs to render the current session and to
    react to user input.  The record is still {b mutable} because the
    refactor towards a pure Elm-style architecture (immutable model +
    explicit patches) is carried out incrementally.  A future change will
    turn [t] into an immutable value that gets rebuilt by
    {!apply_patch} instead of modified in place.

    Until then the module provides two things:

    • a thin constructor {!create} that packs pre-existing references into a
      single bundle so they can be passed around conveniently; and
    • a set of *helper* functions that encapsulate common mutations such as
      toggling Vim-style modes, pushing undo states or applying the
      high-level {!Types.patch} commands emitted by the controller.

    @canonical Chat_tui.Model
*)

open Types

type t =
  { mutable history_items : Openai.Responses.Item.t list
  ; mutable messages : message list
  ; mutable input_line : string
  ; mutable auto_follow : bool
  ; msg_buffers : (string, Types.msg_buffer) Base.Hashtbl.t
  ; function_name_by_id : (string, string) Base.Hashtbl.t
  ; reasoning_idx_by_id : (string, int ref) Base.Hashtbl.t
  ; mutable tasks : Session.Task.t list
  ; kv_store : (string, string) Base.Hashtbl.t
  ; mutable fetch_sw : Eio.Switch.t option
  ; scroll_box : Notty_scroll_box.t
  ; mutable cursor_pos : int (** Current position inside [input_line] (bytes). *)
  ; mutable selection_anchor : int option
    (* --------------------------------------------------------------------------- *)
    (* Command-mode scaffolding (Phase 0)                                           *)
    (* --------------------------------------------------------------------------- *)
    (** Anchor position for active selection. *)
  ; mutable mode : editor_mode (** Current editor mode (Insert/Normal). *)
  ; mutable draft_mode : draft_mode
    (** Whether the draft buffer is plain text or raw XML. *)
  ; mutable selected_msg : int option
    (** Currently selected message (Normal mode), if any. *)
  ; mutable undo_stack : (string * int) list
    (** Undo ring – previous states (line, cursor) *)
  ; mutable redo_stack : (string * int) list
  ; mutable cmdline : string (** Current command-line buffer (":"-prefix excluded). *)
  ; mutable cmdline_cursor : int (** Cursor position inside [cmdline]. *)
  ; mutable active_fork : string option
    (** Currently running fork tool call-id, if any. *)
  ; mutable fork_start_index : int option (** History length when fork started. *)
  }
[@@deriving fields ~getters ~setters]

and editor_mode =
  | Insert
  | Normal
  | Cmdline

and draft_mode =
  | Plain
  | Raw_xml

(** Editor-mode of the input area.

    • [Insert] – default; printable keys modify {!input_line} and move the
      cursor.
    • [Normal] – Vim-style command mode.  Keystrokes operate on messages or
      selections instead of inserting characters.
    • [Cmdline] – a ':' prompt is active at the bottom.  The content is kept
      in {!cmdline} / {!cmdline_cursor}.  Leaving the prompt returns to
      [Insert]. *)

(** Draft representation of the {i scratch} buffer that will be sent to the
    assistant next.

    • [Plain] – regular markdown that goes straight to the OpenAI API.
    • [Raw_xml] – low-level XML encoded function call.  This mode is used by
      the command palette to prepare structured tool invocations. *)

(** [create …] bundles the many independent references that make up the
    current application state into a single record.  The constructor is
    deliberately {e shallow}: it stores the arguments {i as-is} without
    copying or validating them so mutating the original reference later
    still affects the model.

    The function is expected to disappear once the codebase migrates to an
    immutable model. *)
val create
  :  history_items:Openai.Responses.Item.t list
  -> messages:message list
  -> input_line:string
  -> auto_follow:bool
  -> msg_buffers:(string, Types.msg_buffer) Base.Hashtbl.t
  -> function_name_by_id:(string, string) Base.Hashtbl.t
  -> reasoning_idx_by_id:(string, int ref) Base.Hashtbl.t
  -> tasks:Session.Task.t list
  -> kv_store:(string, string) Base.Hashtbl.t
  -> fetch_sw:Eio.Switch.t option
  -> scroll_box:Notty_scroll_box.t
  -> cursor_pos:int
  -> selection_anchor:int option
  -> mode:editor_mode
  -> draft_mode:draft_mode
  -> selected_msg:int option
  -> undo_stack:(string * int) list
  -> redo_stack:(string * int) list
  -> cmdline:string
  -> cmdline_cursor:int
  -> t

(** Convenience accessors – added on demand. *)

(** [input_line t] returns the editable contents of the prompt at the bottom
    of the screen.  The value is never [\n]-terminated. *)
val input_line : t -> string

(** [cursor_pos t] is the {e byte} index of the caret inside
    {!input_line}.  The value is always between [0] and
    [String.length (input_line t)]. *)
val cursor_pos : t -> int

(** Position at which the current selection started, if any.  [None] means
    no active selection. *)
val selection_anchor : t -> int option

(** [clear_selection t] drops any active selection and resets
    {!selection_anchor} to [None]. *)
val clear_selection : t -> unit

(** [set_selection_anchor t p] marks byte‐offset [p] as the start of a text
    selection.  Calling the function implicitly enables selection mode. *)
val set_selection_anchor : t -> int -> unit

(** [selection_active t] is [true] when a selection anchor is set. *)
val selection_active : t -> bool

(** [messages t] returns the list of renderable messages in top-down order.
    Each element is a [(role, text)] pair as defined in {!Types.message}. *)
val messages : t -> message list

(** List of tasks currently associated with the session. *)
val tasks : t -> Session.Task.t list

(** Key–value store for arbitrary plugin data. *)
val kv_store : t -> (string, string) Base.Hashtbl.t

(** Auto-scroll flag.  When [true] the view follows new incoming messages
    automatically; otherwise the scroll position stays unchanged. *)
val auto_follow : t -> bool

(** {1 Command-mode helpers} *)

(** [toggle_mode t] switches between [Insert] and [Normal].  Calling the
    function while in [Cmdline] also returns to [Insert]. *)
val toggle_mode : t -> unit

(** [set_draft_mode t m] sets the interpretation of the prompt to [m]. *)
val set_draft_mode : t -> draft_mode -> unit

(** [select_message t idx] marks message [idx] as the focussed item in
    normal mode.  [None] clears the selection.  The index is zero-based and
    refers to the list returned by {!messages}. *)
val select_message : t -> int option -> unit

(** {1 Command-line helpers} *)

(** Current contents of the ':' command-line buffer (without the leading
    ':'). *)
val cmdline : t -> string

(** [cmdline_cursor t] is the byte offset of the caret inside {!cmdline}. *)
val cmdline_cursor : t -> int

(** Overwrites the command-line buffer. *)
val set_cmdline : t -> string -> unit

(** Moves the cursor inside the command-line buffer. *)
val set_cmdline_cursor : t -> int -> unit

(** {1 Fork helpers} *)

(** Identifier of a long-running {!functions.fork} call that streams into
    the UI, or [None] if no fork is active. *)
val active_fork : t -> string option

(** Updates {!active_fork}. *)
val set_active_fork : t -> string option -> unit

(** Index into the message list that marked the boundary when the current
    fork started.  Used to highlight new assistant output. *)
val fork_start_index : t -> int option

(** Updates {!fork_start_index}. *)
val set_fork_start_index : t -> int option -> unit

(** {1 Undo / Redo helpers} *)

(** [push_undo t] stores the current [input_line] / [cursor_pos] pair at the
    top of the undo ring.  Any redo history is cleared. *)
val push_undo : t -> unit

(** [undo t] reverts the most recent change to the prompt.  Returns [true]
    when a state was restored, [false] if the stack was empty. *)
val undo : t -> bool

(** [redo t] reapplies the last undone change.  Returns [true] on success. *)
val redo : t -> bool

(** {1 Applying patches}

    Refactoring step 6 introduces a {e patch} based update mechanism that
    abstracts over concrete mutations to the UI state.  For the time being
    the implementation still performs inplace updates to the interior
    {!ref} values – later steps will turn [t] into an immutable record and
    rebuild a fresh value instead. *)

(** [apply_patch t p] executes the pure {!Types.patch} command [p] by
    mutating [t] in place and returns the same value for ergonomic
    piping.

    Example – append a streamed delta to an assistant message:
    {[
      let patch = Types.Append_text { id; role = "assistant"; text = "…" } in
      ignore (Model.apply_patch model patch)
    ]} *)
val apply_patch : t -> Types.patch -> t

(** Folds {!apply_patch} over a list of commands. *)
val apply_patches : t -> Types.patch list -> t

(** Appends a raw OpenAI history item to the canonical list and returns the
    (mutated) model.  Unlike [Add_user_message] the helper bypasses any UI
    manipulation. *)
val add_history_item : t -> Openai.Responses.Item.t -> t
