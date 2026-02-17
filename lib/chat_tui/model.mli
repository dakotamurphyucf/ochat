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

    {1 Pages and page-local renderer state}

    The full-screen UI is rendered as a {e page} chosen by
    {!active_page}. Page-local renderer state (scroll boxes, selection, and
    render caches) is stored under {!pages}.  Today the only page is
    {!Page_id.Chat} with state {!Chat_page_state.t} stored at [pages.chat].

    @canonical Chat_tui.Model
*)

open Types

(** Cached render for a single history message at a fixed history-pane
    width.

    The record stores the original [text] together with pre-rendered
    {!Notty.I.t} images for unselected and (optionally) selected states,
    plus their heights in terminal cells.  Selected images are constructed
    lazily on first use. *)
type msg_img_cache =
  { width : int
  ; text : string
  ; img_unselected : Notty.I.t
  ; height_unselected : int
  ; img_selected : Notty.I.t option
  ; height_selected : int option
  }

module Page_id : sig
  (** Identifier of a full-screen renderer page. *)
  type t = Chat
end

module Chat_page_state : sig
  (** Mutable state and caches used by the chat page renderer.  Stored in
      {!pages} under [pages.chat]. *)
  type t =
    { scroll_box : Notty_scroll_box.t
    ; mutable selected_msg : int option
    ; mutable msg_img_cache : (int, msg_img_cache) Base.Hashtbl.t
    ; mutable last_history_width : int option
    ; mutable msg_heights : int array
    ; mutable height_prefix : int array
    ; mutable dirty_height_indices : int list
    }
end

module Pages : sig
  type t = { chat : Chat_page_state.t }
end

(** {1 Type-ahead completion state}

    A type-ahead completion is a single candidate suffix computed for the
    current prompt.  The completion is never merged into {!input_line} unless
    explicitly accepted by the controller.

    The [base_*] fields snapshot the prompt state at computation time; they are
    used to detect whether a completion is still applicable after further
    edits/movement. *)
type typeahead_completion =
  { text : string
  ; base_input : string
  ; base_cursor : int
  ; generation : int
  }

type t =
  { mutable history_items : Openai.Responses.Item.t list
  ; mutable messages : message list
  ; mutable input_line : string
  ; mutable auto_follow : bool
  ; msg_buffers : (string, Types.msg_buffer) Base.Hashtbl.t
  ; function_name_by_id : (string, string) Base.Hashtbl.t
  ; reasoning_idx_by_id : (string, int ref) Base.Hashtbl.t
  ; tool_output_by_index : (int, Types.tool_output_kind) Base.Hashtbl.t
  ; call_id_by_item_id : (string, string) Base.Hashtbl.t
  ; tool_path_by_call_id : (string, string option) Base.Hashtbl.t
  ; mutable active_page : Page_id.t
  ; pages : Pages.t
  ; mutable tasks : Session.Task.t list
  ; kv_store : (string, string) Base.Hashtbl.t
  ; mutable fetch_sw : Eio.Switch.t option
  ; mutable cursor_pos : int (** Current position inside [input_line] (bytes). *)
  ; mutable selection_anchor : int option (** Anchor position for active selection. *)
  ; mutable mode : editor_mode (** Current editor mode (Insert/Normal). *)
  ; mutable draft_mode : draft_mode
    (** Whether the draft buffer is plain text or raw XML. *)
  ; mutable undo_stack : (string * int) list
    (** Undo ring – previous states (line, cursor) *)
  ; mutable redo_stack : (string * int) list
  ; mutable cmdline : string (** Current command-line buffer (":"-prefix excluded). *)
  ; mutable cmdline_cursor : int (** Cursor position inside [cmdline]. *)
  ; mutable active_fork : string option
    (** Currently running fork tool call-id, if any. *)
  ; mutable fork_start_index : int option (** History length when fork started. *)
  ; mutable typeahead_completion : typeahead_completion option
  ; mutable typeahead_preview_open : bool
  ; mutable typeahead_preview_scroll : int
  ; mutable typeahead_generation : int
  }
[@@deriving fields ~getters ~setters]

(** Editor-mode of the input area.

    • [Insert] – default; printable keys modify {!input_line} and move the
      cursor.
    • [Normal] – Vim-style command mode.  Keystrokes operate on messages or
      selections instead of inserting characters.
    • [Cmdline] – a ':' prompt is active at the bottom.  The content is kept
      in {!cmdline} / {!cmdline_cursor}.  Leaving the prompt returns to
      [Insert]. *)
and editor_mode =
  | Insert
  | Normal
  | Cmdline

(** Draft representation of the {i scratch} buffer that will be sent to the
    assistant next.

    • [Plain] – regular markdown that goes straight to the OpenAI API.
    • [Raw_xml] – low-level XML encoded function call.  This mode is used by
      the command palette to prepare structured tool invocations. *)
and draft_mode =
  | Plain
  | Raw_xml

(** [create …] bundles the many independent references that make up the
    current application state into a single record.  The constructor is
    deliberately {e shallow}: it stores the arguments {i as-is} without
    copying or validating them so mutating the original reference later
    still affects the model.

    The function is expected to disappear once the codebase migrates to an
    immutable model.

    @param history_items Raw OpenAI history items (canonical source of truth).
    @param messages Renderable transcript derived from [history_items].
    @param input_line Current insert buffer (without trailing newline).
    @param auto_follow Auto-scroll flag for the history viewport.
    @param msg_buffers Per-stream buffers keyed by OpenAI stream id.
    @param function_name_by_id Tool/function name by call id for streaming.
    @param reasoning_idx_by_id Per-call reasoning token counters (streaming).
    @param tool_output_by_index Classification metadata for tool outputs keyed by
           message index.
    @param tasks Session task list.
    @param kv_store Mutable key/value store for ad-hoc metadata.
    @param fetch_sw Optional switch used to cancel in-flight background fetches.
    @param scroll_box Scroll box backing the history viewport.
    @param cursor_pos Byte offset of the caret inside [input_line] (or the active
           buffer).
    @param selection_anchor Optional selection anchor (byte offset).
    @param mode Current editor mode.
    @param draft_mode Current draft-mode flag (Plain vs Raw XML).
    @param selected_msg Optional selected message index (Normal mode).
    @param undo_stack Undo history (line, cursor) pairs.
    @param redo_stack Redo history (line, cursor) pairs.
    @param cmdline Command-line buffer (without the leading ':').
    @param cmdline_cursor Cursor position inside [cmdline] (byte offset). *)
val create
  :  history_items:Openai.Responses.Item.t list
  -> messages:message list
  -> input_line:string
  -> auto_follow:bool
  -> msg_buffers:(string, Types.msg_buffer) Base.Hashtbl.t
  -> function_name_by_id:(string, string) Base.Hashtbl.t
  -> reasoning_idx_by_id:(string, int ref) Base.Hashtbl.t
  -> tool_output_by_index:(int, Types.tool_output_kind) Base.Hashtbl.t
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

(** [active_page t] indicates which full-screen page is currently shown.
    Initially this is always {!Page_id.Chat}. *)
val active_page : t -> Page_id.t

(** [set_active_page t page] changes the active page. *)
val set_active_page : t -> Page_id.t -> unit

(** [chat_page t] returns the chat page's renderer state/caches.  This is
    the authoritative location for chat-only scroll state and render
    caches. *)
val chat_page : t -> Chat_page_state.t

(** [scroll_box t] is the chat page's scroll box used by history
    virtualisation and scrolling commands. *)
val scroll_box : t -> Notty_scroll_box.t

(** [selected_msg t] is the currently selected message (Normal mode), if
    any.  The index is zero-based and refers to the list returned by
    {!messages}. *)
val selected_msg : t -> int option

(** [input_line t] returns the editable contents of the prompt at the bottom
    of the screen.  The value is never [\n]-terminated. *)
val input_line : t -> string

(** [cursor_pos t] is the {e byte} index of the caret inside
    {!input_line}.  The value is always between [0] and
    [String.length (input_line t)]. *)
val cursor_pos : t -> int

(** [selection_anchor t] is the position at which the current selection
    started, if any.  [None] means no active selection. *)
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

(** [tasks t] returns the list of tasks currently associated with the
    session. *)
val tasks : t -> Session.Task.t list

(** [kv_store t] returns the mutable key–value store used by plugins and
    tools to stash arbitrary metadata. *)
val kv_store : t -> (string, string) Base.Hashtbl.t

(** [tool_output_by_index t] returns classification metadata for tool-output
    messages keyed by message index in {!messages}.

    Entries are present only for messages whose [role] is tool-like and for
    which the TUI managed to infer the corresponding tool call; the stored
    values are {!Types.tool_output_kind} tags that guide specialised
    rendering of built-in tools (for example, path-aware styling for
    [read_file]). *)
val tool_output_by_index : t -> (int, Types.tool_output_kind) Base.Hashtbl.t

(** [auto_follow t] is the auto-scroll flag.  When [true] the view follows
    new incoming messages automatically; otherwise the scroll position stays
    unchanged. *)
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

(** [cmdline t] returns the current contents of the ':' command-line buffer
    (without the leading ':'). *)
val cmdline : t -> string

(** [cmdline_cursor t] is the byte offset of the caret inside {!cmdline}. *)
val cmdline_cursor : t -> int

(** [set_cmdline t s] overwrites the ':' command-line buffer with [s]. *)
val set_cmdline : t -> string -> unit

(** [set_cmdline_cursor t n] moves the cursor inside the ':' command-line
    buffer to byte offset [n]. *)
val set_cmdline_cursor : t -> int -> unit

(** {1 Fork helpers} *)

(** [active_fork t] is the identifier of a long-running {!functions.fork}
    call that streams into the UI, or [None] if no fork is active. *)
val active_fork : t -> string option

(** [set_active_fork t id] updates {!active_fork} to [id]. *)
val set_active_fork : t -> string option -> unit

(** [fork_start_index t] is the index into the message list that marked the
    boundary when the current fork started.  Used to highlight new
    assistant output. *)
val fork_start_index : t -> int option

(** [set_fork_start_index t idx] updates {!fork_start_index} to [idx]. *)
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

(** {1 Type-ahead completion helpers}

    Type-ahead completion augments the prompt editor with a single-candidate
    suffix:
    {ul
    {- the reducer triggers background requests (debounced after edits) and
       publishes results into {!typeahead_completion};}
    {- the controller handles key bindings to accept/dismiss completions and to
       open/scroll/close the preview popup; and}
    {- the renderer shows a dim inline "ghost" suffix, optional hint text, and
       a preview overlay.}}

    Invariants:
    {ul
    {- The completion must be a suffix to insert at [base_cursor] in
       [base_input]; it must not repeat the prefix before the cursor.}
    {- A completion is considered relevant only when [base_input] /
       [base_cursor] still match the current editor state (see
       {!typeahead_is_relevant}).}}
*)

(** [typeahead_completion t] is the current inline completion candidate, if any. *)
val typeahead_completion : t -> typeahead_completion option

(** [set_typeahead_completion t c] overwrites the current completion candidate. *)
val set_typeahead_completion : t -> typeahead_completion option -> unit

(** [clear_typeahead t] drops the completion candidate and resets preview state. *)
val clear_typeahead : t -> unit

(** [typeahead_preview_open t] is [true] when the preview popup should be shown. *)
val typeahead_preview_open : t -> bool

(** [set_typeahead_preview_open t b] opens/closes the preview popup. *)
val set_typeahead_preview_open : t -> bool -> unit

(** [typeahead_preview_scroll t] is the preview popup scroll offset (lines). *)
val typeahead_preview_scroll : t -> int

(** [set_typeahead_preview_scroll t n] updates the preview scroll offset. *)
val set_typeahead_preview_scroll : t -> int -> unit

(** [bump_typeahead_generation t] increments the generation counter and returns
    the updated value. *)
val bump_typeahead_generation : t -> int

(** [typeahead_is_relevant t] is [true] iff:

    - the editor mode is [Insert], and
    - a completion exists, and
    - the completion's [base_input] and [base_cursor] still match the current
      {!input_line} / {!cursor_pos}.
*)
val typeahead_is_relevant : t -> bool

(** [accept_typeahead_all t] inserts the current relevant completion at the
    cursor and clears the completion state.

    Returns [true] if a completion was accepted, [false] otherwise.

    The operation:

    - sanitises the completion text with {!Util.sanitize} [[~strip:false]]
    - calls {!push_undo} exactly once
    - clears any active selection
    - inserts the completion at {!cursor_pos} and advances the cursor
    - clears the completion and closes the preview
    - bumps the type-ahead generation counter
*)
val accept_typeahead_all : t -> bool

(** [accept_typeahead_line t] inserts the first line of the relevant completion
    at the cursor and keeps the remainder as a new completion (progressive
    accept).

    Returns [true] if a completion was accepted, [false] otherwise.

    The inserted segment is the prefix up to and including the first ['\n'] if
    present; otherwise the whole completion is inserted.

    The operation calls {!push_undo} exactly once, clears any active selection,
    closes the preview, and bumps the generation counter. *)
val accept_typeahead_line : t -> bool

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

(** [add_history_item t item] appends the raw OpenAI history [item] to the
    canonical list and returns the (mutated) model.  Unlike the
    [Add_user_message] patch the helper bypasses any UI manipulation and
    does not touch {!messages}. *)
val add_history_item : t -> Openai.Responses.Item.t -> t

(** [rebuild_tool_output_index t] recomputes {!tool_output_by_index} from
    the current {!history_items}.

    The helper walks the OpenAI history, pairs each renderable item (as
    determined by {!Chat_tui.Conversation.pair_of_item}) with its message
    index in {!messages}, and populates the map with
    {!Types.tool_output_kind} values for corresponding
    [Function_call_output] entries.

    Use this when the entire history is replaced at once (initial model
    construction, history compaction, or handling of a [`Replace_history]
    event).  Streaming updates do not need this helper – they classify tool
    outputs incrementally via the [Set_function_output] patch. *)
val rebuild_tool_output_index : t -> unit

(** {1 Rendering cache helpers}

    Low-level helpers for the renderer.  Callers outside the rendering path
    should not need these. *)

(** [last_history_width t] is the width (in terminal cells) for which the
    chat page's cached message images and heights are currently valid, or
    [None] if no cache has been built yet. *)
val last_history_width : t -> int option

(** [set_last_history_width t w] updates {!last_history_width}.  Calling the
    function does **not** invalidate individual entries – that is done by
    the renderer which knows whether a global flush or targeted
    invalidations are cheaper. *)
val set_last_history_width : t -> int option -> unit

(** [clear_all_img_caches t] completely clears {!msg_img_cache} and the
    associated height caches.  Use this when the terminal has been resized
    and *all* cached images are now stale. *)
val clear_all_img_caches : t -> unit

(** [invalidate_img_cache_index t ~idx] removes the cache entry for the
    message at index [idx].  Called whenever the underlying text changes
    (e.g. when streaming deltas arrive). *)
val invalidate_img_cache_index : t -> idx:int -> unit

(** [find_img_cache t ~idx] returns the cached render of the message at
    [idx] or [None] if no entry exists or the cache was invalidated. *)
val find_img_cache : t -> idx:int -> msg_img_cache option

(** [set_img_cache t ~idx entry] stores [entry] in the per-message cache.
    Existing data for the same index is overwritten. *)
val set_img_cache : t -> idx:int -> msg_img_cache -> unit

(** [take_and_clear_dirty_height_indices t] returns and clears the list of
    indices whose heights may be stale.  The list may contain duplicates and
    is not guaranteed to be ordered. *)
val take_and_clear_dirty_height_indices : t -> int list

(** [msg_heights t] are the cached rendered heights (in cells) for the chat
    transcript at {!last_history_width}. *)
val msg_heights : t -> int array

(** [set_msg_heights t a] updates {!msg_heights}. *)
val set_msg_heights : t -> int array -> unit

(** [height_prefix t] are prefix sums of {!msg_heights}. *)
val height_prefix : t -> int array

(** [set_height_prefix t a] updates {!height_prefix}. *)
val set_height_prefix : t -> int array -> unit
