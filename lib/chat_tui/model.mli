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
    render caches) is stored under {!pages}.

    @canonical Chat_tui.Model
*)

open Types

(** Cached render for a single history message at a fixed history-pane
    width.

    The record stores one overlay-neutral layout and image. Selection and
    search attributes are applied while composing the displayed history. *)
type msg_img_cache =
  { row_revision : int
  ; width : int
  ; role : string
  ; text : string
  ; image : Notty.I.t
  ; height : int
  ; layout : Chat_message_render_job.Layout.t
  ; layout_plan : Chat_message_render_job.Layout_plan.t
  }

type msg_semantic_cache =
  { row_revision : int
  ; role : string
  ; text : string
  ; tool_output : Types.tool_output_kind option
  ; prepared : Chat_message_render_job.Prepared_message.t
  ; highlights : Chat_message_render_job.Highlight_cache.binding list
  }

type scroll_direction =
  | Toward_older
  | Toward_newer

type prepared_boundary_distances =
  { older : int
  ; newer : int
  }

type chat_scroll_result =
  { changed : bool
  ; clamped : bool
  ; distances : prepared_boundary_distances option
  }

module Resize_anchor : sig
  type t

  type resolution =
    | Followed_bottom
    | Preserved
    | Repaired
    | Empty
  [@@deriving sexp_of]
end

module Page_id : sig
  (** Identifier of a full-screen renderer page. *)
  type t =
    | Chat
    | Agent
    | Shell_security
end

module Chat_page_state : sig
  module Destination : sig
    type reason =
      | Earlier_conversation
      | Search_result
      | Latest_conversation

    type placement =
      | Top
      | Center
      | Bottom

    type t =
      { id : Projected_message.Id.t
      ; revision : int
      ; reason : reason
      ; placement : placement
      }
  end

  type history_chunk_row =
    { row_id : Projected_message.Id.t
    ; row_revision : int
    }
  [@@deriving equal]

  type history_chunk =
    { rows : history_chunk_row array
    ; image : Notty.I.t
    ; height : int
    }

  type materialization =
    | Loading
    | Resizing
    | Corridor
    | Warm

  type history_image_cache =
    { width : int
    ; transcript_generation : int
    ; render_generation : int
    ; chunks : history_chunk array
    ; chunk_tree : Notty.I.t array
    ; chunk_tree_base : int
    ; image : Notty.I.t
    }

  type corridor_history_cache =
    { width : int
    ; request_generation : int
    ; rows : History_chunk.Range.t
    ; mutable image : Notty.I.t
    }

  type width_snapshot
  type preparing_width

  type width_preparation_status =
    | Preparing
    | Complete
    | Cancelled

  type preparation_layout =
    { input_box_height : int
    ; history_height : int
    ; sticky_height : int
    ; scroll_height : int
    }

  (** Mutable state and caches used by the chat page renderer.  Stored in
      {!pages} under [pages.chat]. *)
  type t =
    { scroll_box : Notty_scroll_box.t
    ; mutable msg_img_cache :
        (Projected_message.Id.t, msg_img_cache) Base.Hashtbl.t
    ; msg_semantic_cache :
        (Projected_message.Id.t, msg_semantic_cache) Base.Hashtbl.t
    ; mutable active_history_width : int option
    ; mutable preparing_width : preparing_width option
    ; geometry : Renderer_virtual_list.Geometry.t
    ; mutable dirty_height_rows : (Projected_message.Id.t * int) list
    ; mutable scroll_direction : scroll_direction
    ; mutable materialization : materialization
    ; mutable history_image_cache : history_image_cache option
    ; mutable corridor_history_cache : corridor_history_cache option
    ; mutable width_snapshots : width_snapshot list
    ; row_chunk_by_id : (Projected_message.Id.t, int) Base.Hashtbl.t
    ; dirty_history_chunks : int Core.Hash_set.t
    }
end

module Projected_state : sig
  type t
end

(** Transient state for the Agent live view.

    Active calls and progress are UI-only. They do not modify canonical
    history, messages, or persisted ChatMarkdown. *)
module Agent_page_state : sig
  type progress_entry
  type render_block
  type render_block_view =
    | Invocation of
        { name : string
        ; payload : string
        ; agent_page_kind : Chat_response.Tool_execution_event.agent_page_kind
        }
    | Truncation
    | Waiting
    | Progress of progress_entry
    | Status of Ochat_function.Trace.outcome
  type call
  type t

  val empty : unit -> t
  (** [empty ()] creates state with no calls or selection and an independent
      scroll box that follows new output from the bottom. *)
end

module Shell_security_page_state = Shell_security_page_state

module Pages : sig
  type t =
    { chat : Chat_page_state.t
    ; agent : Agent_page_state.t
    ; shell_security : Shell_security_page_state.t
    }
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

type assistant_activity =
  | Thinking
  | Writing
  | Working

type activity =
  | Assistant of assistant_activity
  | Compacting

type viewport_relation =
  | Above
  | Visible
  | Below
  | Unknown

type projection_damage =
  | No_damage
  | Below_viewport
  | Visible_damage
  | Above_viewport
  | Unknown_damage

type msg_buffer =
  { buf : Buffer.t
  ; row_id : Projected_message.Id.t
  }

type t =
  { mutable history_items : History_entry.t list
  ; mutable message_array : message array
  ; mutable render_row_ids : Projected_message.Id.t array
  ; mutable transcript_generation : int
  ; mutable render_generation : int
  ; mutable message_revisions : int array
  ; mutable input_line : string
  ; mutable auto_follow : bool
  ; msg_buffers : (string, msg_buffer) Base.Hashtbl.t
  ; function_name_by_id : (string, string) Base.Hashtbl.t
  ; reasoning_idx_by_id : (string, int ref) Base.Hashtbl.t
  ; tool_output_by_index : (int, Types.tool_output_kind) Base.Hashtbl.t
  ; tool_output_by_id
      : (Projected_message.Id.t, Types.tool_output_kind) Base.Hashtbl.t
  ; call_id_by_item_id : (string, string) Base.Hashtbl.t
  ; tool_call_id_by_id : (Projected_message.Id.t, string) Base.Hashtbl.t
  ; tool_call_outcome_by_call_id
      : (string, Ochat_function.Trace.outcome) Base.Hashtbl.t
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
  ; mutable search_query : string
  ; mutable search_cursor : int
  ; mutable last_search_query : string option
  ; mutable last_search_dir : search_dir option
  ; mutable typeahead_completion : typeahead_completion option
  ; mutable typeahead_preview_open : bool
  ; mutable typeahead_preview_scroll : int
  ; mutable typeahead_generation : int
  ; mutable activity : activity option
  ; mutable animation_frame : int
  ; mutable normal_input_enabled : bool
  ; projected : Projected_state.t
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
and search_dir =
  | Forward
  | Backward

and editor_mode =
  | Insert
  | Normal
  | Cmdline
  | Search of search_dir

(** Draft representation of the {i scratch} buffer that will be sent to the
    assistant next.

    • [Plain] – regular markdown that goes straight to the OpenAI API.
    • [Raw_xml] – low-level XML encoded function call.  This mode is used by
      the command palette to prepare structured tool invocations. *)
and draft_mode =
  | Plain
  | Raw_xml

val set_chat_materialization_loading : t -> unit
val set_chat_materialization_resizing : t -> unit
val set_chat_materialization_corridor : t -> unit
val set_chat_materialization_warm : t -> unit
val chat_materialization : t -> Chat_page_state.materialization
val normal_input_is_enabled : t -> bool
val set_normal_input_enabled : t -> bool -> unit
(** [normal_input_is_enabled t] returns [true] after the first exact Corridor
    or Warm frame completes terminal presentation. *)

(** Width materialization keeps the currently displayable exact width separate
    from one generation-scoped target width. [Resizing] displays a loader while
    detached workers prepare target rows. [Corridor] exposes only a bounded,
    exact target-width range; distant geometry may remain estimated. [Warm]
    means every current row and the complete history root are exact. *)

val history_image_cache : t -> Chat_page_state.history_image_cache option
val corridor_history_cache : t -> Chat_page_state.corridor_history_cache option
val clear_corridor_history_cache : t -> unit
val set_corridor_history_image : t -> Notty.I.t -> unit
val set_history_image_cache : t -> Chat_page_state.history_image_cache option -> unit
val defer_dirty_height_rows : t -> (Projected_message.Id.t * int) list -> unit
val remember_current_width : t -> unit
val restore_width : t -> width:int -> bool
val reusable_layout_width : t -> width:int -> int option
val restore_layout_width : t -> source_width:int -> width:int -> bool

val width_preparation : t -> Chat_page_state.preparing_width option
(** [width_preparation t] returns the current generation-scoped target-width
    preparation. Stale transcript or render generations are cleared and return
    [None]. *)

val start_width_preparation
  :  t
  -> request_generation:int
  -> terminal_size:int * int
  -> layout:Chat_page_state.preparation_layout
  -> theme_generation:int
  -> grammar_generation:int
  -> anchor:Resize_anchor.t
  -> unit
(** [start_width_preparation t ...] replaces any previous target-width
    preparation without changing active exact-width rendering state. At most
    one preparation exists, and stale generations cannot publish. *)

val width_preparation_request_generation
  : Chat_page_state.preparing_width -> int

val width_preparation_target_width : Chat_page_state.preparing_width -> int

val width_preparation_terminal_size
  : Chat_page_state.preparing_width -> int * int

val width_preparation_layout
  : Chat_page_state.preparing_width -> Chat_page_state.preparation_layout

val width_preparation_generations
  : Chat_page_state.preparing_width -> int * int

val width_preparation_highlight_generations
  : Chat_page_state.preparing_width -> int * int

val width_preparation_status
  : Chat_page_state.preparing_width -> Chat_page_state.width_preparation_status

val width_preparation_active_geometry
  :  Chat_page_state.preparing_width
  -> Renderer_virtual_list.Geometry.Snapshot.t

val width_preparation_scroll_direction
  : Chat_page_state.preparing_width -> scroll_direction

val width_preparation_anchor
  : Chat_page_state.preparing_width -> Resize_anchor.t

val width_preparation_viewport_intent
  : t -> Chat_page_state.preparing_width -> int * bool
(** [width_preparation_viewport_intent t preparation] resolves the captured
    stable anchor to [(requested_scroll, follow_bottom)] without mutating the
    active scroll box. *)

val width_preparation_row_count : Chat_page_state.preparing_width -> int

val width_preparation_exact_row_count
  : t -> Chat_page_state.preparing_width -> int

val width_preparation_is_exact
  : t -> Chat_page_state.preparing_width -> bool
(** [width_preparation_is_exact t preparation] validates that every current
    stable row and revision has exact output at the target width. *)

val invalidate_width_preparation_row
  : t -> id:Projected_message.Id.t -> unit
(** [invalidate_width_preparation_row t ~id] removes only [id]'s target-width
    output and its derived batch/chunk readiness. *)

val reconcile_width_preparation : t -> unit
(** [reconcile_width_preparation t] retains only target rows whose stable ID,
    revision, role, text, and width remain current after projection changes. *)

val find_width_preparation_row
  :  t
  -> request_generation:int
  -> id:Projected_message.Id.t
  -> msg_img_cache option

val set_width_preparation_row
  :  t
  -> request_generation:int
  -> id:Projected_message.Id.t
  -> msg_img_cache
  -> bool
(** [set_width_preparation_row t ...] records an exact target-width row
    without modifying the active image cache or geometry. *)

val mark_width_preparation_complete
  : t -> request_generation:int -> bool

val mark_width_preparation_batch
  : t -> request_generation:int -> batch_index:int -> bool

val mark_width_preparation_chunk
  : t -> request_generation:int -> chunk_index:int -> bool

val set_width_preparation_partial_chunk
  :  t
  -> request_generation:int
  -> chunk_index:int
  -> Chat_page_state.history_chunk
  -> bool

val set_width_preparation_corridors
  :  t
  -> request_generation:int
  -> desired:History_chunk.Range.t option
  -> published:History_chunk.Range.t option
  -> bool

val width_preparation_batch_is_ready
  : t -> request_generation:int -> batch_index:int -> bool

val width_preparation_chunk_is_ready
  : t -> request_generation:int -> chunk_index:int -> bool

val width_preparation_corridors
  :  Chat_page_state.preparing_width
  -> History_chunk.Range.t option * History_chunk.Range.t option

val width_preparation_destination
  : Chat_page_state.preparing_width -> Chat_page_state.Destination.t option

val width_preparation_destination_is_current
  : t -> Chat_page_state.preparing_width -> bool
(** [width_preparation_destination_is_current t preparation] returns whether
    the stable destination and captured revision still identify a current row. *)

val set_width_preparation_destination
  :  t
  -> request_generation:int
  -> Chat_page_state.Destination.t option
  -> bool

val publish_width_preparation_corridor
  : t -> request_generation:int -> bool
(** [publish_width_preparation_corridor t ~request_generation] atomically
    promotes a fully prepared visible corridor into active target-width
    caches, coherent partial geometry, scroll state, and corridor history. *)

val promote_width_preparation_rows
  : t -> request_generation:int -> bool
(** [promote_width_preparation_rows t ~request_generation] installs every
    validated target row and globally exact geometry without changing
    materialization or constructing the complete history root. *)

val finish_width_preparation_promotion
  : t -> request_generation:int -> bool
(** [finish_width_preparation_promotion t ~request_generation] transitions a
    complete exact target cache to [Warm], clears corridor restrictions, and
    retains the width in the recent-width LRU. *)

val clear_width_preparation
  :  t
  -> request_generation:int
  -> Chat_page_state.preparing_width option

val cancel_width_preparation
  : t -> request_generation:int -> bool
(** [cancel_width_preparation t ~request_generation] atomically discards the
    matching preparation without changing active rendering state. *)
val row_viewport_relation
  :  t
  -> viewport_height:int
  -> id:Projected_message.Id.t
  -> viewport_relation

val relation_at_index
  : t -> viewport_height:int -> index:int -> viewport_relation
(** [relation_at_index t ~viewport_height ~index] classifies a current row
    using exact-prefix reasoning even when distant geometry is estimated. *)

val buffer_row_id : t -> string -> Projected_message.Id.t option
val mark_history_row_dirty : t -> id:Projected_message.Id.t -> unit
val mark_all_history_chunks_dirty : t -> unit
val take_dirty_history_chunks : t -> int list
val has_dirty_history_chunks : t -> bool
(** [has_dirty_history_chunks t] returns [true] when history chunks are pending
    recomposition. *)

val defer_dirty_history_chunks : t -> int list -> unit
val projection_damage_requires_redraw : projection_damage -> bool
val commit_startup_render_result : t -> Chat_message_render_job.result -> bool

val reconcile_projected_messages_with_damage
  :  t
  -> viewport_height:int
  -> rows:Projected_message.t list
  -> messages:Types.message list
  -> projection_damage

(** [create …] bundles the many independent references that make up the
    current application state into a single record.  The constructor is
    deliberately {e shallow}: it stores the arguments {i as-is} without
    copying or validating them so mutating the original reference later
    still affects the model.

    The function is expected to disappear once the codebase migrates to an
    immutable model.

    @param history_items Identity-bearing canonical history entries.
    @param messages Renderable transcript derived from [history_items].
    @param input_line Current insert buffer (without trailing newline).
    @param auto_follow Auto-scroll flag for the history viewport.
    @param msg_buffers Per-stream buffers keyed by canonical entry ID when
           available, with provider IDs retained only as transport fallbacks.
    @param function_name_by_id Tool/function name by call id for streaming.
    @param reasoning_idx_by_id Per-call reasoning token counters (streaming).
    @param tool_output_by_index Compatibility projection of ID-keyed tool
           metadata onto current layout indexes.
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
  :  history_items:History_entry.t list
  -> messages:message list
  -> input_line:string
  -> auto_follow:bool
  -> msg_buffers:(string, msg_buffer) Base.Hashtbl.t
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

val activity : t -> activity option
(** [activity t] returns the long-running operation currently presented in
    the UI. Runtime operation state remains authoritative. *)

val set_activity : t -> activity option -> unit
(** [set_activity t activity] changes the presented long-running operation
    and resets its animation. Setting the current activity again preserves the
    current animation frame. *)

val animation_frame : t -> int
(** [animation_frame t] returns the current loader frame. *)

val advance_animation_frame : t -> unit
(** [advance_animation_frame t] advances the loader animation by one frame. *)

(** [active_page t] indicates which full-screen page is currently shown.
    Initially this is always {!Page_id.Chat}. *)
val active_page : t -> Page_id.t

(** [set_active_page t page] changes the active page. *)
val set_active_page : t -> Page_id.t -> unit

val shell_security_page : t -> Shell_security_page_state.t
val shell_security_snapshot : t -> Shell_security_page_state.snapshot
val set_shell_security_snapshot : t -> Shell_security_page_state.snapshot -> unit
val shell_security_tab : t -> Shell_security_page_state.tab
val set_shell_security_tab : t -> Shell_security_page_state.tab -> unit
val shell_security_scroll_box : t -> Notty_scroll_box.t
val shell_approval_modal : t -> Shell_security_page_state.approval_modal option
val shell_grant_revoke_modal
  : t -> Shell_security_page_state.grant_revoke_modal option
val moderator_modal : t -> Shell_security_page_state.moderator_modal option
val selected_shell_grant_id : t -> string option
val move_shell_grant_selection : t -> int -> unit
val open_shell_approval_modal
  :  t
  -> request:Shell_runtime.Approval_broker.ui_request
  -> queue_count:int
  -> unit
val close_shell_approval_modal : t -> unit
val open_shell_grant_revoke_modal : t -> unit
val close_shell_grant_revoke_modal : t -> unit
val mark_shell_grant_revoking : t -> generation:int -> grant_id:string -> unit
val fail_shell_grant_revoke
  : t -> generation:int -> grant_id:string -> string -> unit
val open_moderator_modal
  :  t
  -> Chat_response.In_memory_stream.pending_ui_request
  -> unit
val close_moderator_modal : t -> unit
val set_moderator_validation_error : t -> string option -> unit
val shell_audit_load_state : t -> Shell_security_page_state.audit_load_state
val selected_shell_audit_request_id : t -> string option
val begin_shell_management_load : t -> int
val finish_shell_management_load
  :  t
  -> generation:int
  -> Shell_security_page_state.audit_page
  -> bool
val fail_shell_management_load : t -> generation:int -> string -> bool
val move_shell_audit_selection : t -> int -> unit
val set_shell_approval_choice : t -> Shell_security_page_state.approval_choice -> unit
val toggle_shell_approval_more_options : t -> unit
val toggle_shell_approval_details : t -> unit
val set_shell_approval_stage : t -> Shell_security_page_state.approval_stage -> unit
val shell_interaction_id : t -> string option
val shell_interaction_uses_cursor : t -> bool

(** [chat_page t] returns the chat page's renderer state/caches.  This is
    the authoritative location for chat-only scroll state and render
    caches. *)
val chat_page : t -> Chat_page_state.t
val agent_page : t -> Agent_page_state.t

(** [scroll_box t] is the chat page's scroll box used by history
    virtualisation and scrolling commands. *)
val scroll_box : t -> Notty_scroll_box.t
val agent_scroll_box : t -> Notty_scroll_box.t
val agent_auto_follow : t -> bool
(** [agent_auto_follow t] is [true] while Agent output remains pinned to the
    bottom as progress arrives. *)

val set_agent_auto_follow : t -> bool -> unit
(** [set_agent_auto_follow t enabled] changes only the Agent page's follow
    behavior and never affects Chat scrolling. *)

val agent_call_started
  :  t
  -> call_id:string
  -> name:string
  -> kind:[ `Function | `Custom ]
  -> payload:string
  -> agent_page_kind:Chat_response.Tool_execution_event.agent_page_kind
  -> bool
(** [agent_call_started t ~call_id ~name ~kind ~payload ~agent_page_kind] adds
    a transient Agent-page call. [payload] is the exact function arguments or
    custom-tool input.
    It preserves start order and selects the first call without opening Agent.
    Duplicate active IDs return [true] idempotently without replacing
    metadata. Terminal IDs return [false]. Canonical history is unchanged. *)

val agent_call_progress
  :  t
  -> call_id:string
  -> Ochat_function.Progress.t
  -> bool
(** [agent_call_progress t ~call_id progress] updates an active call without
    changing canonical history. Unknown or terminal calls return [false].

    [Append] coalesces with an immediately preceding entry on the same
    channel. [Replace] replaces the latest replaceable entry on its channel,
    or creates one. Retention is bounded to 1,000,000 bytes per call and
    16,000,000 bytes globally; oldest progress is discarded first and retained
    oversized text is a valid UTF-8 suffix. *)

val agent_call_trace : t -> call_id:string -> Ochat_function.Trace.t -> bool
(** [agent_call_trace t ~call_id trace] updates structured nested-tool
    activity under the active outer call. Unknown outer or nested call IDs are
    rejected. Structured traces remain transient and never modify history. *)

val agent_call_finished
  :  t
  -> call_id:string
  -> outcome:Ochat_function.Trace.outcome
  -> output:Openai.Responses.Tool_output.Output.t option
  -> bool
(** [agent_call_finished t ~call_id ~outcome ~output] marks a current-operation
    call terminal and retains it for display until {!clear_agent_calls}.
    Selection and active page remain unchanged. Duplicate/unknown completions,
    restarts, progress, and traces for terminal calls are rejected. [output] is
    a non-authoritative transient projection. *)

val clear_agent_calls : t -> unit
(** [clear_agent_calls t] clears current-operation Agent calls and transient
    Chat tool-completion decorations, resets Agent scrolling, then activates
    Chat. Canonical history is unchanged. *)

val active_agent_calls : t -> Agent_page_state.call list
(** [active_agent_calls t] returns calls in accepted start order. *)

val selected_agent_call : t -> Agent_page_state.call option
(** [selected_agent_call t] returns the selected active call. It is [None]
    exactly when no calls are active. *)

val select_next_agent_call : t -> unit
val select_previous_agent_call : t -> unit
val agent_call_id : Agent_page_state.call -> string
val agent_call_name : Agent_page_state.call -> string
val agent_call_kind : Agent_page_state.call -> [ `Function | `Custom ]
val agent_call_payload : Agent_page_state.call -> string
val agent_call_agent_page_kind
  :  Agent_page_state.call
  -> Chat_response.Tool_execution_event.agent_page_kind
val agent_call_start_order : Agent_page_state.call -> int
val agent_call_progress_entries
  :  Agent_page_state.call
  -> Agent_page_state.progress_entry list
(** [agent_call_progress_entries call] returns retained display entries from
    oldest to newest. *)

val agent_call_is_truncated : Agent_page_state.call -> bool
(** [agent_call_is_truncated call] remains [true] after any progress is
    discarded to satisfy a retention limit. *)
val agent_call_outcome
  :  Agent_page_state.call
  -> Ochat_function.Trace.outcome option
val agent_call_output
  :  Agent_page_state.call
  -> Openai.Responses.Tool_output.Output.t option
val progress_entry_text_view
  :  Agent_page_state.progress_entry
  -> (Ochat_function.Progress.channel * string) option

val progress_entry_text : Agent_page_state.progress_entry -> string
(** [progress_entry_text entry] returns text used by aggregate tests and
    diagnostics. Nested tool entries include their payload, progress, and
    transient returned output. *)

val progress_entry_tool_view
  :  Agent_page_state.progress_entry
  -> (string
      * string
      * Ochat_function.Trace.tool_kind
      * string
      * (Ochat_function.Progress.channel * string) list
      * Ochat_function.Trace.outcome option
      * Openai.Responses.Tool_output.Output.t option)
       option
(** The entry views expose either a text message or a structured nested tool
    invocation with its ordered progress and terminal display projection. *)

val agent_call_render_blocks
  :  Agent_page_state.call
  -> Agent_page_state.render_block list
(** [agent_call_render_blocks call] returns stable-ID display blocks in document
    order. Revisions change only when the corresponding rendered content
    changes. *)

val agent_render_block_id : Agent_page_state.render_block -> int
val agent_render_block_revision : Agent_page_state.render_block -> int
val agent_render_block_view
  :  Agent_page_state.render_block
  -> Agent_page_state.render_block_view

val prepare_agent_render_width : Agent_page_state.call -> width:int -> unit
(** [prepare_agent_render_width call ~width] invalidates wrapping-dependent
    caches only when [width] changes. *)

val find_agent_render_cache
  :  Agent_page_state.call
  -> Agent_page_state.render_block
  -> (Notty.I.t * int) option

val set_agent_render_cache
  :  Agent_page_state.call
  -> Agent_page_state.render_block
  -> image:Notty.I.t
  -> unit

val prune_agent_render_cache
  :  Agent_page_state.call
  -> block_ids:int list
  -> unit

val agent_render_block_ids : Agent_page_state.call -> int array
val agent_render_block_revisions : Agent_page_state.call -> int array
val agent_render_geometry
  :  Agent_page_state.call
  -> Renderer_virtual_list.Geometry.t
val agent_render_heights : Agent_page_state.call -> int array
val agent_render_prefix : Agent_page_state.call -> int array

val set_agent_render_geometry
  :  Agent_page_state.call
  -> block_ids:int array
  -> revisions:int array
  -> heights:int array
  -> prefix:int array
  -> unit
(** The render-cache helpers support Agent message virtualization. Callers
    outside the Agent renderer and deterministic renderer tests should not
    mutate them. *)

(** [selected_msg t] resolves the stable selected projected-row ID to its
    current zero-based render index. The returned value is an ephemeral layout
    coordinate and must not be used to mutate canonical history. *)
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

(** [clear_last_search t] clears the last search query and direction. *)
val clear_last_search : t -> unit

(** [set_selection_anchor t p] marks byte‐offset [p] as the start of a text
    selection.  Calling the function implicitly enables selection mode. *)
val set_selection_anchor : t -> int -> unit

(** [selection_active t] is [true] when a selection anchor is set. *)
val selection_active : t -> bool

(** [messages t] returns the list of renderable messages in top-down order.
    Each element is a [(role, text)] pair as defined in {!Types.message}. *)
val messages : t -> message list
val set_messages : t -> message list -> unit
(** [set_messages t messages] structurally replaces visible Chat messages. *)

val reconcile_messages : t -> message list -> unit
(** [reconcile_messages t messages] replaces visible Chat messages while
    retaining compatible prefix caches and geometry. When Chat is manually
    scrolled, it remaps the top-row anchor by stable row ID, falling back to
    message occurrence only for legacy projections, and restores that semantic
    position in the new transcript. *)

val reconcile_projected_messages
  :  t
  -> rows:Projected_message.t list
  -> messages:message list
  -> unit
(** [reconcile_projected_messages t ~rows ~messages] atomically replaces an
    identity-bearing projection. When manually scrolled, it captures the
    stable row and within-row viewport anchor before either rows or geometry
    change, then restores it after both projections reconcile. *)

val capture_resize_anchor : t -> viewport_height:int -> Resize_anchor.t
(** [capture_resize_anchor t ~viewport_height] captures bottom-follow intent or
    the stable projected row identity, revision, intra-row offset, and screen
    placement of a manual viewport. *)

val restore_resize_anchor
  :  t
  -> viewport_height:int
  -> Resize_anchor.t
  -> Resize_anchor.resolution
(** [restore_resize_anchor t ~viewport_height anchor] resolves [anchor] against
    the current projection and geometry on the UI domain. Removed or revised
    rows are repaired to the nearest unchanged captured neighbor, preferring
    the older neighbor at equal distance. *)

val render_messages : t -> message array
(** [render_messages t] is the array-backed Chat rendering projection. It
    shares message strings with {!messages} and supports constant-time
    viewport lookup. *)

val transcript_generation : t -> int
(** [transcript_generation t] changes whenever the visible transcript is
    structurally replaced or extended. *)

val message_revision : t -> idx:int -> int option
(** [message_revision t ~idx] identifies render-affecting content at [idx]
    within the current transcript generation. Detached render results must
    match both identities before the UI reducer may commit them. *)

val render_generation : t -> int
(** [render_generation t] changes whenever visible message structure, text, or
    rendering metadata changes. Startup background warming aborts when this
    value changes. *)

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
val tool_output_for_row
  :  t
  -> id:Projected_message.Id.t
  -> Types.tool_output_kind option

val set_tool_output_kind
  :  t
  -> idx:int
  -> Types.tool_output_kind
  -> bool
(** [set_tool_output_kind t ~idx kind] updates render metadata and its message
    revision when [kind] changes. It returns whether state changed. *)

val set_tool_output_kind_for_row
  :  t
  -> id:Projected_message.Id.t
  -> Types.tool_output_kind
  -> bool

val mark_tool_call_finished
  :  t
  -> call_id:string
  -> outcome:Ochat_function.Trace.outcome
  -> bool
(** [mark_tool_call_finished t ~call_id ~outcome] records transient completion
    metadata for the corresponding Chat tool-call message. It never changes
    message text or canonical history. *)

val tool_call_outcome_for_message
  :  t
  -> idx:int
  -> Ochat_function.Trace.outcome option
(** [tool_call_outcome_for_message t ~idx] returns transient terminal metadata
    for a Chat tool-call message index. *)

val tool_call_outcome_for_row
  :  t
  -> id:Projected_message.Id.t
  -> Ochat_function.Trace.outcome option

val clear_tool_call_outcomes : t -> unit
(** [clear_tool_call_outcomes t] clears transient Chat completion decorations
    and invalidates Chat rendering caches. Canonical messages and history are
    unchanged. *)

(** [auto_follow t] is the auto-scroll flag.  When [true] the view follows
    new incoming messages automatically; otherwise the scroll position stays
    unchanged. *)
val auto_follow : t -> bool

val chat_max_scroll : t -> viewport_height:int -> int
(** [chat_max_scroll t ~viewport_height] returns the authoritative bottom
    offset from virtual history geometry. *)

val prepared_row_range : t -> History_chunk.Range.t option
(** [prepared_row_range t] returns the exact row range published by the
    active corridor. *)

val prepared_scroll_interval
  : t -> viewport_height:int -> (int * int) option
(** [prepared_scroll_interval t ~viewport_height] returns the inclusive
    scroll interval whose complete viewport lies in the active exact
    corridor. *)

val prepared_boundary_distances
  : t -> viewport_height:int -> prepared_boundary_distances option
(** [prepared_boundary_distances t ~viewport_height] returns the current
    viewport's distance in terminal rows from each exact corridor boundary. *)

val requested_scroll_is_prepared
  : t -> viewport_height:int -> requested_scroll:int -> bool
(** [requested_scroll_is_prepared t ~viewport_height ~requested_scroll] is
    [true] when the requested complete viewport lies in the active corridor. *)

val reveal_prepared_row
  :  t
  -> viewport_height:int
  -> id:Projected_message.Id.t
  -> placement:Chat_page_state.Destination.placement
  -> bool
(** [reveal_prepared_row t ...] positions an already exact corridor row
    without rendering. It returns [false] when the destination viewport is
    outside the published corridor. *)

val scroll_chat : t -> viewport_height:int -> int -> chat_scroll_result
(** [scroll_chat t ~viewport_height delta] applies a geometry-only scroll.
    Corridor movement is clamped to the exact published interval; Warm
    movement retains whole-history behavior. Rejected movement is discarded. *)

val scroll_chat_by : t -> viewport_height:int -> int -> bool
(** [scroll_chat_by t ~viewport_height delta] scrolls against virtual history
    geometry and enables auto-follow only at the current bottom. *)

val follow_chat_bottom : t -> viewport_height:int -> unit
(** [follow_chat_bottom t ~viewport_height] enables auto-follow and commits
    the geometry-derived bottom offset. *)

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

(** [set_last_search t ~query ~dir] updates {!last_search} to [query] and [dir]. *)
val set_last_search : t -> query:string -> dir:search_dir -> unit

val selected_render_revision : t -> string
(** [selected_render_revision t] identifies search-dependent selected-message
    rendering state for image-cache validation. *)

val chat_scroll_direction : t -> scroll_direction
val set_chat_scroll_direction : t -> scroll_direction -> unit

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

(** [add_history_item t item] appends identity-bearing canonical [item] and
    returns the mutated model. Unlike the
    [Add_user_message] patch the helper bypasses any UI manipulation and
    does not touch {!messages}. *)
val add_history_item : t -> History_entry.t -> t

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

(** [rebuild_tool_output_index_for_items t items] recomputes
    {!tool_output_by_index} from [items].

    Use this when the visible transcript is projected from moderator-visible
    history rather than directly from canonical {!history_items}. *)
val rebuild_tool_output_index_for_items : t -> History_entry.t list -> unit

(** [clamp_selected_message t] keeps {!selected_msg} within the bounds of
    {!messages}.

    If there are no visible messages, the selection is cleared. *)
val clamp_selected_message : t -> unit

(** {1 Rendering cache helpers}

    Low-level UI-domain helpers for the renderer. Detached worker domains
    consume immutable {!Chat_message_render_job.t} snapshots and return
    immutable results; they never access these caches, the virtual geometry,
    scroll boxes, or terminal state. Callers outside the rendering path should
    not need these helpers. *)

(** [active_history_width t] is the width (in terminal cells) for which the
    chat page's cached message images and heights are currently valid, or
    [None] if no cache has been built yet. *)
val active_history_width : t -> int option

(** [set_active_history_width t w] updates {!active_history_width}. Calling the
    function does **not** invalidate individual entries – that is done by
    the renderer which knows whether a global flush or targeted
    invalidations are cheaper. *)
val set_active_history_width : t -> int option -> unit

(** [clear_all_img_caches t] completely clears {!msg_img_cache} and the
    associated height caches.  Use this when the terminal has been resized
    and *all* cached images are now stale. *)
val clear_all_img_caches : t -> unit

(** [clear_img_caches_preserving_heights t] clears every Chat message image
    while retaining current heights and prefix sums as provisional geometry.
    Use this only when transcript shape is unchanged. If rendering width
    changes, capture any required manual viewport anchor before calling it. *)
val clear_img_caches_preserving_heights : t -> unit

(** [invalidate_img_cache_index t ~idx] removes the cache entry for the
    message at index [idx].  Called whenever the underlying text changes
    (e.g. when streaming deltas arrive). *)
val invalidate_img_cache_index : t -> idx:int -> unit

(** [find_img_cache t ~id ~revision] returns the cached render for [id] when
    it belongs to [revision]. *)
val find_img_cache
  :  t
  -> id:Projected_message.Id.t
  -> revision:int
  -> msg_img_cache option

(** [set_img_cache t ~id entry] stores [entry] for the projected row [id]. *)
val set_img_cache : t -> id:Projected_message.Id.t -> msg_img_cache -> unit

val find_cached_width_row
  :  t
  -> width:int
  -> id:Projected_message.Id.t
  -> revision:int
  -> msg_img_cache option
(** [find_cached_width_row t ~width ~id ~revision] returns an exact row from
    active state or a retained width snapshot without changing active state. *)

val find_compatible_layout_row
  :  t
  -> width:int
  -> id:Projected_message.Id.t
  -> revision:int
  -> msg_img_cache option
(** [find_compatible_layout_row t ~width ~id ~revision] returns a current row
    whose layout plan proves unchanged wrap boundaries at [width]. *)

val find_semantic_cache
  :  t
  -> id:Projected_message.Id.t
  -> revision:int
  -> role:string
  -> text:string
  -> tool_output:Types.tool_output_kind option
  -> msg_semantic_cache option
(** [find_semantic_cache t ...] returns a width-independent prepared message
    and worker-produced highlight bindings for the current row revision. *)

(** [commit_render_result t result] validates [result] against current
    transcript, width, message metadata, selection, and search state, then
    atomically updates the corresponding image variant and exact virtual
    geometry. Geometry generation prevents a delayed result from committing
    after resize, transcript replacement, or another incompatible geometry
    transition. It returns [false] for stale results. *)
val commit_render_result : t -> Chat_message_render_job.result -> bool

val commit_width_preparation_result
  :  t
  -> theme_generation:int
  -> grammar_generation:int
  -> Chat_message_render_job.result
  -> bool
(** [commit_width_preparation_result t ... result] stores width-specific output
    only after validating the current preparation and row identity. A stale
    width may still contribute current immutable semantic products. *)

(** [take_and_clear_dirty_height_rows t] returns and clears projected row IDs
    and revisions whose heights may be stale. *)
val take_and_clear_dirty_height_rows
  : t -> (Projected_message.Id.t * int) list

(** [msg_heights t] are the cached rendered heights (in cells) for the chat
    transcript at {!active_history_width}. *)
val msg_heights : t -> int array

(** [height_prefix t] are prefix sums of {!msg_heights}. *)
val height_prefix : t -> int array

val chat_render_geometry : t -> Renderer_virtual_list.Geometry.t
(** [chat_render_geometry t] is the shared virtual-list geometry used by the
    Chat renderer. *)

val set_chat_render_geometry
  :  t
  -> heights:int array
  -> prefix:int array
  -> unit
(** [set_chat_render_geometry t ~heights ~prefix] atomically replaces coherent
    Chat message geometry. *)


(** {1 Staged identity-bearing Chat projection} *)

val projected_rows : t -> Projected_message.t array
val projected_messages : t -> Types.message list
val projected_index : t -> id:Projected_message.Id.t -> int option
val projected_row : t -> id:Projected_message.Id.t -> Projected_message.t option
val render_row_identity
  : t -> idx:int -> (Projected_message.Id.t * int) option
val render_index_by_id : t -> id:Projected_message.Id.t -> int option
val reconcile_projected_rows : t -> Projected_message.t list -> unit
val selected_projected_id : t -> Projected_message.Id.t option
val select_projected : t -> Projected_message.Id.t option -> unit
val request_projected_reveal : t -> id:Projected_message.Id.t -> unit
val take_projected_reveal_request : t -> Projected_message.Id.t option
val set_projected_height : t -> id:Projected_message.Id.t -> height:int -> unit
val projected_height : t -> id:Projected_message.Id.t -> int option

val selected_projected_row : t -> Projected_message.t option
val delete_selected_canonical_entry : t -> [ `Deleted | `Rejected of string ]
