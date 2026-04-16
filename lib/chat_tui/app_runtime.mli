(** Mutable runtime state for {!Chat_tui.App}'s event loop.

    This module defines the runtime state record that is threaded through
    {!Chat_tui.App_reducer.run}.  It captures:
    {ul
    {- The current in-flight operation (streaming or compaction), if any.}
    {- A FIFO queue of user actions that are deferred while an operation runs.}
    {- Cancellation requests that arrive while an operation is still
       "starting" (before the worker fibre has published its {!Eio.Switch.t}).}}

    The types are intentionally low-level because multiple helper modules
    coordinate via mutable fields.  Treat this module as internal
    plumbing; it is exposed primarily to keep {!Chat_tui.App} small and to
    support white-box tests of the event loop. *)

(** A currently running (or about-to-run) background operation. *)
type op =
  | Streaming of
      { sw : Eio.Switch.t
      ; id : int
      }
  (** An assistant response is currently streaming.

          The operation can be cancelled by failing [sw]. *)
  | Compacting of
      { sw : Eio.Switch.t
      ; id : int
      } (** History compaction is currently running. *)
  | Starting_streaming of { id : int }
  (** A streaming worker has been forked but has not yet published its
          switch. *)
  | Starting_compaction of { id : int }
  (** A compaction worker has been forked but has not yet published its
          switch. *)

(** Tracking state for background type-ahead completion work. *)
type typeahead_op =
  | Typeahead of
      { sw : Eio.Switch.t
      ; id : int
      }
  (** A type-ahead worker is currently running.

      The operation can be cancelled by failing [sw]. *)
  | Starting_typeahead of { id : int }
  (** A type-ahead worker has been forked but has not yet published its
      switch. *)

(** A snapshot of the editor state that is submitted to the assistant. *)
type submit_request =
  { text : string
  ; draft_mode : Model.draft_mode
  }

(** Why the session controller wants to start a turn. *)
type turn_start_reason =
  | User_submit
  | Moderator_request
  | Idle_followup

(** A user-authored steering note captured during an active turn. *)
type deferred_user_note = { text : string }

(** Session-controller state that sits above the foreground operation state. *)
type session_controller_state =
  { mutable moderator_dirty : bool
  ; deferred_user_notes : deferred_user_note Core.Queue.t
  ; mutable pending_turn_request : turn_start_reason option
  }

(** Work that was requested while another operation is running. *)
type queued_action =
  | Submit of submit_request
  | Compact

(** Runtime container used by the app reducer and its helper modules. *)
type t =
  { model : Model.t
  ; mutable op : op option
  ; mutable typeahead_op : typeahead_op option
  ; moderator : Chat_response.In_memory_stream.moderator option
  ; session_controller : session_controller_state
  ; shown_notice_keys : string Core.Hash_set.t
  ; mutable active_turn_start_reason : turn_start_reason option
  ; mutable halted_reason : string option
  ; pending : queued_action Core.Queue.t
  ; quit_via_esc : bool ref
    (** [true] iff the user hit [Esc] while idle, causing the app to exit. *)
  ; mutable next_op_id : int
    (** Monotonically increasing operation id used to tag events. *)
  ; mutable cancel_streaming_on_start : bool
    (** Records a cancellation request that arrived while the state was
          [Starting_streaming]. *)
  ; mutable cancel_compaction_on_start : bool
    (** Records a cancellation request that arrived while the state was
          [Starting_compaction]. *)
  ; mutable cancel_typeahead_on_start : bool
    (** Records a cancellation request that arrived while the state was
        [Starting_typeahead]. *)
  }

val visible_history_items_of_history
  :  t
  -> Openai.Responses.Item.t list
  -> Openai.Responses.Item.t list

val visible_messages_of_history : t -> Openai.Responses.Item.t list -> Types.message list
val refresh_messages : t -> unit
val moderator_snapshot : t -> (Session.Moderator_snapshot.t option, string) result

(** [create ?moderator ?halted_reason ~model ()] creates a fresh runtime container for [model].

    @param model Mutable UI model that the reducer will mutate.

    The returned value starts with no active operation, an empty pending queue,
    and a fresh operation id counter.

    Example:
    {[
      let runtime = Chat_tui.App_runtime.create ~model () in
      ignore (Chat_tui.App_runtime.alloc_op_id runtime : int)
    ]} *)
val create
  :  ?moderator:Chat_response.In_memory_stream.moderator
  -> ?halted_reason:string
  -> model:Model.t
  -> unit
  -> t

(** [alloc_op_id t] allocates a fresh operation id.

    The returned id is used to tag internal events so that the reducer can
    ignore stale messages from previously cancelled operations.

    @param t Runtime container whose internal counter should advance. *)
val alloc_op_id : t -> int

val has_active_turn : t -> bool
val has_active_op : t -> bool
val is_idle : t -> bool
val may_start_turn_now : t -> bool
val is_moderator_dirty : t -> bool
val has_pending_turn_request : t -> bool
val string_of_turn_start_reason : turn_start_reason -> string
val active_turn_start_reason : t -> turn_start_reason option
val mark_moderator_dirty : t -> unit
val clear_moderator_dirty : t -> unit
val request_turn_start : t -> turn_start_reason -> unit
val clear_pending_turn_request : t -> unit
val dequeue_pending_turn_request : t -> turn_start_reason option
val set_active_turn_start_reason : t -> turn_start_reason -> unit
val clear_active_turn_start_reason : t -> unit
val add_placeholder_message : t -> role:string -> text:string -> unit
val add_system_notice : t -> string -> unit
val add_system_notice_once : t -> key:string -> string -> bool
val enqueue_deferred_user_note : t -> submit_request -> bool
val has_deferred_user_notes : t -> bool
val dequeue_deferred_user_notes : t -> deferred_user_note list
val render_deferred_user_note : deferred_user_note -> string
val render_deferred_user_notes : deferred_user_note list -> string option
val consume_deferred_user_notes_for_safe_point : t -> string option
val safe_point_input_source : t -> Chat_response.In_memory_stream.Safe_point_input.t
