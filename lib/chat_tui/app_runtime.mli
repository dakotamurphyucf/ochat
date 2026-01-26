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

(** A snapshot of the editor state that is submitted to the assistant. *)
type submit_request =
  { text : string
  ; draft_mode : Model.draft_mode
  }

(** Work that was requested while another operation is running. *)
type queued_action =
  | Submit of submit_request
  | Compact

(** Runtime container used by the app reducer and its helper modules. *)
type t =
  { model : Model.t
  ; mutable op : op option
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
  }

(** [create ~model] creates a fresh runtime container for [model].

    @param model Mutable UI model that the reducer will mutate.

    The returned value starts with no active operation, an empty pending queue,
    and a fresh operation id counter.

    Example:
    {[
      let runtime = Chat_tui.App_runtime.create ~model in
      ignore (Chat_tui.App_runtime.alloc_op_id runtime : int)
    ]} *)
val create : model:Model.t -> t

(** [alloc_op_id t] allocates a fresh operation id.

    The returned id is used to tag internal events so that the reducer can
    ignore stale messages from previously cancelled operations.

    @param t Runtime container whose internal counter should advance. *)
val alloc_op_id : t -> int
