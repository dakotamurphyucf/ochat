(** Chat page renderer.

    This page is the primary full-screen view: a scrollable transcript, an
    optional sticky header, a status bar, and the input box.  The implementation
    composes the smaller renderer components:

    {ul
    {- {!Renderer_component_history} for the scrollable transcript;}
    {- {!Renderer_component_message} for message framing and highlighting;}
    {- {!Renderer_component_status_bar} for mode hints;}
    {- {!Renderer_component_input_box} for the prompt box.}} *)

(** [render ~size ~model] renders the chat page.

    @param size Terminal size [(width, height)] in cells.
    @param model Current UI state. The renderer updates caches in [model] (see
           {!Chat_tui.Renderer}).

    Type-ahead preview: when Insert mode is active and
    {!Chat_tui.Model.typeahead_preview_open} is true, the renderer overlays a
    preview popup inside the transcript region (using Notty's overlay operator)
    so the input box geometry and scroll-box state do not change.

    Returns [(img, (cx, cy))] where [(cx, cy)] is the absolute cursor position
    for the input box. *)
val render : size:int * int -> model:Model.t -> Notty.I.t * (int * int)

(** [render_with_layout ~size ~layout ~model] uses one page partition for
    both the history viewport and input box. *)
val render_with_layout
  :  size:int * int
  -> layout:Chat_page_layout.t
  -> model:Model.t
  -> Notty.I.t * (int * int)

(** [prepare_startup_history ~size ~model] establishes the active history
    width and estimated geometry without rendering transcript rows. *)
val prepare_startup_history : size:int * int -> model:Model.t -> unit

(** [complete_history_cache ~size ~model] publishes a complete transcript
    image when every current row has a valid image at the active width. *)
val complete_history_cache : size:int * int -> model:Model.t -> unit

(** [publish_startup_history ~size ~model] atomically rebuilds exact geometry
    from completed startup row caches and publishes the canonical history
    image. Returns [true] when publication succeeds. *)
val publish_startup_history : size:int * int -> model:Model.t -> bool

(** [warm_history_synchronously ~size ~model] fills missing row images in
    dirty history chunks and then republishes those chunks. *)
val warm_history_synchronously : size:int * int -> model:Model.t -> unit

(** [relayout_history_synchronously ~size ~model] computes exact row images and
    heights for a new active width without constructing the complete history
    composition. *)
val relayout_history_synchronously : size:int * int -> model:Model.t -> unit

(** [relayout_history_with_layout_synchronously ~width ~layout ~model]
    relayouts history using the same page partition as the next frame.

    Progressive preparation is the primary uncached-width path. This operation
    visits every transcript row synchronously and remains only as the explicit
    last-resort fallback and deterministic parity baseline. Exact recent-width
    snapshots bypass both paths. *)
val relayout_history_with_layout_synchronously
  :  width:int
  -> layout:Chat_page_layout.t
  -> model:Model.t
  -> unit

(** [startup_background_jobs ~theme_generation ~grammar_generation ~model]
    snapshots unselected
    background jobs for messages that lack a full-fidelity base image at the
    established Chat width. *)
val startup_background_jobs
  :  theme_generation:int
  -> grammar_generation:int
  -> model:Model.t
  -> Chat_message_render_job.t list

(** [target_width_jobs ~indices ~priority ~model] prepares current rows for the
    isolated target width. Exact target rows and proven compatible layouts are
    committed immediately to the preparation; remaining jobs carry validated
    width-independent semantic seeds for detached rendering. *)
val target_width_jobs
  :  indices:int list
  -> priority:Chat_message_render_job.Priority.t
  -> model:Model.t
  -> Chat_message_render_job.t list

(** [initial_target_width_batches ?policy ~model ()] predicts the captured target
    viewport, records its bounded 16-row corridor, and returns visible,
    directional, then guard batches ready for worker submission. *)
val initial_target_width_batches
  :  ?policy:Prepared_corridor.policy
  -> model:Model.t
  -> unit
  -> (Prepared_corridor.t * (int * Chat_render_worker.ranked_job list) list) option

(** [current_target_width_batches ?policy ~model ()] replans bounded target
    batches around the active corridor viewport without rendering rows. *)
val current_target_width_batches
  :  ?policy:Prepared_corridor.policy
  -> model:Model.t
  -> unit
  -> (Prepared_corridor.t * (int * Chat_render_worker.ranked_job list) list) option

(** [destination_target_width_batches ?policy ~model ()] plans bounded work
    around the current stable nonlocal destination without rendering rows. *)
val destination_target_width_batches
  :  ?policy:Prepared_corridor.policy
  -> model:Model.t
  -> unit
  -> (Prepared_corridor.t * (int * Chat_render_worker.ranked_job list) list) option

(** [remaining_target_width_batches ?policy ~model ()] returns every current
    16-row batch in visible-first, directional, guard, then background order.
    Exact rows are omitted without rendering. *)
val remaining_target_width_batches
  :  ?policy:Prepared_corridor.policy
  -> model:Model.t
  -> unit
  -> (int * Chat_render_worker.ranked_job list) list option

(** [promote_width_preparation ~size ~model ~request_generation] atomically
    installs a globally exact target width, constructs its canonical chunk
    tree/root, restores the stable viewport, and transitions to [Warm]. *)
val promote_width_preparation
  :  size:int * int
  -> model:Model.t
  -> request_generation:int
  -> bool

module For_testing : sig
  val capture_materialized_indices : (unit -> 'a) -> 'a * int list
  val warm_dirty_chunks_synchronously : size:int * int -> model:Model.t -> unit
  val history_chunk_count : Model.t -> int
end
