(** Virtualised history viewport renderer.

    This module renders the scrollable transcript area of the chat page.  It is
    “virtualised” in the sense that it only renders the messages that can
    intersect the current [height]-row viewport, while still returning an image
    whose {e logical} height matches the full transcript.  The caller can then
    feed the result to {!Notty_scroll_box.set_content} and let
    {!Notty_scroll_box.render} crop the visible window.

    The implementation relies on (and mutates) renderer caches stored in
    {!Chat_tui.Model.Chat_page_state.t}:

    {ul
    {- a per-message image cache keyed by stable projected row ID and revision;}
    {- cached per-message heights and their prefix sums used to translate
       scroll offsets into visible indices.}}

    The module may correct the Chat scroll offset to preserve a manual viewport
    anchor as estimates become exact or to satisfy a pending search reveal. *)

type render_plan =
  { image : Notty.I.t
  ; viewport : Renderer_virtual_list.Viewport.t
  ; top_visible_idx : int option
  ; prefetch_indices : int list
  }

type rendered =
  | Ready of Notty.I.t
  | Pending of Notty.I.t

(** [initialize_geometry ~geometry ~messages ~width] reconciles provisional
    row geometry with [messages] without rendering rows. *)
val initialize_geometry
  :  geometry:Renderer_virtual_list.Geometry.t
  -> messages:Types.message array
  -> width:int
  -> unit

(** [prefetch_candidate_indices ~model ~viewport ~height] returns estimated
    off-screen indices covering three viewports in the current scroll
    direction and one viewport behind, nearest first. *)
val prefetch_candidate_indices
  :  model:Model.t
  -> viewport:Renderer_virtual_list.Viewport.t
  -> height:int
  -> int list

(** [render ~model ~width ~height ~messages ~selected_idx ~render_message]
    measures the demanded region and returns a coherent render plan.

    @param model Mutable model holding caches and the current scroll position.
    @param width Target width in terminal cells. The returned image is
           [hsnap]-ed to this width.
    @param height Height of the scroll viewport in terminal cells.
    @param messages Transcript to render (top-to-bottom).
    @param selected_idx Zero-based index of the selected message (Normal mode),
           or [None] when nothing is selected.
    @param render_message Callback that renders one message. It is expected to
           produce an image sized for [width] (typically by [hsnap]-ing).

    The function updates the model's cached message images and height arrays.
    When [selected_idx] is set, the selected variant of the corresponding
    message is computed lazily and cached.

    The image includes transparent padding above and below the visible block
    so that its logical height matches the provisional transcript height.
    Visible items always have exact rich-rendered geometry. *)
val render
  :  model:Model.t
  -> width:int
  -> height:int
  -> messages:Types.message array
  -> selected_idx:int option
  -> render_message:(idx:int -> selected:bool -> Types.message -> Notty.I.t)
  -> render_plan

(** [render_with_anchor ~initial_anchor ...] behaves like {!render}, but uses
    an anchor captured from compatible transcript geometry before a global
    presentation invalidation such as a width change. *)
val render_with_anchor
  :  initial_anchor:Renderer_virtual_list.Anchor.t
  -> model:Model.t
  -> width:int
  -> height:int
  -> messages:Types.message array
  -> selected_idx:int option
  -> render_message:(idx:int -> selected:bool -> Types.message -> Notty.I.t)
  -> render_plan

val render_async
  :  model:Model.t
  -> width:int
  -> height:int
  -> messages:Types.message array
  -> selected_idx:int option
  -> render_message:(idx:int -> selected:bool -> Types.message -> rendered)
  -> render_plan

(** [render_async] and [render_async_with_anchor] keep [Pending] placeholders
    outside the rich-image cache and leave their geometry provisional.
    This lower-level API remains available for detached tests. The production
    Chat viewport uses {!render} so visible messages are always full fidelity. *)
val render_async_with_anchor
  :  initial_anchor:Renderer_virtual_list.Anchor.t
  -> model:Model.t
  -> width:int
  -> height:int
  -> messages:Types.message array
  -> selected_idx:int option
  -> render_message:(idx:int -> selected:bool -> Types.message -> rendered)
  -> render_plan
