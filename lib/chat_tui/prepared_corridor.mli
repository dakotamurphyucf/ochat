module Range = History_chunk.Range

type direction =
  | Toward_older
  | Toward_newer

type policy

val create_policy : ?older_viewports:int -> ?newer_viewports:int -> unit -> policy
val default_policy : policy

type batch_class =
  | Visible
  | Directional
  | Guard
  | Remaining

type batch =
  { index : int
  ; rows : Range.t
  ; class_ : batch_class
  ; distance : int
  }

type t =
  { viewport : Renderer_virtual_list.Viewport.t
  ; visible_rows : Range.t
  ; desired_rows : Range.t
  ; scheduled_rows : Range.t
  ; batches : batch list
  }

(** [plan ...] deterministically ranks every 16-row batch around an exact
    predicted viewport. The initial scheduled corridor is bounded by the
    policy independently of total history length. *)
val plan
  :  ?policy:policy
  -> geometry:Renderer_virtual_list.Geometry.Snapshot.t
  -> requested_scroll:int
  -> viewport_height:int
  -> follow_bottom:bool
  -> direction:direction
  -> unit
  -> t
