(** Efficient vertical lists with cached row geometry.

    Renderers retain page-specific image caches and use this module for shared
    height maintenance, viewport selection, and sparse image composition. *)

module Geometry : sig
  type t

  module Snapshot : sig
    type t

    val generation : t -> int
    val length : t -> int
    val height : t -> index:int -> int option
    val is_exact : t -> index:int -> bool
    val item_start : t -> index:int -> int option
    val total_height : t -> int
    val range_is_exact : t -> first:int -> past:int -> bool
    val exact_prefix_length : t -> int
    val heights : t -> int array
    val exactness : t -> bool array
    val prefix : t -> int array
  end

  type batch_result =
    | Applied
    | Stale_generation
  [@@deriving sexp_of]

  val create : unit -> t
  val clear : t -> unit
  val generation : t -> int
  val length : t -> int
  val heights : t -> int array
  val prefix : t -> int array
  val total_height : t -> int
  val shape_matches : t -> length:int -> bool
  val snapshot : t -> Snapshot.t
  (** [snapshot t] copies [t] into an immutable geometry value suitable for
      detached readers. Arrays returned from {!Snapshot.heights},
      {!Snapshot.exactness}, and {!Snapshot.prefix} are defensive copies. *)

  val initialize_estimated
    :  t
    -> length:int
    -> estimated_height_at_index:(int -> int)
    -> unit
  (** [initialize_estimated t ~length ~estimated_height_at_index] replaces
      geometry with provisional non-negative heights and advances its
      generation. *)

  val extend_estimated
    :  t
    -> length:int
    -> estimated_height_at_index:(int -> int)
    -> unit
  (** [extend_estimated t ~length ~estimated_height_at_index] appends
      provisional items while preserving existing exactness and heights.

      @raise Invalid_argument if [length] is less than the current length or
      an estimate is negative. *)

  val reconcile_prefix
    :  t
    -> preserved_length:int
    -> length:int
    -> estimated_height_at_index:(int -> int)
    -> unit
  (** [reconcile_prefix t ~preserved_length ~length
       ~estimated_height_at_index] retains geometry and exactness for the
      first [preserved_length] items and provisionally initializes the
      remaining [length - preserved_length] items. *)

  val rebuild
    :  t
    -> length:int
    -> height_at_index:(int -> int)
    -> unit
  (** [rebuild t ~length ~height_at_index] replaces all cached heights and
      prefix sums.

      @raise Invalid_argument if an item has a negative height. *)

  val replace : t -> heights:int array -> prefix:int array -> unit
  (** [replace t ~heights ~prefix] installs coherent geometry.

      @raise Invalid_argument if [prefix] does not contain the prefix sums of
      [heights] or a height is negative. *)

  val replace_partial
    : t -> heights:int array -> prefix:int array -> exact:bool array -> unit
  (** [replace_partial t ~heights ~prefix ~exact] installs coherent geometry
      while retaining explicit per-row materialization state. *)

  val update_height : t -> index:int -> height:int -> unit
  (** [update_height t ~index ~height] updates one height and adjusts all
      following prefix sums.

      @raise Invalid_argument if [index] is invalid or [height] is negative. *)

  val apply_exact_batch
    :  t
    -> expected_generation:int
    -> start_index:int
    -> heights:int array
    -> batch_result
  (** [apply_exact_batch t ~expected_generation ~start_index ~heights]
      publishes contiguous exact heights, rebuilds prefix sums once, and
      advances the generation once.

      Returns [Stale_generation] without mutation when the generation differs.

      @raise Invalid_argument if the batch is empty, the range is outside [t],
      a height is negative, or cumulative height overflows. *)

  val item_start : t -> index:int -> int option
  val is_exact : t -> index:int -> bool
  val range_is_exact : t -> first:int -> past:int -> bool
  (** [range_is_exact t ~first ~past] returns whether every row in the
      half-open range [first, past) is exact.

      @raise Invalid_argument if the range is invalid or outside [t]. *)

  val exact_prefix_length : t -> int
(** [exact_prefix_length t] returns the number of leading rows whose heights
    and absolute prefix coordinates are exact. *)

  val all_exact : t -> bool
  val mark_all_estimated : t -> unit
  (** [mark_all_estimated t] retains current heights and prefix sums while
      making every item provisional and advancing the generation. *)

  val mark_exact : t -> index:int -> height:int -> unit
  val estimated_indices : t -> int list
end

module Viewport : sig
  type t

  val compute
    :  geometry:Geometry.t
    -> requested_scroll:int
    -> height:int
    -> follow_bottom:bool
    -> t

  val compute_snapshot
    :  geometry:Geometry.Snapshot.t
    -> requested_scroll:int
    -> height:int
    -> follow_bottom:bool
    -> t
  (** [compute_snapshot ~geometry ...] computes a viewport from immutable
      geometry using the same boundary rules as {!compute}. *)

  val scroll : t -> int
  val max_scroll : t -> int
  val first_visible : t -> int option
  val last_visible : t -> int option
  val top_spacer : t -> int
  val bottom_spacer : t -> int
  val visible_indices : t -> int list
  val estimated_visible_indices : geometry:Geometry.t -> t -> int list
  val is_exact : geometry:Geometry.t -> t -> bool
  (** [is_exact ~geometry t] returns whether every row intersecting [t] is
      exact. *)

  val bottom_up_candidates
    :  geometry:Geometry.t
    -> height:int
    -> overscan_rows:int
    -> int list
  (** [bottom_up_candidates ~geometry ~height ~overscan_rows] returns
      unmeasured tail indices from newest to oldest until their provisional
      cumulative height covers the requested rows. *)
end

module Anchor : sig
  type t

  val index : t -> int
  (** [index t] is the item index captured by [t]. *)

  val at_start : index:int -> offset:int -> screen_row:int -> t
  (** [at_start ~index ~offset ~screen_row] anchors [offset] rows from the
      beginning of [index] at [screen_row]. Negative offsets and screen rows
      are clamped to zero. *)

  val at_end : index:int -> offset:int -> screen_row:int -> t
  (** [at_end ~index ~offset ~screen_row] anchors [offset] rows from the end of
      [index] at [screen_row]. Negative offsets and screen rows are clamped to
      zero. *)

  val create
    :  geometry:Geometry.t
    -> viewport:Viewport.t
    -> screen_row:int
    -> t option
  (** [create ~geometry ~viewport ~screen_row] captures the item and intra-item
      row currently displayed at [screen_row]. Exact items retain their offset
      from the start. Estimated items retain their distance from the end when
      exact measurement changes their height. *)

  val create_at_scroll : geometry:Geometry.t -> scroll:int -> t option
  (** [create_at_scroll ~geometry ~scroll] captures the content row at the
      top of a manual viewport without requiring the viewport height. *)

  val remap_index : t -> index:int -> t
  (** [remap_index t ~index] preserves [t]'s intra-item and screen-row
      offsets while associating them with [index] in reconciled geometry. *)

  val corrected_scroll : t -> geometry:Geometry.t -> int option

  val corrected_scroll_snapshot
    : t -> geometry:Geometry.Snapshot.t -> int option
  (** [corrected_scroll_snapshot t ~geometry] restores [t] against immutable
      captured geometry. *)
  (** [corrected_scroll t ~geometry] preserves the captured content row at the
      same screen row after height corrections. *)
end

val render
  :  viewport:Viewport.t
  -> width:int
  -> image_at_index:(int -> Notty.I.t)
  -> Notty.I.t
(** [render ~geometry ~viewport ~width ~image_at_index] constructs a
    full-height sparse image containing only visible item images. *)
