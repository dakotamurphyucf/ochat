open Core

(** Scrollable view onto a {!Notty} image.

    This module provides a tiny state-ful wrapper around a {!Notty.I.t}
    that allows the caller to expose only a *window* – a rectangular
    viewport – of a potentially larger image.  Vertical scrolling is
    achieved by cropping [scroll] rows from the top of the original
    image and then {i snapping} the result to the requested viewport
    size using {!Notty.I.vsnap} and {!Notty.I.hsnap}.

    The state carried by a scroll box consists of

    - [content] — the full underlying image
    - [scroll]  — the number of rows currently hidden {b above} the viewport

    A value of [scroll = 0] therefore shows the beginning of the image, and
    increasing the offset moves the viewport further {b down}.  Horizontal
    scrolling is not supported; the content is simply cropped or padded to the
    desired width.

    {1 High-level operations}

    • {!render} converts a scroll box into an image of the exact dimensions
      requested by the caller.
    • {!scroll_by}, {!scroll_to_top} and {!scroll_to_bottom} adjust the scroll
      offset while ensuring it stays within the valid range for a given
      viewport height.

    All mutating helpers make sure that the offset is clamped to
    [[0, max_scroll]] where [max_scroll] depends on the height of the viewport
    that will later be used for rendering.  Calling {!render} also performs
    this clamping, so the invariant is preserved even if the user sets an
    invalid offset directly.  *)

module I = Notty.I

type t =
  { mutable content : I.t (** Entire image to scroll through. *)
  ; mutable scroll : int (** Rows hidden {b above} the viewport. *)
  }

(** [create ?scroll content] creates a new scroll box initialised with
    [content].  [scroll] is clamped to a non-negative value but is {e not}
    limited by any particular viewport height until {!render} is called. *)
let create ?(scroll = 0) content = { content; scroll = Int.max 0 scroll }

(** [set_content t img] replaces the underlying image with [img].  The current
    [scroll] offset is kept intact; it will be clamped on the next
    {!render}. *)
let set_content t content = t.content <- content

(** Return the full underlying image. *)
let content t = t.content

(** Current scroll offset (rows hidden at the top). *)
let scroll t = t.scroll

(** [max_scroll t ~height] is the largest scroll offset that still leaves
    at least one row visible in a viewport of [height] rows.  The result is
    [max 0 (I.height t.content - height)]. *)
let max_scroll t ~height = Int.max 0 (I.height t.content - height)

(** Ensure that [t.scroll] lies in [[0, max_scroll]].  The check depends on the
    viewport [~height] that will be used for rendering. *)
let clamp_scroll t ~height =
  let max_s = max_scroll t ~height in
  if t.scroll < 0 then t.scroll <- 0;
  if t.scroll > max_s then t.scroll <- max_s
;;

(** [scroll_to t n] sets the scroll offset to [n] without clamping.  The value
    will be brought into range by the next call to {!clamp_scroll} or
    {!render}. *)
let scroll_to t n = t.scroll <- n

(** [scroll_by t ~height delta] adjusts the scroll offset by [delta] rows and
    immediately clamps it for a viewport of height [height]. *)
let scroll_by t ~height delta =
  t.scroll <- t.scroll + delta;
  clamp_scroll t ~height
;;

(** Move viewport to the very top of the image. *)
let scroll_to_top t = t.scroll <- 0

(** Move viewport to the last full screenful of the image (bottom-most valid
    offset for a viewport of height [~height]). *)
let scroll_to_bottom t ~height = t.scroll <- max_scroll t ~height

(** [render t ~width ~height] returns an image of exactly [width ✕ height]
    cells.  The function first clamps the current offset using [height] and
    then constructs the viewport as

    {v
      t.content                      (full image)
      |> I.vcrop t.scroll 0          (* drop rows above the viewport *)
      |> I.vsnap ~align:`Top height  (* pad or crop to desired height *)
      |> I.hsnap ~align:`Left width  (* pad or crop to desired width  *)
    v}

    Horizontal snapping uses [`Left] alignment so that the leftmost columns of
    the content remain visible when the image is wider than the viewport. *)
let render t ~width ~height =
  clamp_scroll t ~height;
  t.content
  |> I.vcrop t.scroll 0
  |> I.vsnap ~align:`Top height
  |> I.hsnap ~align:`Left width
;;
