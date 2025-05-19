open Core

(* A small helper module that turns a Notty image into a scrollable viewport.

   The component keeps track of a mutable [scroll] offset that represents the
   number of rows cropped from the {i top} of the underlying image.  A scroll
   value of [0] therefore means that the viewport shows the beginning of the
   image, while larger values move the viewport further down the content.

   The main operation is {!render}, which turns the component state into an
   image of the requested dimensions.  The helper functions [scroll_by],
   [scroll_to_top] and [scroll_to_bottom] adjust the offset while making sure
   that it stays within the valid range for a given viewport height. *)

module I = Notty.I

type t =
  { mutable content : I.t  (** Full content image. *)
  ; mutable scroll : int   (** Rows cropped from the top. *)
  }

let create ?(scroll = 0) content = { content; scroll }

let set_content t content = t.content <- content

let content t = t.content

let scroll t = t.scroll

let max_scroll t ~height = Int.max 0 (I.height t.content - height)

let clamp_scroll t ~height =
  let max_s = max_scroll t ~height in
  if t.scroll < 0 then t.scroll <- 0;
  if t.scroll > max_s then t.scroll <- max_s

let scroll_to t n = t.scroll <- n

let scroll_by t ~height delta =
  t.scroll <- t.scroll + delta;
  clamp_scroll t ~height

let scroll_to_top t = t.scroll <- 0

let scroll_to_bottom t ~height = t.scroll <- max_scroll t ~height

let render t ~width ~height =
  (* Ensure the scroll offset is valid for the current viewport height. *)
  clamp_scroll t ~height;
  (* Crop, then pad/crop to the exact viewport size. *)
  t.content
  |> I.vcrop t.scroll 0
  |> I.vsnap ~align:`Top height
  |> I.hsnap ~align:`Left width

