(** Public renderer entry points.

    The concrete chat page implementation lives in {!Renderer_page_chat}
    and is invoked via {!Renderer_pages}. *)

let decorate ~size ~model image = Renderer_shell_approval.overlay ~size ~model image

let render_full ~size ~model =
  let image, cursor = Renderer_pages.render ~size ~model in
  let cursor = Option.value (Renderer_shell_approval.cursor ~size ~model) ~default:cursor in
  decorate ~size ~model image, cursor
;;
let lang_of_path = Renderer_lang.lang_of_path

let visible_fingerprint ~size:(width, height) image =
  let buffer = Buffer.create (Int.max 256 (width * height)) in
  Notty.Render.to_buffer
    buffer
    Notty.Cap.ansi
    (0, 0)
    (Int.max 0 width, Int.max 0 height)
    image;
  Buffer.contents buffer
;;

let visible_components_equal ~size ~left ~right =
  String.equal (visible_fingerprint ~size left) (visible_fingerprint ~size right)
;;

let%test_unit "visible fingerprints ignore Notty expression-tree identity" =
  let direct = Notty.I.string Notty.A.empty "same" in
  let reconstructed = Notty.I.pad ~r:1 (Notty.I.string Notty.A.empty "same") in
  assert (not (Notty.I.equal direct reconstructed));
  assert (visible_components_equal ~size:(4, 1) ~left:direct ~right:reconstructed)
;;
