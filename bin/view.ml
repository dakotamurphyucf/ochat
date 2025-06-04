(* ───────────────────────────────────────────────────────────────────── *)
(* 1.  Load a bitmap with Bimage                                        *)
(*     (works for PNG, JPEG, GIF, BMP, … as long as bimage-codecs       *)
(*      can recognise the file extension)                               *)
(* ───────────────────────────────────────────────────────────────────── *)

let bimage_of_file path =
  Bimage_unix.Magick.convert_command := "magick";
  match Bimage_unix.Magick.read Bimage.u8 Bimage.rgb path with
  | Error (`Msg m) -> failwith m
  | Error (`Invalid_shape | `Invalid_kernel_shape _ | `Invalid_input _ | `Invalid_color)
    -> failwith "Invalid image"
  | Ok img -> img
;;

let resize_to_width rgb8 ~max_w =
  let open Bimage in
  let open Image in
  let w = rgb8.width in
  if w <= max_w
  then rgb8
  else (
    let h = rgb8.height in
    let ratio = float max_w /. float w in
    let new_h = int_of_float (ratio *. float h) in
    let expr = Expr.resize max_w new_h in
    Filter.eval_expr
      expr
      ~width:max_w
      ~height:new_h
      rgb8.Image.ty
      rgb8.color
      [| Image.any rgb8 |])
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 3.  Convert Bimage → Notty using half-block ▀ technique               *)
(* ───────────────────────────────────────────────────────────────────── *)

let notty_of_rgb8 rgb8 =
  let module I = Notty.I in
  let module A = Notty.A in
  let open Bimage in
  let open Image in
  let w, h = rgb8.width, rgb8.height in
  let get_pixel x y =
    (* Image.get returns 0–255 ints already *)
    let r = get rgb8 x y 0 in
    let g = get rgb8 x y 1 in
    let b = get rgb8 x y 2 in
    r, g, b
  in
  let make_row y =
    (List.init w
     @@ fun x ->
     let r1, g1, b1 = get_pixel x y in
     let r2, g2, b2 = get_pixel x (y + 1) in
     let attr = A.(fg (rgb_888 ~r:r1 ~g:g1 ~b:b1) ++ bg (rgb_888 ~r:r2 ~g:g2 ~b:b2)) in
     I.uchar attr (Uchar.of_int 0x2580) 1 1)
    |> I.hcat
  in
  List.init (h / 2) (fun i -> make_row (i * 2)) |> I.vcat
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 4.  Entrypoint                                                       *)
(* ───────────────────────────────────────────────────────────────────── *)

let () =
  if Array.length Sys.argv < 1
  then (
    prerr_endline "Usage: view_image_bimage FILE [MAX_COLUMNS]";
    exit 1);
  let file = Sys.argv.(1) in
  (* Determine target width: 2nd CLI arg or current terminal columns *)
  let term_cols =
    match Notty_unix.winsize Unix.stdout with
    | Some (c, _) -> c
    | None -> 80
  in
  let max_w =
    if Array.length Sys.argv >= 3 then int_of_string Sys.argv.(2) else term_cols
  in
  (* pipeline *)
  let rgb8 = bimage_of_file file |> resize_to_width ~max_w in
  let img = notty_of_rgb8 rgb8 in
  Notty_unix.output_image img
;;
