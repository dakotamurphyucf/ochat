(*
   Notty playground and key-event inspector.

  Modes
  • 1 Colors   2 Styles   3 Layout   4 Unicode   5 Mouse   6 Paint   7 Select   8 Scroll   k Key-dump
  • h Help     c Clear    q Quit
  • In Styles: b Bold  i Italic  u Underline  r Reverse  n Blink
*)

open Core
open Eio.Std
module A = Notty.A
module I = Notty.I
module Infix = Notty.Infix

module Ring = struct
  type t =
    { cap : int
    ; q : string Queue.t
    }

  let create cap = { cap; q = Queue.create () }

  let add t s =
    if Queue.length t.q >= t.cap then ignore (Queue.dequeue t.q : string option);
    Queue.enqueue t.q s
  ;;

  let to_list t = Queue.to_list t.q
  let clear t = Queue.clear t.q
end

let string_of_special = function
  | `Escape -> "Escape"
  | `Enter -> "Enter"
  | `Tab -> "Tab"
  | `Backspace -> "Backspace"
  | `Insert -> "Insert"
  | `Delete -> "Delete"
  | `Home -> "Home"
  | `End -> "End"
  | `Arrow d ->
    "Arrow "
    ^
      (match d with
      | `Up -> "Up"
      | `Down -> "Down"
      | `Left -> "Left"
      | `Right -> "Right")
  | `Page d ->
    "Page "
    ^
      (match d with
      | `Up -> "Up"
      | `Down -> "Down")
  | `Function n -> sprintf "Function %d" n
;;

let string_of_key = function
  | `ASCII c -> sprintf "ASCII %C (0x%02x)" c (Char.to_int c)
  | `Uchar u -> sprintf "Uchar U+%04X" (Uchar.to_scalar u)
  | #Notty.Unescape.special as s -> string_of_special s
;;

let string_of_mods mods =
  match mods with
  | [] -> "[]"
  | ms ->
    ms
    |> List.map ~f:(function
      | `Ctrl -> "Ctrl"
      | `Meta -> "Meta"
      | `Shift -> "Shift")
    |> String.concat ~sep:","
    |> sprintf "[%s]"
;;

let event_to_string : [ Notty.Unescape.event | `Resize ] -> string = function
  | `Key (k, mods) ->
    sprintf "Key   %-25s mods=%s" (string_of_key k) (string_of_mods mods)
  | `Mouse (((`Press _ | `Drag | `Release) as m), (x, y), mods) ->
    sprintf
      "Mouse %s at (%d,%d) mods=%s"
      (match m with
       | `Press b ->
         (match b with
          | `Left -> "Press Left"
          | `Middle -> "Press Middle"
          | `Right -> "Press Right"
          | `Scroll `Up -> "Scroll Up"
          | `Scroll `Down -> "Scroll Down")
       | `Drag -> "Drag"
       | `Release -> "Release")
      x
      y
      (string_of_mods mods)
  | `Paste `Start -> "Paste Start"
  | `Paste `End -> "Paste End"
  | `Resize -> "Resize"
;;

type mode =
  [ `Key_dump
  | `Colors
  | `Styles
  | `Layout
  | `Unicode
  | `Mouse
  | `Mouse_paint
  | `Mouse_select
  | `Mouse_scroll
  | `Gradient
  | `Gauges
  | `Table
  | `Pixels
  | `Borders
  | `Anim_typing
  | `Anim_spinner
  | `Anim_progress
  | `Anim_marquee
  ]

type styles =
  { bold : bool
  ; italic : bool
  ; underline : bool
  ; reverse : bool
  ; blink : bool
  }

module Canvas = struct
  type t =
    { w : int
    ; h : int
    ; rows : Bytes.t array
    }

  let create w h = { w; h; rows = Array.init h ~f:(fun _ -> Bytes.make w '\000') }

  let clear t =
    Array.iter t.rows ~f:(fun row -> Bytes.fill row ~pos:0 ~len:(Bytes.length row) '\000')
  ;;

  let set t x y =
    if x >= 0 && x < t.w && y >= 0 && y < t.h then Bytes.set t.rows.(y) x '\001'
  ;;

  let is_set t x y =
    if x < 0 || x >= t.w || y < 0 || y >= t.h
    then false
    else (
      let row = t.rows.(y) in
      Char.equal (Bytes.get row x) '\001')
  ;;
end

type state =
  { mutable mode : mode
  ; mutable show_help : bool
  ; mutable last_event : string option
  ; mutable mouse_pos : (int * int) option
  ; mutable styles : styles
  ; mutable paint_points : (int * int) list
  ; mutable paint_prev : (int * int) option
  ; mutable paint_canvas : Canvas.t option
  ; mutable select_start : (int * int) option
  ; mutable select_end : (int * int) option
  ; mutable scroll_offset : int
  ; mutable scroll_drag : (int * int) option
  ; mutable tick : int
  ; mutable spinner_style : int
  ; log : Ring.t
  }

let default_styles =
  { bold = false; italic = false; underline = false; reverse = false; blink = false }
;;

let header ~w ~mode : Notty.image =
  let mode_s =
    match mode with
    | `Key_dump -> "Key-dump"
    | `Colors -> "Colors"
    | `Styles -> "Styles"
    | `Layout -> "Layout"
    | `Unicode -> "Unicode"
    | `Mouse -> "Mouse"
    | `Mouse_paint -> "Mouse Paint"
    | `Mouse_select -> "Mouse Select"
    | `Mouse_scroll -> "Mouse Scroll"
    | `Gradient -> "Gradient"
    | `Gauges -> "Gauges"
    | `Table -> "Table"
    | `Pixels -> "Half-block Pixels"
    | `Borders -> "Borders"
    | `Anim_typing -> "Anim: Typing"
    | `Anim_spinner -> "Anim: Spinner"
    | `Anim_progress -> "Anim: Progress"
    | `Anim_marquee -> "Anim: Marquee"
  in
  let bar = I.char A.(bg blue) ' ' (Int.max 0 w) 1 in
  let text =
    I.strf
      ~attr:A.(fg lightwhite ++ bg blue)
      " Notty playground · %s  |  1 Colors  2 Styles  3 Layout  4 Unicode  5 Mouse  6 \
       Paint  7 Select  8 Scroll  9 Gradient  0 Gauges  t Table  p Pixels  o Borders  y \
       Typing  s Spinner  l Progress  m Marquee  k Key-dump  h Help  c Clear  q Quit "
      mode_s
  in
  Infix.(bar </> I.hsnap w text)
;;

let help_box ~w =
  let body =
    [ "Controls:"
    ; "  1 Colors   2 Styles   3 Layout   4 Unicode   5 Mouse   6 Paint   7 Select   8 \
       Scroll   9 Gradient   0 Gauges   t Table   p Pixels   o Borders   y Typing   s \
       Spinner   l Progress   m Marquee   k Key-dump"
    ; "  h Help     c Clear     q Quit"
    ; "Styles demo toggles:"
    ; "  b Bold  i Italic  u Underline  r Reverse  n Blink"
    ; "Mouse demo: click/drag/wheel to see decoded events."
    ]
  in
  let content = body |> List.map ~f:(fun s -> I.string A.(fg lightwhite) s) |> I.vcat in
  let title = I.string A.(fg lightyellow) " Help " in
  let pad_w = Int.min w (I.width content + 4) in
  let border = A.(fg lightwhite) in
  let top = I.char border '-' pad_w 1 in
  let side s = Infix.(I.string border "│ " <|> s <|> I.string border " │") in
  I.vcat [ title; top; side content; top ]
;;

let attr_of_styles s =
  let a = ref A.empty in
  (if s.bold then a := A.(st bold ++ !a));
  (if s.italic then a := A.(st italic ++ !a));
  (if s.underline then a := A.(st underline ++ !a));
  (if s.reverse then a := A.(st reverse ++ !a));
  (if s.blink then a := A.(st blink ++ !a));
  !a
;;

let palette_core16 : (A.color * string) list =
  [ A.black, "black"
  ; A.red, "red"
  ; A.green, "green"
  ; A.yellow, "yellow"
  ; A.blue, "blue"
  ; A.magenta, "magenta"
  ; A.cyan, "cyan"
  ; A.white, "white"
  ; A.lightblack, "lightblack"
  ; A.lightred, "lightred"
  ; A.lightgreen, "lightgreen"
  ; A.lightyellow, "lightyellow"
  ; A.lightblue, "lightblue"
  ; A.lightmagenta, "lightmagenta"
  ; A.lightcyan, "lightcyan"
  ; A.lightwhite, "lightwhite"
  ]
;;

let tile_for_color (c, name) =
  let tfg = A.lightwhite in
  let l1 = I.strf ~attr:A.(fg c) " fg %-12s " name in
  let l2 = I.strf ~attr:A.(fg tfg ++ bg c) " bg %-12s " name in
  I.vcat [ l1; l2 ] |> I.pad ~l:1 ~r:1 ~t:0 ~b:0
;;

let render_colors ~w ~h : Notty.image =
  let tiles = List.map palette_core16 ~f:tile_for_color in
  let row1, row2 = List.split_n tiles 8 in
  let grid = I.vcat [ I.hcat row1; I.hcat row2 ] in
  I.vsnap h (I.hsnap w grid)
;;

let render_styles ~w ~h s =
  let sample = "The quick brown fox jumps over the lazy dog 0123456789" in
  let a = attr_of_styles s in
  let line1 = I.strf ~attr:a " %s " sample in
  let line2 =
    I.strf
      ~attr:A.(fg lightwhite)
      " bold=%b italic=%b underline=%b reverse=%b blink=%b "
      s.bold
      s.italic
      s.underline
      s.reverse
      s.blink
  in
  I.vsnap h (I.hsnap w (I.vcat [ line1; line2 ]))
;;

let box ~w ~h ~attr label =
  let bg = I.char attr ' ' (Int.max 0 w) (Int.max 0 h) in
  let lbl = I.string A.(fg lightyellow) label |> I.pad ~l:1 ~t:0 ~r:0 ~b:0 in
  Infix.(bg </> lbl)
;;

let render_layout ~w ~h =
  let a1 = A.(fg lightwhite ++ bg red)
  and a2 = A.(fg lightwhite ++ bg green)
  and a3 = A.(fg lightwhite ++ bg blue) in
  let beside_demo = I.(string a1 " left " <|> void 2 1 <|> string a2 " right ") in
  let above_demo = I.(string a3 " above " <-> string a2 " below ") in
  let over_demo =
    let bg = I.char A.(bg (A.gray 8)) ' ' 22 3 in
    let fg = I.pad ~l:5 ~t:1 (I.string A.(fg lightyellow) " overlay ") in
    I.(fg </> bg)
  in
  let align_demo = I.(hsnap 20 (string A.(fg lightwhite) " centered ") |> vsnap 3) in
  let row1 =
    I.hcat
      [ beside_demo |> I.pad ~l:1 ~r:1
      ; above_demo |> I.pad ~l:1 ~r:1
      ; over_demo |> I.pad ~l:1 ~r:1
      ; align_demo |> I.pad ~l:1 ~r:1
      ]
  in
  let crop_pad_demo =
    let s = I.string A.(fg lightwhite ++ bg (A.gray 6)) "   CROPPED & PADDED   " in
    I.(hcrop 2 2 s |> vpad 1 1 |> hpad 2 2)
  in
  let boxes =
    I.hcat
      [ box ~w:18 ~h:5 ~attr:A.(bg (A.gray 4)) " box "
      ; box ~w:18 ~h:5 ~attr:A.(bg (A.gray 6)) " box 2 "
      ]
  in
  let row2 = I.hcat [ crop_pad_demo |> I.pad ~l:1 ~r:1; boxes |> I.pad ~l:1 ~r:1 ] in
  I.vsnap h (I.hsnap w (I.vcat [ row1; I.void 0 1; row2 ]))
;;

let render_unicode ~w ~h =
  let l1 = I.strf ~attr:A.(fg lightwhite) " Box: %s " "┌─┬┐│ │└─┴┘" in
  let l2 = I.strf ~attr:A.(fg lightwhite) " Shades: %s  Blocks: %s " "░▒▓ █" "▁▂▃▄▅▆▇█" in
  I.vsnap h (I.hsnap w (I.vcat [ l1; l2 ]))
;;

let render_mouse ~w ~h pos =
  let base = I.char A.empty ' ' (Int.max 0 w) (Int.max 0 h) in
  let dot =
    match pos with
    | None -> I.void 0 0
    | Some (x, y) ->
      I.(
        pad ~l:(Int.max 0 (x - 1)) ~t:(Int.max 0 (y - 1)) (string A.(fg lightyellow) "●"))
  in
  Infix.(dot </> base)
;;

let add_paint_point st (x, y) =
  st.paint_points <- (x, y) :: st.paint_points;
  match st.paint_canvas with
  | None -> ()
  | Some c ->
    let px = Int.max 0 (x - 1) in
    let py = Int.max 0 (y - 1) in
    Canvas.set c px py
;;

let add_paint_line st (x0, y0) (x1, y1) =
  let dx = x1 - x0 in
  let dy = y1 - y0 in
  let steps = Int.max (Int.abs dx) (Int.abs dy) in
  if steps = 0
  then add_paint_point st (x0, y0)
  else
    for
      (* include end point; start from 1 to avoid double-adding x0,y0 if caller already did *)
      i = 1 to steps
    do
      let xf = float x0 +. (float dx *. float i /. float steps) in
      let yf = float y0 +. (float dy *. float i /. float steps) in
      let x = Int.of_float (Float.round xf) in
      let y = Int.of_float (Float.round yf) in
      st.paint_points <- (x, y) :: st.paint_points;
      match st.paint_canvas with
      | None -> ()
      | Some c ->
        let px = Int.max 0 (x - 1) in
        let py = Int.max 0 (y - 1) in
        Canvas.set c px py
    done
;;

let render_paint ~w ~h (canvas : Canvas.t option) =
  match canvas with
  | None -> I.char A.empty ' ' (Int.max 0 w) (Int.max 0 h)
  | Some c ->
    let w' = Int.min w c.w
    and h' = Int.min h c.h in
    I.tabulate w' h' (fun x y ->
      if Canvas.is_set c x y then I.string A.(fg lightyellow) "•" else I.void 1 1)
;;

let render_select ~w ~h ~(start : (int * int) option) ~(stop : (int * int) option) =
  let base = I.char A.empty ' ' (Int.max 0 w) (Int.max 0 h) in
  match start, stop with
  | Some (x0, y0), Some (x1, y1) ->
    let x_min = Int.min x0 x1
    and x_max = Int.max x0 x1 in
    let y_min = Int.min y0 y1
    and y_max = Int.max y0 y1 in
    let x0' = Int.max 0 (x_min - 1) in
    let y0' = Int.max 0 (y_min - 1) in
    let w' = Int.max 1 (x_max - x_min + 1) in
    let h' = Int.max 1 (y_max - y_min + 1) in
    let a = A.(fg lightyellow) in
    let horiz = I.string a (String.make w' '-') in
    let top = I.pad ~l:x0' ~t:y0' horiz in
    let bottom = I.pad ~l:x0' ~t:(y0' + h' - 1) horiz in
    let vcell = I.string a "|" in
    let vert = I.vcat (List.init h' ~f:(fun _ -> vcell)) in
    let left = I.pad ~l:x0' ~t:y0' vert in
    let right = I.pad ~l:(x0' + w' - 1) ~t:y0' vert in
    Infix.(top </> bottom </> left </> right </> base)
  | _ -> base
;;

let render_scroll ~w ~h ~offset =
  let items = 200 in
  let lines =
    List.init h ~f:(fun i ->
      let idx = offset + i in
      if idx < items then I.strf ~attr:A.(fg lightwhite) " Item %03d" idx else I.void 0 0)
  in
  I.vcat lines |> I.hsnap w |> I.vsnap h
;;

let render_gradient ~w ~h =
  let cell x y =
    let r = x * 6 / Int.max 1 w |> Int.min 5 |> Int.max 0 in
    let g = y * 6 / Int.max 1 h |> Int.min 5 |> Int.max 0 in
    let b = (x + y) * 6 / Int.max 1 (w + h) |> Int.min 5 |> Int.max 0 in
    let a = A.(bg (rgb ~r ~g ~b)) in
    I.char a ' ' 1 1
  in
  I.tabulate w h cell
;;

let gauge_bar ~w ~label ~pct ~color =
  let pct = Float.clamp_exn ~min:0. ~max:1. pct in
  let inner_w = Int.max 0 (w - 2) in
  let fill = Int.of_float (Float.round_down (pct *. float inner_w)) in
  let empty = Int.max 0 (inner_w - fill) in
  let filled = I.char A.(bg color) ' ' fill 1 in
  let unfilled = I.char A.(bg (A.gray 3)) ' ' empty 1 in
  let bar = Infix.(I.string A.empty "" <|> filled <|> unfilled <|> I.string A.empty "") in
  let text = I.strf ~attr:A.(fg lightwhite) " %s %3.0f%% " label (pct *. 100.) in
  Infix.(bar </> I.hsnap w text)
;;

let render_gauges ~w ~h =
  let rows =
    [ "CPU", 0.27, A.lightgreen
    ; "Mem", 0.61, A.lightblue
    ; "Disk", 0.83, A.lightmagenta
    ; "Net", 0.48, A.lightyellow
    ]
  in
  let gauges =
    rows
    |> List.map ~f:(fun (lbl, pct, col) -> gauge_bar ~w ~label:lbl ~pct ~color:col)
    |> I.vcat
  in
  I.vsnap h (I.hsnap w gauges)
;;

let render_table ~w ~h =
  let cols = Int.max 2 (Int.min 6 (w / 16)) in
  let rows = Int.max 2 (Int.min 8 (h / 2)) in
  let cell_w = Int.max 10 (w / cols) in
  let make_cell r c =
    let bgc = if (r + c) mod 2 = 0 then A.(bg (gray 4)) else A.(bg (gray 6)) in
    let s = sprintf " r%02d c%02d " r c in
    I.(hsnap cell_w (string A.(fg lightwhite ++ bgc) s) |> vsnap 1)
  in
  let row r = I.hcat (List.init cols ~f:(fun c -> make_cell r c)) in
  let grid = I.vcat (List.init rows ~f:row) in
  I.vsnap h (I.hsnap w grid)
;;

let render_pixels ~w ~h =
  let h2 = Int.max 1 (h / 2) in
  let cell x y =
    let top_y = y * 2 in
    let bot_y = Int.min (h - 1) ((y * 2) + 1) in
    let r1 = x * 6 / Int.max 1 w |> Int.min 5 |> Int.max 0 in
    let g1 = top_y * 6 / Int.max 1 h |> Int.min 5 |> Int.max 0 in
    let b1 = (x + top_y) * 6 / Int.max 1 (w + h) |> Int.min 5 |> Int.max 0 in
    let r2 = x * 6 / Int.max 1 w |> Int.min 5 |> Int.max 0 in
    let g2 = bot_y * 6 / Int.max 1 h |> Int.min 5 |> Int.max 0 in
    let b2 = (x + bot_y) * 6 / Int.max 1 (w + h) |> Int.min 5 |> Int.max 0 in
    let a = A.(fg (rgb ~r:r2 ~g:g2 ~b:b2) ++ bg (rgb ~r:r1 ~g:g1 ~b:b1)) in
    I.string a "▄"
  in
  I.tabulate w h2 cell |> I.vsnap h
;;

let render_borders ~w ~h =
  let outer_w = Int.max 10 (w - 4) in
  let outer_h = Int.max 5 (h - 3) in
  let inner_w = Int.max 6 (outer_w - 6) in
  let inner_h = Int.max 3 (outer_h - 4) in
  let mk_box ~w ~h ~attr ~title =
    let repeat s n =
      let b = Buffer.create (String.length s * Int.max 0 n) in
      for _i = 1 to Int.max 0 n do
        Buffer.add_string b s
      done;
      Buffer.contents b
    in
    let horiz = repeat "─" (Int.max 0 (w - 2)) in
    let top = I.strf ~attr "┌%s┐" horiz in
    let bottom = I.strf ~attr "└%s┘" horiz in
    let mid =
      let space = I.char attr ' ' (Int.max 0 (w - 2)) 1 in
      I.hcat [ I.string attr "│"; space; I.string attr "│" ]
      |> I.vpad 0 (Int.max 0 (h - 2 - 1))
    in
    let titled =
      Infix.(top <-> I.hsnap w (I.string A.(fg lightyellow) title) </> mid <-> bottom)
    in
    I.vsnap h (I.hsnap w titled)
  in
  let base = I.char A.empty ' ' (Int.max 0 w) (Int.max 0 h) in
  let outer = mk_box ~w:outer_w ~h:outer_h ~attr:A.(fg lightwhite) ~title:" Outer " in
  let inner = mk_box ~w:inner_w ~h:inner_h ~attr:A.(fg lightblue) ~title:" Inner " in
  let o = I.pad ~l:2 ~t:2 outer in
  let i = I.pad ~l:5 ~t:4 inner in
  Infix.(o </> i </> base)
;;

(* Animation demos *)
let spinner_sets : string array list =
  [ [| "-"; "\\"; "|"; "/" |]
  ; [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
  ; [| "◐"; "◓"; "◑"; "◒" |]
  ]
;;

let render_anim_typing ~w ~h ~tick =
  let n = tick / 6 mod 4 in
  let dots = String.init n ~f:(fun _ -> '.') in
  let msg = sprintf " Assistant is typing%s " dots in
  I.vsnap h (I.hsnap w (I.string A.(fg lightwhite) msg))
;;

let render_anim_spinner ~w ~h ~tick ~style =
  let sets_len = List.length spinner_sets in
  let set = List.nth_exn spinner_sets (Int.abs style mod sets_len) in
  let frame = set.(tick mod Array.length set) in
  let msg = I.strf ~attr:A.(fg lightwhite) " Loading %s " frame in
  I.vsnap h (I.hsnap w msg)
;;

let render_anim_progress ~w ~h ~tick =
  let phase = Float.sin (float tick *. 0.15) in
  let pct = (phase +. 1.) *. 0.5 in
  gauge_bar ~w ~label:" Downloading " ~pct ~color:A.lightblue |> I.vsnap h |> I.hsnap w
;;

let render_anim_marquee ~w ~h ~tick =
  let s = " Streaming tokens · Press q to quit " in
  let period = Int.max 1 (w + String.length s + 8) in
  let pos = tick * 2 mod period in
  let img = I.pad ~l:pos (I.string A.(fg lightwhite) s) in
  I.vsnap h (I.hsnap w img)
;;

let is_anim_mode = function
  | `Anim_typing | `Anim_spinner | `Anim_progress | `Anim_marquee -> true
  | _ -> false
;;

let render_keydump ~w ~h (log : Ring.t) last_event =
  let rows = Int.max 0 h in
  let lines = Ring.to_list log in
  let lines = List.rev lines |> fun l -> List.take l rows |> List.rev in
  let attr_line = A.(fg lightwhite) in
  let img_lines = List.map lines ~f:(fun s -> I.strf ~attr:attr_line " %s" s) |> I.vcat in
  let hint =
    match last_event with
    | None -> I.void 0 0
    | Some s -> I.strf ~attr:A.(fg lightyellow) " last: %s " s
  in
  I.vsnap rows (I.hsnap w (I.vcat [ img_lines; hint ]))
;;

let draw term st =
  let w, h = Notty_eio.Term.size term in
  let header_img = header ~w ~mode:st.mode in
  let content_h = Int.max 0 (h - 1) in
  (match st.paint_canvas with
   | Some c when c.w = w && c.h = content_h -> ()
   | _ -> st.paint_canvas <- Some (Canvas.create w content_h));
  let content =
    match st.mode with
    | `Colors -> render_colors ~w ~h:content_h
    | `Styles -> render_styles ~w ~h:content_h st.styles
    | `Layout -> render_layout ~w ~h:content_h
    | `Unicode -> render_unicode ~w ~h:content_h
    | `Mouse -> render_mouse ~w ~h:content_h st.mouse_pos
    | `Mouse_paint -> render_paint ~w ~h:content_h st.paint_canvas
    | `Mouse_select ->
      render_select ~w ~h:content_h ~start:st.select_start ~stop:st.select_end
    | `Mouse_scroll -> render_scroll ~w ~h:content_h ~offset:st.scroll_offset
    | `Gradient -> render_gradient ~w ~h:content_h
    | `Gauges -> render_gauges ~w ~h:content_h
    | `Table -> render_table ~w ~h:content_h
    | `Pixels -> render_pixels ~w ~h:content_h
    | `Borders -> render_borders ~w ~h:content_h
    | `Anim_typing -> render_anim_typing ~w ~h:content_h ~tick:st.tick
    | `Anim_spinner ->
      render_anim_spinner ~w ~h:content_h ~tick:st.tick ~style:st.spinner_style
    | `Anim_progress -> render_anim_progress ~w ~h:content_h ~tick:st.tick
    | `Anim_marquee -> render_anim_marquee ~w ~h:content_h ~tick:st.tick
    | `Key_dump -> render_keydump ~w ~h:content_h st.log st.last_event
  in
  let body =
    match st.show_help with
    | false -> content
    | true -> Infix.(content </> I.pad ~l:2 ~t:2 (help_box ~w:(Int.min w 70)))
  in
  Notty_eio.Term.image term Infix.(header_img <-> body);
  Notty_eio.Term.cursor term None
;;

let handle_key st (k, mods) =
  match k, mods with
  | `ASCII 'q', _ | `ASCII 'C', [ `Ctrl ] -> `Quit
  | `ASCII 'h', _ ->
    st.show_help <- not st.show_help;
    `Redraw
  | `ASCII 'k', _ ->
    st.mode <- `Key_dump;
    `Redraw
  | `ASCII '1', _ ->
    st.mode <- `Colors;
    `Redraw
  | `ASCII '2', _ ->
    st.mode <- `Styles;
    `Redraw
  | `ASCII '3', _ ->
    st.mode <- `Layout;
    `Redraw
  | `ASCII '4', _ ->
    st.mode <- `Unicode;
    `Redraw
  | `ASCII '5', _ ->
    st.mode <- `Mouse;
    `Redraw
  | `ASCII '6', _ ->
    st.mode <- `Mouse_paint;
    `Redraw
  | `ASCII '7', _ ->
    st.mode <- `Mouse_select;
    `Redraw
  | `ASCII '8', _ ->
    st.mode <- `Mouse_scroll;
    `Redraw
  | `ASCII '9', _ ->
    st.mode <- `Gradient;
    `Redraw
  | `ASCII '0', _ ->
    st.mode <- `Gauges;
    `Redraw
  | `ASCII 't', _ ->
    st.mode <- `Table;
    `Redraw
  | `ASCII 'p', _ ->
    st.mode <- `Pixels;
    `Redraw
  | `ASCII 'o', _ ->
    st.mode <- `Borders;
    `Redraw
  | `ASCII 'y', _ ->
    st.mode <- `Anim_typing;
    `Redraw
  | `ASCII 's', _ ->
    st.mode <- `Anim_spinner;
    `Redraw
  | `ASCII 'l', _ ->
    st.mode <- `Anim_progress;
    `Redraw
  | `ASCII 'm', _ ->
    st.mode <- `Anim_marquee;
    `Redraw
  | `ASCII 'S', _ ->
    st.spinner_style <- st.spinner_style + 1;
    `Redraw
  | `ASCII 'c', _ ->
    (match st.mode with
     | `Key_dump -> Ring.clear st.log
     | `Mouse -> st.mouse_pos <- None
     | `Mouse_paint ->
       st.paint_points <- [];
       st.paint_prev <- None;
       (match st.paint_canvas with
        | None -> ()
        | Some c -> Canvas.clear c)
     | `Mouse_select ->
       st.select_start <- None;
       st.select_end <- None
     | `Mouse_scroll ->
       st.scroll_offset <- 0;
       st.scroll_drag <- None
     | _ -> ());
    `Redraw
  | `ASCII 'b', _ ->
    st.styles <- { st.styles with bold = not st.styles.bold };
    `Redraw
  | `ASCII 'i', _ ->
    st.styles <- { st.styles with italic = not st.styles.italic };
    `Redraw
  | `ASCII 'u', _ ->
    st.styles <- { st.styles with underline = not st.styles.underline };
    `Redraw
  | `ASCII 'r', _ ->
    st.styles <- { st.styles with reverse = not st.styles.reverse };
    `Redraw
  | `ASCII 'n', _ ->
    st.styles <- { st.styles with blink = not st.styles.blink };
    `Redraw
  | _ -> `Noop
;;

let main env =
  Switch.run
  @@ fun sw ->
  let st =
    { mode = `Colors
    ; show_help = false
    ; last_event = None
    ; mouse_pos = None
    ; styles = default_styles
    ; paint_points = []
    ; paint_prev = None
    ; paint_canvas = None
    ; select_start = None
    ; select_end = None
    ; scroll_offset = 0
    ; scroll_drag = None
    ; tick = 0
    ; spinner_style = 0
    ; log = Ring.create 500
    }
  in
  let events = Eio.Stream.create Int.max_value in
  Notty_eio.Term.run
    ~nosig:true
    ~mouse:true
    ~bpaste:true
    ~input:env#stdin
    ~output:env#stdout
    ~on_event:(fun ev -> ignore (Eio.Stream.add events ev : unit))
    (fun term ->
       let quit = ref false in
       let _clock =
         Fiber.fork ~sw (fun () ->
           let rec tick_loop () =
             Eio.Time.sleep env#clock 0.1;
             st.tick <- st.tick + 1;
             (match is_anim_mode st.mode with
              | true -> ignore (Eio.Stream.add events `Resize : unit)
              | false -> ());
             if !quit then () else tick_loop ()
           in
           tick_loop ())
       in
       let handle ev =
         match ev with
         | `Resize -> ()
         | `Mouse (`Press (`Scroll dir), (_x, _y), _mods) as ev ->
           st.last_event <- Some (event_to_string ev);
           Ring.add st.log (event_to_string ev);
           (match st.mode, dir with
            | `Mouse_scroll, `Up -> st.scroll_offset <- Int.max 0 (st.scroll_offset - 1)
            | `Mouse_scroll, `Down -> st.scroll_offset <- st.scroll_offset + 1
            | _ -> ())
         | `Mouse (`Press ((`Left | `Middle | `Right) as _b), (x, y), _mods) as ev ->
           st.mouse_pos <- Some (x, y);
           st.last_event <- Some (event_to_string ev);
           Ring.add st.log (event_to_string ev);
           (match st.mode with
            | `Mouse_paint ->
              st.paint_prev <- Some (x, y);
              add_paint_point st (x, y)
            | `Mouse_select ->
              st.select_start <- Some (x, y);
              st.select_end <- Some (x, y)
            | `Mouse_scroll -> st.scroll_drag <- Some (y, st.scroll_offset)
            | _ -> ())
         | `Mouse (`Drag, (x, y), _mods) as ev ->
           st.mouse_pos <- Some (x, y);
           st.last_event <- Some (event_to_string ev);
           (* Avoid spamming the log with drag events; keep only last_event. *)
           (match st.mode, st.scroll_drag with
            | `Mouse_paint, _ ->
              (match st.paint_prev with
               | None ->
                 st.paint_prev <- Some (x, y);
                 add_paint_point st (x, y)
               | Some (x0, y0) ->
                 add_paint_line st (x0, y0) (x, y);
                 st.paint_prev <- Some (x, y))
            | `Mouse_select, _ -> st.select_end <- Some (x, y)
            | `Mouse_scroll, Some (y0, off0) -> st.scroll_offset <- off0 - (y - y0)
            | _ -> ())
         | `Mouse (`Release, (x, y), _mods) as ev ->
           st.mouse_pos <- Some (x, y);
           st.last_event <- Some (event_to_string ev);
           Ring.add st.log (event_to_string ev);
           (match st.mode with
            | `Mouse_paint -> st.paint_prev <- None
            | `Mouse_scroll -> st.scroll_drag <- None
            | _ -> ())
         | `Paste _ as p ->
           st.last_event <- Some (event_to_string p);
           Ring.add st.log (event_to_string p)
         | `Key (k, mods) as ev ->
           st.last_event <- Some (event_to_string ev);
           Ring.add st.log (event_to_string ev);
           (match handle_key st (k, mods) with
            | `Quit -> quit := true
            | `Redraw | `Noop -> ())
       in
       let rec loop () =
         (* Block for at least one event, then drain, then draw once. *)
         let ev0 = Eio.Stream.take events in
         handle ev0;
         let rec drain () =
           match Eio.Stream.is_empty events with
           | true -> ()
           | false ->
             let ev = Eio.Stream.take events in
             handle ev;
             drain ()
         in
         drain ();
         draw term st;
         if !quit then () else loop ()
       in
       draw term st;
       loop ())
;;

let () = Eio_main.run main
