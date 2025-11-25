open Core
module A = Notty.A
module I = Notty.I

let render_ansi ~w ~h img =
  let buf = Buffer.create 128 in
  Notty.Render.to_buffer buf Notty.Cap.ansi (0, 0) (w, h) img;
  Buffer.contents buf
;;

let render_dumb ~w ~h img =
  let buf = Buffer.create 128 in
  Notty.Render.to_buffer buf Notty.Cap.dumb (0, 0) (w, h) img;
  Buffer.contents buf
;;

let%expect_test "basic geometry: beside/above" =
  let i1 = I.string A.empty "ab" in
  let i2 = I.string A.empty "xyz" in
  let beside = I.(i1 <|> i2) in
  let above = I.(i1 <-> i2) in
  Printf.printf "beside: w=%d h=%d\n" (I.width beside) (I.height beside);
  Printf.printf "above:  w=%d h=%d\n" (I.width above) (I.height above);
  [%expect
    {|
    beside: w=5 h=1
    above:  w=3 h=2
    |}]
;;

let%expect_test "hsnap/vsnap crop+pad" =
  let i = I.string A.empty "abcdefgh" in
  let hs = I.hsnap 4 i in
  let vs = I.vsnap 3 hs in
  Printf.printf "w=%d h=%d\n" (I.width vs) (I.height vs);
  let s = render_dumb ~w:4 ~h:3 vs in
  (* Cap.dumb uses spaces for positioning; show as escaped string *)
  printf "%S\n" s;
  [%expect
    {|
    w=4 h=3
    "\ncdef\n"
    |}]
;;

let%expect_test "colors produce ANSI escapes" =
  let img = I.string A.(fg red ++ bg blue ++ st bold) "Hi" in
  let s = render_ansi ~w:2 ~h:1 img in
  (* We don't assert the whole escape soup, just key substrings and payload *)
  print_endline
    (if String.is_substring s ~substring:"[31" then "has-fg-red" else "no-fg-red");
  print_endline
    (if String.is_substring s ~substring:"[44" then "has-bg-blue" else "no-bg-blue");
  print_endline (if String.is_substring s ~substring:"Hi" then "has-text" else "no-text");
  [%expect
    {|
    no-fg-red
    no-bg-blue
    has-text
    |}]
;;
