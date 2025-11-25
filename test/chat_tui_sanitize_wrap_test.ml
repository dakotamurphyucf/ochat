open Core

let%expect_test "sanitize expands tabs, keeps newlines, filters control chars" =
  let s = "a\tb\nc\bd\x7F" in
  let out = Chat_tui.Util.sanitize s in
  print_s [%sexp (out : string)];
  [%expect
    {|
     "a    b\
    \nc d"
    |}]
;;

let%expect_test "wrap_line respects UTF-8 boundaries (é, limit=1)" =
  let s = "éé" in
  let parts = Chat_tui.Util.wrap_line ~limit:1 s in
  print_s [%sexp (parts : string list)];
  [%expect {| ("\195\169" "\195\169") |}]
;;

let%expect_test "wrap_line ASCII split" =
  let s = "abcdef" in
  let parts = Chat_tui.Util.wrap_line ~limit:2 s in
  print_s [%sexp (parts : string list)];
  [%expect {| (ab cd ef) |}]
;;
