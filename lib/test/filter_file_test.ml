open Core
open Filter_file

let%expect_test "filter_lines" =
  let input_file = "test_input.txt" in
  let output_file = "test_output.txt" in
  let condition line = String.length line > 3 in
  Out_channel.write_lines input_file ["hey"; "longer"; "even longer"; "yo"] ;
  filter_lines ~input_file ~output_file ~condition;
  let filtered_lines = In_channel.read_lines output_file in
  print_s [%sexp (filtered_lines : string list)];
  [%expect {| (longer "even longer") |}];