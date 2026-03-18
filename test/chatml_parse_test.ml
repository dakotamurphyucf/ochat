open Core
open Chatml

let parse_message code =
  match Chatml_parse.parse_program code with
  | Ok _ -> "ok"
  | Error diagnostic -> diagnostic.message
;;

let%expect_test "structured parse diagnostic for syntax error" =
  let code =
    {|
      let x =
    |}
  in
  print_endline (parse_message code);
  [%expect {| Syntax error |}]
;;

let%expect_test "structured parse diagnostic for lexer failure" =
  let code = "@" in
  print_endline (parse_message code);
  [%expect {| Unknown token '@' at line 1, char 0 |}]
;;

let%expect_test "formatted parse diagnostic includes location context" =
  let code = "@" in
  (match Chatml_parse.parse_program code with
   | Ok _ -> print_endline "unexpected success"
   | Error diagnostic ->
     print_endline diagnostic.message;
     print_endline (Bool.to_string (Option.is_some diagnostic.span)));
  [%expect
    {|
    Unknown token '@' at line 1, char 0
    true
    |}]
;;
