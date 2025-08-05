open! Core

(***********************************************************************
 *  Helpers                                                           *
 ***********************************************************************)

let make_user_msg (text : string) : Openai.Responses.Item.t =
  let open Openai.Responses in
  let open Input_message in
  let item : Input_message.t =
    { role = User; content = [ Text { text; _type = "input_text" } ]; _type = "message" }
  in
  Item.Input_message item
;;

let%expect_test "summariser – offline stub" =
  let relevant_items =
    List.init 5 ~f:(fun i -> make_user_msg (Printf.sprintf "Line %d" i))
  in
  let summary = Context_compaction.Summarizer.summarise ~relevant_items ~env:None in
  print_endline summary;
  [%expect
    {|user: Line 0
user: Line 1
user: Line 2
user: Line 3
user: Line 4|}]
;;
