open! Core

(***********************************************************************)
(* Helpers                                                             *)
(***********************************************************************)

let make_user_msg (text : string) : Openai.Responses.Item.t =
  let open Openai.Responses in
  let open Input_message in
  let item : Input_message.t =
    { role = User; content = [ Text { text; _type = "input_text" } ]; _type = "message" }
  in
  Item.Input_message item
;;

let%expect_test "compactor – identity on empty" =
  Eio_main.run
  @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let result = Context_compaction.Compactor.compact_history ~env:(Some env) ~history:[] in
  (* Expect two system messages: initial + stub summary *)
  printf "items: %d\n" (List.length result);
  [%expect {|items: 2|}]
;;

let%expect_test "compactor – long transcript reduces to summary" =
  Eio_main.run
  @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  (* Create a synthetic conversation of 60 lines *)
  let history = List.init 60 ~f:(fun i -> make_user_msg (Printf.sprintf "Line %d" i)) in
  let result = Context_compaction.Compactor.compact_history ~env:(Some env) ~history in
  printf "original: %d  compacted: %d\n" (List.length history) (List.length result);
  (match result with
   | _ :: summary_msg :: _ ->
     (match summary_msg with
      | Openai.Responses.Item.Input_message m ->
        (match m.Openai.Responses.Input_message.content with
         | Openai.Responses.Input_message.Text { text; _ } :: _ ->
           printf
             "has system-reminder: %b\n"
             (String.is_substring ~substring:"<system-reminder>" text)
         | _ -> printf "unexpected content\n")
      | _ -> printf "unexpected second item kind\n")
   | _ -> printf "unexpected result length\n");
  [%expect
    {|original: 60  compacted: 2
has system-reminder: true|}]
;;
