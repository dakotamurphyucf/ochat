open Core

let%expect_test "session reset clears history and updates prompt" =
  (* Build a session with non-empty history. *)
  let reasoning : Openai.Responses.Reasoning.t =
    { summary = []; _type = "reasoning"; id = "r"; status = None }
  in
  let item : Openai.Responses.Item.t = Openai.Responses.Item.Reasoning reasoning in
  let session = Session.create ~prompt_file:"orig.md" ~history:[ item ] ~tasks:[] () in
  let reset = Session.reset ~prompt_file:"new.md" session in
  let history_len = List.length reset.history
  and prompt_ok = String.equal reset.prompt_file "new.md" in
  print_s [%sexp { history_len : int; prompt_ok : bool }];
  [%expect {| ((history_len 0) (prompt_ok true)) |}]
;;
