open Core

let%expect_test "session reset clears history and updates prompt" =
  (* Build a session with non-empty history. *)
  let reasoning : Openai.Responses.Reasoning.t =
    { summary = []; _type = "reasoning"; id = "r"; status = None }
  in
  let item : Openai.Responses.Item.t = Openai.Responses.Item.Reasoning reasoning in
  let allocator =
    History_entry.Allocator.create ~namespace:"reset-production" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let entry = Openai.Responses_history.create ~allocator item |> Result.ok_or_failwith in
  let session =
    Session.create
      ~id:"reset-production"
      ~prompt_file:"orig.md"
      ~history:[ entry ]
      ~next_history_sequence:(History_entry.Allocator.next_sequence allocator)
      ~tasks:[]
      ()
  in
  let reset = Session.reset ~prompt_file:"new.md" session in
  let retained = Session.reset_keep_history ~prompt_file:"new.md" session in
  let history_len = List.length reset.history
  and prompt_ok = String.equal reset.prompt_file "new.md"
  and reset_next = reset.next_history_sequence
  and retained_next = retained.next_history_sequence in
  print_s
    [%sexp { history_len : int; prompt_ok : bool; reset_next : int; retained_next : int }];
  [%expect {| ((history_len 0) (prompt_ok true) (reset_next 1) (retained_next 1)) |}]
;;
