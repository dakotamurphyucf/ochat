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
  let entry = History_entry.create ~allocator item |> Result.ok_or_failwith in
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

let%expect_test "staged reset never rewinds history identity" =
  let item : Openai.Responses.Item.t =
    Openai.Responses.Item.Reasoning
      { summary = []; _type = "reasoning"; id = "r"; status = None }
  in
  let legacy : Session.Legacy.V3.t =
    { version = 3
    ; id = "reset"
    ; prompt_file = "orig.md"
    ; local_prompt_copy = None
    ; history = [ item ]
    ; tasks = []
    ; moderator_snapshot = None
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let staged =
    match Session.V4.of_v3 legacy with
    | Ok session -> session
    | Error error -> failwith error
  in
  let reset = Session.V4.reset staged in
  print_s
    [%sexp
      { history_length = (List.length reset.history : int)
      ; next_history_sequence = (reset.next_history_sequence : int)
      }];
  [%expect {| ((history_length 0) (next_history_sequence 1)) |}]
;;
