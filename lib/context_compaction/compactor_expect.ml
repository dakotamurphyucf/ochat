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

let make_reminder index =
  make_user_msg
    (Printf.sprintf "<system-reminder>previous compaction %d</system-reminder>" index)
;;

let make_role_msg role text =
  let open Openai.Responses in
  Item.Input_message
    { Input_message.role
    ; content = [ Text { text; _type = "input_text" } ]
    ; _type = "message"
    }
;;

let create_entry allocator item =
  History_entry.create ~allocator item |> Result.ok_or_failwith
;;

let%expect_test "compactor keeps the newest ten previous reminders" =
  let allocator =
    History_entry.Allocator.create ~namespace:"reminders" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let history =
    List.init 12 ~f:(fun index -> create_entry allocator (make_reminder index))
  in
  let _, pruned, _ =
    Context_compaction.Compactor.For_testing.process_current_entries history
  in
  pruned
  |> List.map ~f:History_entry.item
  |> List.iter ~f:(function
    | Openai.Responses.Item.Input_message
        { content = Openai.Responses.Input_message.Text { text; _ } :: _; _ } ->
      print_endline text
    | _ -> ());
  [%expect
    {|
    <system-reminder>previous compaction 2</system-reminder>
    <system-reminder>previous compaction 3</system-reminder>
    <system-reminder>previous compaction 4</system-reminder>
    <system-reminder>previous compaction 5</system-reminder>
    <system-reminder>previous compaction 6</system-reminder>
    <system-reminder>previous compaction 7</system-reminder>
    <system-reminder>previous compaction 8</system-reminder>
    <system-reminder>previous compaction 9</system-reminder>
    <system-reminder>previous compaction 10</system-reminder>
    <system-reminder>previous compaction 11</system-reminder>|}]
;;

let%expect_test "entry compaction retains IDs and allocates one reminder" =
  let allocator =
    History_entry.Allocator.create ~namespace:"compactor" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let system = create_entry allocator (make_role_msg System "policy") in
  let developer = create_entry allocator (make_role_msg Developer "guidance") in
  let duplicate_one = create_entry allocator (make_reminder 1) in
  let duplicate_two = create_entry allocator (make_reminder 1) in
  let ordinary = create_entry allocator (make_user_msg "discard me") in
  let before = History_entry.Allocator.next_sequence allocator in
  let result =
    Context_compaction.Compactor.compact_entries
      ~allocator
      ~env:None
      ~history:[ system; ordinary; developer; duplicate_one; duplicate_two ]
    |> Result.ok_exn
  in
  let retained = List.take result 4 in
  let expected = [ system; developer; duplicate_one; duplicate_two ] in
  let ids_preserved =
    List.map2_exn expected retained ~f:(fun expected actual ->
      History_entry.Id.equal (History_entry.id expected) (History_entry.id actual))
  in
  print_s
    [%sexp
      (ids_preserved : bool list)
    , (List.length result : int)
    , (History_entry.Allocator.next_sequence allocator - before : int)];
  [%expect {| ((true true true true) 5 1) |}]
;;

let%expect_test "entry partition prunes old reminders without rewrapping retained ones" =
  let allocator =
    History_entry.Allocator.create ~namespace:"partition" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let reminders =
    List.init 12 ~f:(fun index -> create_entry allocator (make_reminder index))
  in
  let _, retained, relevant =
    Context_compaction.Compactor.For_testing.process_current_entries reminders
  in
  let expected = List.drop reminders 2 in
  let ids_equal entries =
    List.map2_exn expected entries ~f:(fun expected actual ->
      History_entry.Id.equal (History_entry.id expected) (History_entry.id actual))
    |> List.for_all ~f:Fn.id
  in
  print_s [%sexp ((ids_equal retained, ids_equal relevant) : bool * bool)];
  [%expect {| (true true) |}]
;;

let%expect_test "failed entry compaction does not allocate or expose history" =
  let allocator =
    History_entry.Allocator.create ~namespace:"failure" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let original = create_entry allocator (make_user_msg "original") in
  let before = History_entry.Allocator.next_sequence allocator in
  let result =
    Context_compaction.Compactor.For_testing.compact_entries_with
      ~summarise:(fun ~relevant_items:_ ~env:_ -> Error (Failure "failed"))
      ~allocator
      ~env:None
      ~history:[ original ]
  in
  print_s
    [%sexp
      (Result.is_error result : bool)
    , (History_entry.Allocator.next_sequence allocator = before : bool)
    , (History_entry.Id.sequence (History_entry.id original) : int)];
  [%expect {| (true true 0) |}]
;;

let%expect_test "cancelled entry compaction does not allocate" =
  let allocator =
    History_entry.Allocator.create ~namespace:"cancel" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let original = create_entry allocator (make_user_msg "original") in
  let before = History_entry.Allocator.next_sequence allocator in
  let cancelled =
    try
      ignore
        (Context_compaction.Compactor.For_testing.compact_entries_with
           ~summarise:(fun ~relevant_items:_ ~env:_ ->
             raise (Eio.Cancel.Cancelled (Failure "cancelled")))
           ~allocator
           ~env:None
           ~history:[ original ]
         : (History_entry.t list, exn) result);
      false
    with
    | Eio.Cancel.Cancelled _ -> true
  in
  print_s
    [%sexp
      (cancelled : bool)
    , (History_entry.Allocator.next_sequence allocator = before : bool)];
  [%expect {| (true true) |}]
;;
