open! Core
module Search = Chatml_repl_terminal.History_search

let%expect_test "reverse search selects newest literal matches and cycles older matches" =
  let search = Search.create ~history:[ "let alpha = 3"; "beta"; "let alpha = 1" ] in
  let search = Search.insert search "alpha" |> Or_error.ok_exn in
  print_s [%sexp (Search.matched_source search : string option)];
  let search = Search.next search in
  print_s [%sexp (Search.matched_source search : string option)];
  let search = Search.next search in
  print_s [%sexp (Search.matched_source search : string option)];
  let search = Search.insert search "missing" |> Or_error.ok_exn in
  print_s [%sexp (Search.matched_source search : string option)];
  [%expect
    {|
    ("let alpha = 3")
    ("let alpha = 1")
    ("let alpha = 3")
    ()
    |}]
;;

let%test_unit "search query bounds reject whole edits and backspace erases one grapheme" =
  let search = Search.create ~history:[] in
  let query = "e\204\129" in
  let search = Search.insert search query |> Or_error.ok_exn in
  if not (String.equal (Search.query (Search.backspace search)) "")
  then failwith "split grapheme";
  (match Search.insert search (String.make 256 'x') with
   | Ok _ -> failwith "accepted oversized query"
   | Error _ -> ());
  (match Search.insert search "\255" with
   | Ok _ -> failwith "accepted invalid UTF-8"
   | Error _ -> ());
  if not (String.equal (Search.query search) query)
  then failwith "failed search edit mutated query"
;;

let%test_unit
    "search cancellation restores exact editor cursor and acceptance remains a draft"
  =
  let module State = Chatml_repl_terminal.State in
  let must = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
  in
  let limits =
    State.Limits.create
      ~max_source_bytes:4096
      ~max_source_lines:32
      ~max_history_entries:16
      ~max_history_bytes:4096
      ~max_notice_bytes:1024
    |> must
  in
  let state = State.create ~limits ~context:"search fixture" |> must in
  let state = State.submitted state ~source:"let answer = 42" ~preserve_editor:false in
  let draft =
    State.Editor.insert (State.editor state) ~limits "draft e\204\129" |> must
  in
  let draft = State.Editor.move draft Left in
  let state = State.with_editor state draft |> must in
  let search = State.start_history_search state in
  let search =
    State.edit_history_search search ~f:(fun search -> Search.insert search "answer")
    |> Or_error.ok_exn
  in
  let cancelled = State.cancel_history_search search in
  if
    (not
       (String.equal
          (State.Editor.source (State.editor cancelled))
          (State.Editor.source draft)))
    || State.Editor.cursor_byte (State.editor cancelled) <> State.Editor.cursor_byte draft
  then failwith "search cancellation lost exact draft/cursor";
  let accepted = State.accept_history_search search in
  if
    (not (String.equal (State.Editor.source (State.editor accepted)) "let answer = 42"))
    || List.length (State.history accepted) <> 1
    || Option.is_some (State.history_search accepted)
  then failwith "history search acceptance did not leave an unsubmitted draft";
  List.iter
    [ State.Needs_reset; State.Uncertain Submit; State.Closed; State.Busy Observe ]
    ~f:(fun activity ->
      let blocked = State.start_history_search (State.with_activity state activity) in
      if Option.is_some (State.history_search blocked)
      then failwith "search entered nonidle state")
;;
