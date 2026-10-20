open! Core
module State = Chatml_repl_terminal.State
module Render = Chatml_repl_terminal.Render

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let limits () =
  State.Limits.create
    ~max_source_bytes:131_072
    ~max_source_lines:1_024
    ~max_history_entries:16
    ~max_history_bytes:131_072
    ~max_notice_bytes:1_024
  |> ok
;;

let state () = State.create ~limits:(limits ()) ~context:"captured project" |> ok

let boundaries source =
  Uuseg_string.fold_utf_8
    `Grapheme_cluster
    (fun (offset, result) cluster ->
       let offset = offset + String.length cluster in
       offset, offset :: result)
    (0, [ 0 ])
    source
  |> snd
;;

let assert_cursor editor =
  let source = State.Editor.source editor in
  let cursor = State.Editor.cursor_byte editor in
  if not (List.mem (boundaries source) cursor ~equal:Int.equal)
  then raise_s [%sexp "cursor split grapheme", (source : string), (cursor : int)]
;;

let%test_unit "editor transitions retain grapheme cursor and bounded history" =
  let initial = state () in
  let insert editor text =
    State.Editor.insert editor ~limits:(State.limits initial) text |> ok
  in
  let editor = State.editor initial |> fun editor -> insert editor "a\rX\nb" in
  let editor = State.Editor.move editor Home in
  let editor = State.Editor.move editor Left in
  let editor = State.Editor.move editor Left in
  let editor = State.Editor.delete editor in
  if not (String.equal (State.Editor.source editor) "a\r\nb")
  then failwith "delete did not form CRLF";
  assert_cursor editor;
  let editor = State.Editor.move editor End in
  assert_cursor editor;
  let editor = insert editor "e\204\129" in
  assert_cursor editor;
  let editor = State.Editor.backspace editor in
  assert_cursor editor;
  let command = insert editor ":submit" in
  let command = State.Editor.without_final_line command in
  assert_cursor command;
  let submitted = State.submitted initial ~source:"let x = 1" ~preserve_editor:false in
  let draft =
    State.Editor.insert (State.editor submitted) ~limits:(limits ()) "draft" |> ok
  in
  let submitted = State.with_editor submitted draft |> ok in
  let previous = State.history_previous submitted in
  if not (String.equal (State.Editor.source (State.editor previous)) "let x = 1")
  then failwith "history did not select prior source";
  let restored = State.history_next previous in
  if not (String.equal (State.Editor.source (State.editor restored)) "draft")
  then failwith "history did not restore draft";
  let retried = State.submitted submitted ~source:"let x = 1" ~preserve_editor:true in
  if not (String.equal (State.Editor.source (State.editor retried)) "draft")
  then failwith "exact retry erased a newer editor draft";
  let small_limits =
    State.Limits.create
      ~max_source_bytes:4
      ~max_source_lines:1
      ~max_history_entries:2
      ~max_history_bytes:4
      ~max_notice_bytes:32
    |> ok
  in
  let small = State.create ~limits:small_limits ~context:"small" |> ok in
  (match State.with_editor small draft with
   | Error _ -> ()
   | Ok _ -> failwith "foreign editor exceeded receiver limit");
  let small = State.submitted small ~source:"\255" ~preserve_editor:false in
  if not (List.is_empty (State.history small))
  then failwith "invalid source entered history"
;;

let%test_unit "render bounds visible cursor and sanitizes multiline notice" =
  let state = state () in
  let prefix = String.make 100_000 'x' in
  let editor =
    State.Editor.insert
      (State.editor state)
      ~limits:(State.limits state)
      (prefix ^ "\nend")
    |> ok
  in
  let state =
    State.with_editor state editor
    |> ok
    |> fun state -> State.with_notice state "line 1\nline 2\027[31m"
  in
  let renderer = Render.create ~max_display_bytes:2_048 |> ok in
  List.iter
    [ 0, 0; 1, 1; 2, 2; 32, 12 ]
    ~f:(fun (width, height) ->
      let image, cursor = Render.image renderer state ~width ~height in
      if Notty.I.width image > width || Notty.I.height image > height
      then failwith "render exceeded viewport";
      match cursor with
      | None -> ()
      | Some (x, y) when x >= 0 && x < width && y >= 0 && y < height -> ()
      | Some _ -> failwith "cursor is outside viewport");
  let _, cursor = Render.image renderer state ~width:32 ~height:12 in
  if Option.is_none cursor then failwith "visible editor cursor was lost"
;;
