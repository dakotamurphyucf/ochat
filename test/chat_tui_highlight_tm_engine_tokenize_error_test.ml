open Core
open Expect_test_helpers_core

let fallback_to_string = function
  | None -> "None"
  | Some Chat_tui.Highlight_tm_engine.No_registry -> "No_registry"
  | Some (Chat_tui.Highlight_tm_engine.Unknown_language l) ->
    "Unknown_language(" ^ l ^ ")"
  | Some Chat_tui.Highlight_tm_engine.Tokenize_error -> "Tokenize_error"
;;

let%expect_test "highlight_text_with_info reports Tokenize_error and falls back for all lines" =
  let reg = Chat_tui.Highlight_tm_loader.create_registry () in
  let grammar =
    Jsonaf.parse
      {|{
          "name": "bad",
          "scopeName": "source.bad",
          "patterns": [ { "include": "#missing" } ],
          "repository": {}
        }|}
    |> Or_error.ok_exn
  in
  Chat_tui.Highlight_tm_loader.add_grammar_jsonaf reg grammar |> Or_error.ok_exn;
  let engine =
    Chat_tui.Highlight_tm_engine.create ~theme:Chat_tui.Highlight_theme.github_dark
    |> Chat_tui.Highlight_tm_engine.with_registry ~registry:reg
  in
  let spans, info =
    Chat_tui.Highlight_tm_engine.highlight_text_with_info
      engine
      ~lang:(Some "bad")
      ~text:"hi\nthere"
  in
  let fallback = fallback_to_string info.fallback in
  let lines = List.map spans ~f:(List.map ~f:snd) in
  print_s [%sexp { fallback : string; lines : string list list }];
  [%expect
    {|
    ((fallback Tokenize_error)
     (lines (
       (hi)
       (there))))
    |}]
;;

let%expect_test "highlight_text_with_scopes_with_info also reports Tokenize_error and falls back" =
  let reg = Chat_tui.Highlight_tm_loader.create_registry () in
  let grammar =
    Jsonaf.parse
      {|{
          "name": "bad",
          "scopeName": "source.bad",
          "patterns": [ { "include": "#missing" } ],
          "repository": {}
        }|}
    |> Or_error.ok_exn
  in
  Chat_tui.Highlight_tm_loader.add_grammar_jsonaf reg grammar |> Or_error.ok_exn;
  let engine =
    Chat_tui.Highlight_tm_engine.create ~theme:Chat_tui.Highlight_theme.github_dark
    |> Chat_tui.Highlight_tm_engine.with_registry ~registry:reg
  in
  let spans, info =
    Chat_tui.Highlight_tm_engine.highlight_text_with_scopes_with_info
      engine
      ~lang:(Some "bad")
      ~text:"hi\nthere"
  in
  let fallback = fallback_to_string info.fallback in
  let lines =
    List.map spans ~f:(fun spans ->
      List.map spans ~f:(fun { Chat_tui.Highlight_tm_engine.text; scopes; _ } ->
        text, scopes))
  in
  print_s [%sexp { fallback : string; lines : (string * string list) list list }];
  [%expect
    {|
    ((fallback Tokenize_error)
     (lines (
       ((hi    ()))
       ((there ())))))
    |}]
;;

