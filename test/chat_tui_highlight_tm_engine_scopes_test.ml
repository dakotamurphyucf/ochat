open Core
open Expect_test_helpers_core

let%expect_test "highlight_text_with_scopes preserves punctuation scopes per segment" =
  let reg = Chat_tui.Highlight_tm_loader.create_registry () in
  let grammar =
    Jsonaf.parse
      {|{
          "name": "tiny",
          "scopeName": "source.tiny",
          "fileTypes": ["tiny"],
          "patterns": [
            { "name": "punctuation.definition.raw.tiny", "match": "`+" },
            { "name": "punctuation.definition.bold.tiny", "match": "\\*\\*" },
            { "name": "punctuation.definition.italic.tiny", "match": "\\*" }
          ]
        }|}
    |> Or_error.ok_exn
  in
  Chat_tui.Highlight_tm_loader.add_grammar_jsonaf reg grammar |> Or_error.ok_exn;
  let engine =
    Chat_tui.Highlight_tm_engine.create ~theme:Chat_tui.Highlight_theme.github_dark
    |> Chat_tui.Highlight_tm_engine.with_registry ~registry:reg
  in
  let lines =
    Chat_tui.Highlight_tm_engine.highlight_text_with_scopes
      engine
      ~lang:(Some "tiny")
      ~text:"**bold**\n`code`"
  in
  let has_prefix scopes prefix =
    List.exists scopes ~f:(fun s -> String.is_prefix s ~prefix)
  in
  let simplified =
    List.map lines ~f:(fun spans ->
      List.map spans ~f:(fun { Chat_tui.Highlight_tm_engine.text; scopes; _ } ->
        ( text
        , has_prefix scopes "punctuation.definition.bold"
        , has_prefix scopes "punctuation.definition.italic"
        , has_prefix scopes "punctuation.definition.raw" )))
  in
  print_s [%sexp (simplified : (string * bool * bool * bool) list list)];
  [%expect
    {|
    (((**   true  false false)
      (bold false false false)
      (**   true  false false))
     ((`    false false true)
      (code false false false)
      (`    false false true)))
    |}]
;;
