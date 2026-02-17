open Core
open Expect_test_helpers_core

let fallback_to_string = function
  | None -> "None"
  | Some Chat_tui.Highlight_tm_engine.No_registry -> "No_registry"
  | Some (Chat_tui.Highlight_tm_engine.Unknown_language l) ->
    "Unknown_language(" ^ l ^ ")"
  | Some Chat_tui.Highlight_tm_engine.Tokenize_error -> "Tokenize_error"
;;

let markdown_test_engine () : Chat_tui.Highlight_tm_engine.t =
  let open Chat_tui in
  let grammar =
    {|{
      "scopeName": "text.html.markdown",
      "name": "Markdown",
      "fileTypes": ["md"],
      "patterns": [
        {
          "match": "^\\*\\s+",
          "name": "punctuation.definition.list.begin.markdown"
        },
        {
          "name": "markup.bold.markdown",
          "begin": "\\*\\*",
          "beginCaptures": { "0": { "name": "punctuation.definition.bold.markdown" } },
          "end": "\\*\\*",
          "endCaptures": { "0": { "name": "punctuation.definition.bold.markdown" } },
          "patterns": [
            {
              "name": "markup.italic.markdown",
              "begin": "\\*",
              "beginCaptures": { "0": { "name": "punctuation.definition.italic.markdown" } },
              "end": "\\*",
              "endCaptures": { "0": { "name": "punctuation.definition.italic.markdown" } }
            }
          ]
        },
        {
          "name": "markup.italic.markdown",
          "begin": "\\*",
          "beginCaptures": { "0": { "name": "punctuation.definition.italic.markdown" } },
          "end": "\\*",
          "endCaptures": { "0": { "name": "punctuation.definition.italic.markdown" } }
        },
        {
          "name": "markup.inline.raw.string.markdown",
          "begin": "`",
          "beginCaptures": { "0": { "name": "punctuation.definition.raw.markdown" } },
          "end": "`",
          "endCaptures": { "0": { "name": "punctuation.definition.raw.markdown" } }
        }
      ]
    }|}
  in
  let reg = Highlight_tm_loader.create_registry () in
  grammar
  |> Jsonaf.parse
  |> Or_error.ok_exn
  |> Highlight_tm_loader.add_grammar_jsonaf reg
  |> Or_error.ok_exn;
  Highlight_tm_engine.create ~theme:Highlight_theme.github_dark
  |> Highlight_tm_engine.with_registry ~registry:reg
;;

let render_to_string ~(w : int) ~(h : int) (img : Notty.I.t) : string =
  let buf = Buffer.create 4096 in
  Notty.Render.to_buffer buf Notty.Cap.dumb (0, 0) (w, h) img;
  Buffer.contents buf
;;

let render_message_to_string ~(width : int) ~(hi_engine : Chat_tui.Highlight_tm_engine.t) ~text =
  let img =
    Chat_tui.Renderer_component_message.render_message
      ~width
      ~selected:false
      ~tool_output:None
      ~role:"user"
      ~text
      ~hi_engine
  in
  render_to_string ~w:width ~h:(Notty.I.height img) img
;;

let%expect_test "should_drop_markdown_delimiter: unit cases" =
  let open Chat_tui.Renderer_component_message in
  let cases =
    [ [ "punctuation.definition.bold.markdown" ], "**", true
    ; [ "punctuation.definition.italic.markdown" ], "*", true
    ; [ "punctuation.definition.raw.markdown" ], "`", true
    ; [ "punctuation.definition.raw.markdown" ], "``", true
    ; [ "markup.bold.markdown"; "punctuation.definition.bold.markdown" ], "**", true
    ; [ "punctuation.definition.list.begin.markdown" ], "*", false
    ; [ "punctuation.definition.italic.markdown" ], "*a", false
    ; [ "punctuation.definition.italic.markdown" ], "", false
    ; [ "punctuation.definition.italic.markdown" ], "_", true
    ; [ "punctuation.definition.bold.markdown" ], "__", true
    ]
  in
  List.iter cases ~f:(fun (scopes, text, expected) ->
    let got = should_drop_markdown_delimiter ~scopes ~text in
    printf !"%s %S -> %b\n" (String.concat ~sep:"," scopes) text got;
    require
      [%here]
      (Bool.equal got expected)
      ~if_false_then_print_s:
        (lazy [%message "unexpected result" (scopes : string list) (text : string) (got : bool) (expected : bool)]));
  [%expect
    {|
    punctuation.definition.bold.markdown "**" -> true
    punctuation.definition.italic.markdown "*" -> true
    punctuation.definition.raw.markdown "`" -> true
    punctuation.definition.raw.markdown "``" -> true
    markup.bold.markdown,punctuation.definition.bold.markdown "**" -> true
    punctuation.definition.list.begin.markdown "*" -> false
    punctuation.definition.italic.markdown "*a" -> false
    punctuation.definition.italic.markdown "" -> false
    punctuation.definition.italic.markdown "_" -> true
    punctuation.definition.bold.markdown "__" -> true
    |}]
;;

let%expect_test "scope-aware suppression removes markers and preserves content (scoped spans)" =
  let open Chat_tui in
  let hi_engine = markdown_test_engine () in
  let text =
    String.concat
      ~sep:"\n"
      [ "**BOLD** then *ITALIC* and `CODE`."
      ; "* ITEM"
      ; "**OUTER *INNER* OUTER**"
      ]
  in
  let lines, info =
    Highlight_tm_engine.highlight_text_with_scopes_with_info
      hi_engine
      ~lang:(Some "markdown")
      ~text
  in
  printf "fallback=%s\n" (fallback_to_string info.fallback);
  let lines = List.map lines ~f:Renderer_component_message.suppress_markdown_delimiters in
  let line_text spans =
    spans
    |> List.map ~f:(fun s -> s.Chat_tui.Highlight_tm_engine.text)
    |> String.concat
  in
  let rendered = lines |> List.map ~f:line_text |> String.concat ~sep:"\n" in
  print_endline rendered;
  let line0 = List.nth_exn lines 0 in
  let line2 = List.nth_exn lines 2 in
  let find_span_exn spans ~substring =
    List.find_exn spans ~f:(fun s ->
      String.is_substring s.Chat_tui.Highlight_tm_engine.text ~substring)
  in
  let bold = find_span_exn line0 ~substring:"BOLD" in
  let italic = find_span_exn line0 ~substring:"ITALIC" in
  let code = find_span_exn line0 ~substring:"CODE" in
  let inner = find_span_exn line2 ~substring:"INNER" in
  let has_scope scopes scope = List.exists scopes ~f:(String.equal scope) in
  let has_prefix scopes ~prefix = List.exists scopes ~f:(String.is_prefix ~prefix) in
  printf
    "bold: markup=%b punct=%b\n"
    (has_scope bold.Chat_tui.Highlight_tm_engine.scopes "markup.bold.markdown")
    (has_prefix
       bold.Chat_tui.Highlight_tm_engine.scopes
       ~prefix:"punctuation.definition.bold");
  printf
    "italic: markup=%b punct=%b\n"
    (has_scope italic.Chat_tui.Highlight_tm_engine.scopes "markup.italic.markdown")
    (has_prefix
       italic.Chat_tui.Highlight_tm_engine.scopes
       ~prefix:"punctuation.definition.italic");
  printf
    "code: markup=%b punct=%b\n"
    (has_scope
       code.Chat_tui.Highlight_tm_engine.scopes
       "markup.inline.raw.string.markdown")
    (has_prefix code.Chat_tui.Highlight_tm_engine.scopes ~prefix:"punctuation.definition.raw");
  printf
    "inner: bold=%b italic=%b\n"
    (has_scope inner.Chat_tui.Highlight_tm_engine.scopes "markup.bold.markdown")
    (has_scope inner.Chat_tui.Highlight_tm_engine.scopes "markup.italic.markdown");
  [%expect
    {|
    fallback=None
    BOLD then ITALIC and CODE.
    * ITEM
    OUTER INNER OUTER
    bold: markup=true punct=false
    italic: markup=true punct=false
    code: markup=true punct=false
    inner: bold=true italic=true
    |}]
;;

let%expect_test "renderer output: markers are hidden (scoped + fallback engines)" =
  let open Chat_tui in
  let text_basic =
    String.concat
      ~sep:"\n"
      [ "**BOLD** then *ITALIC* and `CODE`."
      ; "* ITEM"
      ; "**OUTER *INNER* OUTER**"
      ]
  in
  let text_multicode = "``CODE`WITH`BACKTICKS``" in
  let width = 140 in
  let check_basic ~which ~hi_engine =
    let s = render_message_to_string ~width ~hi_engine ~text:text_basic in
    let has sub = String.is_substring s ~substring:sub in
    printf
      "%s basic: **BOLD**=%b *ITALIC*=%b `CODE`=%b *INNER*=%b\n"
      which
      (has "**BOLD**")
      (has "*ITALIC*")
      (has "`CODE`")
      (has "*INNER*");
    String.split_lines s
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun line ->
      String.is_substring line ~substring:"BOLD"
      || String.is_substring line ~substring:"ITALIC"
      || String.is_substring line ~substring:"CODE"
      || String.is_substring line ~substring:"ITEM"
      || String.is_substring line ~substring:"OUTER"
      || String.is_substring line ~substring:"INNER")
    |> List.iter ~f:(fun line -> printf "%s basic: %s\n" which line)
  in
  let check_multicode_fallback () =
    let hi_engine = Highlight_tm_engine.create ~theme:Highlight_theme.github_dark in
    let s = render_message_to_string ~width ~hi_engine ~text:text_multicode in
    let has sub = String.is_substring s ~substring:sub in
    printf "fallback multicode: has_delims=%b has_content=%b\n" (has "``") (has "CODE`WITH`BACKTICKS");
    String.split_lines s
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun line ->
      String.is_substring line ~substring:"CODE" || String.is_substring line ~substring:"BACKTICKS")
    |> List.iter ~f:(fun line -> printf "fallback multicode: %s\n" line)
  in
  check_basic ~which:"scoped" ~hi_engine:(markdown_test_engine ());
  check_basic
    ~which:"fallback"
    ~hi_engine:(Highlight_tm_engine.create ~theme:Highlight_theme.github_dark);
  check_multicode_fallback ();
  [%expect
    {|
    scoped basic: **BOLD**=false *ITALIC*=false `CODE`=false *INNER*=false
    scoped basic: BOLD then ITALIC and CODE.
    scoped basic: * ITEM
    scoped basic: OUTER INNER OUTER
    fallback basic: **BOLD**=false *ITALIC*=false `CODE`=false *INNER*=false
    fallback basic: BOLD then ITALIC and CODE.
    fallback basic: * ITEM
    fallback basic: OUTER INNER OUTER
    fallback multicode: has_delims=false has_content=true
    fallback multicode: CODE`WITH`BACKTICKS
    |}]
;;

