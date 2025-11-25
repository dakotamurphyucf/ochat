open Core

let ocaml_tm_json =
  {|{
  "name": "OCaml",
  "scopeName": "source.ocaml",
  "fileTypes": ["ml", "mli"],
  "patterns": [
    { "name": "comment.block.documentation.ocaml", "begin": "\\(\\*\\*", "end": "\\*\\)" },
    { "name": "comment.block.ocaml", "begin": "\\(\\*", "end": "\\*\\)" },
    { "name": "string.quoted.double.ocaml", "begin": "\"", "end": "\"", "patterns": [
      { "name": "constant.character.escape.ocaml", "match": "\\\\." }
    ]},
    { "name": "constant.character.ocaml", "match": "'([^'\\\\]|\\\\.)'" },
    { "name": "variable.language.type-parameter.ocaml", "match": "\\B'[a-z_][a-z0-9_]*" },
    { "name": "meta.annotation.ocaml", "match": "\\[@@?[^\\]]+\\]" },
    { "name": "meta.extension.ocaml", "match": "\\[%[^\\]]+\\]" },
    { "name": "entity.name.type.ocaml", "match": "\\b[A-Z][A-Za-z0-9_']*\\b" },
    { "name": "keyword.control.ocaml", "match": "\\b(let|rec|in|and|match|with|function|type|module|module\\s+type|open|include|struct|sig|end|if|then|else|fun|try|raise|exception|of|val|mutable|class|object|inherit|method|virtual|constraint|new|as|do|done|downto|for|to|while|when)\\b" },
    { "name": "constant.numeric.ocaml", "match": "\\b(0[xX][0-9A-Fa-f_]+|0[oO][0-7_]+|0[bB][01_]+|[0-9][0-9_]*(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?)\\b" },
    { "name": "operator.ocaml", "match": "::|:=|->|<-|\\|\\||&&|\\+\\.|-\\.|\\*\\.|/\\.|\\+|-|\\*|/|=|<>|>=|<=|>|<|@|\\^|;" },
    { "name": "variable.parameter.label.ocaml", "match": "[~?][a-z_][a-z0-9_]*:" },
    { "name": "entity.name.type.variant.ocaml", "match": "`[A-Za-z_][A-Za-z0-9_']*" },
    { "name": "constant.language.ocaml", "match": "\\b(true|false)\\b|\\(\\)" },
    { "match": "\\blet\\s+(?:rec\\s+)?([a-z_][A-Za-z0-9_']*)", "captures": { "1": { "name": "entity.name.function.ocaml" } } },
    { "name": "punctuation.bracket.ocaml", "match": "[()\\[\\]{}:,]" }
  ]
}
|}
;;

let add_ocaml (reg : Highlight_tm_loader.registry) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let add_file path =
    let%bind contents = Or_error.try_with (fun () -> In_channel.read_all path) in
    let%bind json = Jsonaf.parse contents in
    Highlight_tm_loader.add_grammar_jsonaf reg json
  in
  match add_file "lib/chat_tui/grammars/ocaml.json" with
  | Ok () -> Ok ()
  | Error _ ->
    (match add_file "lib/chat_tui/grammars/ocaml.tmLanguage.json" with
     | Ok () -> Ok ()
     | Error _ ->
       let%bind json = Jsonaf.parse ocaml_tm_json in
       Highlight_tm_loader.add_grammar_jsonaf reg json)
;;

let dune_tm_json =
  {|{
  "name": "Dune",
  "scopeName": "source.dune",
  "fileTypes": ["dune", "dune-project", "dune-workspace"],
  "patterns": [
    { "name": "comment.line.semicolon.dune", "match": ";.*$" },
    { "name": "string.quoted.double.dune", "begin": "\"", "end": "\"", "patterns": [
      { "name": "constant.character.escape.dune", "match": "\\\\." }
    ]},
    { "name": "keyword.control.dune", "match": "\\b(library|executables?|rule|alias|name|public_name|modules|libraries|flags|preprocess|deps|action|run|copy|copy#|system|setenv|env|test|inline_tests)\\b" },
    { "name": "variable.other.dune", "match": "\\b(:[a-zA-Z_][a-zA-Z0-9_-]*)\\b" },
    { "name": "constant.numeric.dune", "match": "\\b[0-9]+\\b" },
    { "name": "punctuation.section.parens.dune", "match": "[()]+" }
  ]
}
|}
;;

let add_dune (reg : Highlight_tm_loader.registry) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind json = Jsonaf.parse dune_tm_json in
  Highlight_tm_loader.add_grammar_jsonaf reg json
;;

let opam_tm_json =
  {|{
  "name": "OPAM",
  "scopeName": "source.opam",
  "fileTypes": ["opam"],
  "patterns": [
    { "name": "comment.line.number-sign.opam", "match": "#.*$" },
    { "name": "string.quoted.double.opam", "begin": "\"", "end": "\"", "patterns": [
      { "name": "constant.character.escape.opam", "match": "\\\\." }
    ]},
    { "name": "keyword.control.opam", "match": "\\b(opam-version|version|depends|depopts|conflicts|build|install|remove|maintainer|authors|license|homepage|bug-reports|dev-repo|url|synopsis|description|flags)\\b" },
    { "name": "constant.numeric.opam", "match": "\\b[0-9]+(\\.[0-9]+)?\\b" },
    { "name": "punctuation.separator.opam", "match": "[\\[\\]{}()=,:]" }
  ]
}
|}
;;

let add_opam (reg : Highlight_tm_loader.registry) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind json = Jsonaf.parse opam_tm_json in
  Highlight_tm_loader.add_grammar_jsonaf reg json
;;

let shell_tm_json =
  {|{
  "name": "Shell Script",
  "scopeName": "source.shell",
  "fileTypes": ["sh", "bash"],
  "patterns": [
    { "name": "comment.line.number-sign.shell", "match": "#.*$" },
    { "name": "string.quoted.double.shell", "begin": "\"", "end": "\"", "patterns": [
      { "name": "constant.character.escape.shell", "match": "\\\\." }
    ]},
    { "name": "string.quoted.single.shell", "begin": "'", "end": "'" },
    { "name": "keyword.control.shell", "match": "\\b(if|then|else|elif|fi|for|in|do|done|while|until|case|esac|function)\\b" },
    { "name": "variable.other.shell", "match": "\\$[A-Za-z_][A-Za-z0-9_]*|\\$\\{[^}]+\\}" },
    { "name": "string.interpolated.command-substitution.shell", "begin": "\\$\\(", "end": "\\)" },
    { "name": "string.other.backtick.shell", "begin": "`", "end": "`" },
    { "name": "meta.arithmetic.shell", "begin": "\\$\\(\\(", "end": "\\)\\)" },
    { "name": "variable.parameter.option.shell", "match": "--?[A-Za-z0-9][A-Za-z0-9_-]*" },
    { "name": "operator.redirection.shell", "match": "\\d?>&?\\d?|<<<?|>>?|<|>" },
    { "name": "punctuation.separator.shell", "match": "[|&;()<>]" },
    { "name": "operator.equal.shell", "match": "=" },
    { "name": "entity.name.keyword.function.shell", "match": "\\b(grep|tee|ls|mkdir)\\b" },
    { "name": "entity.name.keyword.echo.shell", "match": "\\b(echo)\\b" },
    { "name": "entity.name.type.shell", "match": "[A-Za-z0-9][A-Za-z0-9_-]*" }
    
  ]
}
|}
;;

let add_shell (reg : Highlight_tm_loader.registry) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind json = Jsonaf.parse shell_tm_json in
  Highlight_tm_loader.add_grammar_jsonaf reg json
;;

let diff_tm_json =
  {|{
  "name": "Diff",
  "scopeName": "source.diff",
  "fileTypes": ["diff", "patch"],
  "patterns": [
    { "name": "meta.diff.header", "match": "^diff --git .*$" },
    { "name": "meta.diff.index", "match": "^index .*$" },
    { "name": "meta.diff.file.a", "match": "^--- .*" },
    { "name": "meta.diff.file.b", "match": "^\\+\\+\\+ .*" },
    { "name": "meta.diff.hunk", "match": "^@@.*@@" },
    { "name": "markup.inserted.diff", "match": "^\\+.*$" },
    { "name": "markup.deleted.diff", "match": "^-.*$" },
    { "name": "markup.changed.diff", "match": "^!.*$" },
    { "name": "text.diff.context", "match": "^\\s.*$" }
  ]
}
|}
;;

let add_diff (reg : Highlight_tm_loader.registry) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind json = Jsonaf.parse diff_tm_json in
  Highlight_tm_loader.add_grammar_jsonaf reg json
;;

let json_tm_json =
  {|{
  "name": "JSON",
  "scopeName": "source.json",
  "fileTypes": ["json"],
  "patterns": [
    { "name": "entity.name.type.json", "match": "\"(?:\\\\.|[^\"\\\\])*\"(?=[ \t\r\n]*:)" },
    { "name": "string.quoted.double.json", "begin": "\"", "end": "\"", "patterns": [
      { "name": "constant.character.escape.json", "match": "\\\\." }
    ]},
    { "name": "constant.numeric.json", "match": "-?\\b[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?\\b" },
    { "name": "constant.language.json", "match": "\\b(true|false|null)\\b" },
    { "name": "punctuation.separator.json", "match": "[{}\[\]:,]" }
  ]
}
|}
;;

let add_json (reg : Highlight_tm_loader.registry) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind json = Jsonaf.parse json_tm_json in
  Highlight_tm_loader.add_grammar_jsonaf reg json
;;

let add_markdown (reg : Highlight_tm_loader.registry) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let add_file path =
    let%bind contents = Or_error.try_with (fun () -> In_channel.read_all path) in
    let%bind json = Jsonaf.parse contents in
    Highlight_tm_loader.add_grammar_jsonaf reg json
  in
  add_file "lib/chat_tui/grammars/markdown.tmLanguage.json"
;;

(* A very small, self-contained HTML grammar good enough for tag/attribute
   highlighting and comments/entities. This is intentionally minimal to keep
   startup fast and avoid heavy vendoring; Markdown’s HTML embedding mostly
   needs tags and attributes to be recognized. *)
let html_basic_tm_json =
  {|{
  "name": "HTML",
  "scopeName": "text.html.basic",
  "patterns": [
    { "include": "#comment" },
    { "include": "#tag" },
    { "include": "#entity" }
  ],
  "repository": {
    "comment": {
      "begin": "<!--",
      "beginCaptures": {"0": {"name": "punctuation.definition.comment.begin.html"}},
      "end": "-->",
      "endCaptures":   {"0": {"name": "punctuation.definition.comment.end.html"}},
      "name": "comment.block.html"
    },
    "entity": { "match": "&[A-Za-z0-9#]+;", "name": "constant.character.entity.html" },
    "tag": {
      "begin": "<(/)?(?=[A-Za-z])",
      "beginCaptures": {
        "0": {"name": "punctuation.definition.tag.begin.html"},
        "1": {"name": "punctuation.definition.tag.begin.html"}
      },
      "end": ">",
      "endCaptures":   {"0": {"name": "punctuation.definition.tag.end.html"}},
      "name": "meta.tag.inline.any.html",
      "patterns": [
        { "match": "[A-Za-z][A-Za-z0-9:-]*", "name": "entity.name.tag.html" },
        { "include": "#attributes" },
        { "match": "/", "name": "punctuation.definition.tag.end.html" }
      ]
    },
    "attributes": {
      "patterns": [
        {
          "match": "[A-Za-z_:][A-Za-z0-9_.:-]*",
          "name": "entity.other.attribute-name.html"
        },
        { "match": "=", "name": "punctuation.separator.key-value.html" },
        {
          "begin": "\"",
          "end": "\"",
          "name": "string.quoted.double.html",
          "patterns": [ { "include": "#entity" } ]
        },
        {
          "begin": "'",
          "end": "'",
          "name": "string.quoted.single.html",
          "patterns": [ { "include": "#entity" } ]
        }
      ]
    }
  }
|}
;;

(* A shim aliasing text.html.derivative to basic HTML, so that Markdown’s
   includes of text.html.derivative resolve even if we don’t load a heavier
   framework-specific derivative. *)
let html_derivative_shim_tm_json =
  {|{
  "name": "HTML (derivative shim)",
  "scopeName": "text.html.derivative",
  "patterns": [ { "include": "text.html.basic" } ]
|}
;;

let add_html (reg : Highlight_tm_loader.registry) : unit Or_error.t =
  let open Or_error.Let_syntax in
  (* Try to load a vendored HTML grammar if present; otherwise fall back to
     the built-in minimal grammar above. *)
  let add_file path =
    let%bind contents = Or_error.try_with (fun () -> In_channel.read_all path) in
    let%bind json = Jsonaf.parse contents in
    Highlight_tm_loader.add_grammar_jsonaf reg json
  in
  let%bind () =
    match add_file "lib/chat_tui/grammars/html.tmLanguage.json" with
    | Ok () -> Ok ()
    | Error _ ->
      let%bind json = Jsonaf.parse html_basic_tm_json in
      Highlight_tm_loader.add_grammar_jsonaf reg json
  in
  let%bind shim = Jsonaf.parse html_derivative_shim_tm_json in
  Highlight_tm_loader.add_grammar_jsonaf reg shim
;;
