# Chat_tui.Highlight_tm_engine

Terminal syntax-highlighting engine for the TUI. Transform plain text into
per-line sequences of `(Notty.A.t * string)` spans, ready to render with
`Notty.I.string`.

- Namespace: `Chat_tui.Highlight_tm_engine`
- Depends on: `Core` (line splitting), `Notty` (attributes and images),
  `textmate-language` (tokenization), `Chat_tui.Highlight_theme` (scopes -> attr),
  `Chat_tui.Highlight_tm_loader` (loading and resolving grammars)

With a TextMate grammar registry and a `lang` tag (e.g. "ocaml", "bash",
"diff"), tokenise lines using `TmLanguage.tokenize_exn` and map TextMate scope
stacks to `Notty.A.t` via the theme. If no registry is provided or the language
cannot be resolved, fall back to one plain span per line with `Notty.A.empty`.

Note on Notty: `Notty.I.string` rejects control characters (including newlines);
the engine never emits newlines in span text but does not filter other control
characters.

## Types

- `t` — highlighter handle (theme + optional registry)
- `span = Notty.A.t * string` — highlighted fragment of a single line
- `fallback_reason` — why plain rendering was used: `No_registry` |
  `Unknown_language of string` | `Tokenize_error`
- `info = { fallback : fallback_reason option }` — diagnostic summary for a run

## Functions

- `create ~theme` — create a highlighter configured with `theme`
- `with_theme t ~theme` — return a copy of `t` using `theme`
- `with_registry t ~registry` — return a copy of `t` that can resolve grammars
  from `registry`
- `highlight_text t ~lang ~text` — split `text` into lines and produce spans
  for each line, colourised when possible
- `highlight_text_with_info t ~lang ~text` — like `highlight_text` but also
  return an `info` describing whether a fallback occurred

### Invariants

- Number of output lines equals `Core.String.split_lines text`
- Each span belongs to exactly one input line and contains no newline
- Concatenating all span texts on a line reconstructs that line
- Adjacent spans with the same attribute are merged
- Tokenisation state flows across lines (`TmLanguage.stack`) to handle
  multi-line constructs

## Examples

Create an engine and highlight without a registry (plain spans):

```ocaml
let engine =
  Chat_tui.Highlight_tm_engine.create
    ~theme:Chat_tui.Highlight_theme.default_dark
in
let lines =
  Chat_tui.Highlight_tm_engine.highlight_text
    engine ~lang:None ~text:"hello\nworld"
in
List.length lines = 2
```

Load a registry, resolve `ocaml`, and colourise:

```ocaml
let reg = Chat_tui.Highlight_tm_loader.create_registry () in
let (_ : unit Core.Or_error.t) =
  Chat_tui.Highlight_tm_loader.add_grammar_jsonaf_file reg ~path:"ocaml.tmLanguage.json"
in
let engine =
  Chat_tui.Highlight_tm_engine.(create ~theme:Chat_tui.Highlight_theme.default_dark
                                |> with_registry ~registry:reg)
in
let ocaml_spans =
  Chat_tui.Highlight_tm_engine.highlight_text
    engine ~lang:(Some "ocaml") ~text:"let x = 1\nlet y = x + 2"
in
List.length ocaml_spans = 2
```

Render spans to a Notty image:

```ocaml
let row spans =
  List.fold_left
    (fun img (attr, s) -> Notty.I.(img <|> Notty.I.string attr s))
    Notty.I.empty
    spans
in
let to_image lines =
  List.fold_left (fun acc l -> Notty.I.(acc <-> row l)) Notty.I.empty lines
in
let (_ : Notty.image) = to_image ocaml_spans
```

Get diagnostic information about fallbacks:

```ocaml
let lines, info =
  Chat_tui.Highlight_tm_engine.highlight_text_with_info
    engine ~lang:(Some "unknown-lang") ~text:"foo\nbar"
in
match info.Chat_tui.Highlight_tm_engine.fallback with
| Some (Chat_tui.Highlight_tm_engine.Unknown_language _) -> true
| _ -> false
```

## Known issues and limitations

- `TmLanguage.tokenize_exn` expects newline-terminated input lines; the engine
  appends a newline internally and strips it from spans
- If tokenisation raises an error, the engine uses a plain span for that line
  and continues
- Notty prohibits control characters in `I.string`; ensure input text does not
  contain such characters (the engine does not filter them)

## See also

- `Chat_tui.Highlight_theme` — maps TextMate scopes to `Notty.A.t`
- `Chat_tui.Highlight_tm_loader` — creates and populates a registry and resolves
  grammars by language tags
- Notty `A` and `I` modules — attributes and image combinators used to render
  spans
