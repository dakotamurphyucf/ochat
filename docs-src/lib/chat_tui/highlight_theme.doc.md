# Highlight_theme — mapping TextMate scopes to Notty attributes

`chat_tui/highlight_theme` defines colour palettes and matching logic for
syntax highlighting in the terminal UI. Given a list of TextMate-style
scopes (e.g. `"keyword.operator"`, `"string"`, `"entity.name.function"`),
it returns a `Notty.A.t` attribute describing foreground/background colours
and text styles.

Used by: [`Highlight_tm_engine`](highlight_tm_engine.doc.md)

---

## How it works

- A theme is an ordered list of rules.
- Each rule maps a scope selector prefix (left-most components) to a `Notty.A.t`.
- Matching is dot-segment-aware: a selector matches a scope only if it is an
  exact match or a prefix followed by a dot. This prevents `"source.js"`
  from matching `"source.json"`.
- Given a non-empty list of scopes, the single best matching rule across all
  scopes is selected by specificity. If none match, `Notty.A.empty` is returned.

Examples of prefixes and scopes:

- Prefix `"keyword"` matches `"keyword.operator"`, `"keyword.control"`.
- Prefix `"markup.heading.3.markdown"` matches exactly that scope; it
  outranks a broader `"markup.heading.markdown"` rule because its prefix is
  longer.

Colour/style values come from `Notty.A` and therefore depend on terminal
capabilities (ANSI 16 colours, 256-colour cube via `A.rgb`, grayscale via
`A.gray`, or true colour via `A.rgb_888`).

### Specificity and tie-breaking

Across all provided scopes, the best match is chosen by the tuple below
(higher is better):

1. Number of dot-separated segments in the selector (more segments win)
2. Exactness (exact match wins over prefix match)
3. Selector length in characters (longer wins)
4. Earlier appearance in the theme list (stable, left-to-right)

---

## Themes

- default_dark: tuned for dark backgrounds with cyan/yellow accents and muted punctuation.
- default_light: tuned for light backgrounds with readable contrasts.
- github_dark: matches GitHub Dark Default using truecolor where available; strings azure, keywords salmon, functions/types purple, tags green, variables orange, comments gray; links underlined; inline code uses a subtle chip background.

---

## API

### `empty : t`

Theme with no rules. Always returns `Notty.A.empty`.

```ocaml
let attr = Chat_tui.Highlight_theme.attr_of_scopes Chat_tui.Highlight_theme.empty ~scopes:["keyword"]
(* attr = Notty.A.empty *)
```

### `default_dark : t`

Built-in palette for dark backgrounds. Uses `Notty.A.gray`, `Notty.A.rgb_888`,
and the ANSI colour names (e.g. `lightwhite`, `magenta`). Aims for clear
contrast without overwhelming saturation.

### `default_light : t`

Light-background variant mirroring `default_dark` with adjusted hues.

### `github_dark : t`

GitHub-inspired palette for dark terminals. Prefer this if you want familiar
token colours from GitHub/VS Code in the TUI. Uses helpers from
`Chat_tui.Highlight_styles` (e.g. `fg_hex`, `bg_hex`) to express truecolor
values.

```ocaml
let open Chat_tui in
let theme = Highlight_theme.github_dark in
let kw = Highlight_theme.attr_of_scopes theme ~scopes:["keyword"] in
let (_ : Notty.A.t) = kw
```

### `attr_of_scopes : t -> scopes:string list -> Notty.A.t`

Pick the attribute for the given scopes using longest-prefix matching.
Linear in the number of rules times the number of scopes; suitable for
per-token calls.

```ocaml
let open Chat_tui in
let theme = Highlight_theme.default_dark in
let kw = Highlight_theme.attr_of_scopes theme ~scopes:["keyword"; "source.ocaml"] in
let str = Highlight_theme.attr_of_scopes theme ~scopes:["string"; "source.ocaml"] in
(* Compose with the current attribute if needed: *)
let strong_kw = Notty.A.(kw ++ st bold) in
```

Integrating with the highlighter:

```ocaml
let engine = Highlight_tm_engine.create ~theme:Highlight_theme.default_dark in
let lines = Highlight_tm_engine.highlight_text engine ~lang:(Some "ocaml") ~text:"let x = 1" in
(* Render: turn (attr * text) spans into a Notty image *)
let row spans =
  List.fold_left (fun img (a, s) -> Notty.I.(img <|> Notty.I.string a s)) Notty.I.empty spans
in
let image = List.fold_left (fun acc l -> Notty.I.(acc <-> row l)) Notty.I.empty lines in
let (_ : Notty.image) = image in
()
```

---

## Known behaviours and limitations

- Terminal palette differences: the visual result depends on terminal
  support; extended and true-colour attributes may be remapped or ignored
  on some terminals.
- Specificity: if both `"markup.heading"` and
  `"markup.heading.3.markdown"` match, the latter is used per specificity
  rules above. Provide more specific rules to override general ones.
- Customisation API: the module currently exposes only predefined themes and
  the query function. If you need fine-grained theming, extend the module or
  add new constructors in your fork.
- Complexity: resolution cost is linear in (number of rules × number of scopes per token).

---

## Examples

Inline markdown emphasis with the dark palette:

```ocaml
let bold =
  Chat_tui.Highlight_theme.attr_of_scopes
    Chat_tui.Highlight_theme.default_dark
    ~scopes:["markup.bold"]

let img = Notty.I.string bold "strong"
```

Styling fenced code blocks via the engine:

```ocaml
let code = """
```ocaml
let add a b = a + b
```
""" in
let engine = Chat_tui.Highlight_tm_engine.create ~theme:Chat_tui.Highlight_theme.default_dark in
let (_spans : (Notty.A.t * string) list list) =
  Chat_tui.Highlight_tm_engine.highlight_text engine ~lang:(Some "markdown") ~text:code
```

---

## See also

- [`Notty.A` documentation](https://pqwy.github.io/notty/doc/Notty.A.html) — colours and styles
- [`Highlight_tm_engine`](highlight_tm_engine.doc.md) — produces `(attr * text)` spans
- [`Highlight_styles`](highlight_styles.doc.md) — concise constructors and helpers used by themes
- [`Renderer`](renderer.doc.md) — where attributes are applied to draw chat UI
