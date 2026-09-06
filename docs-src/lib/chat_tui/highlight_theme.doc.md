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
- Each rule maps a scope selector prefix (left-most components) to a
  `Notty.A.t`.
- Matching is dot-segment-aware: a selector matches a scope only if it is an
  exact match or a prefix followed by a dot. This prevents `"source.js"`
  from matching `"source.json"`.
- Given a non-empty list of scopes, all rules whose prefixes match at least
  one scope are considered. Among those, only the rules with maximal
  specificity contribute to the result. Their attributes are combined in
  theme order.

Examples of prefixes and scopes:

- Prefix `"keyword"` matches `"keyword.operator"`, `"keyword.control"`.
- Prefix `"markup.heading.3.markdown"` matches exactly that scope; it
  outranks a broader `"markup.heading.markdown"` rule because its prefix is
  longer.

Colour/style values come from `Notty.A` and therefore depend on terminal
capabilities (ANSI 16 colours, 256-colour cube via `A.rgb`, grayscale via
`A.gray`, or true colour via `A.rgb_888`).

### Specificity and composition

Across all provided scopes, every rule that matches at least one scope is
assigned a specificity key `(segments, exact)`:

1. `segments` – number of dot-separated segments in the selector prefix;
   more segments are more specific.
2. `exact` – `1` if the selector exactly equals the scope, `0` if it is a
   strict prefix.

Only rules with **maximal** `(segments, exact)` across all scopes
contribute to the final attribute. Their `Notty.A.t` values are composed in
theme order (earlier rules apply first, later rules can override parts of
the style using `Highlight_styles.(++)`).

---

## Themes

- `empty`: theme with no rules. Always yields `Notty.A.empty` regardless of
  scopes.
- `github_dark`: matches GitHub Dark Default using truecolor where
  available; strings azure, keywords salmon, functions/types purple, tags
  green, variables orange, comments gray; links underlined; inline code uses
  a subtle chip background.

---

## API

### `empty : t`

Theme with no rules. Always returns `Notty.A.empty`.

```ocaml
let attr = Chat_tui.Highlight_theme.attr_of_scopes Chat_tui.Highlight_theme.empty ~scopes:["keyword"]
(* attr = Notty.A.empty *)
```

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

Pick the attribute for the given scopes using longest-prefix matching on
dot-separated segments.

- All rules whose prefixes match at least one of the supplied scopes are
  candidates.
- Only rules with maximal `(segments, exact)` specificity contribute.
- Their attributes are combined in theme order using
  `Highlight_styles.(++)`.
- The order of `scopes` does **not** matter; they are treated as a set.

Internally the implementation precompiles the theme into an index and keeps
a small cache keyed by canonicalised scope sets. In the worst case the work
is linear in the number of rules times the number of scopes, but repeated
calls for the same scope sets are typically much cheaper. This makes the
function suitable for per-token use in the highlighter.

```ocaml
let open Chat_tui in
let theme = Highlight_theme.github_dark in
let kw = Highlight_theme.attr_of_scopes theme ~scopes:["keyword"; "source.ocaml"] in
let str = Highlight_theme.attr_of_scopes theme ~scopes:["string"; "source.ocaml"] in
(* Compose with the current attribute if needed: *)
let strong_kw = Notty.A.(kw ++ st bold) in
```

Integrating with the highlighter:

```ocaml
let engine = Highlight_tm_engine.create ~theme:Highlight_theme.github_dark in
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
- Complexity: in the worst case resolution cost is linear in (number of
  rules × number of scopes per token). Internally a small cache keyed by
  canonicalised scope sets ensures that repeated calls with the same scopes
  are typically much cheaper.

---

## Examples

Inline markdown emphasis with the dark palette:

```ocaml
let bold =
  Chat_tui.Highlight_theme.attr_of_scopes
    Chat_tui.Highlight_theme.github_dark
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
let engine = Chat_tui.Highlight_tm_engine.create ~theme:Chat_tui.Highlight_theme.github_dark in
let (_spans : (Notty.A.t * string) list list) =
  Chat_tui.Highlight_tm_engine.highlight_text engine ~lang:(Some "markdown") ~text:code
```

---

## See also

- [`Notty.A` documentation](https://pqwy.github.io/notty/doc/Notty.A.html) — colours and styles
- [`Highlight_tm_engine`](highlight_tm_engine.doc.md) — produces `(attr * text)` spans
- [`Highlight_styles`](highlight_styles.doc.md) — concise constructors and helpers used by themes
- [`Renderer`](renderer.doc.md) — where attributes are applied to draw chat UI

## Additional implementation notes

The following retained notes describe the original local/controller implementation.
For native/daemon ownership and projections use [the current integration guide](../../agent-server/embedding.md).
Shared rendering APIs remain useful; legacy persistence/controller assumptions
are not daemon session ownership. Consult the current `.mli` for exact signatures.

### `Chat_tui.Highlight_theme`

Theme definitions and lookup utilities for syntax-highlighting inside the
terminal user-interface.

The module maps **TextMate scope names** – as produced by the tokenizer used in the TUI – to [`Notty.A.t`][notty-a] attributes. The
resulting attribute is passed to the renderer so that each token is drawn in
the right colour.

* Namespace: `Chat_tui.Highlight_theme`
* Depends on: `Notty` (for colours and attributes), `Chat_tui.Highlight_styles`

## Quick start

```ocaml
(* Choose a built-in theme … *)
let theme = Chat_tui.Highlight_theme.github_dark

(* … and obtain the attribute for a set of scopes. *)
let base =
  Chat_tui.Highlight_theme.attr_of_scopes
    theme ~scopes:[ "keyword.operator"; "source.ocaml" ]
in

(* [base] is the themed attribute for the best-matching scopes.
   You can compose styles using Chat_tui.Highlight_styles helpers: *)
let attr = Chat_tui.Highlight_styles.(base ++ underline)
```

Customising colour using helpers from `Chat_tui.Highlight_styles`:
```ocaml
let theme = Chat_tui.Highlight_theme.github_dark in
let base =
  Chat_tui.Highlight_theme.attr_of_scopes theme ~scopes:[ "string" ]
in
let blue = Chat_tui.Highlight_styles.fg_hex "#79B8FF" in
let attr = Chat_tui.Highlight_styles.(base ++ blue)
```

## API overview

| Value | Description |
|-------|-------------|
| `type t` | Opaque theme value (internally an ordered list of `prefix -> attribute` rules). |
| `empty` | Theme with no rules – always yields `Chat_tui.Highlight_styles.empty` (equivalently `Notty.A.empty`). Useful as a neutral base you can extend yourself. |
| `github_dark` | Palette that approximates GitHub Dark Default using truecolor; includes link, heading, diff, and code-chip styling. |
| `attr_of_scopes` | `t -> scopes:string list -> Notty.A.t` – composes the attributes of the best-matching rules for the given scopes. |

## Matching semantics

1. For every *scope* in the `scopes` list, consider all rules whose `prefix`
   matches the scope on dot-segment boundaries (the scope is exactly the
   prefix, or starts with `prefix ^ "."`).
2. Each matching rule is assigned a **specificity** key
   `(segments, exact)` where `segments` is the number of dot-separated
   segments in `prefix` and `exact` is `1` for an exact match and `0`
   for a proper prefix.
3. Among all matches across all scopes, only rules with maximal
   specificity contribute to the result.
4. Attributes from these rules are composed in theme order using
   `Chat_tui.Highlight_styles.(++)`. Later rules in this maximum-specificity
   group override earlier ones for overlapping properties (e.g. foreground
   colour); styles are unioned.
5. The order of scopes in the list does not matter; the list is treated as a
   set.
6. When no rule matches, the function returns `Chat_tui.Highlight_styles.empty`
   (equivalently `Notty.A.empty`).

This mirrors how most TextMate colour-schemes are resolved. Fine-grained
palettes are typically expressed using more specific prefixes (e.g.
`"keyword.operator.logical"` vs `"keyword"`).

## Customising a theme

The public type `t` is opaque; there is currently no constructor for rules in
the public API. You can still customise the output in a few ways:

- Compose additional styles at use sites:
  ```ocaml
  let base =
    Chat_tui.Highlight_theme.attr_of_scopes theme ~scopes
  in
  let attr = Chat_tui.Highlight_styles.(base ++ underline)
  ```
- Overlay your own small mapping before or after calling `attr_of_scopes`,
  e.g. detect specific scopes and substitute your own attribute, otherwise
  delegate to the theme.
- Contribute or maintain an alternate palette by editing the module and using
  `github_dark` as an example.

## Performance considerations

The implementation is intentionally simple – a nested loop over rules and
scopes – and therefore **O(m × n)** where *m* is the number of rules and *n* the number of
scopes.  In practice both numbers are small (dozens), so a per-token call is
perfectly fine even on slow terminals.

## Limitations

* Built-in palettes primarily use colours, and apply a few styles (e.g. `bold`, `italic`, `underline`) for headings and links.
* No attempt is made to validate hierarchical scope paths; matching is purely
  string-prefix-based with dot-segment boundaries. Scopes are compared as
  strings; the function does not understand individual segments.
* Theme files are hard-coded.  Future versions might load “.tmTheme”/JSON files
  at runtime.

## Relation to other modules

`Highlight_theme` is consumed by [`Highlight_tm_engine`], the TextMate-based
tokeniser that produces scopes for each token.  The engine feeds
those scopes to `attr_of_scopes` during rendering.

## References

* [TextMate scope documentation](https://macromates.com/manual/en/language_grammars) – nomenclature used by this module.
* [Notty documentation](https://pqwy.github.io/notty/doc/Notty.html) – colours
  and attributes.

[notty-a]: https://pqwy.github.io/notty/doc/Notty/A/index.html



## Built-in palettes

There is currently a single built-in palette:

- github_dark: Approximates GitHub Dark Default using truecolor via helpers
  from `Chat_tui.Highlight_styles` (`fg_hex`, `bg_hex`). It includes
  dedicated styling for headings, links, inline code chips, diffs, and patch
  metadata.
