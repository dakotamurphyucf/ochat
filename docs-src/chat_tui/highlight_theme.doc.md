# `Chat_tui.Highlight_theme`

Theme definitions and lookup utilities for syntax-highlighting inside the
terminal user-interface.

The module maps **TextMate scope names** – as produced by the tokenizer used in the TUI – to [`Notty.A.attr`][notty-a] values.  The
resulting attribute is passed to the renderer so that each token is drawn in
the right colour.

* Namespace: `Chat_tui.Highlight_theme`
* Depends on: `Notty` (for colours and attributes), `Chat_tui.Highlight_styles`

## Quick start

```ocaml
open Chat_tui.Highlight_theme

(* Choose one of the built-ins … *)
let theme = default_dark (* or [default_light] *)

(* … and obtain the attribute for a set of scopes. *)
let attr = attr_of_scopes theme ~scopes:[ "keyword.operator" ; "source.ocaml" ]

(* [attr] is the themed attribute for the best-matching scope.
   You can compose styles if desired: Notty.A.(attr ++ st underline) *)
```

Customising colour using helpers from Chat_tui.Highlight_styles:
```ocaml
let theme = Chat_tui.Highlight_theme.default_dark in
let base = Chat_tui.Highlight_theme.attr_of_scopes theme ~scopes:["string"] in
let blue = Chat_tui.Highlight_styles.fg_hex "#79B8FF" in
let attr = Notty.A.(base ++ blue)
```

## API overview

| Value | Description |
|-------|-------------|
| `type t` | Opaque theme value (internally an ordered set of prefix -> attribute rules). |
| `empty` | A theme with no rules – always yields `Notty.A.empty`. Useful as a neutral base you can extend yourself. |
| `default_dark` | Reasonable defaults for dark terminals (black background). |
| `default_light` | Counterpart of `default_dark` adapted to light backgrounds. |
| `github_dark` | Palette that approximates GitHub Dark Default using truecolor; includes link, heading, diff, and code-chip styling. |
| `attr_of_scopes` | `t -> scopes:string list -> Notty.A.t` – picks the attribute for the best-matching scope. |

## Matching semantics

1. For every *scope* in the `scopes` list, iterate over the rules.
2. A rule matches when its `prefix` is a prefix of the scope string.
3. If multiple rules match the same scope, the longest prefix wins.
4. If several scopes match different rules, the overall winner is the rule with the
   longest prefix amongst all matches.
5. If multiple rules tie on prefix length, the earlier rule in the theme list wins.
6. The order of scopes in the list does not matter; only the single best match across the union of scopes is considered.
7. When no rule matches, the function returns `Notty.A.empty` (identity).

This mirrors how most TextMate colour-schemes are resolved. Fine-grained
palettes are expressed using longer, more specific prefixes (e.g.
"keyword.operator.logical" vs "keyword").

## Customising a theme

The public type `t` is opaque; there is currently no constructor for rules in
the public API. You can still customise the output in a few ways:

- Compose additional styles at use sites:
  ```ocaml
  let base = Chat_tui.Highlight_theme.attr_of_scopes theme ~scopes in
  let attr = Notty.A.(base ++ st underline)
  ```
- Overlay your own small mapping before or after calling `attr_of_scopes`,
  e.g. detect specific scopes and substitute your own attribute, otherwise
  delegate to the theme.
- Contribute or maintain an alternate palette by editing the module and using
  `default_dark`/`default_light` as examples.

## Performance considerations

The implementation is intentionally simple – a double `List.fold_left` – and
therefore **O(m × n)** where *m* is the number of rules and *n* the number of
scopes.  In practice both numbers are small (dozens), so a per-token call is
perfectly fine even on slow terminals.

## Limitations

* Built-in palettes primarily use colours, and apply a few styles (e.g. `bold`, `italic`, `underline`) for headings and links.
* No attempt is made to parse hierarchical scope paths; matching is purely
  string-prefix based.
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

- default_dark: Dark-terminal friendly colours with occasional bold/italic for headings and links.
- default_light: Light-background counterpart of default_dark.
- github_dark: Approximates GitHub Dark Default using truecolor via helpers from Chat_tui.Highlight_styles (fg_hex, bg_hex).