# `Chat_tui.Highlight_theme`

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
