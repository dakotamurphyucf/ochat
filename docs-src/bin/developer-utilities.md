# Terminal and highlighting utilities

These installed utilities are for development and diagnostics, not alternative
agent hosts. See [commands](README.md) for the normal local TUI and server paths.

## highlight-debug

Print TextMate token scopes and byte ranges for a file:

```sh
highlight-debug -lang ocaml lib/history_entry.ml
highlight-debug -split-markdown docs-src/README.md
```

`FILE` is required; `-lang LANG` overrides language detection and
`-split-markdown` splits fenced code from Markdown text. Without splitting,
Markdown is processed as one document. Output contains source text, so use it
only on material appropriate for diagnostic logs. Missing grammar errors can be
printed without a nonzero exit; inspect output, not only status. The source is
[bin/highlight_debug.ml](../../bin/highlight_debug.ml).

## terminal_render

Render a bitmap as colored half-block characters:

```sh
terminal_render assets/tui-snapshot.png 80
```

Arguments are `FILE` and optional maximum columns. It uses the current terminal
width or 80 when no width is available, and depends on ImageMagick's `magick`
command through Bimage. This is not an agent image-import tool. Always supply
the file argument; the current missing-argument guard does not catch an empty
argument list before indexing it. See [source](../../bin/terminal_render.ml).

## Source-only and historical demos

[`bin/gpt.ml` documentation](gpt.doc.md) and
[`bin/mp_prompt.ml` documentation](mp_prompt.doc.md) describe sources not registered as current installed
executables in `bin/dune`. Retained pages describe their source, not launchable
public commands. `piaf_example` and `eio_get` are source-tree demonstrations
without public executable names; inspect their source before running network
examples; [eio_get notes](eio_get.doc.md) describe one of them. `dsl_script` is
installed but is a [hard-coded language demo](dsl_script.doc.md).
