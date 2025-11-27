# Chat_tui.Highlight_grammars

Vendored or built-in TextMate grammars used by the terminal UI.

`Chat_tui.Highlight_grammars` exposes a small set of helper functions, each of
which registers one language grammar into a
`Chat_tui.Highlight_tm_loader.registry`. The registry can then be attached to
`Chat_tui.Highlight_tm_engine` to enable syntax highlighting for chat messages
and code blocks.

- Namespace: `Chat_tui.Highlight_grammars`
- Depends on: `Core` (error handling and IO), `Jsonaf` (parsing JSON
  grammars), `Chat_tui.Highlight_tm_loader` (registry construction and grammar
  loading), `textmate-language` (underlying tokenizer, via the loader)

In most cases you do not have to call these functions directly; the
`Chat_tui.Highlight_registry` module builds a singleton registry that is reused
across the UI. Use `Chat_tui.Highlight_registry.get ()` when you just need
"the default set of grammars".

## Quick start: custom registry

Construct a registry and selectively install grammars:

```ocaml
open Core

let registry () =
  let reg = Chat_tui.Highlight_tm_loader.create_registry () in
  let add f =
    match f reg with
    | Ok () -> ()
    | Error e ->
      Core.eprintf "grammar load failed: %s\n" (Core.Error.to_string_hum e)
  in
  List.iter
    ~f:add
    [ Chat_tui.Highlight_grammars.add_ocaml
    ; Chat_tui.Highlight_grammars.add_dune
    ; Chat_tui.Highlight_grammars.add_opam
    ; Chat_tui.Highlight_grammars.add_shell
    ; Chat_tui.Highlight_grammars.add_diff
    ; Chat_tui.Highlight_grammars.add_json
    ; Chat_tui.Highlight_grammars.add_html
    ; Chat_tui.Highlight_grammars.add_markdown
    ];
  reg
```

Attach the registry to the highlighting engine:

```ocaml
let reg = registry () in
let theme = Chat_tui.Highlight_theme.default_dark in
let engine =
  Chat_tui.Highlight_tm_engine.(
    create ~theme |> with_registry ~registry:reg)
in
(* [engine] now understands "ocaml", "bash", "json", "diff", "markdown", … *)
```

If you do not care about customisation, you can instead use the shared
registry:

```ocaml
let reg = Chat_tui.Highlight_registry.get ()
```

and pass `reg` to `Highlight_tm_engine.with_registry`.

## API overview

| Function | Scope name(s) | Typical language tags / extensions | Notes |
|---------|----------------|-------------------------------------|-------|
| `add_ocaml` | `source.ocaml` | `ocaml`, `ml`, `mli` | Tries vendored grammar JSON, then falls back to a small embedded grammar. |
| `add_dune` | `source.dune` | `dune`, `dune-project`, `dune-workspace` | Embedded grammar for Dune files. |
| `add_opam` | `source.opam` | `opam` | Embedded grammar for `.opam` manifests. |
| `add_shell` | `source.shell` | `sh`, `bash` | Embedded grammar for POSIX shell/Bash. |
| `add_diff` | `source.diff` | `diff`, `patch` | Embedded grammar highlighting headers and line prefixes. |
| `add_json` | `source.json` | `json` | Embedded grammar for JSON structures. |
| `add_markdown` | `source.gfm` | `md`, `markdown`, `gfm` | Loads a vendored GitHub-flavoured Markdown grammar from disk. |
| `add_html` | `text.html.basic`, `text.html.derivative` | `html`, inline HTML in markdown | Uses a minimal embedded grammar unless a vendored TextMate HTML grammar is present. |
| `add_ochat_apply_patch` | `source.ochat-apply-patch` | internal | Grammar tuned for `ochat`'s `apply_patch` tool output, recognising the banner header, per-file operations (Add/Update/Delete), numbered snippet lines, and delegating inner hunks to the Diff grammar. |

All functions share the same contract:

- they **mutate** the supplied registry by adding one grammar (or, for
  `add_html`, two related grammars);
- they return `Ok ()` on success;
- they return `Error _` if no valid grammar could be installed.

It is safe to ignore errors if you are happy to silently fall back to plain
rendering for the affected language. The default registry implementation logs
failures to `stderr` and keeps going.

## Error handling and fallbacks

The implementations differ slightly in how they source grammars:

- **Embedded-only grammars**: `add_dune`, `add_opam`, `add_shell`, `add_diff`,
  `add_json`, and `add_ochat_apply_patch` parse fixed JSON strings compiled
  into the binary. Errors here indicate an internal bug in the application.

- **Vendored with fallback**:
  - `add_ocaml` first tries `lib/chat_tui/grammars/ocaml.json` and then
    `lib/chat_tui/grammars/ocaml.tmLanguage.json`. If both attempts fail, it
    falls back to a lightweight built-in grammar that recognises comments,
    strings, numbers, operators, and a few syntactic forms.
  - `add_html` similarly prefers `lib/chat_tui/grammars/html.tmLanguage.json`
    and otherwise registers a minimal built-in HTML grammar. It always
    registers a small "shim" grammar whose scope name is
    `text.html.derivative` and which simply includes `text.html.basic`. This
    keeps Markdown grammars that refer to `text.html.derivative` working even
    without a full HTML bundle.

- **Vendored only**:
  - `add_markdown` loads
    `lib/chat_tui/grammars/markdown.tmLanguage.json`. If the file is missing
    or invalid, the function returns `Error _`; there is no built-in fallback.
    The rest of the UI treats such failures as "no Markdown grammar" and
    falls back to plain-text rendering.

Because paths are relative (`lib/chat_tui/…`), code that embeds this library
must either run from the project root or ensure those paths exist in its
current working directory.

## Known issues and limitations

- The bundled grammars are intentionally small and biased towards the needs of
  the TUI. They do not aim to provide full coverage of every language
  construct.
- The minimal HTML grammar does not attempt to be a complete HTML parser; it
  only recognises tags, attributes, quoted attribute values, entities, and
  comments. Complex templating languages embedded in HTML will not be
  highlighted.
- `add_markdown` depends on a vendored GitHub-Flavoured Markdown grammar and
  does not yet offer an embedded fallback. When the file is not available,
  markdown blocks are rendered as plain text.
- There is currently no public API for removing or replacing grammars in a
  registry; to change the set of installed grammars, create a fresh registry
  and call the appropriate `add_*` functions again.

## Related modules

- `Chat_tui.Highlight_tm_loader` — defines the `registry` type and low-level
  helpers for loading TextMate grammars from `Jsonaf.t` or JSON files.
- `Chat_tui.Highlight_tm_engine` — consumes a registry and a theme to produce
  coloured spans for text and code blocks.
- `Chat_tui.Highlight_theme` — maps TextMate scope names (e.g.
  `"keyword.operator"`) to `Notty.A.t` attributes.
- `Chat_tui.Highlight_registry` — exposes a process-wide singleton registry
  pre-populated using the functions in this module.

