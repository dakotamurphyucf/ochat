# Chat_tui.Highlight_registry

Process-wide TextMate grammar registry shared across the terminal UI.

`Chat_tui.Highlight_registry` lazily constructs a
`Chat_tui.Highlight_tm_loader.registry` and populates it with the built-in
grammars from `Chat_tui.Highlight_grammars`. The resulting registry is reused
by all components of the TUI that need syntax highlighting.

- Namespace: `Chat_tui.Highlight_registry`
- Depends on: `Core` (logging failures), `Chat_tui.Highlight_tm_loader`
  (registry type and creation), `Chat_tui.Highlight_grammars` (installing
  grammars), `Chat_tui.Highlight_tm_engine` (consuming the registry)

In most cases you do not need to construct your own registry. Use
`Chat_tui.Highlight_registry.get ()` when you just want "the standard set of
grammars" understood by the UI.

## Quick start

Attach the shared registry to a highlighting engine:

```ocaml
let reg = Chat_tui.Highlight_registry.get () in
let engine =
  Chat_tui.Highlight_tm_engine.(
    create ~theme:Chat_tui.Highlight_theme.default_dark
    |> with_registry ~registry:reg)
in
let ocaml_lines =
  Chat_tui.Highlight_tm_engine.highlight_text
    engine ~lang:(Some "ocaml") ~text:"let x = 1\nlet y = x + 2"
in
List.length ocaml_lines = 2
```

## API

### `get : unit -> Highlight_tm_loader.registry`

`get ()` returns the shared registry pre-populated with the built-in grammars.

- On the first call it:
  - creates a fresh `Chat_tui.Highlight_tm_loader.registry` via
    `Chat_tui.Highlight_tm_loader.create_registry ()`;
  - calls each of the `Chat_tui.Highlight_grammars.add_*` helpers to install
    grammars;
  - logs any failures using `Core.printf` with a human-readable error
    message;
  - returns the resulting registry.
- On subsequent calls it returns the same registry value without re-running
  the installation steps.

Because the registry value is shared:

- adding additional grammars via
  `Chat_tui.Highlight_tm_loader.add_grammar_jsonaf` or
  `Chat_tui.Highlight_tm_loader.add_grammar_jsonaf_file` is visible to all
  callers;
- there is no public API for removing grammars once installed.

The typical consumer of the registry is
`Chat_tui.Highlight_tm_engine.with_registry`.

## Registry contents

`get ()` installs the same curated set of grammars that is described in
`Chat_tui.Highlight_grammars`:

- OCaml — `add_ocaml`, scope name `source.ocaml` (covers `.ml` / `.mli`)
- Dune — `add_dune`, scope name `source.dune`
- OPAM — `add_opam`, scope name `source.opam`
- Shell / Bash — `add_shell`, scope name `source.shell`
- Unified diff — `add_diff`, scope name `source.diff`
- Ochat apply-patch format — `add_ochat_apply_patch`, scope name
  `source.ochat-apply-patch`
- JSON — `add_json`, scope name `source.json`
- HTML — `add_html`, scope names `text.html.basic` and `text.html.derivative`
- Markdown — `add_markdown`, scope name `source.gfm`

If a particular grammar fails to load (for example, a vendored JSON file is
missing or invalid), the failure is logged and the rest of the registry is
still constructed. Callers do not need to handle errors explicitly when using
`Chat_tui.Highlight_registry.get`.

## Additional usage examples

### Share the registry between multiple components

Because `get ()` returns the same registry instance each time, you can freely
stash it in your own state or re-request it where needed:

```ocaml
type model =
  { highlighter : Chat_tui.Highlight_tm_engine.t
  ; registry    : Chat_tui.Highlight_tm_loader.registry
  }

let create_model () =
  let registry = Chat_tui.Highlight_registry.get () in
  let highlighter =
    Chat_tui.Highlight_tm_engine.(
      create ~theme:Chat_tui.Highlight_theme.default_dark
      |> with_registry ~registry)
  in
  { highlighter; registry }
```

### Extend the shared registry with a custom grammar

If you want to add an extra grammar on top of the built-in set, you can mutate
the shared registry after calling `get ()`:

```ocaml
let reg = Chat_tui.Highlight_registry.get () in
let result =
  Chat_tui.Highlight_tm_loader.add_grammar_jsonaf_file
    reg ~path:"grammars/my-language.tmLanguage.json"
in
match result with
| Ok () -> ()
| Error e ->
  Core.eprintf "failed to load custom grammar: %s\n"
    (Core.Error.to_string_hum e)
```

All subsequent highlighting that uses the same registry (through `get ()`) can
now resolve the new language tag.

## Error handling and fallbacks

- `Highlight_registry.get` never raises and never returns an error result; it
  always returns a `registry`.
- Loading failures for individual grammars are reported via `Core.printf` but
  do not stop construction of the registry.
- When a grammar is missing (because loading failed or the file is absent),
  the downstream `Highlight_tm_engine` simply falls back to plain-text
  rendering for that language (see its `fallback_reason` type).

## Known issues and limitations

- Grammar files are resolved via relative paths under `lib/chat_tui/grammars`.
  Running the application from a different working directory or repackaging it
  without these files may result in some grammars failing to load.
- Errors are currently logged with `Core.printf` to standard output, which may
  interleave with normal UI output. In batch or logging-heavy environments you
  may prefer to redirect or filter stdout.
- The registry is global to the process. If you need completely independent
  sets of grammars (for example, for sandboxed sessions or tests that should
  not interfere with each other), construct your own registries with
  `Chat_tui.Highlight_tm_loader.create_registry` instead of using
  `Chat_tui.Highlight_registry`.
- There is no API for removing or replacing grammars once installed; if you
  need a different set, build a fresh registry.

## Related modules

- `Chat_tui.Highlight_grammars` — defines the individual `add_*` functions
  used to populate the registry.
- `Chat_tui.Highlight_tm_loader` — low-level registry type and functions for
  loading grammars.
- `Chat_tui.Highlight_tm_engine` — consumes a registry and theme to turn text
  into `Notty` spans.
- `Chat_tui.Highlight_theme` — maps TextMate scopes to visual attributes.

