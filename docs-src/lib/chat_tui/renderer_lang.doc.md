# `Renderer_lang` — language inference for `read_file` output

`Chat_tui.Renderer_lang` provides a tiny helper for mapping file extensions to
TextMate language identifiers.

## API

```ocaml
val lang_of_path : string -> string option
```

Example:

```ocaml
Chat_tui.Renderer_lang.lang_of_path "foo.ml" = Some "ocaml";
Chat_tui.Renderer_lang.lang_of_path "README.md" = Some "markdown";
Chat_tui.Renderer_lang.lang_of_path "data.json" = Some "json";
Chat_tui.Renderer_lang.lang_of_path "script.sh" = Some "bash";
Chat_tui.Renderer_lang.lang_of_path "no_extension" = None
```

## Notes

- The mapping is intentionally conservative: unknown extensions return `None`.
- Returning `Some "markdown"` is still useful: the message renderer uses the normal
  markdown pipeline rather than treating markdown as a single monolithic code block.

