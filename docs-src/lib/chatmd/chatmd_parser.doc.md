# ChatMD parser

`Chatmd_parser.document` consumes the shared ChatMD token stream and produces
`Chatmd_ast.document`. The lexer recognizes the closed vocabulary described in
the [language reference](../../overview/chatmd-language.md); unknown markup
inside recognized elements remains text. The parser checks matching tags and
rejects non-whitespace top-level text.

## One grammar, two builders

The authoritative Menhir grammar is
[`chatmd_parser_engine.mly`](../../../lib/chatmd/chatmd_parser_engine.mly).
Its builder parameter changes node construction, not accepted syntax.

`Chatmd_parser` instantiates the ordinary semantic builder, preserving the
existing `document` and token API. `Chatmd_located_ast.Make` instead retains each
element's exact parser-produced start/end bytes and input owner. The lexer and
both builders use one token representation. No second grammar, text search or
AST-to-token matching reconstructs locations.

```ocaml
let parse text =
  let next = Chatmd_lexer.create () in
  Chatmd_parser.document next (Lexing.from_string text)
```

This low-level operation does not load imports, preprocess a prompt or convert
runtime declarations. Use [Prompt.Chat_markdown](prompt.doc.md) for those stages.
Its owned preparation path supplies input/token/depth limits and checkpoints.
Plain text nodes do not claim declaration ranges; recognized element ranges are
nonempty half-open byte spans in the exact effective input.

[Builder interface](../../../lib/chatmd/chatmd_parser_builder.mli) ·
[Located nodes](../../../lib/chatmd/chatmd_located_ast.mli) ·
[Input ownership](../../../lib/chatmd/chatmd_parser_input.mli)
