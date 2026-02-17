# `Chat_tui.Markdown_fences` – Fenced blocks and inline-code splitter

`Chat_tui.Markdown_fences` provides two focused helpers for working with
markdown-like text in the terminal UI:

- Split a string into block-level segments, separating free text from fenced
  code blocks delimited by exactly three backticks (```), or by three tildes
  (~~~).
- Split a single line into inline parts, separating plain text from segments
  enclosed in backticks (runs of one or more backticks).

The implementation is intentionally conservative and tuned for rendering, not
full Markdown compliance.

---

## API overview

```ocaml
module Markdown_fences : sig
  type segment =
    | Text of string
    | Code_block of { lang : string option; code : string }

  val split : string -> segment list
  (** Partition a string into text and fenced code blocks. *)

  type inline =
    | Inline_text of string
    | Inline_code of string

  val split_inline : string -> inline list
  (** Split a line into plain text and backtick-delimited code spans. *)
end
```

---

## `split` — block-level segmentation

```ocaml
val split : string -> segment list
```

- Recognizes fences only at the start of a line (after up to three spaces).
- Accepts exactly three backticks (```) or exactly three tildes (~~~).
- The closing fence must match the opening fence character and length.
- The language is parsed as the first non-empty token on the opening fence line
  and returned as `lang` (e.g. `Some "ocaml"`). Any remaining characters on
  that line are ignored.
- Newlines in both `Text` and `Code_block.code` are preserved.
- If the input ends while inside a fence, the entire unterminated region
  (including the opening fence line) is returned as a single `Text` segment.

Example — code block with language:

```ocaml
let input = "before\n```ocaml\nlet x = 1\n```\nafter" in
let expected =
  [ Chat_tui.Markdown_fences.Text "before"
  ; Chat_tui.Markdown_fences.Code_block { lang = Some "ocaml"; code = "let x = 1" }
  ; Chat_tui.Markdown_fences.Text "after"
  ] in
Chat_tui.Markdown_fences.split input = expected
```

Example — unclosed fence becomes plain text:

```ocaml
let input = "```python\nprint(1)" in
Chat_tui.Markdown_fences.split input
= [ Chat_tui.Markdown_fences.Text "```python\nprint(1)" ]
```

### Limitations

- Only exactly three fence characters are recognized; longer fences are treated
  as regular text.
- Only spaces count toward the “up to three leading spaces” rule; tabs do not.

---

## `split_inline` — inline backtick splitting

```ocaml
val split_inline : string -> inline list
```

- Recognizes backtick runs of length `n >= 1` and forms a code span by finding
  a closing delimiter that has at least `n` consecutive backticks.
  (Only `n` backticks are consumed as the delimiter; any extra backticks are
  treated as normal text.)
- A backtick preceded by a backslash is treated as a literal character and
  does not start or end a code span. (The backslash is preserved; no unescaping
  is performed.)
- Nesting is not supported.
- If a closing delimiter is missing, the opening delimiter is treated as plain
  text (it is not dropped).

Example — current behavior with a closed span:

```ocaml
let parts = Chat_tui.Markdown_fences.split_inline "a `b` c" in
parts
= [ Chat_tui.Markdown_fences.Inline_text "a "
  ; Chat_tui.Markdown_fences.Inline_code "b"
  ; Chat_tui.Markdown_fences.Inline_text " c"
  ]
```

Example — missing closing backtick:

```ocaml
Chat_tui.Markdown_fences.split_inline "x `y"
= [ Chat_tui.Markdown_fences.Inline_text "x `y" ]
```

Example — multi-backtick code span (content can include single backticks):

```ocaml
Chat_tui.Markdown_fences.split_inline "a ``b`c`` d"
= [ Chat_tui.Markdown_fences.Inline_text "a "
  ; Chat_tui.Markdown_fences.Inline_code "b`c"
  ; Chat_tui.Markdown_fences.Inline_text " d"
  ]
```

---

## Known issues and notes

- The block parser is intentionally minimal: it does not accept fences longer
  than three characters, nor does it support tab indentation on the fence line.

---

Last updated: 2026-02-17

