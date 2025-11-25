# `Chat_tui.Markdown_fences` – Fenced blocks and inline-code splitter

`Chat_tui.Markdown_fences` provides two focused helpers for working with
markdown-like text in the terminal UI:

- Split a string into block-level segments, separating free text from fenced
  code blocks delimited by exactly three backticks (```), or by three tildes
  (~~~).
- Split a single line into inline parts, separating plain text from segments
  enclosed in single backticks.

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
  (** Split a line into plain text and single-backtick code spans. *)
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

- Recognizes only single-backtick spans.
- Does not support escaping, multi-backtick delimiters, or nesting.
- If a closing backtick is missing, the unterminated code is treated as plain
  text and the opening backtick is dropped.

Example — current behavior with a closed span:

```ocaml
let parts = Chat_tui.Markdown_fences.split_inline "a `b` c" in
(* Note: current implementation duplicates the code contents in the
   surrounding text segment. *)
parts
= [ Chat_tui.Markdown_fences.Inline_text "a "
  ; Chat_tui.Markdown_fences.Inline_code "b"
  ; Chat_tui.Markdown_fences.Inline_text "b c"
  ]
```

Example — missing closing backtick:

```ocaml
Chat_tui.Markdown_fences.split_inline "x `y"
= [ Chat_tui.Markdown_fences.Inline_text "x "; Chat_tui.Markdown_fences.Inline_text "y" ]
```

---

## Known issues and notes

- `split_inline` currently accumulates characters inside a closed code span into
  the surrounding `Inline_text` buffer as well, which can lead to duplication in
  the returned list (see example). Callers should account for this until the
  implementation is fixed.
- The block parser is intentionally minimal: it does not accept fences longer
  than three characters, nor does it support tab indentation on the fence line.

---

Last updated: 2025-08-10

