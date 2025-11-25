(**
  Parse fenced code blocks and inline code spans from markdown-like text.

  The module provides two small, allocation-friendly utilities:
  - [split] separates a string into block-level segments, recognizing
    fenced code blocks delimited by exactly three backticks (```) or three
    tildes (~~~).
  - [split_inline] scans a single line for inline code spans delimited by
    single backticks.

  The parsers are intentionally conservative and designed for rendering in
  the TUI rather than full markdown compliance. See the function docs for
  invariants and known limitations.
*)

(** Block-level segment produced by {!val:split}.

    - [Text s] is a contiguous region of non-code text. Newlines inside [s]
      are preserved.
    - [Code_block { lang; code }] is the body captured between a matching
      opening and closing fence. [lang] is the first non-empty token that
      follows the opening fence on the same line (e.g. ["ocaml"] for
      [```ocaml]), or [None] if absent. [code] contains the lines strictly
      between the fences, without the fence markers themselves. *)
type segment =
  | Text of string
  | Code_block of
      { lang : string option
      ; code : string
      }

(** [split s] partitions [s] into text and fenced code blocks.

    Recognizes a fence only if it appears at the start of a line, optionally
    preceded by up to three spaces, and consists of exactly three identical
    characters chosen from backtick (`) or tilde (~). The closing fence must
    use the same character and length.

    The optional language/info string is parsed as the first non-empty token
    after the opening fence and exposed as [lang]. Any remaining characters on
    the fence line are ignored.

    - Unclosed blocks: if the input ends while inside a fence, the entire
      unterminated region (including the opening fence line) is returned as a
      single [Text] segment.
    - Newlines: segment boundaries always occur on line boundaries; newlines in
      [Text] and [Code_block.code] are preserved as in the input.

    Limitations:
    - Only exactly three backticks or three tildes are treated as fences; more
      than three are not recognized.
    - Tabs before the fence are not treated as indentation; only spaces count
      toward the “up to three leading spaces” rule. *)
val split : string -> segment list

type inline =
  | Inline_text of string
  | Inline_code of string

(** [split_inline s] scans [s] for inline code spans delimited by single
    backticks and returns an alternating sequence of [Inline_text] and
    [Inline_code].

    Behaviour and limitations:
    - Only single backticks are recognized; backtick escaping and multi-
      backtick delimiters are not supported.
    - Nesting is not supported.
    - If a closing backtick is missing, the unterminated code content is
      treated as plain text, and the opening backtick is dropped.
    - Known issue: characters captured inside a closed code span are also
      accumulated into the surrounding [Inline_text] buffer, which can lead to
      duplication in the returned list (e.g. ["a ", `b`, "b c"]). Callers
      should account for this until the implementation is fixed. *)
val split_inline : string -> inline list
