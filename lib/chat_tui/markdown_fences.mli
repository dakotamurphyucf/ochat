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

(** [split_inline s] scans [s] for inline code spans delimited by backticks
    and returns an alternating sequence of [Inline_text] and [Inline_code].

    Behaviour and limitations:
    - Backtick runs of length [n >= 1] are recognized, and a code span is
      formed by the next run with at least [n] consecutive backticks. (Only
      [n] backticks are consumed as the delimiter; any remaining backticks are
      treated as normal text.)
    - A backtick preceded by a backslash is treated as a literal character and
      does not start or end a code span.
    - Nesting is not supported.
    - If a closing delimiter is missing, the opening delimiter is treated as
      plain text (it is not dropped). *)
val split_inline : string -> inline list
