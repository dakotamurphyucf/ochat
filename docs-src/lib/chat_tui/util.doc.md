# `Chat_tui.Util` – Pure text-processing helpers

`Chat_tui.Util` is a grab-bag of small, side-effect-free helpers that make it
easier to prepare arbitrary user input for safe use inside a terminal user
interface.

Although the functions live in a Ochat front-end they do **not** depend on
any of the project-specific types.  Re-using them in other applications is as
simple as adding `open Chat_tui.Util`.

---

## Table of contents

1. [Sanitising control characters – `sanitize`](#sanitize)
2. [Truncating long strings – `truncate`](#truncate)
3. [Byte-budget aware wrapping – `wrap_line`](#wrap_line)
4. [Known limitations](#known-limitations)

---

### `sanitize` <a id="sanitize"></a>

```ocaml
val sanitize : ?strip:bool -> string -> string
```

Replaces every ASCII control character – everything below `U+0020` plus
`DEL (U+007F)` – with a space.  Newlines are kept so callers can preserve
intentional line breaks while TABs expand to four spaces to avoid cursor
jumps.

The optional `strip` parameter (default `true`) removes leading and trailing
whitespace from the *sanitised* result.

Example – filter unprintable bytes that sometimes creep into markdown blocks:

```ocaml
# Chat_tui.Util.sanitize "hello\b world";;
- : string = "hello  world"
```

### `truncate` <a id="truncate"></a>

```ocaml
val truncate : ?max_len:int -> string -> string
```

Cuts a string to *at most* `max_len` bytes (defaults to `300`).  When data is
lost an ellipsis (`…`, U+2026) is appended so the reader can tell that the
value was shortened.

Leading and trailing whitespace do not count towards the budget because they
are stripped first.

Example – enforce a 60 byte limit on log entries:

```ocaml
let msg = Chat_tui.Util.truncate ~max_len:60 really_long_message
``` 

### `wrap_line` <a id="wrap_line"></a>

```ocaml
val wrap_line : limit:int -> string -> string list
```

Splits a UTF-8 string into slices whose *byte* length does not exceed
`limit`.  The function guarantees that cuts never happen inside a multi-byte
code-point, therefore every part is valid UTF-8 by itself.

Display width is **not** considered – East-Asian wide glyphs still count as
one unit, combining sequences stay together and so on.  For UI layout one
usually pipes the result into a Notty widget that performs its own
width-aware wrapping.

```ocaml
# Chat_tui.Util.wrap_line ~limit:4 "👍👍👍";;  (* each thumbs-up = 4 bytes *)
- : string list = ["👍"; "👍"; "👍"]
```

### Known limitations <a id="known-limitations"></a>

1. **Grapheme clusters & width.**  `wrap_line` looks only at the UTF-8 byte
   sequence.  Wide characters, combining accents or zero-width joiners might
   break visual alignment.  The TUI currently relies on Notty to fix this at
   render time.
2. **Performance.**  All helpers allocate fresh strings.  For the typical
   Ochat message sizes this is not an issue but it might matter in
   high-frequency code paths.

---

*Last updated*: 2025-07-20

