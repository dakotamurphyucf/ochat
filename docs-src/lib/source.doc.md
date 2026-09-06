# `Source` – immutable documents with positional helpers

`Source` is a tiny utility module that represents the contents of a text file
and provides a few convenience functions to:

* turn a raw `string` *or* a file on disk into an immutable document;
* query the character at a given offset safely, without exceptions;
* slice an arbitrary **span** (`left`, `right` positions) out of the document;
* combine two spans into one that covers both.

The module is purpose-built for lexers, parsers and type-checkers that need to
track accurate locations for error reporting while keeping the dependency
surface minimal – it only relies on `core` and `sexp` derivers.

---

## Quick overview

```ocaml
open! Core

let example () =
  let src = Source.make "Hello world" in
  let point offset = Source.{ line = 1; column = offset; offset } in
  let first = Source.{ left = point 0; right = point 5 } in
  let second = Source.{ left = point 5; right = point 11 } in
  assert (Option.equal Char.equal (Source.at src 0) (Some 'H'));
  assert (Option.is_none (Source.at src 1_000_000));
  assert (String.equal (Source.read src first) "Hello");
  assert (String.equal (Source.read src (Source.merge second first)) "Hello world")
;;
```

---

## API summary

### Types

* `Source.t` – the whole document (`path option` + `content`).
* `Source.position` – absolute location `(line, column, offset)`.
* `Source.span` – half-open interval `[left, right)` inside a document.

### Functions

| Function | Description |
|----------|-------------|
| `make : string -> t` | Build an in-memory document from a raw string. |
| `from_file : string -> t` | Legacy blocking file read; retains the supplied filename as metadata. |
| `length : t -> int` | Number of bytes in the document. O(1). |
| `at : t -> int -> char option` | Safe character lookup. O(1). |
| `read : t -> span -> string` | Extract a substring, clamping out-of-bounds spans. |
| `merge : span -> span -> span` | Smallest span covering the two inputs. |

In-memory helpers do not mutate the document. File reading is neither pure nor
bounded by the resulting span. For Eio code, use
`Source.make (Eio.Path.load path)` when filename metadata is unnecessary.

---

## Design decisions

1. **Byte offsets, not Unicode code points** – all positions and spans are
   expressed in *bytes* to keep arithmetic simple and predictable.  If you
   work with UTF-8 you may need to translate between byte offsets and
   character indices at the boundaries of your application.
2. **Eager file reading** – `from_file` slurps the entire file into a string.
   For large files or streaming you will have to implement your own chunking
   logic on top of the primitives exposed here.
3. **No invariant enforcement** – the module trusts its callers to supply
   valid positions and spans.  Doing so keeps the runtime cost at a minimum
   and leaves enforcement to the richer abstractions that usually sit on top
   (tokenisers, parsers…).

---

## Known limitations

* Tabs are treated as a single column in `column` counts – expansion is left
  entirely to the caller.
* `merge` supports overlap and either order, choosing the minimum left and
  maximum right byte offsets. Inputs must be well-formed spans from one
  document. Endpoint line/column metadata is retained, not recalculated;
  equal offsets keep the first span's endpoint.
* `from_file` reads the whole file using blocking channels and can raise
  `Sys_error`; there is no incremental or Eio-capability-taking file constructor.

---

## Implementation notes

The implementation is intentionally straightforward – roughly ~60 lines of
code – yet worth a brief mention:

```ocaml
open! Core

let read (src : Source.t) (span : Source.span) =
  let start = max 0 (min span.left.offset (Source.length src)) in
  let stop = max start (min span.right.offset (Source.length src)) in
  String.sub src.content ~pos:start ~len:(stop - start)
```

Both bounds are clamped. A reversed interval returns an empty string instead of
raising a negative-length exception. This is byte slicing, not UTF-8/grapheme
validation, and allocation can still fail.
