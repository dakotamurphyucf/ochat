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
(* Read a file once and remember its contents *)
let src = Source.from_file "example.chatml"

(* Safe single-character query                              *)
assert (Source.at src 0 = Some 'H');
assert (Source.at src 1_000_000 = None);  (* out of bounds   *)

(* Build a span manually – here the first five characters   *)
let p0 = { Source.line = 1; column = 0; offset = 0 } in
let p5 = { Source.line = 1; column = 5; offset = 5 } in
let span = { Source.left = p0; right = p5 } in

(* Extract the substring designated by that span            *)
assert (Source.read src span = "Hello");

(* Merge spans, e.g. when combining AST node locations      *)
let span2  = { Source.left = p5; right = { p5 with offset = 11; column = 11 }} in
let merged = Source.merge span span2 in
assert (Source.read src merged = "Hello world");
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
| `from_file : string -> t` | Read the file’s contents eagerly into memory. |
| `length : t -> int` | Number of bytes in the document. O(1). |
| `at : t -> int -> char option` | Safe character lookup. O(1). |
| `read : t -> span -> string` | Extract a substring, clamping out-of-bounds spans. |
| `merge : span -> span -> span` | Smallest span covering the two inputs. |

All operations are pure and allocate at most the size of the returned
substring; nothing is ever mutated in place.

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
* `merge` assumes the first span ends *before* the second one starts; passing
  overlapping spans yields unspecified results.
* `from_file` reads the whole file in one go; there is no incremental API.

---

## Implementation notes

The implementation is intentionally straightforward – roughly ~60 lines of
code – yet worth a brief mention:

```ocaml
let read src span =
  let start  = max 0 (min span.Source.left.offset  (length src)) in
  let stop   =       (min span.Source.right.offset (length src)) in
  String.sub src.content ~pos:start ~len:(stop - start)
```

The `start`/`stop` clamping ensures no exception can escape even if the span
comes from untrusted input.



