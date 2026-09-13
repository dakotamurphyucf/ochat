# String operations and byte boundaries

ChatML strings contain bytes. `String` operations run immediately and return
ordinary values, rather than task objects. Their names and argument order are
ChatML's API: `String.concat(a, b)` concatenates two strings, and
`String.contains(text, pattern)` searches for a substring, not an OCaml character.
Use explicit calls and backtick variants as described in the
[language guide](chatml-ocaml-differences.md).

This reference covers every export of `String` on the four extensibility
surfaces. The examples use the one-off `main : json -> json task` entrypoint with
no tools or host operations. The same pure operations are available inside
standalone handlers and ordinary/delegated moderators. Retrieve
`reference.signatures` for their exact inferred schemes.

## Operations

| Call | Meaning |
|---|---|
| `String.length(text)` | Byte count, returned as an integer. |
| `String.is_empty(text)` | Whether the byte count is zero. |
| `String.concat(left, right)` | The bytes of `left` followed by `right`; this does not join an array. |
| `String.equal(left, right)` | Exact, case-sensitive byte equality. |
| `String.contains(text, pattern)` | Whether the literal substring occurs anywhere. An empty pattern matches. |
| `String.starts_with(text, prefix)` | Literal prefix comparison; an empty prefix matches. |
| `String.ends_with(text, suffix)` | Literal suffix comparison; an empty suffix matches. |
| `String.trim(text)` | Remove ASCII whitespace at both ends. Interior whitespace remains. |
| `String.slice(text, start, length)` | Copy `length` bytes starting at the zero-based byte offset. The third argument is a length, not an ending offset. |
| `String.find(text, pattern)` | First matching byte offset as `Some(index)`, or `None`; an empty pattern yields `Some(0)`. |
| `String.split(text, separator)` | Split on a literal, nonempty separator string and return an array. Adjacent separators and separators at the ends produce empty elements. Empty input produces one empty element. |
| `String.to_upper(text)` | Convert ASCII letters to uppercase; other bytes remain unchanged. |
| `String.to_lower(text)` | Convert ASCII letters to lowercase; other bytes remain unchanged. |
| `String.replace_all(text, pattern, replacement)` | Replace nonoverlapping literal matches from left to right. The pattern must be nonempty; replacement may be empty. Inserted replacement bytes are not searched again. |

Search, split and replacement do not interpret regular expressions. These
functions do not mutate an input string, normalize Unicode, or apply locale-aware
case conversion. Byte offsets are not character or grapheme indices. A slice can
cut a UTF-8 sequence, so choose valid boundaries before returning text through a
JSON or provider interface.

`slice` requires nonnegative start and length with the entire interval in bounds.
An empty slice at the byte length is valid. Invalid ranges and empty split/replace
patterns raise immediate runtime failures. They are not `Task.fail` outcomes;
constructing a surrounding `Task.catch` cannot catch an exception raised while
its arguments are still being evaluated. See [task boundaries](chatml-task-effects.md).

## Normalize and inspect delimited text

The trailing separator is retained as an empty element. Replacement consumes
the original nonoverlapping matches; it does not repeatedly rewrite its output.

<!-- ochat-authoring-example: {"id":"strings.pipeline","surface":"one_off_v1","result":{"parts":["alpha","beta",""],"tag":"tag-BETA","prefix":true,"suffix":true,"contains":true,"same":true,"empty":true,"replacement":"bb"}} -->
```ocaml
let main input =
  let normalized = String.to_lower(String.trim("  ALPHA::Beta::  ")) in
  let parts = String.split(normalized, "::") in
  let replacement = String.replace_all("aaaa", "aa", "b") in
  Task.pure(`Object([
    { key = "parts"; value = `Array(Array.map(parts, fun part -> `String(part))) },
    { key = "tag"; value = `String(String.concat("tag-", String.to_upper(parts[1]))) },
    { key = "prefix"; value = `Bool(String.starts_with(normalized, "alpha")) },
    { key = "suffix"; value = `Bool(String.ends_with(normalized, "::")) },
    { key = "contains"; value = `Bool(String.contains(normalized, "beta")) },
    { key = "same"; value = `Bool(String.equal(replacement, "bb")) },
    { key = "empty"; value = `Bool(String.is_empty(parts[2])) },
    { key = "replacement"; value = `String(replacement) }
  ]))
```

## Byte offsets and empty matches

The UTF-8 encoding of `é` uses two bytes. Finding `x` therefore returns offset
two; ASCII uppercase conversion leaves `é` unchanged. The helper converts the
integer into a JSON number explicitly.

<!-- ochat-authoring-example: {"id":"strings.byte-offsets","surface":"one_off_v1","result":{"bytes":3,"index":2,"slice":"é","upper":"éX","empty_find":0,"empty_split":[""]}} -->
```ocaml
let number value = Json.parse(to_string(value))
let main input =
  let text = "éx" in
  Task.pure(`Object([
    { key = "bytes"; value = number(String.length(text)) },
    { key = "index"; value = number(Option.get_or(String.find(text, "x"), -1)) },
    { key = "slice"; value = `String(String.slice(text, 0, 2)) },
    { key = "upper"; value = `String(String.to_upper(text)) },
    { key = "empty_find"; value = number(Option.get_or(String.find(text, ""), -1)) },
    { key = "empty_split"; value = `Array(Array.map(String.split("", ":"), fun part -> `String(part))) }
  ]))
```

## Invalid ranges and patterns

These examples deliberately fail before producing a result task.

<!-- ochat-authoring-example: {"id":"strings.slice-bounds","surface":"one_off_v1","runtime_error":"String.slice: start+len out of bounds"} -->
```ocaml
let main input = Task.pure(`String(String.slice("abc", 2, 2)))
```

<!-- ochat-authoring-example: {"id":"strings.split-empty","surface":"one_off_v1","runtime_error":"String.split: separator must be non-empty"} -->
```ocaml
let main input =
  let parts = String.split("abc", "") in
  Task.pure(`String(parts[0]))
```

<!-- ochat-authoring-example: {"id":"strings.replace-empty","surface":"one_off_v1","runtime_error":"String.replace_all: pattern must be non-empty"} -->
```ocaml
let main input = Task.pure(`String(String.replace_all("abc", "", "x")))
```
