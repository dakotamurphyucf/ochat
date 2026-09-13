# JSON values, access and conversion

Use `Json` for tool inputs, results, serializable workflow data and JSON text.
Its sixteen exports are available on all four extensibility surfaces. They run
immediately rather than returning tasks. The examples are complete one-off
programs checked without tools or provider calls. See [execution contracts](chatml-authoring-runtime.md)
for passing their results across the runtime boundary.

## Representation and operations

The built-in `json` type is a recursive structural variant: `Null`, `Bool(bool)`,
`Number(float)`, `String(string)`, `Array(json array)` and
`Object({key: string; value: json} array)`. Constructors in source need a backtick.
Objects contain an ordered array of key/value records, not ChatML record fields.
An arbitrary record, option, ref, function or task is not a JSON value. Annotating
a helper as `json -> json` can make the intended recursive type explicit when
constructing variants and mutable arrays; see [inference differences](chatml-ocaml-differences.md).

| Call | Meaning |
|---|---|
| `Json.parse(text)` | Parse JSON text and convert it to the recursive value representation; failure raises immediately. |
| `Json.parse_opt(text)` | Same parse/conversion, returning `Some(value)` or `None` on failure. |
| `Json.validate(text)` | Check JSON text syntax only; returns a boolean. It does not validate a schema or guarantee lossless numeric conversion. |
| `Json.stringify(value)` | Encode JSON as compact text. This is JSON encoding, unlike the general `to_string` renderer. |
| `Json.pretty(value)` | Encode JSON with human-readable whitespace. Do not rely on a particular layout for a protocol. |
| `Json.tag(value)` | Constructor name as text: `Null`, `Bool`, `Number`, `String`, `Array` or `Object`. |
| `Json.as_bool(value)` | `Some(bool)` for a boolean, otherwise `None`. |
| `Json.as_number(value)` | `Some(float)` for a number, otherwise `None`; it does not coerce numeric strings. |
| `Json.as_string(value)` | `Some(string)` for a string, otherwise `None`. |
| `Json.as_array(value)` | `Some(elements)` for an array, otherwise `None`. Elements are the original mutable payload, not a copy. |
| `Json.as_object(value)` | `Some(entries)` for an object, otherwise `None`. Entries are the original mutable payload, not a copy. |
| `Json.object_keys(value)` | New array of keys in stored order, including duplicates; empty array for nonobjects. |
| `Json.get_field(value, key)` | First matching field as `Some(value)`; `None` for a missing field or nonobject. |
| `Json.get_path(value, segments)` | Follow an array of string segments through objects and arrays, returning an option. Empty path returns `Some(value)`. |
| `Json.set_field(object, key, value)` | New outer object: replace the first matching key and drop later duplicates of that key, or append a missing key. |
| `Json.remove_field(object, key)` | New outer object with every occurrence of the key removed. |

The typed accessors do not coerce between JSON kinds. `None` for a missing path
differs from `Some(Null)` for an explicitly null value. Use exhaustive matching
or the [Option helpers](chatml-collections.md); `get_or` defaults are eager.
`set_field` and `remove_field` reject nonobjects with immediate failures. They
are shallow transformations: unchanged nested arrays and objects remain shared.
If you need isolation, copy the required structure rather than assuming that a
new outer object deep-copies its children. Avoid building cyclic mutable payloads;
the JSON boundary represents trees, not object references.

## Paths and typed access

Object path segments are exact keys, including keys that look numeric. At an
array, a segment is parsed as an integer index and must be in bounds; use ordinary
nonnegative decimal index text. Missing keys, invalid indices and traversal through
a scalar return `None`. Paths do not split dotted strings or interpret JSON Pointer
escape sequences; pass one segment per level.

<!-- ochat-authoring-example: {"id":"json.paths-and-kinds","surface":"one_off_v1","result":{"label":"ready","enabled":true,"count":2.5,"tag":"Object","numeric_key":"literal key","explicit_null":true,"missing":true,"wrong_kind":true,"bad_index":true,"root":true,"scalar_keys":0}} -->
```ocaml
let main input =
  let root = Json.parse("{\"rows\":[{\"label\":\"ready\",\"enabled\":true,\"count\":2.5}],\"empty\":null,\"0\":\"literal key\"}") in
  let label = Option.get_or(Json.get_path(root, ["rows", "0", "label"]), `Null) in
  let enabled = Option.get_or(Json.get_path(root, ["rows", "0", "enabled"]), `Null) in
  let count = Option.get_or(Json.get_path(root, ["rows", "0", "count"]), `Null) in
  let explicit_null = match Json.get_field(root, "empty") with
    | `None -> false
    | `Some(value) -> String.equal(Json.tag(value), "Null")
  in
  Task.pure(`Object([
    {key = "label"; value = `String(Option.get_or(Json.as_string(label), "missing"))},
    {key = "enabled"; value = `Bool(Option.get_or(Json.as_bool(enabled), false))},
    {key = "count"; value = `Number(Option.get_or(Json.as_number(count), 0.0))},
    {key = "tag"; value = `String(Json.tag(root))},
    {key = "numeric_key"; value = Option.get_or(Json.get_path(root, ["0"]), `Null)},
    {key = "explicit_null"; value = `Bool(explicit_null)},
    {key = "missing"; value = `Bool(Option.is_none(Json.get_field(root, "absent")))},
    {key = "wrong_kind"; value = `Bool(Option.is_none(Json.as_number(label)))},
    {key = "bad_index"; value = `Bool(Option.is_none(Json.get_path(root, ["rows", "9"])))},
    {key = "root"; value = `Bool(Option.is_some(Json.get_path(root, [])))},
    {key = "scalar_keys"; value = Json.parse(to_string(Array.length(Json.object_keys(`Null))))}
  ]))
```

## Duplicate keys and shared payloads

Parsing and encoding retain object entry order and duplicate keys. Lookup returns
the first matching entry. Updating one key collapses its duplicates while keeping
its first position; other keys retain their order. Removing a key removes all its
entries. Do not assume another JSON consumer makes the same duplicate-key choice;
produce unique keys at external boundaries.

This example also demonstrates that a borrowed array mutates the original nested
value, and that an outer field update still shares that nested value. Replacing a
borrowed object entry changes the original object without changing an already
replaced entry in the new outer object.

<!-- ochat-authoring-example: {"id":"json.duplicates-and-aliases","surface":"one_off_v1","result":{"first":"first","original_keys":["x","keep","x"],"updated_keys":["x","keep","tail"],"removed_keys":["keep"],"original_after":"through view","updated_x":"updated","shared_nested":"shared"}} -->
```ocaml
let field object key = Option.get_or(Json.get_field(object, key), `Null)
let keys : json -> json = fun object ->
  `Array(Array.map(Json.object_keys(object), fun key -> `String(key)))
let main input =
  let original = Json.parse("{\"x\":\"first\",\"keep\":[\"old\"],\"x\":\"second\"}") in
  let first = field(original, "x") in
  let updated = Json.set_field(Json.set_field(original, "x", `String("updated")), "tail", `Null) in
  let removed = Json.remove_field(original, "x") in
  (match Json.as_array(field(original, "keep")) with
   | `None -> fail("expected an array")
   | `Some(values) -> values[0] <- `String("shared"));
  (match Json.as_object(original) with
   | `None -> fail("expected an object")
   | `Some(entries) -> entries[0] <- {key = "x"; value = `String("through view")});
  Task.pure(`Object([
    {key = "first"; value = first},
    {key = "original_keys"; value = keys(Json.parse(Json.stringify(original)))},
    {key = "updated_keys"; value = keys(updated)},
    {key = "removed_keys"; value = keys(removed)},
    {key = "original_after"; value = field(original, "x")},
    {key = "updated_x"; value = field(updated, "x")},
    {key = "shared_nested"; value = Option.get_or(Json.get_path(updated, ["keep", "0"]), `Null)}
  ]))
```

## Syntax validity and floating-point numbers

JSON number tokens are converted to binary floating-point values. Exact decimal
spelling and integers beyond floating-point precision need not survive a round
trip. Carry exact large identifiers or decimal quantities as strings when their
spelling or precision matters. `Json.validate` is only a syntax check; it is not
a tool schema validator. Successful parsing also does not guarantee that a value
can be exported: extremely large exponents can become infinity. `stringify`,
`pretty` and runtime JSON export reject nonfinite numbers.

JSON text parsing handles JSON escapes independently of the ChatML source-string
lexer. The following programs contain escaped quotes because the JSON text itself
is inside a ChatML string. Parsing JSON is not evaluating code.

<!-- ochat-authoring-example: {"id":"json.syntax-and-numeric-boundary","surface":"one_off_v1","result":{"bad_syntax":false,"bad_parse":true,"null_parse":true,"large_syntax":true,"large_parse":true,"rounded_equal":true,"pretty_valid":true,"pretty_value":"ready"}} -->
```ocaml
let main input =
  let left = Json.stringify(Json.parse("9007199254740992")) in
  let right = Json.stringify(Json.parse("9007199254740993")) in
  let pretty = Json.pretty(Json.parse("{\"state\":\"ready\"}")) in
  Task.pure(`Object([
    {key = "bad_syntax"; value = `Bool(Json.validate("{"))},
    {key = "bad_parse"; value = `Bool(Option.is_none(Json.parse_opt("{")))},
    {key = "null_parse"; value = `Bool(Option.is_some(Json.parse_opt("null")))},
    {key = "large_syntax"; value = `Bool(Json.validate("1e999"))},
    {key = "large_parse"; value = `Bool(Option.is_some(Json.parse_opt("1e999")))},
    {key = "rounded_equal"; value = `Bool(String.equal(left, right))},
    {key = "pretty_valid"; value = `Bool(Json.validate(pretty))},
    {key = "pretty_value"; value = Option.get_or(Json.get_field(Json.parse(pretty), "state"), `Null)}
  ]))
```

## Immediate errors

These failures happen while evaluating ordinary functions. A surrounding task
catch does not turn them into recoverable task outcomes; validate dynamic input
or use `parse_opt` before proceeding. See [task failure boundaries](chatml-task-effects.md).

<!-- ochat-authoring-example: {"id":"json.invalid-text","surface":"one_off_v1","runtime_error":"Json.parse:"} -->
```ocaml
let main input = Task.pure(Json.parse("{"))
```

<!-- ochat-authoring-example: {"id":"json.nonobject-update","surface":"one_off_v1","runtime_error":"Json.set_field: expected `Object(...)"} -->
```ocaml
let main input = Task.pure(Json.set_field(`Null, "state", `String("ready")))
```

<!-- ochat-authoring-example: {"id":"json.nonfinite-export","surface":"one_off_v1","runtime_error":"JSON numbers must be finite"} -->
```ocaml
let main input = Task.pure(`String(Json.stringify(Json.parse("1e999"))))
```
