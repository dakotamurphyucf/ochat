# Global helpers and language-value rendering

The global helpers below supplement the core modules. Calls use ChatML's exact
arity and evaluate their arguments eagerly. These operations do not construct
tasks. Ten helpers are available on every extensibility surface; ordinary
`moderator_v1` additionally exposes `print`. Retrieve `reference.signatures` for
the actual selected surface. Declared names do not grant tool or host access.

| Call | Meaning |
|---|---|
| `to_string(value)` | Render a language value for display. Strings are returned without JSON quoting; arrays, records and variants use language-oriented notation. |
| `length(values)` | Array length, equivalent to `Array.length`; it does not accept strings. |
| `string_length(text)` | Byte length, equivalent to `String.length`. |
| `string_is_empty(text)` | Whether the string contains zero bytes. |
| `array_copy(values)` | Shallow outer-array copy, equivalent to `Array.copy`. |
| `record_keys(record_or_module)` | Sorted array of field names or module export names. It does not enumerate arbitrary tool capabilities. |
| `variant_tag(value)` | Variant constructor name without its backtick or payload; it does not return the payload. |
| `swap_ref(cell, next)` | Replace the cell contents and return its previous value. Referenced mutable values remain shared. |
| `fail(message)` | Raise an immediate runtime failure with the string message; this is not `Task.fail`. |
| `hash_md5(text)` | Lowercase hexadecimal MD5 digest of the string bytes. Useful for legacy checksums; not a secure signature or collision-resistant identity. |
| `print(value)` | Render using `to_string`, writing through the configured print sink or otherwise standard output with a newline. Ordinary moderator only among the four extensibility surfaces. |

Use `Json.stringify` to encode JSON. `to_string` does not escape/quote strings as
JSON, does not preserve a reconstructible type representation, and does not execute
a task passed to it. Closures/modules are placeholders, while refs and task
descriptions can expose their contents/arguments. Avoid logging secrets and cyclic
mutable values. Rendering traverses data and is subject to applicable execution
budgets; it is not an unlimited, durable serialization format.

`print` is absent from `one_off_v1`, `tool_v1` and `delegated_moderator_v1`. It is
an immediate side effect, not staged output, a model message, or a notification.
Prefer the selected host's `Log` task operations for workflow logging. Printing
does not invoke an agent or request a model turn. References describe this
surface distinction without enabling `print` on targets that omit it.

## Inspect values and preserve an old binding

This complete one-off program demonstrates sorted field names, tag inspection,
copy independence, byte lengths and ref replacement. The hash is a fixed byte
checksum; runtime capability identities use their own host-managed mechanisms.
The documentation gate executes this example without tools or providers.

<!-- ochat-authoring-example: {"id":"globals.inspection-and-copy","surface":"one_off_v1","result":{"keys":["a","z"],"tag":"Ready","old":"initial","current":"ready","original":1,"copy":9,"length":2,"bytes":2,"empty":true,"digest":"900150983cd24fb0d6963f7d28e17f72","rendered":"[|1, 2|]","raw_string":"ready"}} -->
```ocaml
let number : int -> json = fun value -> Json.parse(to_string(value))
let main input =
  let original = [1, 2] in
  let copied = array_copy(original) in
  copied[0] <- 9;
  let phase = ref("initial") in
  let old = swap_ref(phase, "ready") in
  let names = record_keys({z = 2; a = 1}) in
  Task.pure(`Object([
    {key = "keys"; value = `Array(Array.map(names, fun name -> `String(name)))},
    {key = "tag"; value = `String(variant_tag(`Ready("payload")))},
    {key = "old"; value = `String(old)},
    {key = "current"; value = `String(!phase)},
    {key = "original"; value = number(original[0])},
    {key = "copy"; value = number(copied[0])},
    {key = "length"; value = number(length(original))},
    {key = "bytes"; value = number(string_length("é"))},
    {key = "empty"; value = `Bool(string_is_empty(""))},
    {key = "digest"; value = `String(hash_md5("abc"))},
    {key = "rendered"; value = `String(to_string(original))},
    {key = "raw_string"; value = `String(to_string("ready"))}
  ]))
```

## Immediate failure versus a failing task

An eager argument failure occurs before `Task.catch` exists. Use a failing task
for recoverable workflow errors; see [task boundaries](chatml-task-effects.md).

<!-- ochat-authoring-example: {"id":"globals.fail-is-immediate","surface":"one_off_v1","runtime_error":"invalid workflow input"} -->
```ocaml
let main input =
  Task.catch(Task.pure(fail("invalid workflow input")),
    fun message -> Task.pure(`String("not reached")))
```

## Surface-specific rejection

This one-off candidate is deliberately rejected during typechecking. Knowing the
ordinary moderator API does not make a global available on a different target.

<!-- ochat-authoring-example: {"id":"globals.print-unavailable","surface":"one_off_v1","stage":"typecheck","contains":"Unknown variable 'print'","span":true} -->
```ocaml
let main input =
  print("diagnostic");
  Task.pure(`Null)
```
