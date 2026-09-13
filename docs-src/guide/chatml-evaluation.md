# Source text and evaluation order

Use this reference when constructing source programmatically or when a script's
mutation, callbacks or errors make evaluation order matter. It complements the
[program guide](chatml-authoring-language.md), [inference rules](chatml-inference.md)
and [task semantics](chatml-task-effects.md). Examples are complete one-off
candidates checked offline without host operations.

## Lexical boundaries

Identifiers use ASCII letters, digits and underscores, and cannot start with a
digit. Lowercase names bind values; uppercase names identify modules. `_` alone
is reserved for patterns. Keywords are reserved, and apostrophes are not part of
identifiers. Backtick variant tags accept ASCII letters, digits and underscores.
Strings may contain UTF-8 text; identifier restrictions do not make strings ASCII.

<!-- ochat-authoring-example: {"id":"evaluation.identifier-boundaries","surface":"one_off_v1","result":"λ"} -->
```ocaml
(* Quotes do not hide (* nested comment *) delimiters: "text". *)
module Names2 = struct
  let _seed2 = "λ"
  let match_value = `9_ready(_seed2)
end
let main input =
  match Names2.match_value with | `9_ready(value) -> Task.pure(`String(value))
```

Integer literals are unsigned decimal digits within the runtime's machine-int
range. Negation is an operator. Floats require digits on both sides of the dot;
hexadecimal, exponent and numeric underscore notation are not literal syntax.
Integers use native OCaml machine arithmetic, not arbitrary precision. Do not
assume an overflowing integer computation preserves mathematical precision.

Strings recognize `\n`, `\t`, `\\` and `\"`. Other backslash sequences are kept
literally; `\u0041` does not become `A`. Physical LF, CR or CRLF inside a string
becomes one newline character. Outside strings, those line endings separate
tokens. Space/tab runs are ignored. Nested `(* ... *)` comments are supported;
comment delimiters still count inside quote characters in a comment. Unterminated
strings/comments and unknown characters produce parse diagnostics.

<!-- ochat-authoring-example: {"id":"evaluation.literal-escapes","surface":"one_off_v1","result":true} -->
```ocaml
let main input =
  let literal = "\r\u0041" in
  let escaped = "x\n" in
  if String.length(literal) == 8 then
    Task.pure(`Bool(String.length(escaped) == 2))
  else Task.pure(`Bool(false))
```

<!-- ochat-authoring-example: {"id":"evaluation.negative-pattern-rejected","surface":"one_off_v1","stage":"parse","contains":"Syntax error","span":true} -->
```ocaml
let classify value = match value with | -1 -> "negative" | _ -> "other"
let main input = Task.pure(`String(classify(-1)))
```

Numeric patterns accept literal tokens; a negative pattern is not a negation
expression. Match a variable and use a conditional when a negative value matters.

## Delimiters and precedence

Calls, arrays and variant payloads use commas. Record fields use semicolons, and
expression sequences use semicolons too. A semicolon inside one array element
sequences expressions; it does not introduce another element. Empty calls are
`f()`, empty arrays `[]`, empty records `{}`, and unit is `()`.

Multiplication/division bind tighter than addition/subtraction/string
concatenation, then comparisons/equality. These binary operator groups associate
to the left. Float operators have their separate dotted spellings. Prefix
negation uses the additive precedence level; parenthesize compound operands.
Use parentheses around sequences used as record values and around compound
mutation/dereference expressions. Layout and newlines do not define blocks.

<!-- ochat-authoring-example: {"id":"evaluation.sequence-in-array","surface":"one_off_v1","result":true} -->
```ocaml
let main input =
  let values = [1; 2, 3] in
  if Array.length(values) == 2 then Task.pure(`Bool(values[0] == 2))
  else Task.pure(`Bool(false))
```

The example deliberately demonstrates the trap; use `[1, 2, 3]` for three values.
The grammar also contains structural and explicit rejection productions. A
production's presence in a compiler inventory does not mean it accepts a useful
program. Parenthesized, explicit source is preferable to depending on a parser
conflict's resolution for an unusual expression.

## Eager evaluation and lexical capture

Statements and sequential bindings execute in source order. Calls evaluate the
function expression first, then each argument left to right, then invoke it.
Binary operators evaluate the left operand before the right. Record fields,
variant payloads and array elements evaluate in source order. Array updates
evaluate the array, index and replacement before changing storage. Copy-update
evaluates the base before its replacement fields and allocates a new record.

A closure captures the lexical bindings visible where it is created. Later
shadowing does not replace those captured bindings; shared mutable cells and
arrays can still change. Recursive function groups share their recursive binding
environment. An `open` imports names for subsequent statements and rejects
collisions; a module exports its own declarations, not outer/imported names.

<!-- ochat-authoring-example: {"id":"evaluation.argument-and-field-order","surface":"one_off_v1","result":"fabcdeg"} -->
```ocaml
let trace = ref("")
let mark label value = trace := !trace ++ label; value
let choose () = trace := !trace ++ "f"; fun left right -> left + right
let main input =
  let sum = choose()(mark("a", 2), mark("b", 3)) in
  let record = {first = mark("c", 1); second = mark("d", 2)} in
  let values = [mark("e", 3), mark("g", 4)] in
  Task.pure(`String(!trace))
```

Only the selected `if` branch executes. A match evaluates its scrutinee once and
uses the first matching arm. Static checking still checks other arms/branches.
A `while` loop re-evaluates its condition, discards the body value, and returns
unit. A sequence discards its first value. Discarding a task does not run it.
Callbacks in ordinary array/string operations are eager; they are not scheduled
as independent jobs merely because their bodies construct task values.

## Failures and representation boundaries

Both integer and float division reject zero divisors, including floating negative
zero. Array access/update checks bounds. Such failures can occur after earlier
local mutation. Language evaluation alone is not a transactional rollback system;
the host's task/event commit rules determine which staged effects are retained.
An eagerly raised failure occurs before an outer task is constructed and is not
automatically caught by `Task.catch` around a task argument.

<!-- ochat-authoring-example: {"id":"evaluation.float-zero-failure","surface":"one_off_v1","runtime_error":"Division by zero"} -->
```ocaml
let main input = Task.pure(`Number(1.0 /. 0.0))
```

JSON numbers become floats; large integers and original numeric spelling may be
lost. JSON serialization rejects non-finite numbers. Language records differ
from JSON object entries. Refs, closures, modules and pending tasks are runtime
values, not portable JSON or persisted moderator state. Use the declared codec
and the selected tool's schema at each boundary. The
[JSON guide](chatml-json.md) and [execution contracts](chatml-authoring-runtime.md)
show the supported data representations.

Host-configured source, instruction, time and allocation budgets are separate
from these language rules. They grant no tools or filesystem access, and a valid
program is not a proof of termination or effect success.
