# Writing ChatML programs

Use ChatML to combine deterministic computation with the tools selected for an
agent. This guide covers writing programs; the [OCaml differences](chatml-ocaml-differences.md)
explain inference and syntax traps, and the [execution contracts](chatml-authoring-runtime.md)
define how a one-off script, standalone tool or moderator enters the runtime.

Every code block here is a complete `one_off_v1` candidate. The documentation gate
compiles and runs examples with null input and no host operations. These examples teach
language behavior; they do not select tools or enable an execution target.

## Source text and operators

Programs consist of statements evaluated in source order. Whitespace separates
tokens; indentation does not define blocks. Use lowercase names for bindings and
uppercase names for modules. Identifiers contain ASCII letters, digits and
underscores and cannot start with a digit. `_` is a wildcard, not an ordinary name.
Variant tags begin with a backtick. There is no OCaml character literal syntax.

Literals include decimal integers, decimal floats with digits on both sides of
the dot, booleans, double-quoted strings and unit `()`. A negative number uses a
unary operator; exponent notation and hexadecimal literals are not supported.
Strings recognize `\n`, `\t`, `\\` and `\"` and can span lines. Other backslash
sequences remain literal; do not assume OCaml or JSON escape processing. String
operations use byte positions/lengths, not Unicode grapheme indices.

Block comments use `(* ... *)` and nest. Delimiters inside a comment remain active
even between quote characters. Source line tracking continues across comments,
including CRLF line endings. There are no `//` or `#` line comments.

| Operation | Syntax and behavior |
|---|---|
| Integer arithmetic | `+`, `-`, `*`, `/`, unary `-`; operands stay integers |
| Float arithmetic | `+.`, `-.`, `*.`, `/.`, unary `-.`; operands stay floats |
| Integer ordering | `<`, `<=`, `>`, `>=` |
| Float ordering | `<.`, `<=.`, `>.`, `>=.` |
| Equality | `==`, `!=`; matching types must support equality |
| String concatenation | `++`, or `String.concat(a, b)` |
| Mutation | `cell := value`, `array[index] <- value`; both return unit |
| Sequencing | `first; second` evaluates both and returns the second value |

Arithmetic does not implicitly convert integers to floats. Equality rejects
arrays, refs, functions and tasks; it is not a general serialization/comparison
operation. `=` introduces bindings and record fields, not equality. There are no
`&&`, `||`, pipeline, list-cons or modulo operators; use conditionals or matching
for boolean control flow. Multiplication/division bind tighter than addition and
concatenation, which bind tighter than comparison. Parenthesize compound operands
and mutation expressions rather than relying on subtle precedence.

<!-- ochat-authoring-example: {"id":"language.source-and-precedence","surface":"one_off_v1","result":"total=14\ntrue"} -->
```ocaml
(* Outer comment
   (* Nested comment *)
   still inside the outer comment. *)
let main input =
  let total = 2 + 3 * 4 in
  Task.pure(`String("total=" ++ to_string(total) ++ "\n" ++ to_string(total == 14)))
```

## Functions, loops and modules

Named functions declare space-separated parameters but calls pass comma-separated
arguments: `let add x y = ...` is called as `add(1, 2)`. Use `fun x y -> ...` for
callbacks and `fun () -> ...` for zero-argument functions. Calls evaluate their
arguments before entering the function and require the exact arity. A function
can return another function, but missing arguments do not implicitly curry it.

Bindings are lexical. A closure keeps the binding it captured even if a later
`let` shadows that name. `let rec ... and ...` supports mutually recursive
functions; recursive non-functions are rejected. A local binding uses `in`.
Tail-recursive functions reuse the evaluator's call trampoline; this does not
make non-tail recursion or unbounded work free of host limits.

`if` requires both branches and returns a value. `while condition do body done`
returns unit and can mutate arrays or refs. There is no `for`, `break`, `continue`
or early `return` syntax. Use the final expression, branches or recursive helpers
to choose a result. Arrays are homogeneous and mutable; `ref(value)` creates a
mutable cell, read with `!cell`. Copy-updating a record creates a new record;
referenced mutable arrays/cells are not deep-copied.

Modules group definitions in `module Name = struct ... end`. They can use outer
bindings, but export only their own definitions. `open Name` imports values
without re-exporting them and rejects name collisions, including builtins.
Qualified calls such as `Array.fold(...)` avoid collisions. There are no functors,
signatures, module type declarations or module-local type declarations.

<!-- ochat-authoring-example: {"id":"language.closures-loops-recursion","surface":"one_off_v1","result":"6:10:true"} -->
```ocaml
let offset = 10
module Totals = struct
  let captured () = offset
  let sum values =
    let index = ref(0) in
    let total = ref(0) in
    while !index < Array.length(values) do
      total := !total + values[!index];
      index := !index + 1
    done;
    !total
end
let offset = 99
let rec even n = if n == 0 then true else odd(n - 1)
and odd n = if n == 0 then false else even(n - 1)
let main input =
  let total = Totals.sum([1, 2, 3]) in
  Task.pure(`String(to_string(total) ++ ":" ++ to_string(Totals.captured()) ++ ":" ++ to_string(even(total))))
```

## Matching and explicit data types

Patterns include `_`, variable binders, unit and nonnegative numeric/string/boolean
literals, variants with payload patterns, and structural record patterns. There
are no guards, or-patterns, `as` binders, array patterns or list patterns. A closed
record pattern requires its exact field set; `{name = value; _}` accepts additional
fields. Variant tags and payload arity must agree. An arm cannot bind the same
variable name twice. Matching chooses the first matching arm in source order.

Match analysis checks obvious redundancy and conservative coverage. Closed boolean,
unit and variant matches can be exhaustive without a fallback; adding a redundant
wildcard is rejected. Infinite literal domains and open rows generally need a
fallback unless another pattern is provably total. An exhaustive constructor match
can close an inferred variant row, so a function may reject later calls with new
tags. Closed record matching is not full nested-pattern coverage analysis.

Use top-level named types for intentional recursion and for stable record/variant
contracts. Types are structural aliases, not nominal runtime wrappers. Bindings
can have annotations; individual parameters and arbitrary expressions cannot.
Only previously declared type names and the current recursive name are in scope.
Recursive types must be contractive; `type bad = bad` is rejected. There are no
type parameters or mutually recursive type declarations. `array` and `task` are
supported postfix constructors; refs and open rows are inferred rather than
written as explicit annotation syntax.

<!-- ochat-authoring-example: {"id":"language.recursive-data-and-record-patterns","surface":"one_off_v1","result":"total=7"} -->
```ocaml
type expression = [ `Value(int) | `Add(expression, expression) ]
let rec evaluate : expression -> int =
  fun expression ->
    match expression with
    | `Value(value) -> value
    | `Add(left, right) -> evaluate(left) + evaluate(right)
let label record =
  match record with
  | {name = name; _} -> name
let main input =
  let expression = `Add(`Value(3), `Value(4)) in
  let details = {name = "total"; expression = expression} in
  Task.pure(`String(label(details) ++ "=" ++ to_string(evaluate(details.expression))))
```

Do not treat successful inference as validation of a tool's JSON arguments or
results. Those cross a dynamic boundary and need the tool's actual schema.
The [differences guide](chatml-ocaml-differences.md) also covers record joins,
mutation's value restriction and rejected wildcard/arity examples.

## Standard library for structured data

The core modules are `String`, `Array`, `Option`, `Hashtbl`, `Json` and `Task`.
Retrieve `reference.signatures` for the selected surface's exact names and arities.
Use their qualified names; do not substitute familiar OCaml library calls.

Global helpers include `to_string` for language-value rendering (not JSON encoding),
`length`/`array_copy` for arrays, and `string_length`/`string_is_empty` for strings.
`record_keys` returns sorted record field names, and `variant_tag` returns a tag
name. `swap_ref(cell, next)` replaces the cell and returns its previous value;
`fail(message)` raises a runtime failure. `hash_md5` computes an MD5 string digest.
`print`, when exposed by the selected surface, writes through the host's configured
print sink; it is absent from one-off and delegated moderator surfaces.

`String` supports byte-oriented length, equality, substring/prefix/suffix checks,
case conversion, trimming, slicing, find/split and replacement. `String.slice`
takes start and length; invalid bounds fail. Splitting/replacement require a
nonempty separator/pattern. `String.find` returns `None` or `Some(index)` variants.

`Array` supports indexed access/update, allocation, copies/slices, append,
reversal, swap/fill, map/iteration/fold/filter and predicate/search helpers.
Copies are shallow. Allocation rejects negative lengths; indexed operations and
slices check bounds. `Array.fold(values, initial, fun accumulator value -> ...)`
walks left-to-right. Index-aware callbacks receive the index before the value.
`find` returns the first matching value; `find_map` returns the first `Some` result.
Callbacks run immediately during the array operation, not as implicitly scheduled
tasks. If a callback constructs a task, something must still interpret that task.

`Option` uses ordinary `None`/`Some(value)` variants. `Option.get_or` accepts an
already evaluated default, not a lazy callback. `Hashtbl` uses string keys and
mutable values of one inferred type; `get` returns an option, and `set` replaces
the value at a key. It is useful for counters, grouping and small lookups.

`json` is a recursive variant: `Null`, `Bool(bool)`, `Number(float)`,
`String(string)`, `Array(json array)` and `Object({key: string; value: json} array)`.
JSON object entries are records in an array, not ChatML record fields. Numeric
tokens become floats, so do not use this representation for exact large-integer
arithmetic or preserving numeric token spelling. Parse/stringify/pretty operate on
JSON text; `parse_opt` and typed accessors return options. Field/path access returns
options; path array segments are decimal indices when the current value is an
array. `set_field`/`remove_field` return new objects and reject nonobjects.

<!-- ochat-authoring-example: {"id":"language.structured-data-pipeline","surface":"one_off_v1","result":{"alpha":2}} -->
```ocaml
let main input =
  let counts = Hashtbl.create() in
  let names = Array.map(String.split("alpha, beta,alpha", ","), String.trim) in
  Array.iter(names, fun name ->
    let previous = Option.get_or(Hashtbl.get(counts, name), 0) in
    Hashtbl.set(counts, name, previous + 1));
  let count = Option.get_or(Hashtbl.get(counts, "alpha"), 0) in
  let report = Json.set_field(`Object([]), "alpha", Json.parse(to_string(count))) in
  Task.pure(report)
```

## Effects, errors and execution boundaries

Pure computation and mutable language operations execute eagerly. Tasks are values
that describe later work: `Task.pure`, `Task.bind`/`let*`, `Task.map`/`let+`,
`Task.fail` and `Task.catch` compose that work. Return the task required by the
selected entrypoint; discarding a task does not run it. A `let+` body returns a
plain value, whereas a `let*` body continues with a task.

`Task.catch` handles failures while interpreting its protected task; it cannot
catch an eager failure that occurs before that task is constructed. Use a deferred
callback when failure-prone computation belongs inside the protected work.
Static type errors are not recoverable runtime task errors. Out-of-bounds access,
invalid JSON, division by zero and explicit failure can fail execution despite
successful typechecking. There is no OCaml `try ... with` or exception declaration
syntax. See the checked deferred-failure example in the differences guide.

Tool, shell, model and session operations depend on the selected surface and
capabilities. `Tool.call` uses the actual named tool and schema; it does not make
every string a callable tool. Language refs/closures/tasks are not durable state.
Serialized moderator state accepts data-shaped values, and committed records do
not imply that an external effect is exactly-once. Host limits may bound work;
they are separate from the language's syntax and from tool authority.

Read the [runtime guide](chatml-authoring-runtime.md),
[background-work guide](chatml-authoring-background.md) and
[child-session guide](chatml-authoring-children.md) for those contracts.
