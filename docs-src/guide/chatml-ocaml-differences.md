# ChatML differences from OCaml

ChatML is a separate language with ML-style syntax. Use OCaml familiarity to read
it, but use ChatML's grammar, builtin signatures and target contract to write it.
This reference highlights differences that matter when generating scripts; the
[language specification](chatml-language-spec.md) covers the full grammar, and
[match semantics](chatml-match-semantics.md) explains inference and coverage rules.

Every code block below is a complete candidate for the `one_off_v1` compiler
surface. Successful examples define `main(input)` returning a task of JSON;
the documentation check compiles and runs them with JSON null as input and no
host operations installed. Blocks explicitly labelled **Rejected** are checked
for the indicated diagnostic stage and message, without execution. The metadata
next to each block is used by the [checker](../../test/agent_docs/docs_chatml_authoring.ml).
This compiler surface is not a promise that a particular host enables one-off
scripts or grants a tool permission.

## Calls have explicit arity

Define parameters with spaces, but call with parentheses and comma-separated
arguments: `add(1, 2)`. Whitespace application is not the calling convention.
An ordinary two-argument function does not become partially applied when called
with one argument. Write a wrapper when you need a unary function.

<!-- ochat-authoring-example: {"id":"calls.wrapper","surface":"one_off_v1","result":true} -->
```ocaml
let add x y = x + y
let add_one x = add(1, x)
let main input = Task.pure(`Bool(add_one(2) == 3))
```

**Rejected during typechecking:** a missing argument is an arity error.

<!-- ochat-authoring-example: {"id":"calls.partial-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Function arity mismatch","span":true} -->
```ocaml
let add x y = x + y
let add_one = add(1)
let main input = Task.pure(`Null)
```

## Arrays, records and variant payloads use different delimiters

`[1, 2]` is an array, not an OCaml list. Use `values[index]` for access and
`values[index] <- value` for mutation. Array elements share a type. Record fields
use semicolons; variants use a backtick and can have comma-separated payloads.
A multi-payload constructor does not imply that arbitrary tuple expressions exist.

<!-- ochat-authoring-example: {"id":"containers.delimiters","surface":"one_off_v1","result":true} -->
```ocaml
let main input =
  let values = [1, 2] in
  values[0] <- 3;
  let record = {name = "sample"; values = values} in
  match `Pair(record.name, record.values[0]) with
  | `Pair(name, count) -> Task.pure(`Bool(count == 3))
```

**Rejected during parsing:** `(1, 2)` is not a general tuple expression.

<!-- ochat-authoring-example: {"id":"containers.tuple-rejected","surface":"one_off_v1","stage":"parse","contains":"Syntax error","span":true} -->
```ocaml
let pair = (1, 2)
let main input = Task.pure(`Null)
```

## Records are structural, with conservative joins

You do not need an OCaml-style nominal record declaration to create or inspect a
record. A copy update creates a new record and may change a field's type. It does
not mutate the original record. The following returns `"old"` while the original
`age` remains an integer.

<!-- ochat-authoring-example: {"id":"records.type-changing-update","surface":"one_off_v1","result":"old"} -->
```ocaml
let main input =
  let person = {name = "Bob"; age = 20} in
  let labelled = {person with age = "old"} in
  if person.age == 20 then Task.pure(`String(labelled.age))
  else Task.pure(`String("unexpected mutation"))
```

At an `if` or `match` join, only fields guaranteed on every branch remain
available. Do not assume a field added on one branch exists on the joined result.
Prefer the same explicit result shape on each branch.

**Rejected during typechecking:** `timeout_ms` is not present on every path.

<!-- ochat-authoring-example: {"id":"records.join-rejected","surface":"one_off_v1","stage":"typecheck","contains":"timeout_ms","span":true} -->
```ocaml
let choose enabled =
  if enabled then {name = "job"; timeout_ms = 10}
  else {name = "job"}
let main input = Task.pure(`Bool(choose(true).timeout_ms == 10))
```

<!-- ochat-authoring-example: {"id":"records.join-explicit","surface":"one_off_v1","result":true} -->
```ocaml
let choose enabled =
  if enabled then {name = "job"; timeout_ms = 10}
  else {name = "job"; timeout_ms = 0}
let main input = Task.pure(`Bool(choose(true).timeout_ms == 10))
```

Record patterns are structural too: `{name = n}` is a closed pattern with exactly
that field set, while `{name = n; _}` accepts additional fields. The open form
below accepts the extra `age` field without a fallback because the field binder
is total.

<!-- ochat-authoring-example: {"id":"records.open-pattern","surface":"one_off_v1","result":"Bob"} -->
```ocaml
let name_of record = match record with | {name = n; _} -> n
let main input = Task.pure(`String(name_of({name = "Bob"; age = 20})))
```

## Match coverage depends on the inferred type

Variants have structural rows; constructor coverage can close a row. A fallback
is useful for an open set of possibilities, but adding `_` to every match is not
universally valid. Once the checker knows the variant is closed and every
constructor is covered, an extra wildcard is an error. The `close` helper below
constrains the input to `None` or `Some`; the final match covers both.

**Rejected during typechecking:** the wildcard is redundant on this closed input.

<!-- ochat-authoring-example: {"id":"variants.closed-wildcard-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Redundant match arm '_': previous arms already cover all variant constructors","span":true} -->
```ocaml
let close v =
  match v with
  | `None -> `None
  | `Some(x) -> `Some(x)
let read v =
  close(v);
  match v with
  | `None -> 0
  | `Some(x) -> x
  | _ -> 2
let main input = Task.pure(`Bool(read(`Some(1)) == 1))
```

Removing the extra wildcard gives the intended total function for that type.

<!-- ochat-authoring-example: {"id":"variants.closed-total","surface":"one_off_v1","result":true} -->
```ocaml
let close v =
  match v with
  | `None -> `None
  | `Some(x) -> `Some(x)
let read v =
  close(v);
  match v with
  | `None -> 0
  | `Some(x) -> x
let main input =
  if read(`None) == 0 then Task.pure(`Bool(read(`Some(1)) == 1))
  else Task.pure(`Bool(false))
```

An integer match has an unbounded set of possible values and needs a fallback.
Use separate arms instead of OCaml or-patterns. For records, consult the exact
open/closed pattern rules in the match reference rather than applying variant
coverage rules to records.

<!-- ochat-authoring-example: {"id":"matches.integer-fallback","surface":"one_off_v1","result":"other"} -->
```ocaml
let describe n = match n with | 0 -> "zero" | _ -> "other"
let main input = Task.pure(`String(describe(2)))
```

## Annotate bindings; declare recursive data explicitly

ChatML supports binding annotations and structural type aliases. It does not
support arbitrary OCaml typed parameter patterns or general `(expression : type)`
annotations. For recursive structural data, declare a contractive type: recursive
references must occur beneath a real constructor. `type bad = bad` is not valid.
The recursive function below has a binding annotation followed by an ordinary
lambda. Bindings involving explicit recursive types remain monomorphic; do not
assume polymorphic recursion.

<!-- ochat-authoring-example: {"id":"types.recursive-binding","surface":"one_off_v1","result":true} -->
```ocaml
type expr = [ `Int(int) | `Add(expr, expr) ]
let rec eval : expr -> int =
  fun e ->
    match e with
    | `Int(n) -> n
    | `Add(a, b) -> eval(a) + eval(b)
let main input = Task.pure(`Bool(eval(`Add(`Int(1), `Int(2))) == 3))
```

## Mutation restricts polymorphism

Ordinary identity functions can be polymorphic. Mutable references and arrays
cannot be used to smuggle different element types through the same storage.
ChatML applies a value restriction; aliasing mutable storage does not restore
polymorphism.

**Rejected during typechecking:** after storing an integer function, the reference
cannot be used as a boolean function.

<!-- ochat-authoring-example: {"id":"mutation.value-restriction","surface":"one_off_v1","stage":"typecheck","contains":"Cannot unify","span":true} -->
```ocaml
let main input =
  let r = ref(fun x -> x) in
  r := (fun x -> x + 1);
  Task.pure(`Bool((!r)(true)))
```

## Modules export their own declarations

Modules can refer to outer bindings while being defined. Those bindings are not
implicitly exported. An `open` inside a module does not re-export the imported
names either. Prefer qualified builtin access such as `Array.length(values)`;
`open` rejects shadowing rather than silently replacing an existing binding.

**Rejected during typechecking:** `M` exports `y`, not the outer `x`.

<!-- ochat-authoring-example: {"id":"modules.outer-not-exported","surface":"one_off_v1","stage":"typecheck","contains":"Row does not contain label 'x'","span":true} -->
```ocaml
let x = 1
module M = struct
  let y = x
end
let main input = Task.pure(`Bool(M.x == 1))
```

## Operators and builtins are ChatML's own API

Integer arithmetic uses `+`, `-`, `*`, `/`; float arithmetic uses `+.`, `-.`, `*.`,
`/.`. String concatenation uses `++` or `String.concat(left, right)`, not OCaml's
`^`. Do not assume OCaml standard library signatures, labelled arguments or
operators such as `&&` and `||` are available. Use `if` or pattern matching for
short-circuit boolean logic. In particular, ChatML's `String.concat` takes two
strings, not a separator and a list.

<!-- ochat-authoring-example: {"id":"operators.strings-floats","surface":"one_off_v1","result":{"label":"hello world!","amount":3}} -->
```ocaml
let main input =
  let label = String.concat("hello", " world") ++ "!" in
  Task.pure(`Object([
    {key = "label"; value = `String(label)},
    {key = "amount"; value = `Number(1.0 +. 2.0)}
  ]))
```

## Tasks are values; the host runs the returned task

`let* value = task in next_task` composes with `Task.bind`. `let+ value = task in
result` composes with `Task.map`, so its body returns an ordinary value. These
forms are ChatML syntax, not user-defined OCaml binding operators. The host
interprets the task returned by the entrypoint. Merely constructing a host-operation
task does not execute that operation; ordinary expressions and initializers still
evaluate normally when the script is loaded or called.

<!-- ochat-authoring-example: {"id":"tasks.bind-map","surface":"one_off_v1","result":"ready"} -->
```ocaml
let main input =
  let* prefix = Task.pure("rea") in
  let+ suffix = Task.pure("dy") in
  `String(prefix ++ suffix)
```

`Task.fail(message)` constructs a failing task. `Task.catch(task, handler)`
handles a task failure with another task; constructing and discarding a failing
task does not fail the entrypoint. This is not OCaml's exception syntax.

<!-- ochat-authoring-example: {"id":"tasks.deferred-failure-catch","surface":"one_off_v1","result":"recoverable"} -->
```ocaml
let main input =
  let unused = Task.fail("not interpreted") in
  Task.catch(Task.fail("recoverable"), fun message -> Task.pure(`String(message)))
```

One-off `main` receives and returns JSON values, represented in ChatML by the
recursive variants `Null`, `Bool`, `Number`, `String`, `Array` and `Object`.
`Number` carries a float; `Object` carries an array of `{key; value}` entries,
as in the operator example. An ordinary ChatML record is not automatically JSON.

**Rejected during typechecking:** wrapping a record in a task does not satisfy
the entrypoint's JSON result contract. This error identifies the entrypoint;
unlike expression diagnostics, it currently has no source span.

<!-- ochat-authoring-example: {"id":"json.record-not-json","surface":"one_off_v1","stage":"typecheck","contains":"Invalid entrypoint 'main': Cannot unify","span":false} -->
```ocaml
let main input = Task.pure({answer = 42})
```

For effectful scripts, first retrieve the exact target's operation signatures and
entrypoint contract. A standalone tool uses `run(context, input)` and a tool
outcome; a moderator uses state plus events. Task composition does not itself
grant shell, tool, model or session authority. Static success also does not prove
a dynamic tool name, JSON argument schema, permission or operation phase is valid.
See [surface inventories](chatml-surface-inventory.md) and
[extensibility foundations](../agent-server/extensibility-foundations.md).
