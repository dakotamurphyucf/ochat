# Type inference and annotations

Use this reference when a generated program has the right algorithm but its
annotations, polymorphism or record shapes do not typecheck. ChatML shares ML's
basic inference model, but its explicit function arity, structural records and
recursive-type rules differ from OCaml. Start with the
[program guide](chatml-authoring-language.md) for syntax and the
[differences guide](chatml-ocaml-differences.md) for common repairs.

Every example below is a complete one-off candidate. The offline documentation
gate compiles it and either executes it without tools or verifies the indicated
compiler rejection. Success does not validate a tool's dynamic JSON schema.

## An annotation describes explicit arity

Write annotations on a binding, followed by a lambda: `let add : int -> int -> int
= fun left right -> left + right`. Arrow chains describe the arguments of one
call, not automatic currying. `unit -> result` describes a zero-argument function,
called with `f()`. It does not describe a function taking a unit value as one
argument. Function parameters themselves are ordinary names; neither annotated
parameter patterns nor general `(expression : type)` syntax is available.

Right-hand arrows are flattened even inside parentheses or a named alias. Thus
`int -> (int -> int)` still expects two parameters. Inferred functions may return
functions, but this arrow annotation cannot express that curried result directly.
Leave such a binding inferred or return a record containing the function. A
function-valued argument on the left of an arrow is supported as shown below.

<!-- ochat-authoring-example: {"id":"inference.explicit-arrow-arity","surface":"one_off_v1","result":13} -->
```ocaml
let seed : unit -> int = fun () -> 3
let add : int -> int -> int = fun left right -> left + right
let apply : (int -> int) -> int -> int = fun function_ value -> function_(value)
let main input =
  let increment = fun value -> add(value, 1) in
  Task.pure(Json.parse(to_string(apply(increment, seed()) + add(4, 5))))
```

<!-- ochat-authoring-example: {"id":"inference.curried-annotation-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Annotated function expects 2 parameter(s), but lambda has 1","span":true} -->
```ocaml
let curried : int -> (int -> int) = fun left -> fun right -> left + right
let main input = Task.pure(`Null)
```

<!-- ochat-authoring-example: {"id":"inference.function-result-record","surface":"one_off_v1","result":7} -->
```ocaml
let make : int -> {apply: int -> int} =
  fun left -> {apply = fun right -> left + right}
let main input = Task.pure(Json.parse(to_string(make(3).apply(4))))
```

## Generalization depends on how a value is bound

A non-expansive binding can be generalized: literals, names and functions, plus
records/variants whose contents are all non-expansive. An annotation preserves
the classification of its expression. Function application, field access,
conditionals, loops, arrays, refs, mutation and copy-update are expansive. Even
an effect-free application that returns an identity function is bound
monomorphically. ChatML does not apply OCaml's relaxed value restriction.

Wrap a computation in a function when each call should get fresh type variables
or fresh mutable storage. Aliasing an already monomorphic value does not undo
constraints on its captured variables. Arrays are homogeneous, indexes are
integers, and assigning a cell or element cannot change its inferred type.

<!-- ochat-authoring-example: {"id":"inference.expansive-function-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Cannot unify","span":true} -->
```ocaml
let identity value = value
let produced = identity(identity)
let main input =
  let first = produced(1) in
  Task.pure(`Bool(produced(true)))
```

<!-- ochat-authoring-example: {"id":"inference.generalized-wrapper","surface":"one_off_v1","result":true} -->
```ocaml
let identity value = value
let produced value = identity(identity)(value)
let main input =
  let first = produced(1) in
  if first == 1 then Task.pure(`Bool(produced(true)))
  else Task.pure(`Bool(false))
```

## Named types are structural and recursive bindings are functions

Primitive annotation names are `int`, `float`, `bool`, `string` and `unit`.
Postfix `array` and `task` are supported. Record and variant annotations are
closed. There are no user-written type parameters, explicit open-row annotations
or `ref` type constructor. Named types are aliases, not nominal wrappers: two
names with the same structural shape can describe the same value.

Declarations are top-level and ordered. They may refer to earlier type names and
their own recursive name, but cannot redefine a primitive or existing type name.
Recursive references must be guarded by a real type constructor. There are no
mutually recursive type declarations. A binding whose inferred type contains an
explicit recursive type stays monomorphic, even if its expression is a lambda;
do not infer polymorphic recursion from the usual identity-function example.

`let rec` and `and` bind functions, including annotated lambdas. They cannot
construct cyclic refs, records or arbitrary self-referential values. Recursive
calls share monomorphic placeholders while their definitions are checked.
Parameter names within one lambda, and names within one simultaneous recursive
group, must be distinct. Sequential bindings may still reuse a name; earlier
closures retain their lexical bindings. Duplicate-parameter diagnostics currently
point to the whole lambda, and duplicate recursive-name diagnostics to that
binding's right-hand side, because the parsed AST does not retain individual
binder-token spans.

<!-- ochat-authoring-example: {"id":"inference.structural-aliases","surface":"one_off_v1","result":"ready"} -->
```ocaml
type first = {label: string}
type second = {label: string}
let value : first = {label = "ready"}
let copy : second = value
let main input = Task.pure(`String(copy.label))
```

<!-- ochat-authoring-example: {"id":"inference.recursive-value-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Recursive binding 'cycle' must be a function","span":true} -->
```ocaml
let rec cycle = {next = cycle}
let main input = Task.pure(`Null)
```

<!-- ochat-authoring-example: {"id":"inference.duplicate-parameter-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Duplicate lambda parameter 'value'","span":true} -->
```ocaml
let duplicate value value = value
let main input = Task.pure(`Null)
```

<!-- ochat-authoring-example: {"id":"inference.duplicate-recursive-name-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Duplicate recursive binding 'repeat'","span":true} -->
```ocaml
let rec repeat value = value
and repeat value = value
let main input = Task.pure(`Null)
```

<!-- ochat-authoring-example: {"id":"inference.sequential-rebinding","surface":"one_off_v1","result":"first:second"} -->
```ocaml
let label = "first"
let captured () = label
let label = "second"
let main input = Task.pure(`String(captured() ++ ":" ++ label))
```

## Records preserve their shape and inferred field requirements

Field access constrains an inferred record parameter to contain that field.
Unannotated helper parameters can remain open to additional fields. An explicit
closed annotation makes a deliberate narrower contract; it is not a request to
discard extra fields at runtime. Duplicate record fields, update labels, type
labels and pattern binders are rejected.

Copy-update produces a new record, can add a field, and can replace a field with
a different type. Refs and arrays inside either record still share their storage.
All `if` and `match` result branches must have compatible types, including the
same record field set. A join never hides physical fields. This also holds for
nested records and records inside arrays, refs and function results. Use matching
shapes or inferred variant tags when alternatives intentionally carry different
records.

When a branch returns an input record and another overrides one of its fields,
the input must already have that field with a compatible type. Additional input
fields are preserved. If both branches replace the field compatibly, the original
field may be absent or have another type. Standalone updates still add fields or
change their types. An annotation on the original constrains only the original;
an explicit annotation on the new result must match that result's complete shape.

Exact record patterns constrain helper arguments just like closed annotations.
Use an open pattern or field projection when a helper should accept extra fields.

<!-- ochat-authoring-example: {"id":"inference.closed-record-annotation-rejected","surface":"one_off_v1","stage":"typecheck","contains":"extra","span":true} -->
```ocaml
let label : {name: string} -> string = fun value -> value.name
let main input = Task.pure(`String(label({name = "task"; extra = true})))
```

<!-- ochat-authoring-example: {"id":"inference.open-helper-update","surface":"one_off_v1","result":"task:done"} -->
```ocaml
let label value = value.name
let finish value = {value with status = "done"}
let main input =
  let updated = finish({name = "task"; status = 1; extra = true}) in
  Task.pure(`String(label(updated) ++ ":" ++ updated.status))
```

<!-- ochat-authoring-example: {"id":"inference.branch-shape-rejected","surface":"one_off_v1","stage":"typecheck","contains":"extra","span":true} -->
```ocaml
let choose flag = if flag then {x = 1} else {x = 2; extra = true}
let main input = Task.pure(`Null)
```

<!-- ochat-authoring-example: {"id":"inference.conditional-update","surface":"one_off_v1","result":7} -->
```ocaml
let maybe_enable flag state = if flag then {state with enabled = true} else state
let main input =
  let next = maybe_enable(true, {enabled = false; count = 7; name = "kept"}) in
  Task.pure(Json.parse(to_string(next.count)))
```

<!-- ochat-authoring-example: {"id":"inference.exact-helper-rejected","surface":"one_off_v1","stage":"typecheck","contains":"extra","span":true} -->
```ocaml
let exact value = match value with | {x = x} -> x
let main input = Task.pure(Json.parse(to_string(exact({x = 1; extra = true}))))
```

## Modules and records have different representations

A module is an environment of exports, not a record value. Field projection and
`record_keys` accept either representation. A helper inferred only from those
operations can be used independently with modules and records; an ordinary
record argument may still have extra fields. Using a record-only operation
constrains the helper to records: record annotations, exact or open record
patterns, and copy-update cannot accept a module, including through an alias or
a function parameter. `open` accepts modules only.

<!-- ochat-authoring-example: {"id":"inference.module-record-projection","surface":"one_off_v1","result":5} -->
```ocaml
let read value = value.x
module M = struct let x = 2 end
let main input =
  Task.pure(Json.parse(to_string(read(M) + read({x=3; extra=true}))))
```

<!-- ochat-authoring-example: {"id":"inference.module-update-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Cannot unify","span":true} -->
```ocaml
let update value = {value with x=3}
module M = struct let x = 2 end
let main input = Task.pure(Json.parse(to_string(update(M).x)))
```

## Matching and equality use inferred types

Variant rows track constructor names and payload types. A match may close a row;
later calls cannot then introduce unrelated constructors. Closed boolean, unit
and variant matches can be exhaustive without a wildcard, and a redundant
wildcard is an error. Open literal domains generally need a fallback. Record
patterns distinguish an exact field set from `{field = value; _}`. See the
[match reference](chatml-match-semantics.md) for the conservative nested-pattern
coverage rules; a plausible pattern list is not a general proof of exhaustiveness.

Constructor argument count is part of the type. A nullary tag such as
`Ready` differs from `Ready(())`, which carries one unit argument. The same
rule applies to annotations and built-ins: `None` and JSON `Null` are nullary,
whereas `Some(())` carries one argument. Multiple payload arguments keep their
individual positions.

<!-- ochat-authoring-example: {"id":"inference.variant-unit-arity","surface":"one_off_v1","result":"unit:pair"} -->
```ocaml
type message = [ `Empty | `Ready(unit) | `Pair(unit, string) ]
let describe value =
  let value : message = value in
  match value with
  | `Empty -> "empty"
  | `Ready(()) -> "unit"
  | `Pair((), text) -> text
let main input =
  let unit_label = match Option.some(()) with | `None -> "none" | `Some(()) -> describe(`Ready(())) in
  Task.pure(`String(unit_label ++ ":" ++ describe(`Pair((), "pair"))))
```

<!-- ochat-authoring-example: {"id":"inference.variant-arity-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Variant constructor arity mismatch","span":true} -->
```ocaml
type message = [ `Ready(unit) ]
let value : message = `Ready
let main input = Task.pure(`Null)
```

Arithmetic and ordering distinguish integers from floats and do not coerce
between them. Equality unifies operand types. When those types are already known,
it rejects arrays, refs, functions and tasks. A generic equality helper currently
does not retain that restriction for later specialization: it can accept these
values and compare them by identity at runtime. This is a compiler limitation,
not structural array equality. Compare elements explicitly when contents matter.

<!-- ochat-authoring-example: {"id":"inference.generic-equality-boundary","surface":"one_off_v1","result":true} -->
```ocaml
let same left right = left == right
let main input =
  let first = [1, 2] in
  let alias = first in
  let copy = Array.copy(first) in
  if same(first, alias) then Task.pure(`Bool(if same(first, copy) then false else true))
  else Task.pure(`Bool(false))
```

Sequencing and a `while` body may
discard values; neither implicitly interprets a discarded task. `if` branches
and match arms both contribute to inference, including branches that a model
believes will never run.

<!-- ochat-authoring-example: {"id":"inference.unreachable-branch-still-checked","surface":"one_off_v1","stage":"typecheck","contains":"Cannot unify","span":true} -->
```ocaml
let main input =
  let value = if true then 1 else "unreachable" in
  Task.pure(Json.parse(to_string(value)))
```

Validation checks these static rules without executing a candidate. It does not
prove termination, bounds safety, tool availability, schema validity or success
of later effects. Inspect linked diagnostics, repair the submitted source, and
validate that exact revision again before execution.
