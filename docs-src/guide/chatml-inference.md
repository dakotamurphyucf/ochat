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

## Records retain only fields guaranteed by their type

Field access constrains an inferred record parameter to contain that field.
Unannotated helper parameters can remain open to additional fields. An explicit
closed annotation makes a deliberate narrower contract; it is not a request to
discard extra fields at runtime. Duplicate record fields, update labels, type
labels and pattern binders are rejected.

Copy-update produces a new record, can add a field, and can replace a field with
a different type. Refs and arrays inside either record still share their storage.
At an `if` or `match` result join, records keep only common fields and join those
fields recursively. An open tail is preserved only when both sides share the
same tail identity; merely similar-looking unknown rows do not grant fields.
Outside record joins, branch result types must unify. Returning a variant with
different constructors is often clearer than returning incompatible field types.

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

## Matching and equality use inferred types

Variant rows track constructor names and payload types. A match may close a row;
later calls cannot then introduce unrelated constructors. Closed boolean, unit
and variant matches can be exhaustive without a wildcard, and a redundant
wildcard is an error. Open literal domains generally need a fallback. Record
patterns distinguish an exact field set from `{field = value; _}`. See the
[match reference](chatml-match-semantics.md) for the conservative nested-pattern
coverage rules; a plausible pattern list is not a general proof of exhaustiveness.

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
