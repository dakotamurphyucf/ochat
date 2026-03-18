# ChatML language specification

This document is the implementation-faithful specification of ChatML as it
exists in the current codebase.

It describes the language and runtime pipeline implemented by:

- `lib/chatml/chatml_lexer.mll`
- `lib/chatml/chatml_parser.mly`
- `lib/chatml/chatml_lang.ml`
- `lib/chatml/chatml_eval.ml`
- `lib/chatml/frame_env.ml`
- `lib/chatml/chatml_slot_layout.ml`
- `lib/chatml/chatml_typechecker.ml`
- `lib/chatml/chatml_resolver.ml`
- `lib/chatml/chatml_builtin_spec.ml`
- `lib/chatml/chatml_builtin_modules.ml`

When this document and the implementation disagree, the implementation is
authoritative.

---

## 1. Purpose and design constraints

ChatML is a small, statically typed scripting language intended for:

- orchestration scripts
- event-driven glue logic
- lightweight state-machine code
- embedding inside a host runtime with a tiny standard library
- prompt/test-scenario scripting inside the surrounding OCaml project

ChatML is intentionally **not** trying to be:

- a full general-purpose application language
- a rich module language
- a type-class / trait / ad-hoc-overloading language
- a user-extensible type-declaration language
- a high-performance numerical language

The implementation is intentionally biased toward:

1. **sound static typing**
2. **good ergonomics without user-written type annotations**
3. **small surface area**
4. **predictable operational behavior**
5. **enough runtime performance for scripting workloads**

Complexity is deliberately pushed into:

- the host runtime
- the typechecker and resolver
- the internal frame/slot machinery
- builtin host functions

rather than into a large user-facing surface language.

---

## 2. High-level language model

ChatML is:

- expression-oriented
- lexically scoped
- call-by-value
- statically typed with inference
- ML-flavored in syntax
- able to mix immutable and mutable programming styles

The surface language supports:

- first-class functions
- local and recursive bindings
- structural records
- polymorphic variants
- arrays
- refs
- pattern matching
- simple modules

There are currently **no user-written type annotations**.

Function calls use explicit call syntax:

```chatml
f(x)
f(x, y)
g()
```

ChatML does **not** use whitespace application (`f x`) and does **not**
expose currying as the primary call model. Functions are internally modeled
as taking an explicit list of parameters and are called with exact arity.

---

## 3. Implementation architecture

This section describes the actual pipeline used by the implementation.

### 3.1 Phases

The implementation is split into four conceptual phases:

1. **Lexing/parsing**
2. **Type checking**
3. **Resolution/lowering**
4. **Evaluation**

### 3.2 Source AST vs resolved AST

`lib/chatml/chatml_lang.ml` defines two different AST families:

- a **source AST** used by the parser and typechecker
- a **resolved AST** used by the evaluator

Source expressions include forms such as:

- `EVar`
- `ELambda`
- `ELetIn`
- `ELetRec`
- `EMatch`

Resolved expressions include:

- `REVarGlobal`
- `REVarLoc`
- `RELambda`
- `RELetBlock`
- `RELetRec`
- `REMatch`

This phase split is deliberate:

- the typechecker works over source syntax
- the resolver lowers local variables to lexical addresses and binding
  layouts
- the evaluator only runs resolved programs

### 3.3 Program representation

Internally, a parsed program is represented as:

```ocaml
type program =
  { stmts : stmt_node list
  ; source_text : string
  }
```

and a resolved program as:

```ocaml
type resolved_program =
  { stmts : resolved_stmt_node list
  ; source_text : string
  }
```

The stored `source_text` is used for diagnostic formatting.

### 3.4 Type checking

`lib/chatml/chatml_typechecker.ml` implements:

- Hindley–Milner inference
- value restriction
- row-polymorphic records
- row-polymorphic variants
- match checking
- builtin type import

It also records inferred types by source span so the resolver can use that
information later when choosing frame slots.

### 3.5 Resolution

`lib/chatml/chatml_resolver.ml` performs:

- lexical-address resolution for locals
- lowering to resolved AST
- slot selection for locals, parameters, and pattern binders
- non-recursive let-block coalescing

After this pass:

- local variable access is frame-based and indexed
- globals/modules/builtins remain name lookups

### 3.6 Evaluation

`lib/chatml/chatml_eval.ml` evaluates only resolved AST.

The evaluator uses:

- a mutable hash-table environment for top-level/module/global bindings
- a stack of frames for local lexical bindings

Function calls use a trampoline. Tail calls are now **tail-position-aware**:

- closure applications in tail position produce `TailCall`
- closure applications outside tail position are forced immediately

This preserves proper tail-call behavior where it matters while avoiding
unnecessary trampoline traffic in non-tail contexts.

### 3.7 Frames and slots

`lib/chatml/frame_env.ml` provides the low-level local storage runtime.

A frame now stores:

- the raw cells
- the slot layout used to allocate them

Each frame access is validated against its expected slot descriptor.

This means:

- resolver/evaluator slot mismatches fail fast
- out-of-bounds accesses fail fast
- internal frame corruption is less likely to go unnoticed

`lib/chatml/chatml_slot_layout.ml` centralizes the slot-selection policy so
the resolver and evaluator do not maintain duplicated logic.

---

## 4. Lexical structure

### 4.1 Identifiers

ChatML has three main identifier classes:

- lowercase identifiers, e.g. `x`, `state`, `task_index`
- uppercase identifiers, e.g. `M`, `Flow`, `TaskHelpers`
- variant tags, e.g. `` `Some ``, `` `Done ``

Lowercase and uppercase identifiers are tokenized separately. Uppercase
identifiers are mostly intended for modules, but in expression position they
still enter the AST as ordinary variable references.

### 4.2 Literals

Supported literal forms:

- integers: `0`, `1`, `42`
- floats: `1.0`, `3.14`
- booleans: `true`, `false`
- strings: `"hello"`
- unit: `()`

### 4.3 Comments

Comments use OCaml-style block syntax:

```chatml
(* this is a comment *)
```

Nested comments are supported by the lexer.

### 4.4 Strings

Strings support at least:

- `\n`
- `\t`
- `\\`
- `\"`

Strings may span multiple lines.

### 4.5 Whitespace

Whitespace is not significant except as a token separator.

ChatML is **not** indentation-sensitive.

---

## 5. Program structure

A ChatML program is a sequence of top-level statements.

Top-level statement forms:

- `let x = expr`
- `let f a b = expr`
- `let f () = expr`
- `let rec f x = expr and g y = expr`
- `module M = struct ... end`
- `open M`
- a bare expression statement

Evaluation proceeds top-to-bottom in source order.

The top level is mutable in the sense that each statement extends the
current environment, but closures capture lexical bindings stably.

---

## 6. Statement forms

### 6.1 Non-recursive top-level `let`

Examples:

```chatml
let x = 1
let name = "Alice"
let inc n = n + 1
let thunk () = 42
```

Properties:

- the RHS is evaluated before the new binding is introduced
- the new binding is then added to the current environment
- later top-level lets may shadow earlier ones
- already-created closures still observe the lexical binding they captured

### 6.2 Recursive top-level `let rec`

Examples:

```chatml
let rec fact n =
  if n == 0 then 1 else n * fact(n - 1)
```

```chatml
let rec even n = if n == 0 then true else odd(n - 1)
and odd n = if n == 0 then false else even(n - 1)
```

Restrictions:

- recursive bindings must be function-like
- non-function recursive bindings are rejected statically

Operationally:

- recursive names are allocated first
- placeholders are installed
- each RHS is evaluated in an environment where all recursive names are
  visible
- placeholders are updated with the final values

### 6.3 Modules

Example:

```chatml
module Flow = struct
  let x = 1
  let id y = y
end
```

Modules are intentionally simple namespaces.

Properties:

- module bodies may reference outer bindings
- only names explicitly defined in the module body are exported
- names imported via `open` inside a module are not re-exported
- modules are represented as records by the typechecker and as `VModule`
  values at runtime

### 6.4 `open`

Example:

```chatml
open Flow
```

Semantics:

- imports all exported names from the module into the current scope
- does not create a module alias
- is shallow; there is no selective import syntax
- now **rejects shadowing** of existing names

So this is rejected:

```chatml
let x = 1
module M = struct
  let x = 2
end
open M
```

Both the typechecker and the runtime reject such shadowing.

---

## 7. Expression forms

### 7.1 Unit

```chatml
()
```

Type: `unit`

### 7.2 Variables

```chatml
x
state
Flow
```

Variables are lexically scoped.

After resolution:

- local variables become lexical-address lookups into frames
- globals/modules/builtins remain environment lookups

### 7.3 Functions

Anonymous functions:

```chatml
fun x -> x
fun x y -> x
fun () -> 42
```

Named function syntax is sugar for a `let` binding of a lambda:

```chatml
let add x y = x + y
```

Properties:

- functions are first-class
- closures capture lexical environment plus local frame stack
- calls are strict
- exact arity is required
- tail calls are optimized through a trampoline

### 7.4 Function application

Examples:

```chatml
f(x)
f(x, y)
g()
```

Function position may itself be any expression:

```chatml
(fun x -> x)(1)
choose(true)(1)
```

There is no whitespace application syntax such as `f x`.

### 7.5 Local `let ... in`

Examples:

```chatml
let x = 1 in x + 1
let f y = y in f(3)
let rec loop n = ... in loop(10)
```

Properties:

- non-recursive lets are lexical and sequential
- nested non-recursive lets are internally grouped into `RELetBlock`
  layouts by the resolver
- `let rec` inside expressions follows the same recursive-function
  restriction as top-level `let rec`

### 7.6 Conditionals

```chatml
if cond then a else b
```

Rules:

- condition must have type `bool`
- both branches must have the same type

### 7.7 Sequencing

```chatml
e1; e2
```

Rules:

- `e1` is evaluated fully first
- its value is discarded
- the result is the value of `e2`

### 7.8 While loops

```chatml
while cond do body done
```

Rules:

- condition must have type `bool`
- loop result type is `unit`
- loop body may have side effects

### 7.9 Records

Record literal:

```chatml
{ name = "Alice"; age = 30 }
```

Field access:

```chatml
person.name
```

Record copy-update:

```chatml
{ person with age = person.age + 1 }
```

Properties:

- records are structural
- field names are unique within a literal
- duplicate field labels in a literal are rejected
- copy-update is immutable
- copy-update may overwrite fields
- copy-update may add fields to closed records
- copy-update may change field types

### 7.10 Arrays

Array literal:

```chatml
[1, 2, 3]
```

Indexing:

```chatml
arr[i]
```

Update:

```chatml
arr[i] <- v
```

Properties:

- arrays are homogeneous
- arrays are mutable
- index type must be `int`
- out-of-bounds access is a runtime error
- update returns `unit`

### 7.11 References

Creation:

```chatml
ref(0)
```

Dereference:

```chatml
!r
```

Assignment:

```chatml
r := 1
```

Properties:

- refs are mutable cells
- dereference requires a ref
- assignment requires a ref value and a value of the stored type
- assignment returns `unit`

### 7.12 Variants

Examples:

```chatml
`None
`Some(1)
`Pair(1, "x")
```

Properties:

- variants are polymorphic variants
- constructors are identified by tag name
- constructors may carry zero, one, or multiple payload values
- multi-value payloads are typed using internal tuple types

---

## 8. Operators

Operators are built into the core AST. They are not looked up from the
runtime environment and cannot be overridden.

### 8.1 Integer arithmetic

- binary `+`
- binary `-`
- binary `*`
- binary `/`
- unary `-`

Operands must be `int`; result is `int`.

Division by zero is a runtime error.

### 8.2 Float arithmetic

- binary `+.`
- binary `-.`
- binary `*.`
- binary `/.`
- unary `-.`

Operands must be `float`; result is `float`.

Division by zero is a runtime error when the divisor is `0.0`.

### 8.3 String concatenation

- binary `++`

Operands must be `string`; result is `string`.

### 8.4 Integer comparisons

- `<`
- `>`
- `<=`
- `>=`

Operands must be `int`; result is `bool`.

### 8.5 Float comparisons

- `<.`
- `>.`
- `<=.`
- `>=.`

Operands must be `float`; result is `bool`.

### 8.6 Equality and inequality

- `==`
- `!=`

Both operands must have the same type.

However, equality is now **restricted**. It is accepted only for types that
the typechecker considers equality-supporting.

Accepted:

- `int`
- `float`
- `bool`
- `string`
- `unit`
- tuples of equality-supporting element types
- records whose known field types are equality-supporting
- variants whose known payload types are equality-supporting

Rejected:

- arrays
- refs
- functions

Examples:

```chatml
1 == 1         (* ok *)
"a" != "b"     (* ok *)
[1, 2] == [1]  (* type error *)
```

Implementation note:

- runtime equality for records/variants is structural
- runtime equality for arrays/refs/closures/modules/builtins is by identity
- the typechecker now rejects the most problematic unsupported cases

### 8.7 Precedence and associativity

The current parser precedence is roughly:

1. comparisons and equality
2. additive operators
3. multiplicative operators
4. dereference handling

Concretely:

- `+`, `-`, `++`, `+.`, `-.` share a precedence level
- `*`, `/`, `*.`, `/.` share a tighter precedence level
- comparison/equality are looser than arithmetic

Use parentheses whenever readability matters.

---

## 9. Pattern matching

### 9.1 Supported patterns

ChatML supports:

- wildcard: `_`
- variable binder: `x`
- unit: `()`
- integer literal patterns
- boolean literal patterns
- float literal patterns
- string literal patterns
- variant patterns:
  - `` `Tag ``
  - `` `Tag(p1, ..., pn) ``
- record patterns:
  - `{ field = pat; field2 = pat2 }`
  - `{ field = pat; _ }`

### 9.2 Runtime matching

Match arms are tried in source order.

The first matching arm is selected.

If no arm matches at runtime, evaluation raises a runtime error.

### 9.3 Pattern variable order

Pattern variables are collected in deterministic left-to-right order.
This matters for resolver slot layout, but not for user-visible semantics.

### 9.4 Record patterns

Closed record pattern:

```chatml
{ name = n }
```

This requires the record to have exactly the named fields.

Open record pattern:

```chatml
{ name = n; _ }
```

This requires the record to have at least those fields.

### 9.5 Static match checks

The typechecker performs:

- duplicate binder checks
- duplicate simple-arm checks
- some redundancy checks
- conservative exhaustiveness checks

Exhaustiveness is strongest for:

- booleans
- unit
- sufficiently closed variant matches

It is conservative for:

- ints
- floats
- strings
- records
- open variants

### 9.6 Variant narrowing by match

Variant-using functions can become narrower after informative matches.

Example:

```chatml
let f v =
  match v with
  | `Some(x) -> x
```

The parameter type inferred for `v` may be narrowed to compatible variants,
rather than remaining arbitrarily open.

This is intentional.

---

## 10. Type system

ChatML uses Hindley–Milner style inference with extensions for:

- mutation safety via the value restriction
- row-polymorphic records
- row-polymorphic variants

Users do not write type annotations.

### 10.1 Primitive types

- `unit`
- `int`
- `float`
- `bool`
- `string`

### 10.2 Composite types

- function types
- array types
- ref types
- record types
- variant types
- tuple types

Tuple types currently exist in the type system and runtime representation,
but tuple syntax is not exposed as a general user-facing surface feature.
They are most visible as the internal typing of multi-argument variant
payloads.

### 10.3 Let-polymorphism

Non-expansive bindings may be generalized.

Example:

```chatml
let id x = x
id(1)
id("s")
```

### 10.4 Value restriction

Expansive bindings are not generalized.

This is necessary for soundness with:

- refs
- arrays
- mutable aliasing

### 10.5 Records and row polymorphism

Record helpers usually infer open-row behavior.

Example:

```chatml
let get_name p = p.name
```

This can be used on:

```chatml
{name = "A"}
{name = "A"; age = 1}
```

Important implementation detail:

- lambda parameters discovered to be record-shaped are reopened to open rows
- this heuristic is intentionally biased toward record-heavy scripting and
  state-machine helpers
- variants are not reopened by the same heuristic

### 10.6 Variants and row polymorphism

Variant constructors are typed using row-based variant information.

Examples:

```chatml
`None
`Some(1)
`Pair(1, "x")
```

Variant row information interacts with pattern matching and may become
narrower after informative matches.

### 10.7 Recursive bindings

Recursive bindings must be functions.

This avoids unsound and difficult recursive value-inference cases.

### 10.8 Builtin type schemes

The builtin specification language now supports:

- type variables
- primitive types
- arrays
- refs
- tuples
- row-based records
- row-based variants
- function types

This is a host-side capability used by builtin declarations. Users still do
not write these type forms directly.

### 10.9 Module typing

Modules are typed as records of exports.

This is intentionally simple and matches the intended “modules are just
structuring” design.

Implementation note:

- runtime modules are represented as `VModule`
- the typechecker models them as record types of exports

This is usually ergonomic, but it also means module values are not a fully
separate static category.

---

## 11. Runtime model

### 11.1 Values

The runtime supports:

- ints
- bools
- floats
- strings
- variants
- records
- arrays
- refs
- closures
- modules
- unit
- builtins

### 11.2 Closures

Closures capture:

- the lexical environment
- the local frame stack
- the parameter slot layout

Closures capture lexical bindings stably, so later rebinding does not change
what an earlier closure sees.

### 11.3 Environments

There are two main runtime storage mechanisms:

1. a mutable hash-table environment for globals/modules/builtins
2. a stack of local frames for resolved lexical locals

### 11.4 Resolution and lexical addresses

The resolver rewrites local variables into lexical addresses carrying:

- frame depth
- slot index
- slot descriptor

This allows:

- O(1)-style local reads
- one-frame block allocation for grouped lets
- reduced runtime name lookup for locals

### 11.5 Frames

Frames are heterogeneous storage blocks described by packed slot layouts.

The runtime currently distinguishes slots for:

- `int`
- `bool`
- `float`
- `string`
- generic object slots

Each frame now stores its layout explicitly, and frame reads/writes validate
that the requested slot matches the allocated layout.

### 11.6 Slot selection

Slot selection is shared between resolver and evaluator through
`chatml_slot_layout.ml`.

This keeps:

- static slot selection
- runtime slot/value validation
- fallback expression-shape heuristics

consistent.

### 11.7 Tail calls

Closure calls are executed through a trampoline.

Current behavior:

- calls in tail position use `TailCall`
- calls outside tail position are forced immediately

This gives tail recursion support without forcing every function call through
the trampoline.

### 11.8 Runtime errors

Possible runtime failures include:

- division by zero
- array index out of bounds
- dereference of non-ref
- assignment to non-ref
- calling a non-function value
- function arity mismatch
- non-exhaustive runtime pattern match
- invalid field access
- invalid `open`
- `open` shadowing collisions

Ill-typed programs are normally rejected before evaluation in the standard
pipeline.

---

## 12. Modules

Modules are intentionally simple namespace containers.

### 12.1 What modules are for

Modules are for:

- grouping helper functions
- reducing naming clutter
- structuring scripts

Modules are not for:

- signatures
- functors
- generative module behavior
- abstraction-heavy namespace engineering

### 12.2 Export behavior

Only names explicitly defined in the module body are exported.

Example:

```chatml
let x = 1
module M = struct
  let y = x
end
```

Valid:

```chatml
M.y
```

Invalid:

```chatml
M.x
```

### 12.3 `open` behavior

`open M` copies module exports into the current environment for subsequent
lookup.

It does not:

- re-export opened names automatically
- support selective imports
- allow silent shadowing

The language now rejects `open` if it would overwrite an existing binding in
the current scope.

---

## 13. Standard library

The current runtime prelude is still intentionally small, but it now goes a
bit beyond the original minimal core.

Installed builtins:

- `print : 'a -> unit`
- `to_string : 'a -> string`
- `length : 'a array -> int`
- `string_length : string -> int`
- `string_is_empty : string -> bool`
- `array_copy : 'a array -> 'a array`
- `record_keys : { ...r } -> string array`
- `variant_tag : [ ...r ] -> string`
- `swap_ref : ref('a) -> 'a -> 'a`
- `fail : string -> 'a`

Notes:

- `print` renders a stable human-readable representation of runtime values
- `to_string` returns that representation
- `length` works on arrays only
- `array_copy` is a shallow copy of the array container
- `record_keys` works on record values, and also on module values because
  modules are statically record-like
- `variant_tag` returns only the constructor/tag name, not the payload
- `swap_ref r v` stores `v` into `r` and returns the old contents
- `fail` always raises a runtime failure and is typed polymorphically in its
  result position

Arithmetic, comparison, and concatenation are language primitives rather
than builtins.

The builtin type-description language is richer than the currently installed
prelude and can safely describe:

- refs
- tuples
- record-shaped APIs
- variant-shaped APIs
- open-row record/variant interfaces

---

## 14. Surface syntax summary

This is not a full formal grammar, but it summarizes the implemented
surface syntax.

### 14.1 Statements

```chatml
let x = expr
let f x y = expr
let f () = expr
let rec f x = expr and g y = expr
module M = struct stmts end
open M
expr
```

### 14.2 Expressions

```chatml
()
1
1.0
true
"x"
x
fun x -> expr
fun () -> expr
f(x)
if c then t else e
while c do body done
let x = e1 in e2
let rec f x = e1 in e2
match e with | pat -> e
{ a = e; b = e }
e.field
{ e with field = e }
[e1, e2, e3]
arr[i]
arr[i] <- v
ref(e)
!r
r := v
e1; e2
`Tag
`Tag(e1, e2)
```

### 14.3 Operators

```chatml
x + y
x - y
x * y
x / y
-x

x +. y
x -. y
x *. y
x /. y
-.x

x ++ y

x < y
x > y
x <= y
x >= y

x <. y
x >. y
x <=. y
x >=. y

x == y
x != y
```

### 14.4 Patterns

```chatml
_
x
()
1
1.0
true
"x"
`Tag
`Tag(p1, p2)
{ field = pat }
{ field = pat; _ }
```

---

## 15. Diagnostics

### 15.1 Type errors

Type errors are reported with:

- a message
- an optional source span

When a span is available, formatting uses source-text excerpts with caret
markers.

### 15.2 Runtime errors

Runtime errors are also structured:

- message
- optional source span

and are formatted in the same general style as type errors.

### 15.3 Current strengths

Diagnostics are now materially better for:

- row-typed records
- row-typed variants
- equality misuse
- `open` shadowing

### 15.4 Current parser limitation

Parse errors are still comparatively basic. Menhir failure reporting is not
yet elevated to the same level of quality as type/runtime diagnostics.

---

## 16. Soundness-related notes

The current implementation intentionally enforces or relies on:

- lexical closure capture being stable
- recursive bindings being function-only
- mutation interacting with polymorphism via a value restriction
- explicit separation of integer and float operators
- int-only array indexing
- explicit resolver lowering before evaluation
- runtime validation of frame slot layouts
- no silent `open` shadowing
- equality restrictions for unsupported runtime representations

These are central to the language’s current safety/ergonomics tradeoff.

---

## 17. Known intentional limitations

ChatML currently does **not** provide:

- user-written type annotations
- tuple syntax as a general user-facing feature
- algebraic data type declarations
- selective imports
- signatures or functors
- layout-sensitive syntax
- Unicode identifiers
- full ML-style match usefulness analysis
- ad-hoc overloaded numeric operators

The language remains intentionally conservative and small.

---

## 18. Important implementation caveats

These are worth documenting because they shape current behavior.

### 18.1 Modules are statically record-like, but runtime-distinct

The typechecker models modules as records of exports, while the runtime
represents them as `VModule`.

This is deliberate and ergonomic, but it means “module values” are not a
fully separate static category.

### 18.2 Records get stronger row ergonomics than variants

Record-heavy scripting is a primary use case, so lambda parameters that
become record-shaped are reopened to open rows. Variants do not get the
same reopening heuristic.

### 18.3 Builtins remain a host-side facility

The richer builtin type language exists for host/runtime authors, not as a
user-facing type-annotation mechanism.

---

## 19. Recommended style

For the current language, the most ergonomic and robust style is:

- use records for script state
- write small helpers over row-polymorphic state records
- use variants for finite event/state tags
- use modules only for grouping
- keep arithmetic explicit by numeric kind
- prefer `M.name` over aggressive `open` use when readability matters
- push complex host interaction into builtins/runtime services

---

## 20. Reference examples

### 20.1 Record-heavy state helper

```chatml
let bump_attempts st =
  let t = st.tasks[st.task_index] in
  let t = { t with attempts = t.attempts + 1 } in
  st.tasks[st.task_index] <- t;
  st
```

### 20.2 Variant-driven event handler

```chatml
let step st ev =
  match ev with
  | `Start -> { st with running = true }
  | `Stop -> { st with running = false }
```

### 20.3 Float logic with explicit dotted operators

```chatml
let avg x y = (x +. y) /. 2.0
if avg(1.0, 3.0) >=. 2.0 then true else false
```

### 20.4 Simple module namespace

```chatml
module Flow = struct
  let one = 1
  let inc x = x + 1
end

Flow.inc(Flow.one)
```

### 20.5 Shadow-safe module import

```chatml
module Math = struct
  let two = 2
end

open Math
print(two)
```

But:

```chatml
let two = 99
open Math
```

is rejected because `open Math` would shadow `two`.
