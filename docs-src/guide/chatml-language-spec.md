# ChatML language specification

This document is the implementation-faithful specification of ChatML as it
exists in the current codebase.

It describes the language and runtime pipeline implemented by:

- `lib/chatml/chatml_lexer.mll`
- `lib/chatml/chatml_parser.mly`
- `lib/chatml/chatml_lang.ml`
- `lib/chatml/chatml_parse.ml`
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
- a full algebraic-datatype / type-parameter language
- a high-performance numerical language

The implementation is intentionally biased toward:

1. **sound static typing**
2. **good ergonomics with inference-first typing and a small explicit type surface**
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
- top-level named type declarations
- checked binding annotations
- structural records
- polymorphic variants
- arrays
- refs
- pattern matching
- simple modules

ChatML now has a deliberately small user-facing type surface:

- top-level `type` declarations,
- binding annotations on `let`, `let rec`, and `let ... in`,
- explicit recursive types introduced through those declarations.

It still does **not** provide a full ML type language: there are no type
parameters, no mutual recursive type declarations, and no general
expression-level ascription syntax.

Function calls use explicit call syntax:

```ocaml
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

```ocaml
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

- `type t = type_expr`
- `let x = expr`
- `let x : type_expr = expr`
- `let f a b = expr`
- `let f () = expr`
- `let rec f : type_expr = expr and g : type_expr = expr`
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

```ocaml
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

```ocaml
let rec fact n =
  if n == 0 then 1 else n * fact(n - 1)
```

```ocaml
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

### 6.3 Top-level `type`

Examples:

```ocaml
type expr = [ `Int(int) | `Add(expr, expr) ]
type task = { name : string; attempts : int; status : status }
```

Properties:

- type declarations are compile-time only
- they introduce names into a separate type namespace
- they are currently allowed only at the top level
- later statements may refer to earlier type declarations
- module bodies may refer to earlier top-level type declarations
- `open` does not import type names
- type declarations are alias-like, not nominal runtime entities

Recursive type declarations are allowed, but only in explicit checked form.
The typechecker validates them for **contractiveness**:

- accepted:
  - `type expr = [ \`Int(int) | \`Add(expr, expr) ]`
  - `type node = { value : int; next : node }`
- rejected:
  - `type bad = bad`

Current intentional limitations:

- no `type ... and ...`
- no type parameters
- no module-local type declarations
- no forward references to later type declarations

### 6.4 Modules

Example:

```ocaml
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

Type declarations are not statements inside module bodies in the current
surface grammar.

### 6.5 `open`

Example:

```ocaml
open Flow
```

Semantics:

- imports all exported names from the module into the current scope
- does not create a module alias
- is shallow; there is no selective import syntax
- now **rejects shadowing** of existing names

So this is rejected:

```ocaml
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

```ocaml
()
```

Type: `unit`

### 7.2 Variables

```ocaml
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

```ocaml
fun x -> x
fun x y -> x
fun () -> 42
```

Named function syntax is sugar for a `let` binding of a lambda:

```ocaml
let add x y = x + y
```

Properties:

- functions are first-class
- closures capture lexical environment plus local frame stack
- calls are strict
- exact arity is required
- tail calls are optimized through a trampoline

For annotated functions, the current surface syntax annotates the binding,
not individual parameters:

```ocaml
let rec eval : expr -> int =
  fun e -> ...
```

Zero-argument annotated functions use `unit -> t`:

```ocaml
let finish_action : unit -> string =
  fun () -> "done"
```

### 7.4 Function application

Examples:

```ocaml
f(x)
f(x, y)
g()
```

Function position may itself be any expression:

```ocaml
(fun x -> x)(1)
choose(true)(1)
```

There is no whitespace application syntax such as `f x`.

### 7.5 Local `let ... in`

Examples:

```ocaml
let x = 1 in x + 1
let x : int = 1 in x + 1
let f y = y in f(3)
let rec loop n = ... in loop(10)
```

Properties:

- non-recursive lets are lexical and sequential
- nested non-recursive lets are internally grouped into `RELetBlock`
  layouts by the resolver
- `let rec` inside expressions follows the same recursive-function
  restriction as top-level `let rec`
- binding annotations are checked against the inferred RHS type
- there is currently no general `(expr : type)` surface syntax; annotations
  are introduced through binding forms

### 7.6 Conditionals

```ocaml
if cond then a else b
```

Rules:

- condition must have type `bool`
- both branches must have the same type for non-record results
- record-valued branches are combined using a conservative **join**

For records, ChatML keeps only the fields that are guaranteed on **every**
branch. This avoids unsoundly concluding that a field exists just because one
branch adds it with copy-update.

Example:

```ocaml
let maybe_set_running b st =
  if b then st else { st with running = true }
```

The result of `maybe_set_running` is **not** treated as definitely having a
`running` field, because the `then` branch returns `st` unchanged.

By contrast:

```ocaml
let set_running st running =
  { st with running = running }

let ensure_running b st =
  if b then set_running(st, true) else set_running(st, false)
```

does guarantee `running` on every path, so the joined result keeps that field.

### 7.7 Sequencing

```ocaml
e1; e2
```

Rules:

- `e1` is evaluated fully first
- its value is discarded
- the result is the value of `e2`

### 7.8 While loops

```ocaml
while cond do body done
```

Rules:

- condition must have type `bool`
- loop result type is `unit`
- loop body may have side effects

### 7.9 Records

Record literal:

```ocaml
{ name = "Alice"; age = 30 }
```

Field access:

```ocaml
person.name
```

Record copy-update:

```ocaml
{ person with age = person.age + 1 }
```

Properties:

- records are structural
- field names are unique within a literal
- duplicate field labels in a literal are rejected
- copy-update is immutable
- copy-update may overwrite fields
- copy-update may add fields to closed records
- copy-update may also add fields through open-row helper functions
- copy-update may change field types

### 7.10 Arrays

Array literal:

```ocaml
[1, 2, 3]
```

Indexing:

```ocaml
arr[i]
```

Update:

```ocaml
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

```ocaml
ref(0)
```

Dereference:

```ocaml
!r
```

Assignment:

```ocaml
r := 1
```

Properties:

- refs are mutable cells
- dereference requires a ref
- assignment requires a ref value and a value of the stored type
- assignment returns `unit`

### 7.12 Variants

Examples:

```ocaml
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

```ocaml
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

```ocaml
{ name = n }
```

This requires the record to have exactly the named fields.

Open record pattern:

```ocaml
{ name = n; _ }
```

This requires the record to have at least those fields.

### 9.5 Static match checks

The typechecker performs:

- duplicate binder checks
- duplicate simple-arm checks
- some redundancy checks
- conservative exhaustiveness checks

The result type of a `match` follows the same rule as `if`:

- non-record arm results must unify to the same type
- record-valued arm results are combined using the same conservative join
  used for `if`

So if one arm adds a record field and another arm does not guarantee it, the
overall `match` result type does not retain that field.

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

```ocaml
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

- explicit recursive types
- mutation safety via the value restriction
- row-polymorphic records
- row-polymorphic variants

Unlike earlier versions, ChatML now has a small explicit type surface for:

- top-level named type declarations
- checked binding annotations

Ordinary HM inference variables are acyclic again. Recursive types are not
inferred accidentally from ordinary unification; they are introduced only
through explicit checked declarations.

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

### 10.2.1 User-facing type syntax

The current user-facing type-expression syntax supports:

- primitive names:
  - `int`
  - `float`
  - `bool`
  - `string`
  - `unit`
- previously declared type names
- function types:
  - `expr -> int`
  - `state -> event -> state`
  - `unit -> string`
- postfix array types:
  - `task array`
- closed record types:
  - `{ name : string; attempts : int }`
- closed variant types:
  - `[ `Pending | `Done | `Error(string) ]`

Examples:

```ocaml
type status = [ `Pending | `Running | `Done | `Error(string) ]
type task =
  { name : string
  ; attempts : int
  ; status : status
  }

let step : task -> status =
  fun t -> t.status
```

Current user-facing omissions are intentional:

- no tuple type syntax
- no ref type syntax
- no open-row type syntax
- no type parameters
- no mutual recursive type declarations

### 10.2.2 Explicit recursive types

Recursive types are represented internally using explicit recursive binders,
conceptually of the form:

```ocaml
| Mu of string * typ
| Rec_var of string
```

Recursive cycles therefore appear only through this checked representation,
not through cyclic ordinary inference variables.

Surface recursive types are introduced via named `type` declarations:

```ocaml
type expr = [ `Int(int) | `Add(expr, expr) ]
```

The typechecker elaborates those declarations into explicit internal
recursive types and checks that they are contractive.

### 10.2.3 Contractiveness

Recursive type declarations must be **contractive**: self-reference must
appear under a real constructor.

Accepted:

```ocaml
type expr = [ `Int(int) | `Add(expr, expr) ]
type node = { value : int; next : node }
```

Rejected:

```ocaml
type bad = bad
```

This rule keeps recursive types sound while still supporting the recursive
record and recursive variant use-cases ChatML scripts rely on.

### 10.3 Let-polymorphism

Non-expansive bindings may be generalized.

Example:

```ocaml
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

### 10.4.1 Recursive types remain monomorphic

Bindings whose type contains an explicit recursive type are kept
monomorphic in the current design.

This applies even when the binding is otherwise non-expansive.

The implementation intentionally does **not** attempt polymorphic
recursion.

### 10.5 Records and row polymorphism

Record helpers usually infer open-row behavior.

Example:

```ocaml
let get_name p = p.name
```

This can be used on:

```ocaml
{name = "A"}
{name = "A"; age = 1}
```

Important implementation detail:

- lambda parameters discovered to be record-shaped are reopened to open rows
- this heuristic is intentionally biased toward record-heavy scripting and
  state-machine helpers
- variants are not reopened by the same heuristic

### 10.5.1 Control-flow joins for records

Record copy-update can widen a record result:

```ocaml
let with_timeout cfg ms =
  { cfg with timeout_ms = ms }
```

This gives `with_timeout` the expected shape:

```text
{ ...r } -> int -> { timeout_ms : int; ...r }
```

However, `if` and `match` do **not** preserve fields that appear on only some
paths. Instead, they compute a conservative join that keeps only fields
guaranteed on every branch.

This means ChatML no longer relies on branch-shape heuristics. If a field
should be available after control flow, make that field explicit on every
returned branch.

Recommended patterns:

- ensure initialization helpers return the same shape on all paths
- make field updates explicit on every branch

#### Initialization example

Avoid writing a helper whose branches return different record shapes unless
both shapes already guarantee the fields you plan to read later.

For example, the following shape-changing helper is not something the
conservative join will strengthen:

```ocaml
let init_state st =
  if st.inited then st else
    { inited = true
    ; autopilot = true
    ; task_index = 0
    ; task_count = 4
    ; tasks = ...
    }
```

In the copied regression tests, callers already provide a fully initialized
state, so the recommended rewrite is simply:

```ocaml
let init_state st = st
```

This satisfies the join trivially because both paths are the same shape.

#### Explicit field-update example

Instead of:

```ocaml
let step st ev =
  match ev with
  | `Start ->
    if st.idx >= length(st.tasks) then st
    else set_status({ st with running = true }, status_witness(1))

  | `Tick ->
    if st.running == false then st
    else ...
```

prefer:

```ocaml
let set_running st running =
  { st with running = running }

let step st ev =
  match ev with
  | `Start ->
    if st.idx >= length(st.tasks) then set_running(st, false)
    else set_status(set_running(st, true), status_witness(1))

  | `Tick ->
    if st.running == false then set_running(st, false)
    else
      let st = set_running(st, true) in
      ...
```

Why this works:

- every returned branch now explicitly produces a state with `running`
- the conservative join can therefore keep `running`
- no branch-shape heuristic is required

One-sentence summary:

> if a record field must exist after `if` or `match`, make sure every branch
> returns a record that explicitly contains that field.

### 10.6 Variants and row polymorphism

Variant constructors are typed using row-based variant information.

Examples:

```ocaml
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
- explicit recursive types (mu-style binders) used internally by some builtin modules (not user-surface syntax)

Notes:

This builtin type language is richer than the current user-facing type language.
Users still do not write builtin-only forms such as ref types, tuple types, open-row forms, or explicit recursive binders directly; they appear only through host-provided builtin schemes.

### 10.9 Module typing

Modules are typed as records of exports.

This is intentionally simple and matches the intended “modules are just
structuring” design.

Implementation note:

- runtime modules are represented as `VModule`
- the typechecker models them as record types of exports

This is usually ergonomic, but it also means module values are not a fully
separate static category.

Type declarations are not exported as module fields, because they do not
exist at runtime and `open` affects only value bindings.

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

```ocaml
let x = 1
module M = struct
  let y = x
end
```

Valid:

```ocaml
M.y
```

Invalid:

```ocaml
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

ChatML ships with a small runtime prelude consisting of:

a set of global builtin functions, and
a set of builtin modules installed as VModule values (typed as records of exports).
Arithmetic, string concatenation, comparison, and equality operators remain language primitives rather than runtime-installed builtins.

### 13.1 Global builtins
Installed global builtins:

```ocaml
print : 'a -> unit
to_string : 'a -> string
length : 'a array -> int
string_length : string -> int
string_is_empty : string -> bool
array_copy : 'a array -> 'a array
record_keys : { ...r } -> string array
variant_tag : [ ...r ] -> string
swap_ref : ref('a) -> 'a -> 'a
fail : string -> 'a
```

Notes:

- print renders a stable human-readable representation of runtime values.
- to_string returns that representation.
- length works on arrays only.
- array_copy is a shallow copy of the array container.
- record_keys works on record values, and also on module values because modules are record-like at the type level.
- variant_tag returns only the constructor/tag name, not the payload.
- swap_ref r v stores v into r and returns the old contents.
- fail raises a runtime failure and is polymorphic in its result position.
### 13.2 Builtin modules
The runtime also installs several builtin modules. Each module is a VModule value at runtime, typed as a record of its exports by the typechecker.

### 13.2.1 `String` module (updated)

The `String` builtin module provides common string utilities.

Exports:

#### Basic operations

- `String.length : string -> int`
- `String.is_empty : string -> bool`
- `String.concat : string -> string -> string`

Notes:

- `String.concat(a, b)` is ordinary concatenation. The language also provides the `++` operator.

#### Comparison and queries

- `String.equal : string -> string -> bool`
- `String.contains : string -> string -> bool`  
  True if the second string is a substring of the first.
- `String.starts_with : string -> string -> bool`
- `String.ends_with : string -> string -> bool`

#### Transformations

- `String.trim : string -> string`  
  Removes leading and trailing whitespace.
- `String.to_upper : string -> string`
- `String.to_lower : string -> string`

#### Slicing, search, and split

- `String.slice : string -> int -> int -> string`  
  `slice(s, start, len)` returns the substring of length `len` starting at `start`. Raises on invalid bounds.
- `String.find : string -> string -> [ \`None | \`Some(int) ]`  
  Finds the first occurrence of the pattern and returns its starting index.
- `String.split : string -> string -> string array`  
  Splits on a non-empty separator string; raises if the separator is empty.
- `String.replace_all : string -> string -> string -> string`  
  `replace_all(s, pattern, with_)` replaces all non-overlapping occurrences. Raises if `pattern` is empty.

---

### 13.2.2 `Array` module (updated)

The `Array` builtin module provides array utilities. Arrays are homogeneous and mutable.

Exports:

#### Basic operations

- `Array.length : 'a array -> int`
- `Array.copy : 'a array -> 'a array`
- `Array.get : 'a array -> int -> 'a`
- `Array.set : 'a array -> int -> 'a -> unit`

Notes:

- `Array.get` and `Array.set` raise a runtime error on out-of-bounds indices.
- `Array.length` overlaps with the global builtin `length`. Because `open` rejects shadowing, `open Array` may be rejected in scopes where `length` is already bound (including the default prelude).

#### Allocation / structural utilities (non-higher-order)

- `Array.make : int -> 'a -> 'a array`  
  Creates an array of the given length filled with the provided value. Raises on negative length.
- `Array.append : 'a array -> 'a array -> 'a array`  
  Allocates a new array containing the concatenation of the two inputs.
- `Array.sub : 'a array -> int -> int -> 'a array`  
  `sub(arr, start, len)` returns a new array slice. Raises on invalid bounds.
- `Array.reverse : 'a array -> 'a array`  
  Returns a reversed copy.
- `Array.reverse_in_place : 'a array -> unit`  
  Mutates the array by reversing it.
- `Array.swap : 'a array -> int -> int -> unit`  
  Swaps two indices. Raises on invalid bounds.
- `Array.fill : 'a array -> 'a -> unit`  
  Mutates the array by filling every element with the provided value.

#### Higher-order utilities (call back into ChatML)

These functions accept ChatML functions/closures as arguments. They execute those callbacks using the interpreter’s normal call semantics (strict, arity-checked, tail-call aware), and propagate runtime failures from inside the callback.

- `Array.init : int -> (int -> 'a) -> 'a array`  
  Creates a new array by calling the function on indices `0..n-1`. Raises on negative length.
- `Array.map : 'a array -> ('a -> 'b) -> 'b array`
- `Array.mapi : 'a array -> (int -> 'a -> 'b) -> 'b array`
- `Array.iter : 'a array -> ('a -> unit) -> unit`
- `Array.iteri : 'a array -> (int -> 'a -> unit) -> unit`
- `Array.fold : 'a array -> 'b -> ('b -> 'a -> 'b) -> 'b`  
  Left fold in index order.
- `Array.filter : 'a array -> ('a -> bool) -> 'a array`
- `Array.exists : 'a array -> ('a -> bool) -> bool`
- `Array.for_all : 'a array -> ('a -> bool) -> bool`

#### Option-returning search helpers

These use the standard option encoding as variants:

- `` `None ``
- `` `Some(x) ``

Exports:

- `Array.find : 'a array -> ('a -> bool) -> [ \`None | \`Some('a) ]`  
  Returns the first element satisfying the predicate, or `\`None`.
- `Array.find_map : 'a array -> ('a -> [ \`None | \`Some('b) ]) -> [ \`None | \`Some('b) ]`  
  Applies the mapping function left-to-right and returns the first `\`Some(...)` result, or `\`None`.

### 13.2.3 `Option` module
This module uses the convention that option values are represented as variants:

```ocaml
`None
`Some(x)
```

Exports:

```ocaml
Option.none : unit -> [ \None | `Some('a) ]`
Option.some : 'a -> [ \None | `Some('a) ]`
Option.is_none : [ \None | `Some('a) ] -> bool`
Option.is_some : [ \None | `Some('a) ] -> bool`
Option.get_or : [ \None | `Some('a) ] -> 'a -> 'a`
```

Notes:

- This is a convenience module; users can also directly construct and match on `None and `Some(...).

### 13.2.4 `Hashtbl` module (string-keyed)
This is a small builtin hashtable-like abstraction with string keys. It is implemented using existing runtime values (refs + arrays of entries) and is intended for scripting convenience, not high performance.

Exports (conceptual types):

```ocaml
Hashtbl.create : unit -> hashtbl('a)
Hashtbl.set : hashtbl('a) -> string -> 'a -> unit
Hashtbl.get : hashtbl('a) -> string -> [ \None | `Some('a) ]`
Hashtbl.mem : hashtbl('a) -> string -> bool
Hashtbl.remove : hashtbl('a) -> string -> unit
```

Notes:

- The key type is always string.
- Hashtbl.get returns an option-like variant (\None/`Some`).
- Current representation is optimized for simplicity rather than asymptotic performance.

### 13.2.5 `Json` module
The Json module provides a real recursive JSON value type at the ChatML level and conversion to/from JSON text. It is backed by the host-side `Jsonaf` library.


`Json.t` representation
`Json.t` is represented as a recursive variant type equivalent to:

```ocaml
Json.t =
  [ `Null
  | `Bool(bool)
  | `Number(float)
  | `String(string)
  | `Array(Json.t array)
  | `Object({ key : string; value : Json.t } array)
  ]
```

(Internally this is introduced using the builtin-spec recursive type binder; users do not write the binder directly.)

Exports

```ocaml
Json.parse : string -> Json.t
Json.stringify : Json.t -> string
Json.pretty : Json.t -> string
```

Notes:

- `Json.parse` raises a runtime failure on invalid JSON input.
- `Json.stringify` produces a compact JSON representation.
- `Json.pretty` produces a human-readable formatted representation (as provided by Jsonaf).
-  `JSON numbers` are surfaced as float in ChatML. (When parsing, the underlying textual number is converted to float; when stringifying, floats are rendered back to strings.)

### 13.3 Interaction with open and shadowing
open imports module exports into the current scope and rejects any import that would shadow an existing binding.

Because global builtins exist in the initial environment, opening some builtin modules may be rejected due to name collisions. For example:
- `open Array` is rejected by default because Array.length would shadow the global length.

Users can always access module exports through qualified access (Array.length(xs)) without using open.

---

## 14. Surface syntax summary

This is not a full formal grammar, but it summarizes the implemented
surface syntax.

### 14.1 Statements

```ocaml
type t = type_expr
let x = expr
let x : type_expr = expr
let f x y = expr
let f () = expr
let rec f : type_expr = expr
let rec f x = expr and g y = expr
module M = struct stmts end
open M
expr
```

### 14.2 Expressions

```ocaml
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
let x : t = e1 in e2
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

```ocaml
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

```ocaml
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

### 14.5 Type expressions

```ocaml
int
float
bool
string
unit
expr
expr -> int
unit -> string
task array
{ name : string; status : status }
[ `Pending | `Done | `Error(string) ]
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
- ordinary inference variables being acyclic
- recursive types being explicit and checked
- recursive type declarations being contractive
- recursive types remaining monomorphic
- recursive bindings being function-only
- mutation interacting with polymorphism via a value restriction
- explicit separation of integer and float operators
- int-only array indexing
- explicit resolver lowering before evaluation
- runtime validation of frame slot layouts
- no silent `open` shadowing
- equality restrictions for unsupported runtime representations

These are central to the language’s current safety/ergonomics tradeoff.

Recursive builtin types and unification
ChatML supports explicit recursive types internally (Mu / Rec_var) for both user-declared recursive types and some builtin module types (notably Json.t).

Implementation note:

Unification of recursive types uses an alpha-renaming strategy for Mu-vs-Mu unification to avoid non-termination from repeated unfolding.
(This is an internal typechecker detail; surface programs observe only the usual contractiveness and monomorphism rules for recursive types.)

---

## 17. Known intentional limitations

ChatML currently does **not** provide:

- tuple syntax as a general user-facing feature
- type parameters
- mutual recursive type declarations
- module-local type declarations
- general expression ascription syntax
- tuple type syntax as a general user-facing feature
- ref type syntax
- open-row type syntax
- full user-facing algebraic datatype declarations beyond alias-style
  structural `type` declarations
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
complete user-facing type-annotation mechanism.

---

## 19. Recommended style

For the current language, the most ergonomic and robust style is:

- use records for script state
- declare explicit recursive record/variant types when a script's data model
  is genuinely recursive
- annotate recursive helper functions against those declared types
- write small helpers over row-polymorphic state records
- use variants for finite event/state tags
- use modules only for grouping
- keep arithmetic explicit by numeric kind
- prefer `M.name` over aggressive `open` use when readability matters
- push complex host interaction into builtins/runtime services

---

## 20. Reference examples

### 20.1 Record-heavy state helper

```ocaml
let bump_attempts st =
  let t = st.tasks[st.task_index] in
  let t = { t with attempts = t.attempts + 1 } in
  st.tasks[st.task_index] <- t;
  st
```

### 20.2 Variant-driven event handler

```ocaml
let step st ev =
  match ev with
  | `Start -> { st with running = true }
  | `Stop -> { st with running = false }
```

### 20.3 Float logic with explicit dotted operators

```ocaml
let avg x y = (x +. y) /. 2.0
if avg(1.0, 3.0) >=. 2.0 then true else false
```

### 20.4 Simple module namespace

```ocaml
module Flow = struct
  let one = 1
  let inc x = x + 1
end

Flow.inc(Flow.one)
```

### 20.5 Shadow-safe module import

```ocaml
module Math = struct
  let two = 2
end

open Math
print(two)
```

But:

```ocaml
let two = 99
open Math
```

is rejected because `open Math` would shadow `two`.

### 20.6 Explicit recursive type declaration

```ocaml
type expr = [ `Int(int) | `Add(expr, expr) ]

let rec eval : expr -> int =
  fun e ->
    match e with
    | `Int(n) -> n
    | `Add(a, b) -> eval(a) + eval(b)
```

This is the supported way to write recursive structural data in ChatML.

### 20.7 Tiny workflow engine

```ocaml
(* A tiny workflow engine that processes events and mutates tasks in-place. *)

type status = [ `Pending | `Running | `Done | `Error(string) ]
type task =
{ name : string
; attempts : int
; status : status
}
type event = [ `Start | `Tick | `Fail(string) | `Stop ]
type state =
{ tasks : task array
; idx : int
; running : bool
}

(* Witness: forces the status variant row to include all tags we will use. *)
let status_witness : int -> status =
fun n ->
  match n with
  | 0 -> `Pending
  | 1 -> `Running
  | 2 -> `Done
  | _ -> `Error("")

let mk_task : string -> task =
fun name ->
  { name = name; attempts = 0; status = status_witness(0) }

let show_task : task -> string =
fun t ->
  t.name ++ " attempts=" ++ to_string(t.attempts) ++ " status=" ++ variant_tag(t.status)

let set_status : state -> status -> state =
fun st new_status ->
  let i = st.idx in
  let t : task = st.tasks[i] in
  st.tasks[i] <- { t with status = new_status };
  st

let set_running : state -> bool -> state =
fun st running ->
  { st with running = running }

let bump_attempts : state -> state =
fun st ->
  let i = st.idx in
  let t : task = st.tasks[i] in
  st.tasks[i] <- { t with attempts = t.attempts + 1 };
  st

let step : state -> event -> state =
fun st ev ->
  match ev with
  | `Start ->
    if st.idx >= length(st.tasks) then set_running(st, false)
    else set_status(set_running(st, true), status_witness(1))

  | `Tick ->
    if st.running == false then set_running(st, false)
    else
      let st = set_running(st, true) in
      let st = bump_attempts(st) in
      let t : task = st.tasks[st.idx] in
      if t.attempts >= 3 then
        let st = set_status(st, `Done) in
        let st = { st with idx = st.idx + 1 } in
        if st.idx < length(st.tasks) then set_status(set_running(st, true), status_witness(1)) else st
      else st

  | `Fail(msg) ->
    let st = set_status(st, `Error(msg)) in
    set_running(st, false)

  | `Stop ->
    set_running(st, false)

let run : event array -> unit =
fun events ->
let tasks = [ mk_task("fetch"), mk_task("transform"), mk_task("upload") ] in
let st0 = { tasks = tasks; idx = 0; running = false } in

let i = ref(0) in
let st_ref = ref(st0) in

while !i < length(events) do
  let ev = events[!i] in

  st_ref := step(!st_ref, ev);

  let st = !st_ref in
  print(
    "ev=" ++ variant_tag(ev) ++
    " idx=" ++ to_string(st.idx) ++
    " task=" ++
      (if st.idx < length(st.tasks)
        then show_task(st.tasks[st.idx])
        else "<none>")
  );

  i := !i + 1
done

let events =
[ `Start
, `Tick, `Tick, `Tick
, `Tick, `Tick, `Tick
, `Tick, `Fail("network")
, `Stop
]

run(events)
```

### 20.8 Small Expression evaluation

```ocaml
type expr =
  [ `Int(int)
  | `Add(expr, expr)
  | `Sub(expr, expr)
  | `Mul(expr, expr)
  | `Div(expr, expr)
  | `Let(string, expr, expr)
  | `Var(string)
  ]

let rec eval : expr -> int =
  fun e ->
  match e with
  | `Int(n) -> n
  | `Add(a, b) -> eval(a) + eval(b)
  | `Sub(a, b) -> eval(a) - eval(b)
  | `Mul(a, b) -> eval(a) * eval(b)
  | `Div(a, b) ->
      let x = eval(a) in
      let y = eval(b) in
      if y == 0 then fail("division by zero in AST")
      else x / y
  | `Let(name, rhs, body) ->
      (* An environment-less "let" by substitution for demo purposes:
          Let only supports binding `x` here. *)
      if name != "x" then fail("only name \"x\" is supported in this demo")
      else
        let v = eval(rhs) in
        eval(subst_x(body, v))
  | `Var(name) -> fail("free variable in AST: " ++ name)

and subst_x : expr -> int -> expr =
  fun e v ->
  match e with
  | `Int(_) -> e
  | `Var(name) ->
      if name == "x" then `Int(v) else e
  | `Add(a, b) -> `Add(subst_x(a, v), subst_x(b, v))
  | `Sub(a, b) -> `Sub(subst_x(a, v), subst_x(b, v))
  | `Mul(a, b) -> `Mul(subst_x(a, v), subst_x(b, v))
  | `Div(a, b) -> `Div(subst_x(a, v), subst_x(b, v))
  | `Let(name, rhs, body) ->
      if name == "x" then
        (* shadowing: don't substitute into body *)
        `Let(name, subst_x(rhs, v), body)
      else
        `Let(name, subst_x(rhs, v), subst_x(body, v))

let program : expr =
  `Let("x",
        `Add(`Int(10), `Int(5)),
        `Div(`Mul(`Var("x"), `Int(2)), `Sub(`Int(9), `Int(7)))
  )

print("result=" ++ to_string(eval(program)))
```

### 20.9 BFS program

```ocaml
let and_ a b =
  match `Tup(a, b) with
  | `Tup(true, true) -> true
  | _ -> false

module Graph = struct
  (* adjacency matrix: g[u][v] = 1 if edge *)
  let neighbors g u = g[u]

  let bfs_distance g start goal =
    let n = length(g) in

    (* distances initialized to -1 (unvisited) *)
    let dist = [-1, -1, -1, -1, -1, -1] in
    dist[start] <- 0;

    (* simple fixed-size queue of nodes *)
    let q = [0, 0, 0, 0, 0, 0] in
    let head = ref(0) in
    let tail = ref(0) in

    q[0] <- start;
    tail := 1;

    while !head < !tail do
      let u = q[!head] in
      head := !head + 1;

      let du = dist[u] in
      let row = neighbors(g, u) in

      let v = ref(0) in
      while !v < n do
        if (and_(row[!v] == 1, dist[!v] == -1)) then
          dist[!v] <- du + 1;
          q[!tail] <- !v;
          tail := !tail + 1
        else ();
        v := !v + 1
      done
    done;

    dist[goal]
end

let g =
  [ [0,1,1,0,0,0]
  , [1,0,0,1,0,0]
  , [1,0,0,1,1,0]
  , [0,1,1,0,0,1]
  , [0,0,1,0,0,1]
  , [0,0,0,1,1,0]
  ]

print("dist 0->5 = " ++ to_string(Graph.bfs_distance(g, 0, 5)))
```

### 20.10 JSON parse / transform / stringify (builtin Json module)
```ocaml
type json =
  [ `Null
  | `Bool(bool)
  | `String(string)
  | `Number(float)
  | `Array(json array)
  | `Object({ key : string; value : json } array)
  ]

let rec map_numbers : json -> json =
  fun j ->
  match j with
  | `Null -> `Null
  | `Bool(b) -> `Bool(b)
  | `String(s) -> `String(s)

  | `Number(n) ->
    (* Example transform: add 1.0 to every number *)
    `Number(n +. 1.0)

  | `Array(xs) ->
    let ys = array_copy(xs) in
    let i = ref(0) in
    while !i < length(ys) do
      ys[!i] <- map_numbers(ys[!i]);
      i := !i + 1
    done;
    `Array(ys)

  | `Object(entries) ->
    let out = array_copy(entries) in
    let i = ref(0) in
    while !i < length(out) do
      let e = out[!i] in
      (* e : { key : string; value : Json.t } *)
      out[!i] <- { key = e.key; value = map_numbers(e.value) };
      i := !i + 1
    done;
    `Object(out)

let input = "{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4}}"
let j = Json.parse(input)
let j2 = map_numbers(j)

print("in:  " ++ Json.stringify(j))
print("out: " ++ Json.stringify(j2))
print("pretty:\n" ++ Json.pretty(j2))
```
