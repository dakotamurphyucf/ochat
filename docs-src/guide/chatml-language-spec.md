# ChatML language specification

This document is a detailed, implementation-faithful specification of
ChatML as it exists in the current codebase.

It is intended to describe the language that is actually implemented by:

- `lib/chatml/chatml_lexer.mll`
- `lib/chatml/chatml_parser.mly`
- `lib/chatml/chatml_lang.ml`
- `lib/chatml/chatml_typechecker.ml`
- `lib/chatml/chatml_resolver.ml`
- `lib/chatml/chatml_builtin_spec.ml`
- `lib/chatml/chatml_builtin_modules.ml`

This is not a speculative design document. When in doubt, the
implementation and tests are authoritative.

---

## 1. Purpose and design constraints

ChatML is a small, statically typed scripting language intended for:

- orchestration scripts
- event-driven glue logic
- lightweight state-machine code
- embedding inside a host runtime with a small standard library
- prompt/test scenario scripting inside the surrounding OCaml project

ChatML is **not** intended to be:

- a general-purpose application language
- a rich module language
- a type-class / trait / ad-hoc-overloading language
- a high-performance numerical language

The language is intentionally biased toward:

1. **sound static typing**
2. **good ergonomics without annotations**
3. **small surface area**
4. **predictable, conservative semantics**
5. **adequate runtime performance for scripting**

Complexity is deliberately pushed into:

- the host runtime
- a tiny standard library
- builtin host functions

rather than into a large language core.

---

## 2. High-level language model

ChatML is:

- expression-oriented
- lexically scoped
- statically typed with inference
- call-by-value
- ML-flavored in surface syntax
- able to mix immutable and mutable programming styles

The language supports:

- first-class functions
- local and recursive bindings
- structural records
- polymorphic variants
- arrays
- refs
- pattern matching
- simple modules

There are no user-written type annotations in the current language.

---

## 3. Lexical structure

### 3.1 Identifiers

ChatML has three main identifier classes:

- lowercase identifiers, e.g. `x`, `state`, `task_index`
- uppercase identifiers, e.g. `M`, `Flow`, `TaskHelpers`
- polymorphic variant tags, e.g. `` `Some ``, `` `Done ``

Lowercase and uppercase identifiers are tokenized separately by the lexer.
Uppercase identifiers are mainly used for module names, but syntactically
they still enter the AST as variables in expression position.

### 3.2 Literals

Supported literal forms:

- integers: `0`, `1`, `42`
- floats: `1.0`, `3.14`
- booleans: `true`, `false`
- strings: `"hello"`
- unit: `()`

### 3.3 Comments

Comments use OCaml-style block syntax:

```chatml
(* this is a comment *)
```

Nested comments are supported by the lexer implementation.

### 3.4 Strings

Strings support a small set of escapes:

- `\n`
- `\t`
- `\\`
- `\"`

Strings may span multiple lines.

### 3.5 Whitespace

Whitespace is not significant except as a token separator.
ChatML is **not** indentation-sensitive.

---

## 4. Program structure

A ChatML program is a sequence of top-level statements.

Top-level statement forms:

- `let x = expr`
- `let f a b = expr`
- `let f () = expr`
- `let rec f x = expr and g y = expr`
- `module M = struct ... end`
- `open M`
- bare expression statements

Evaluation proceeds top-to-bottom in source order.

The top level is mutable in the sense that each statement contributes new
bindings to the current program environment, but closures capture lexical
bindings stably.

---

## 5. Statement forms

### 5.1 Non-recursive top-level `let`

Examples:

```chatml
let x = 1
let name = "Alice"
let inc n = n + 1
let thunk () = 42
```

Properties:

- the right-hand side is evaluated before the binding is introduced
- the binding is then added to the top-level environment
- later bindings may shadow earlier bindings
- previously created closures still see the lexical binding they captured

### 5.2 Recursive top-level `let rec`

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
- the typechecker rejects non-function recursive bindings

Semantics:

- all recursive names are allocated first
- each RHS is evaluated in an environment where all recursive names are visible
- placeholders are then updated with final values

### 5.3 Modules

Example:

```chatml
module Flow = struct
  let x = 1
  let id y = y
end
```

Modules are intentionally simple namespaces.

Important properties:

- module bodies may reference outer bindings
- only names explicitly defined in the module body are exported
- names imported via `open` inside a module are **not** re-exported
- module values behave like namespace/record-like containers at runtime

### 5.4 `open`

Example:

```chatml
open Flow
```

Semantics:

- imports exported module names into the current scope
- does not create a module alias
- may shadow existing names in the current scope
- is shallow and simple; there is no selective import syntax

---

## 6. Expression forms

### 6.1 Unit

```chatml
()
```

Type: `unit`

### 6.2 Variables

```chatml
x
state
Flow
```

Variables are lexically scoped.

After resolution, local variables become lexical addresses internally.
Top-level/module bindings remain environment lookups, but closure capture is
lexically stable.

### 6.3 Functions

Anonymous functions:

```chatml
fun x -> x
fun x y -> x
fun () -> 42
```

Named function syntax is surface sugar for a `let` binding of a lambda:

```chatml
let add x y = x + y
```

Properties:

- functions are first-class
- closures capture both lexical bindings and local frames
- calls are strict
- tail calls are implemented via a trampoline in the evaluator

### 6.4 Function application

Application uses explicit call syntax:

```chatml
f(x)
f(x, y)
g()
```

There is no whitespace application syntax such as `f x`.

Function position may itself be any expression:

```chatml
(fun x -> x)(1)
choose(true)(1)
```

### 6.5 Local `let ... in`

Examples:

```chatml
let x = 1 in x + 1
let f y = y in f(3)
let rec loop n = ... in loop(10)
```

Properties:

- non-recursive `let` is sequential and lexical
- nested non-recursive lets are internally grouped into block forms by the resolver
- `let rec` inside expressions follows the same recursive-function restrictions as top level

### 6.6 Conditionals

```chatml
if cond then a else b
```

Rules:

- condition must have type `bool`
- both branches must have the same type

### 6.7 Sequencing

```chatml
e1; e2
```

Rules:

- `e1` is evaluated fully first
- its value is discarded
- the overall expression value is the value of `e2`

### 6.8 While loops

```chatml
while cond do body done
```

Rules:

- condition must have type `bool`
- loop result type is `unit`
- loop body may have side effects

### 6.9 Records

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

Important record properties:

- records are structural
- fields are named
- duplicate labels in one literal are rejected
- copy-update is immutable
- copy-update may overwrite existing fields
- copy-update may add fields to closed records
- copy-update may change field types

### 6.10 Arrays

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
- indexing out of bounds is a runtime error
- array update returns `unit`

### 6.11 References

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
- assignment requires a ref and a value of the stored type
- assignment returns `unit`

### 6.12 Variants

Examples:

```chatml
`None
`Some(1)
`Pair(1, "x")
```

Properties:

- ChatML variants are polymorphic variants
- constructors are identified by tag name
- constructors may carry zero, one, or multiple payload values
- internally, multi-value payloads are modeled using tuple types

---

## 7. Operators

Operators are part of the core language.

They are not looked up in the runtime environment and are not overridden by
user code.

### 7.1 Integer arithmetic

- binary `+`
- binary `-`
- binary `*`
- binary `/`
- unary `-`

Type rules:

- operands must be `int`
- result is `int`

Runtime notes:

- division by zero is a runtime error

### 7.2 Float arithmetic

- binary `+.`
- binary `-.`
- binary `*.`
- binary `/.`
- unary `-.`

Type rules:

- operands must be `float`
- result is `float`

Runtime notes:

- division by zero is a runtime error when the divisor is `0.0`

### 7.3 String concatenation

- binary `++`

Type rules:

- operands must be `string`
- result is `string`

### 7.4 Integer comparisons

- `<`
- `>`
- `<=`
- `>=`

Type rules:

- both operands must be `int`
- result is `bool`

### 7.5 Float comparisons

- `<.`
- `>.`
- `<=.`
- `>=.`

Type rules:

- both operands must be `float`
- result is `bool`

### 7.6 Equality and inequality

- `==`
- `!=`

Type rules:

- both operands must have the same type
- result is `bool`

Runtime semantics:

- `int`, `float`, `bool`, `string`, `unit`: value equality
- variants: structural equality
- records: structural equality
- arrays: identity equality
- refs: identity equality
- closures: identity equality
- modules: identity equality
- builtins: identity equality

### 7.7 Precedence and associativity

The parser currently uses precedence tiers roughly as follows:

1. comparisons and equality
2. additive operators
3. multiplicative operators
4. dereference precedence handling

Concretely:

- `+`, `-`, `++`, `+.`, `-.` share a precedence level
- `*`, `/`, `*.`, `/.` share a tighter precedence level
- comparison operators are looser than arithmetic

Use parentheses whenever readability matters.

---

## 8. Pattern matching

### 8.1 Supported patterns

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

### 8.2 Runtime matching semantics

Match arms are tested in source order.

The first arm whose pattern matches the scrutinee is chosen.

If no arm matches at runtime, execution raises a runtime error.

### 8.3 Pattern binding order

Pattern variables are collected in deterministic left-to-right order.
This matters for resolver slot layout but not for user-visible semantics.

### 8.4 Record patterns

Closed record pattern:

```chatml
{ name = n }
```

This requires the record to have exactly the specified fields.

Open record pattern:

```chatml
{ name = n; _ }
```

This requires the record to have at least the specified fields.

### 8.5 Static match checks

The typechecker performs:

- duplicate binder checks
- duplicate simple-arm checks
- some redundancy detection
- conservative exhaustiveness detection

Exhaustiveness is strongest for:

- booleans
- sufficiently closed variant matches

It is intentionally conservative for:

- ints
- floats
- strings
- records
- open variants

For a more specialized discussion, see:

- `docs-src/guide/chatml-match-semantics.md`

### 8.6 Variant closure by match

A key implementation behavior is that sufficiently informative variant
matches can narrow and effectively close the set of constructors accepted
by a function.

That means code like:

```chatml
let f v =
  match v with
  | `Some(x) -> x
```

may infer a parameter type that only accepts compatible variant inputs,
rather than an arbitrarily open variant row.

This is intentional.

---

## 9. Type system

ChatML uses Hindley-Milner style inference with extensions for:

- mutation safety via the value restriction
- row-polymorphic records
- row-polymorphic variants

Users do not write type annotations.

### 9.1 Primitive types

- `unit`
- `int`
- `float`
- `bool`
- `string`

### 9.2 Composite types

- function types
- array types
- ref types
- record types
- variant types
- tuple types (internal typing artifact for multi-argument variant payloads)

### 9.3 Let-polymorphism

Non-expansive bindings may be generalized.

Typical example:

```chatml
let id x = x
id(1)
id("s")
```

### 9.4 Value restriction

Expansive bindings are not generalized.

This is necessary for soundness with:

- refs
- arrays
- mutable aliasing

### 9.5 Records and row polymorphism

Record-using helpers are typically inferred with open-row behavior.

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

- lambda parameters whose type is discovered to be record-shaped are
  reopened to open rows
- this reopening is intentionally tuned toward record-heavy scripting and
  state-machine helpers
- variant rows are **not** reopened by that heuristic

### 9.6 Variants and row polymorphism

Variant constructors are typed using row-based variant information.

Examples:

```chatml
`None
`Some(1)
`Pair(1, "x")
```

Variant row information interacts with pattern matching and may become
narrower after informative matches.

### 9.7 Recursive bindings

Recursive bindings must be functions.

This avoids unsound and difficult recursive value inference cases.

### 9.8 Equality typing

Equality is polymorphic in the sense that both sides may be any same-typed
value, but there is no ad-hoc equality over mismatched operand types.

So:

```chatml
1 == 1
```

is accepted, but:

```chatml
1 == "x"
```

is rejected.

---

## 10. Runtime model

### 10.1 Values

The evaluator runtime supports these value forms:

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

### 10.2 Closures

Closures capture:

- lexical environment bindings
- local frame stack
- parameter slot layout

Closures capture lexical environments stably, so later rebinding does
not retroactively change what an earlier closure sees.

### 10.3 Frames and lexical resolution

The resolver rewrites local variables into lexical addresses carrying:

- frame depth
- slot index
- runtime slot descriptor

This allows:

- O(1) local reads
- one-frame block allocation for grouped lets
- reduced name-based local lookup during evaluation

### 10.4 Tail calls

The evaluator uses a trampoline for closure tail calls, reducing OCaml
stack growth in tail-recursive function execution.

### 10.5 Runtime errors

Possible runtime failures include:

- division by zero
- array index out of bounds
- dereference of non-ref
- assignment to non-ref
- calling a non-function
- function arity mismatch
- non-exhaustive runtime match
- malformed field access on non-record/non-module

Ill-typed programs are normally rejected before evaluation by the standard
pipeline.

---

## 11. Modules

Modules are simple namespace containers.

They are intentionally much less powerful than OCaml modules.

### 11.1 What modules are for

Modules are for:

- grouping helper functions
- reducing naming clutter
- structuring scripts

Modules are not for:

- abstraction boundaries with signatures
- functors
- generative module behavior
- advanced namespace engineering

### 11.2 Export behavior

Only names explicitly defined in a module are exported.

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

### 11.3 `open` behavior

`open M` copies module exports into the current environment for subsequent
lookup. It does not cause opened names to be re-exported automatically from
the surrounding module.

---

## 12. Standard library

The current builtin prelude is intentionally tiny:

- `print : 'a -> unit`
- `to_string : 'a -> string`
- `length : 'a array -> int`

Notes:

- `print` prints a stable human-readable representation of runtime values
- `to_string` returns that representation as a string
- `length` works on arrays only

Notably absent:

- numeric conversion builtins such as `num2str`
- operator builtins for arithmetic or comparison

Those operations are language primitives instead.

---

## 13. Surface syntax summary

This is not a full formal grammar, but it summarizes the supported surface
forms.

### 13.1 Statements

```chatml
let x = expr
let f x y = expr
let f () = expr
let rec f x = expr and g y = expr
module M = struct stmts end
open M
expr
```

### 13.2 Expressions

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

### 13.3 Operators

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

### 13.4 Patterns

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

## 14. Soundness-related notes

The current implementation intentionally enforces or relies on the
following:

- lexical closure capture is stable
- recursive bindings are function-only
- mutation interacts with polymorphism through a value restriction
- integer and float operators are separate and explicit
- array indexing is int-only
- modules export only explicit definitions
- row-polymorphic ergonomics are stronger for records than for variants

These are important to the language’s current safety/ergonomics tradeoff.

---

## 15. Known intentional limitations

ChatML currently does **not** provide:

- user-written type annotations
- tuple syntax as a user-facing general feature
- algebraic data type declarations
- parametric modules or signatures
- selective imports
- layout-sensitive syntax
- Unicode identifiers
- full ML-style match usefulness analysis
- ad-hoc overloaded numeric operators

The language is intentionally conservative and small.

---

## 16. Recommended style

For the current language, the most ergonomic and robust style is:

- use records for script state
- use small helpers over row-polymorphic state records
- use variants for finite event/state tags
- use modules only for grouping
- keep arithmetic explicit by numeric kind
- prefer `M.name` over heavy use of `open` when readability matters
- push complex host interactions into builtins/runtime services

---

## 17. Reference examples

### 17.1 Record-heavy state helper

```chatml
let bump_attempts st =
  let t = st.tasks[st.task_index] in
  let t = { t with attempts = t.attempts + 1 } in
  st.tasks[st.task_index] <- t;
  st
```

### 17.2 Variant-driven event handler

```chatml
let step st ev =
  match ev with
  | `Start -> { st with running = true }
  | `Stop -> { st with running = false }
```

### 17.3 Float logic with explicit dotted operators

```chatml
let avg x y = (x +. y) /. 2.0
if avg(1.0, 3.0) >=. 2.0 then true else false
```

### 17.4 Simple module namespace

```chatml
module Flow = struct
  let one = 1
  let inc x = x + 1
end

Flow.inc(Flow.one)
```

