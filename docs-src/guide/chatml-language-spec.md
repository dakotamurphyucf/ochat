# ChatML language specification

For current native/daemon hosting, see [host modes](../agent-server/concepts.md) and
[agent-host orchestration](../agent-server/chatml-orchestration.md). Daemon work belongs to
the session actor, not a connected UI. The existing language/tool APIs remain
shared; file-backed session/controller descriptions should be read in that host
context. Instruction helper compatibility names emit developer-role messages.

This document is the implementation-faithful specification of ChatML as it
exists in the current codebase.

For a shorter starting point with compiler-checked examples, see
[ChatML differences from OCaml](chatml-ocaml-differences.md).

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
- `lib/chatml/chatml_builtin_surface.ml`
- `lib/chatml/chatml_value_codec.ml`
- `lib/chatml/chatml_moderator_runtime.ml`

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
- nominal algebraic datatypes or generalized algebraic datatypes (GADTs)
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
- named type declarations at the top level and inside modules
- checked binding annotations
- structural records
- immutable heterogeneous tuples
- polymorphic variants
- arrays
- refs
- pattern matching
- modules with optional checked interfaces

ChatML now has a deliberately small user-facing type surface:

- top-level and module-local `type` declarations,
- binding annotations on `let`, `let rec`, and `let ... in`,
- explicit recursive types introduced through those declarations.

Aliases support scoped parameters and regular contractive recursion. The type
language remains small: there are no mutual recursive type declarations or
general expression-level ascriptions.

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
- `ETuple`

Resolved expressions include:

- `REVarGlobal`
- `REVarLoc`
- `RELambda`
- `RELetBlock`
- `RELetRec`
- `REMatch`
- `RETuple`

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

The stored `source_text` supports legacy single-file diagnostic formatting.
Captured compilation additionally retains each unit's source identity and exact
bytes so a deferred failure can select its defining source.

### 3.4 Type checking

`lib/chatml/chatml_typechecker.ml` implements:

- Hindley–Milner inference
- value restriction
- row-polymorphic records
- row-polymorphic variants
- tuple arity, element typing, and irrefutable destructuring
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

- an environment containing mutable binding cells and optional host execution
  control for top-level/module/global bindings
- a stack of frames for local lexical bindings

Copied closure/module environments retain the same host control. The generic
evaluator has no resource budget by default; bounded agent execution installs
host-selected checks. Resource policy and tool authority are separate.

`Chatml_execution` supplies bounded execution for hosts that need it. Its checks
cover pure evaluation, task interpretation, nested execution and estimated
allocations. Preflight checks reserve space for array copies, maps and filters,
JSON object edits, and hash-table updates before those operations run. JSON parsing and validation scan nesting
before entering the native parser, with cancellation checkpoints during that
scan; quoted brackets and escaped quotes do not count as nesting. Rendering with
`to_string` or `print` checks deferred task payloads as well as ordinary values,
and pretty JSON reserves additional space for indentation.

These are conservative resource estimates, not an OCaml heap sandbox or hard
preemption of a native builtin. An estimate can exceed the actual output size.
Trusted root execution may select `Unrestricted`; nested execution cannot remove
an ancestor's limits. The language itself does not impose these host budgets.

One-off and standalone handlers share one execution scope across input/context
projection, evaluation and outcome conversion. Stateful tool invocations open the
moderator's owned scope before projecting their context. Controlled JSON imports
check the projected variant/array/object-entry structure before conversion;
exports reserve an escaping-aware conversion/serialization estimate. Host error
adapters preserve budget exhaustion as a control failure. These scopes do not
grant tools or change invocation ownership.

Ordinary lifecycle events and tool observations use the same owned-scope rule.
Event/history projection and state/queue snapshot preparation are budgeted before
the transaction installer runs. A projection or snapshot budget failure leaves
the prior state intact. A moderator that observes an intentionally large request
needs sufficient context limits even when the target tool rejects that request
under stricter input limits.

Host runtime debug logging uses separate, lazy previews capped by bytes, visited
nodes and depth. Disabled logging does not format values. Enabled logging can
truncate large or cyclic values without consuming script fuel after a state
commit. Normal language `to_string` output is unchanged by these debug previews.

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

Primitive literal forms:

- integers: `0`, `1`, `42`
- floats: `1.0`, `3.14`
- booleans: `true`, `false`
- strings: `"hello"`
- unit: `()`

Composite values include tuples `(|1, "one"|)`, records `{ x = 1 }`,
arrays `[1, 2]`, and variants such as `` `Some(1) ``. Integer literals are
decimal; float literals require digits on both sides of the decimal point.
There are no exponent, hexadecimal, or numeric-separator literal forms.

### 4.3 Comments

Comments use OCaml-style block syntax:

```ocaml
(* this is a comment *)
```

Nested comments are supported by the lexer.

### 4.4 Strings

Strings recognize these escape sequences:

- `\n`
- `\t`
- `\\`
- `\"`

Strings may span multiple lines; LF, CR, and CRLF line breaks normalize to `\n`.
Other backslash sequences are preserved literally, rather than interpreted as
additional escapes. String lengths, indexes, and slices count bytes.

### 4.5 Whitespace

Whitespace is not significant except as a token separator.

ChatML is **not** indentation-sensitive.

---

## 5. Program structure

A ChatML program is a sequence of top-level statements.

Top-level statement forms:

- `type t = type_expr` and parameterized aliases such as `type 'a box = { value : 'a }`
- `let x = expr`
- tuple destructuring such as `let (|x, y|) = expr`
- `let x : type_expr = expr`
- `let f a b = expr`
- `let f () = expr`
- `let rec f : type_expr = expr and g : type_expr = expr`
- `let rec f x = expr and g y = expr`
- `module M = struct ... end`
- `module M : sig ... end = struct ... end`
- `open M`
- a bare expression statement

Evaluation proceeds top-to-bottom in source order.

Optional `;;` separates statements at the top level and inside module bodies.
It can disambiguate a type declaration followed by a qualified call, for example
`type t = int ;; Graph.run()`. Ordinary declaration sequences remain valid without
separators. A single `;` retains expression-sequencing behavior.

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
let (|count, label|) = (|7, "seven"|)
let (|first, _|) : int * bool = (|1, true|)
```

Properties:

- the RHS is evaluated before the new binding is introduced
- the new binding is then added to the current environment
- later top-level lets may shadow earlier ones
- already-created closures still observe the lexical binding they captured
- binders may be names, `_`, or nested tuples of those binders; literals,
  record patterns, and variant patterns are reserved for `match`
- a destructured RHS runs once; its named leaves enter scope together, and
  duplicate names within one binding are rejected

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

### 6.3 Type declarations

Examples:

```ocaml
type expr = [ `Int(int) | `Add(expr, expr) ]
type 'a tree = [ `Leaf('a) | `Branch('a tree, 'a tree) ]
type ('a, 'e) outcome = [ `Ok('a) | `Error('e) ]
type status = [ `Pending | `Done ]
type task = { name : string; attempts : int; status : status }
```

Properties:

- type declarations are compile-time only
- they introduce names into a separate type namespace
- they are allowed at the top level and inside module bodies
- later statements may refer to earlier type declarations
- module bodies may refer to earlier top-level type declarations
- type declarations within a module are exported unless an inline signature hides them
- qualified names such as `Data.t` and `Outer.Inner.t` select public type aliases
- `open` brings public type and nested module names into the lexical type namespace
- type declarations are alias-like, not nominal runtime entities

Parameterized aliases are applied using postfix syntax: `int tree` or
`(int, string) outcome`. The comma-separated form supplies multiple arguments to
an alias; it is not a general tuple type. Parameter names are scoped to their
declaration. Expansion uses the declaration's lexical type namespace, with no
runtime type objects. Duplicate/free parameters and wrong arities are errors.
A type name cannot be declared twice in the same scope; an inner module may
shadow an outer alias without changing previously defined aliases.
Zero-parameter names such as `type task = ...` remain legal alongside the unary
builtin `task` constructor; builtin constructors cannot be redefined with parameters.

Recursive applications must preserve the original parameters, in order.
Nonregular applications such as `('a array) tree` inside `'a tree` are rejected.
The typechecker also validates **contractiveness**:

- accepted:
  - `type expr = [ \`Int(int) | \`Add(expr, expr) ]`
  - `type node = { value : int; next : node }`
- rejected:
  - `type bad = bad`

Current intentional limitations:

- no `type ... and ...`
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
- modules have a distinct static module kind and are `VModule` values at runtime
- module bodies may contain transparent type declarations
- module declarations are non-recursive; the new module name is available only
  after its body succeeds and is not in scope within that body, even in functions

Optional inline signatures select a module's public interface using
`module M : sig ... end = struct ... end`. The
[checked module examples](chatml-inference.md#modules-and-records-have-different-representations)
demonstrate private state, transparent types and polymorphic public members.

Signature items are transparent `type` aliases, `val` specifications and nested
`module Name : sig ... end` specifications. The implementation must satisfy each
claim. Type parameters in a `val` specification are universally quantified;
equality-using generic functions specify the obligation with, for example,
`val same : fun('a, 'a) -> bool where equality('a)`. There are no abstract type
specifications, module type declarations or functors.

Universally quantified value specifications require fixed function arity after
alias expansion. If substituting a parameter could erase a legacy arrow's unit
argument or flatten its function-valued result, use an explicit `fun(...) -> ...`
type. This restriction applies to the universal value claim; ordinary aliases,
concrete applications and inferred functions retain their existing behavior.

A signed module exposes only specified values, types and nested modules.
The same checked selection limits runtime value fields; omitted implementation
bindings remain available privately to exported closures. Type aliases remain
transparent and have no runtime fields. Without a signature, the public interface
is inferred from the module's local declarations.

The declaration-only signature syntax also has a companion `.chatmli` parser.
The prepared compiler checks explicitly captured implementations and optional
companions, and lowers configured value reads to canonical module members.
The linked runtime initializes the entry's value dependency closure once in
dependency-first order. Type references and configured `open` alone stay inert;
actual value uses select initialization. Aliases of one canonical module share
its public runtime instance. Standalone runs get fresh state; a persistent
session retains its initialized graph. See
[evaluation order](chatml-evaluation.md#configured-module-initialization) and
[prepared execution](chatml-execution-limits.md#prepared-execution-validates-before-initialization).

The legacy text compiler does not discover companion files or configured sources.
Explicit host project preparation captures Local directories and full-pin Git
catalogs under admitted host authority, then supplies immutable inputs to these
prepared/linked APIs. See the [working shared-counter project](../examples/chatml/projects/shared-counter/README.md).
The graph compiler itself performs no discovery or native reads. In ChatMD,
each script can select its own `project` and `environment`; the host prepares
and captures that selection for the script. Existing sessions resume their
captured code, while an explicit rebuild adopts file edits. See
[configured ChatML projects](chatmd-authoring-definitions.md#attach-chatml-moderation).

### 6.5 `open`

Example:

```ocaml
open Flow
```

Semantics:

- imports all exported names from the module into the current scope
- includes public type and nested module names in the static namespace
- preserves polymorphic value schemes and shared mutable export constraints
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
fun (|x, y|) -> x + y
fun _ -> 42
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

Use explicit function types when argument count matters:

```ocaml
let finish_action : fun() -> string = fun () -> "done"
let accept_unit : fun(unit) -> string = fun value -> "done"
let add_pair : fun(int * int) -> int = fun (|x, y|) -> x + y
```

These are called as `finish_action()`, `accept_unit(())`, and
`add_pair((|2, 3|))`. A tuple parameter consumes one argument; `_` consumes one
argument without binding a name. Duplicate names anywhere within a parameter
list are rejected. The legacy annotation `unit -> string` also describes a
zero-argument function; `fun(unit) -> string` explicitly preserves one unit
argument.

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
`f(a, b)` supplies two arguments, while `f((|a, b|))` supplies one tuple argument.
Parentheses alone do not construct tuples: `(a, b)` is not a tuple expression.

### 7.5 Local `let ... in`

Examples:

```ocaml
let x = 1 in x + 1
let x : int = 1 in x + 1
let (|x, y|) = (|2, 3|) in x + y
let f y = y in f(3)
let rec loop n = ... in loop(10)
let* x = task1 in task2
let+ x = task1 in value_expr
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
- `let*` and `let+` are task-composition forms that desugar to `Task.bind`
  and `Task.map`
- task-let binders currently accept a single name or `()` only; to destructure
  a task result, use `let* pair = task in let (|x, y|) = pair in ...`

### 7.6 Conditionals

```ocaml
if cond then a else b
```

Rules:

- the condition must have type `bool`
- branch results must unify to one compatible type
- record results must have matching field sets and compatible field types;
  there is no implicit conversion to a smaller common record

For example, this is rejected even though both records contain `x`:

```ocaml
if true then { x = 1 } else { x = 2; y = 3 }
```

A helper that sometimes updates a field and sometimes returns its input imposes
that field's type on the input:

```ocaml
let maybe_set_running b st =
  if b then st else { st with running = true }
```

Here `st` must already contain `running : bool`, and any extra input fields are
retained. Unconditional copy-update can still add fields or change their types;
see [record result compatibility](#1051-control-flow-joins-for-records).

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
- multi-value payloads retain a separate internal constructor-argument list

---

### 7.13 Tuples

Tuples are immutable, ordered groups of at least two values, which may have
different types. Use `(|` and `|)` around comma-separated elements:

<!-- ochat-authoring-example: {"id":"reference.tuple-bindings","surface":"one_off_v1","result":"seven:10"} -->
```ocaml
let pair : int * string = (|7, "seven"|)
let (|count, label|) = pair
let add (|a, b|) step = a + b + step
let main input =
  Task.pure(`String(label ++ ":" ++ to_string(add((|count, 2|), 1))))
```

The example returns the JSON string `"seven:10"`. Tuple elements evaluate
left-to-right. Extract them with `let` destructuring, function parameters, or
`match`; tuples do not use array indexing or named field projection.

`(|1, "one", true|)` is a triple; `(|(|1, "one"|), true|)` is a pair containing
a pair. These shapes are distinct. Empty and one-element tuples do not exist;
`()` remains unit. A tuple cannot replace its elements, but arrays and refs
stored inside it retain their normal mutability and are shared, not copied.

Variant argument lists remain distinct: `` `Pair(a, b) `` has two payload
arguments, whereas `` `Pair((|a, b|)) `` has one tuple payload. See
[tuple patterns](#97-tuple-patterns) and [product types](#102-composite-types).

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
- modules
- tasks

Examples:

```ocaml
1 == 1         (* ok *)
"a" != "b"     (* ok *)
[1, 2] == [1]  (* type error *)
```

Implementation note:

- runtime equality for tuples/records/variants is structural
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
- tuple patterns: `(|p1, p2, ...|)`, including nested tuples
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

- all arm results must unify to one compatible type
- record-valued arm results require matching shapes and compatible field types
- tuple-valued arm results require the same arity and compatible element types

Use explicit variant tags when different arms intentionally return different
shapes. Ordinary record-taking helpers still accept extra fields.

Exhaustiveness is strongest for:

- booleans
- unit
- sufficiently closed variant matches

It is conservative for:

- ints
- floats
- strings
- records
- tuples with refutable element patterns
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

### 9.7 Tuple patterns

Tuple patterns match exact arity and order. Each element may use a literal,
variant, record, wildcard, variable, or another tuple pattern:

<!-- ochat-authoring-example: {"id":"reference.tuple-match","surface":"one_off_v1","result":"seven"} -->
```ocaml
let describe pair =
  match pair with
  | (|0, _|) -> "zero"
  | (|_, name|) -> name
let main input = Task.pure(`String(describe((|7, "seven"|))))
```

Coverage is conservative: include a total tuple pattern, as in the second arm,
or a wildcard/binder fallback. The checker does not combine separate refutable
tuple arms into a proof covering every combination of their elements.

Unlike match patterns, tuple binders in `let` and function parameters contain
only names, `_`, and nested tuples. They cannot fail a literal or tag test.

---

## 10. Type system

ChatML uses Hindley–Milner style inference with extensions for:

- explicit recursive types
- mutation safety via the value restriction
- row-polymorphic records
- row-polymorphic variants

ChatML has a small explicit type surface for:

- named type declarations at the top level and inside modules
- checked binding annotations

Ordinary HM inference variables are acyclic. Recursive types are not
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
- task types
- module interfaces, distinct from record types

Tuple values and match patterns use `(|first, second, ...|)`, with at least two
elements. Products are ordered and may be heterogeneous. Flat triples and nested
pairs have different shapes; `()` remains unit. Product annotations use `int *
string`; postfix constructors bind tighter than products, which bind tighter than
arrows. `(int * string) * bool` is a nested pair, while `int * string * bool` is a
flat triple. `f(a, b)` passes two arguments; `f((|a, b|))` passes one product.
Variant payload argument lists remain separate: `` `Pair(a, b) `` has two payload
arguments, while `` `Pair((|a, b|)) `` has one product payload.

```ocaml
let pair : int * string = (|7, "seven"|)
let label pair = match pair with | (|count, name|) -> name ++ to_string(count)
```

Tuple match elements support existing literal, variant, record, wildcard, variable
and nested tuple patterns. Coverage is conservative: a total tuple pattern or
`_` arm is required; separate refutable product arms are not combined into a
Cartesian completeness proof. Let and parameter binders allow names, `_`, and
nested tuples of those binders. Literals, records and variants remain match-only
patterns. A let RHS runs once in the previous scope; all named leaves enter
scope together. The value restriction applies to the whole RHS before leaf
schemes are distributed, preserving shared weak variables. One tuple parameter
consumes one argument even when it binds several names; `_` also consumes an
argument while binding no name. Recursive declaration heads remain names.
For example, `let (|n, _|) = (|7, true|)` binds only `n`, and
`let add (|a, b|) step = a + b + step` is called as `add((|2, 3|), 4)`.
The immutable tuple container shares mutable child values;
equality is supported only when all component types support equality.

### 10.2.1 User-facing type syntax

The current user-facing type-expression syntax supports:

- primitive names:
  - `int`
  - `float`
  - `bool`
  - `string`
  - `unit`
- previously declared type names and qualified aliases such as `Catalog.id`
- scoped alias parameters and applications such as `int box` or `(int, string) outcome`
- product types such as `int * string` or `(int * string) * bool`
- exact nary function types:
  - `fun() -> int` (zero arguments)
  - `fun(unit) -> int` (one unit argument)
  - `fun(int, bool) -> string` (two arguments)
  - `fun(int) -> fun(int) -> int` (one argument, returning a function)
- legacy flattened function types:
  - `expr -> int`
  - `state -> event -> state`
  - `unit -> string`
- postfix unary type constructors:
  - `task array`
  - `state task`
  - `state task array`
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

- no ref type syntax
- no open-row type syntax
- no mutual recursive type declarations

The parser accepts postfix constructors and multi-argument alias applications.
The checker enforces each declaration's arity. Alias parameters such as `'a` are
scoped to their declaration; `val` specifications in signatures instead quantify
their parameters universally. Ordinary `let` annotations cannot introduce free
type parameters. Builtin unary constructors are:

- `array`
- `task`

### 10.2.2 Contractiveness

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

### 10.4.1 Generalization with recursive data

Non-expansive functions involving regular recursive types follow the same
generalization and value restriction as other functions. A pure JSON helper's
independent callback or result variable can be instantiated differently at
separate calls. Captured weak variables remain shared, and expansive bindings
such as refs and arrays are not generalized.

Recursive calls within a simultaneous function definition use monomorphic
placeholders. This does not permit polymorphic recursion.

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

Field projection and open record patterns infer requirements for the fields
they use. Extra fields are accepted and retain their types. Exact record
annotations and closed patterns remain exact; function inference does not
reopen them into wider shapes.

### 10.5.1 Control-flow joins for records

Record copy-update can add a field or replace a field with a new type while
preserving unmentioned fields. The original record remains unchanged:

<!-- ochat-authoring-example: {"id":"reference.record-copy","surface":"one_off_v1","result":true} -->
```ocaml
let enable cfg = { cfg with enabled = true }
let original = { enabled = 1; name = "worker" }
let updated = enable(original)
let main input =
  Task.pure(`Bool(if original.enabled == 1 then updated.enabled else false))
```

An explicit annotation on `updated` must agree with the resulting shape and
field types. It cannot require `enabled : int` after this update.

Control-flow joins unify full result types; they do not discard fields to find
a smaller shared shape. Both arms must agree on field sets and field types:

<!-- ochat-authoring-example: {"id":"reference.branch-shape-rejected","surface":"one_off_v1","stage":"typecheck","contains":"Row does not contain label","span":true} -->
```ocaml
let choose flag = if flag then { x = 1 } else { x = 2; y = 3 }
let main input = Task.pure(`Null)
```

This is rejected. Construct a common record shape on both paths, or use variants
such as `` `Plain({x = 1}) `` and `` `Detailed({x = 2; y = 3}) `` to express
intentional alternatives. Conditional updates of an unknown input record must
also preserve a compatible result: the `maybe_set_running` helper in
[conditionals](#76-conditionals) requires `running : bool` in its input.

For the narrow case where an exact result annotation constrains a generic
shape-changing helper, an explicit input shape may be necessary. For example,
annotating `{r with enabled = true}` as exactly `{enabled : bool}` does not make
one inferred helper accept both an absent `enabled` field and every possible
previous type of that field. Spell out the intended input type when it matters.

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
Users still do not write builtin-only forms such as ref types, open-row forms, or explicit recursive binders directly; they appear only through host-provided builtin schemes.

### 10.9 Module typing

Modules have a distinct static module kind containing their public value members,
and are represented as `VModule` at runtime. Ordinary record annotations, record
patterns and copy-updates do not accept module values; `open` requires a module.
Field projection works on both kinds, and `record_keys` explicitly supports both.

Module-local type aliases and nested type namespaces are static exports, not
runtime fields. Qualified type paths and `open` use that static namespace. Inline
signatures check transparent type/value/nested-module claims and select one public
interface for both checking and runtime visibility. Opening that interface does
not specialize polymorphic members or duplicate shared mutable state.

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
- tuples
- arrays
- refs
- closures
- modules
- unit
- builtins
- tasks

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
- exposing transparent local types and checked public member boundaries

Modules are not for:

- functors
- module type declarations or abstract signature types
- generative module behavior
- abstraction-heavy namespace engineering

Module declarations are non-recursive: a new module binding is available only
after its body finishes. The module's own name is not introduced inside that
body, even in a function that will be called later. Refer to earlier local
bindings directly; recursive functions use `let rec`. Enclosing bindings retain
their lexical meaning. A failing initializer does not install the new module;
effects already performed are not rolled back.

### 12.2 Export behavior

An unsigned module exports only its locally defined values, types and nested
modules. An inline signature selects a subset and checks its declared types;
unselected runtime members are hidden while exported closures retain private
bindings. Outer bindings and names introduced only by `open` are not re-exported.

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
value lookup and brings public type/nested-module names into the static namespace.
It preserves the exported polymorphic schemes and mutable value identity.

It does not:

- re-export opened names automatically
- support selective imports
- allow silent shadowing

The language now rejects `open` if it would overwrite an existing binding in
the current scope.

---

## 13. Standard library and builtin surfaces

ChatML now uses a composable builtin-surface model rather than a single
hard-coded builtin universe.

A builtin surface may contribute:

- global builtin functions,
- builtin modules installed as `VModule` values (with a distinct module type),
- builtin type aliases injected into the initial type environment.

The generic builtin-surface API exposes these standard assembled surfaces:

- `core_surface`
- `moderator_surface`
- `ui_moderator_surface` (an explicit interactive extension)

Ochat also uses versioned authoring contracts: `one_off_v1`, `tool_v1`,
`developer_tool_v1`, `moderator_v1`, and `delegated_moderator_v1`. These select different entrypoints
and capability modules; they are not synonyms for the generic surfaces. See
section 13.2.2.2.

The signatures below are explanatory reference notation, not source declarations
to paste into a program. An arrow chain lists the arguments of one fixed-arity
call: use `Task.bind(task, callback)`, not partial application. Nullary helpers
such as `Option.none` are called with `()`. Names such as `hashtbl('a)` below
abbreviate a structural representation; they are not installed type aliases.

Arithmetic, string concatenation, comparison, and equality operators remain
language primitives rather than runtime-installed builtins.

### 13.1 Global builtins
Installed global builtins in `core_surface`:

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
hash_md5 : string -> string
```

Notes:

- print renders a stable human-readable representation of runtime values.
- to_string returns that representation.
- length works on arrays only.
- array_copy is a shallow copy of the array container.
- record_keys returns names in sorted order and explicitly supports ordinary records and module values; for a signed module it returns only public runtime member names, not type names.
- variant_tag returns only the constructor/tag name, not the payload.
- `swap_ref(r, v)` stores `v` into `r` and returns the old contents.
- fail raises an immediate runtime failure and is polymorphic in its result position.
- hash_md5 returns the lowercase hexadecimal MD5 digest of the string bytes. It is a legacy checksum helper, not a secure signature or collision-resistant identity.

`print` is present in the generic core surface. It is deliberately omitted from
one-off, standalone-tool, delegated-moderator and shell-hook authoring surfaces;
use the selected host's diagnostic operations instead.

### 13.2 Builtin surfaces

#### 13.2.1 `core_surface`

`core_surface` currently provides:

- the global builtins listed above,
- builtin modules:
  - `Task`
  - `Outcome`
  - `String`
  - `Array`
  - `Json`
  - `Option`
  - `Hashtbl`
- builtin type aliases:
  - `json`

#### 13.2.2 `moderator_surface`

`moderator_surface` extends `core_surface` with moderator-oriented modules
and structural type aliases.

Additional builtin modules:

- `Log`
- `Item`
- `Tool_call`
- `Context`
- `Turn`
- `Tool`
- `Model`
- `Process`
- `Schedule`
- `Runtime`

Additional builtin type aliases:

- `item`
- `tool_desc`
- `tool_call`
- `tool_result`
- `context`

Each builtin module is a `VModule` value at runtime and is typed as a
module of its exports, distinct from an ordinary record, by the typechecker.

#### 13.2.2.1 `ui_moderator_surface`

`ui_moderator_surface` extends `moderator_surface` with UI-only capability
modules:

- `Ui`
- `Approval`

This surface is intended for interactive hosts that support host-local
notifications and live approval pause/resume behavior. The default
`moderator_surface` remains non-UI.

#### 13.2.2.2 Versioned Ochat authoring surfaces

| Contract | Entrypoint and available host facilities |
|---|---|
| `one_off_v1` | `main(input)` returns `json task`. Core computation without `print`, plus `Log`, selected `Tool.call`, and `Job`. |
| `tool_v1` | `run(context, input)` returns `tool_outcome task`; adds the typed invocation context, outcomes and capability aliases. |
| `developer_tool_v1` | The same `run(context, input)` ABI and tool facilities as `tool_v1`, plus `Model.spawn` and `Model.spawn_text` for a developer host with an admitted provider selection. Static availability alone grants no execution authority. |
| `moderator_v1` | `initial_state` and `on_event(context, state, event)` share one state type; adds typed invocation, run, job, subscription, notification, ingress and scheduling operations to the ordinary moderator surface. |
| `delegated_moderator_v1` | The same moderator entrypoint contract, with inherited tool authority; excludes `print`, `Model` and `Process`. |

These contracts statically check their required entrypoints. Compilation does not
install a host service or grant execution authority. Each tool call still needs
actual selected capabilities, schema validation and current invocation admission.
On versioned surfaces `Tool.spawn` aliases transactional `Job.start_tool`; on the
generic moderator surface it retains the legacy external asynchronous handler.
Versioned `Runtime.emit` and `Schedule.after_ms` accept JSON, which the host wraps
as `Internal_event(payload)`, rather than accepting arbitrary native event values.
See [authoring runtime](chatml-authoring-runtime.md),
[owned background work](chatml-authoring-background.md),
[invocation context](chatml-invocation-context.md), and
[model/process/runtime control](chatml-runtime-control.md) for the complete host
contracts. Shell-hook surfaces are separate and do not inherit moderator modules.

### 13.2.3 `Task` module

The `Task` builtin module provides the core task combinators used by
computation and moderator hosts:

```ocaml
Task.pure  : 'a -> 'a task
Task.bind  : 'a task -> ('a -> 'b task) -> 'b task
Task.map   : 'a task -> ('a -> 'b) -> 'b task
Task.map_array : 'a array -> ('a -> 'b task) -> 'b array task
Task.iter_array : 'a array -> ('a -> unit task) -> unit task
Task.fail  : string -> 'a task
Task.catch : 'a task -> (string -> 'a task) -> 'a task
```

These functions construct and compose task values. They do not themselves
perform host-side effects. Their arguments still evaluate eagerly. Reusing a
task interprets it again, rather than memoizing its result. `map_array` and `iter_array` defer their callbacks until
interpretation, then execute each callback task sequentially from left to right.
Each interpretation first takes a fresh shallow snapshot of the input array;
mutable payloads remain shared. `map_array` returns a fresh array, while
`iter_array` requires unit results. Failure stops later callbacks; suspended UI
work resumes the same traversal without replaying completed callbacks.

`Task.fail` constructs deferred failure; global `fail` fails immediately.
`Task.catch` handles failure while interpreting the protected task, not failure
that happened eagerly while constructing its argument. A pure evaluation failure
in a bind/map callback also exits that interpreter invocation rather than being converted to `Task.fail`; return a failing task for
recoverable workflow errors. Catch restores staged local effects, emitted events and end-session requests, but cannot undo completed
external operations or arbitrary ref/array mutations. Host cancellation and bounded-execution control failures bypass `Task.catch`. See
[task composition and effects](chatml-task-effects.md).

### 13.2.4 `String` module (updated)

The `String` builtin module provides common string utilities.

Exports:

#### Basic operations

- `String.length : string -> int` (byte count)
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

Strings contain bytes. Case conversion affects ASCII letters; slicing and search
do not operate on Unicode grapheme boundaries. `trim` strips ASCII whitespace at
the ends. See [string operations](chatml-strings.md).

#### Slicing, search, and split

- `String.slice : string -> int -> int -> string`  
  `slice(s, start, len)` copies `len` bytes starting at zero-based byte offset `start`; the third argument is a length, not an ending offset. An empty slice at the byte length is valid; invalid bounds fail.
- ``String.find : string -> string -> [ `None | `Some(int) ]``
  Finds the first occurrence of the literal pattern and returns its starting byte offset; an empty pattern returns `` `Some(0) ``.
- `String.split : string -> string -> string array`  
  Splits on a non-empty separator string; raises if the separator is empty.
- `String.replace_all : string -> string -> string -> string`  
  `replace_all(s, pattern, with_)` replaces all non-overlapping occurrences. Raises if `pattern` is empty.

---

### 13.2.5 `Array` module (updated)

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
- ``Array.filter_map : 'a array -> ('a -> [ `None | `Some('b) ]) -> 'b array``
  Visits every input in order, skips `None`, and returns the `Some` payloads in a fresh shallow array.
- `Array.exists : 'a array -> ('a -> bool) -> bool`
- `Array.for_all : 'a array -> ('a -> bool) -> bool`

`exists`, `for_all`, `find` and `find_map` stop as soon as their answer is known.
For empty arrays, `exists` is false, `for_all` is true, and searches return `None`.
Eager `Array` callbacks do not interpret returned tasks; use `Task.map_array` or
`Task.iter_array` for sequential effectful work. Copies share contained refs,
arrays and other mutable payloads. `Array.make` and `fill` reuse one supplied
value; `init` evaluates its callback separately for each slot.

#### Option-returning search helpers

These use the standard option encoding as variants:

- `` `None ``
- `` `Some(x) ``

Exports:

- ``Array.find : 'a array -> ('a -> bool) -> [ `None | `Some('a) ]``
  Returns the first element satisfying the predicate, or `` `None ``.
- ``Array.find_map : 'a array -> ('a -> [ `None | `Some('b) ]) -> [ `None | `Some('b) ]``
  Applies the mapping function left-to-right and returns the first `` `Some(...)` `` result, or `` `None ``.

### 13.2.6 `Option` module

Optional values use `` `None `` and `` `Some(value) ``. Outcomes use
`` `Ok(value) `` and `` `Error(error) ``; neither requires a new runtime type.
The following `option(a)` and `outcome(a, e)` names abbreviate these variants
in the reference signatures, not installed source aliases.

```text
Option.none : unit -> option('a)
Option.some : 'a -> option('a)
Option.is_none : option('a) -> bool
Option.is_some : option('a) -> bool
Option.map : option('a) -> ('a -> 'b) -> option('b)
Option.bind : option('a) -> ('a -> option('b)) -> option('b)
Option.get_or : option('a) -> 'a -> 'a

Outcome.map : outcome('a, 'e) -> ('a -> 'b) -> outcome('b, 'e)
Outcome.bind : outcome('a, 'e) -> ('a -> outcome('b, 'e)) -> outcome('b, 'e)
Outcome.map_error : outcome('a, 'e) -> ('e -> 'f) -> outcome('a, 'f)
```

These operations run immediately. `Option.map`/`bind` call their callback only
for `Some`; `Outcome.map`/`bind` only for `Ok`, and `map_error` only for `Error`.
The unselected branch is preserved without calling the callback. Callbacks are
not exception handlers: their failures propagate rather than becoming `None`
or `Error`. `get_or` is an ordinary strict call, so its default argument is
evaluated even when the option contains a value. See
[arrays and optional values](chatml-collections.md).

### 13.2.7 `Hashtbl` module (string-keyed)
This is a small builtin hashtable-like abstraction with string keys. It is implemented using existing runtime values (refs + arrays of entries) and is intended for scripting convenience, not high performance.

Exports (conceptual types):

```ocaml
Hashtbl.create : unit -> hashtbl('a)
Hashtbl.set : hashtbl('a) -> string -> 'a -> unit
Hashtbl.get : hashtbl('a) -> string -> [ `None | `Some('a) ]
Hashtbl.mem : hashtbl('a) -> string -> bool
Hashtbl.remove : hashtbl('a) -> string -> unit
Hashtbl.to_array : hashtbl('a) -> { key : string; value : 'a } array
```

Notes:

- The key type is always string.
- Hashtbl.get returns `` `None `` or `` `Some(value) ``.
- `to_array` returns a fresh shallow array of fresh entry records in insertion order. Replacing a value retains its key's position; removing and reinserting a key moves it to the end. Entry payloads remain shared. Malformed entries or duplicate keys fail.
- The table is structurally represented as a ref containing an array of `{key; value}` records; prefer the module operations over forging that representation.
- Current representation is optimized for simplicity rather than asymptotic performance.


### 13.2.8 `Json` module (updated)

The `Json` module provides:

- a real recursive JSON value type at the ChatML level (`json` in the
  builtin type-alias surface), and
- conversion to/from JSON text via the host-side **Jsonaf** library.

#### `json` representation

The builtin alias `json` is represented as a recursive variant type
equivalent to:

```ocaml
json =
  [ `Null
  | `Bool(bool)
  | `Number(float)
  | `String(string)
  | `Array(json array)
  | `Object({ key : string; value : json } array)
  ]
```

Internally the builtin scheme uses a recursive binder. The source alias `json`
is already installed; it does not require a user declaration.
  
Exports:
#### Text conversion:

- `Json.parse : string -> json`
Parses JSON text into a `json` value. Raises a runtime failure on invalid JSON input.
- ``Json.parse_opt : string -> [ `None | `Some(json) ]``
Like parse, but returns `` `None `` instead of raising on parse errors.
- ``Json.parse_result : string -> [ `Ok(json) | `Error([ `Invalid_syntax({ message : string }) ]) ]``
Returns either the parsed JSON or a typed syntax diagnostic. It does not validate
an application schema or an expected object shape. Use pattern matching or
`Outcome` composition for typed failures; see [JSON decoding](chatml-json.md).
- `Json.stringify : json -> string`
Produces a compact JSON string representation.
Finite numeric values use valid JSON number syntax, including whole-valued
floats (`1.0` becomes `1`). Non-finite values cannot be exported as JSON. Numeric
spelling is not preserved when JSON passes through ChatML's float representation.
- `Json.pretty : json -> string`
Produces a human-readable formatted JSON representation.
- `Json.validate : string -> bool`
Returns true iff the string parses as JSON.
Introspection and shape-safe accessors:

- `Json.tag : json -> string`
Returns one of "Null", "Bool", "Number", "String", "Array", "Object".

- ``Json.as_bool : json -> [ `None | `Some(bool) ]``

- ``Json.as_number : json -> [ `None | `Some(float) ]``

- ``Json.as_string : json -> [ `None | `Some(string) ]``

- ``Json.as_array : json -> [ `None | `Some(json array) ]``

- ``Json.as_object : json -> [ `None | `Some({ key : string; value : json } array) ]``

Object helpers:

- `Json.object_keys : json -> string array`
Returns an array of keys when given an object; returns an empty array on non-object values.
- ``Json.get_field : json -> string -> [ `None | `Some(json) ]``
If the first argument is an object and the key exists, returns
`` `Some(value) ``; otherwise `` `None ``.

Path lookup:

- ``Json.get_path : json -> string array -> [ `None | `Some(json) ]``
Traverses the JSON value using a path of string segments:
when the current value is an object, segments are treated as field names;
when the current value is an array, segments are parsed as in-bounds integer
indices. Use nonnegative decimal index text.
Returns `` `None `` if traversal fails at any point.
Pure object update helpers:

- `Json.set_field : json -> string -> json -> json`
Returns an updated object with the given field set to the new value.
Raises a runtime failure if the first argument is not an object.
- `Json.remove_field : json -> string -> json`
Returns an updated object with all entries for the given key removed.
Raises a runtime failure if the first argument is not an object.

Notes:
- `Json.parse`, `Json.stringify`, and `Json.pretty` are backed by Jsonaf.
- JSON numbers are surfaced as float in ChatML. Parsing converts Jsonaf’s numeric token text to float; stringifying renders the float back to JSON numeric text. Large integers can lose precision. `validate` checks syntax only, not an application schema or lossless numeric conversion.
- Object entries are ordered and may contain duplicate keys. `get_field` selects the first match; `set_field` replaces that match and drops later duplicates, or appends a missing key. `remove_field` removes every match.
- `as_array` and `as_object` return the original mutable payloads. Object updates copy only the outer container; unchanged nested values remain shared.
- An empty path returns the original value in `Some`. Paths do not split dotted strings or decode JSON Pointer escapes.
- Option results use the standard variant encoding: `` `None `` and `` `Some(x) ``.


### 13.3 Interaction with open and shadowing
open imports module exports into the current scope and rejects any import that would shadow an existing binding.

Because global builtins exist in the initial environment, opening some builtin modules may be rejected due to name collisions. For example:
- `open Array` is rejected by default because Array.length would shadow the global length.

Users can always access module exports through qualified access (Array.length(xs)) without using open.

### 13.4 Moderator capability modules

The following effectful modules are available in `moderator_surface` or, for
UI-only features, `ui_moderator_surface`.

Their exported functions return task values and are interpreted by the host
runtime operation registry.

#### 13.4.1 `Log`

```ocaml
Log.debug : string -> unit task
Log.info  : string -> unit task
Log.warn  : string -> unit task
Log.error : string -> unit task
```

These are diagnostic operations observed by the host runtime.

#### 13.4.2 `Turn`

```ocaml
Turn.prepend_system  : string -> unit task
Turn.append_item     : item -> unit task
Turn.replace_item    : string -> item -> unit task
Turn.delete_item     : string -> unit task
Turn.replace_or_append : [ `None | `Some(string) ] -> item -> unit task
Turn.append_notice   : string -> unit task
Turn.halt            : string -> unit task
```

These describe local turn-overlay style mutations. The current runtime
records them as local transactional effects and leaves the concrete overlay
semantics to host handlers.

The legacy `append_message`, `replace_message`, and `delete_message` builtin
names remain available as aliases.

#### 13.4.3 `Tool`

```ocaml
Tool.approve      : unit -> unit task
Tool.reject       : string -> unit task
Tool.rewrite_args : json -> unit task
Tool.redirect     : string -> json -> unit task
Tool.call         : string -> json -> [ `Ok(json) | `Error(string) ] task
Tool.spawn        : string -> json -> string task
```

`Tool.call` is interpreted as an external synchronous operation.
`Tool.spawn` uses an external asynchronous callback on the generic moderator
surface. On extensibility-v1 surfaces it stages transactional `Job.start_tool`
intent: actual execution begins only after the owning commit. Its returned
string is a job ID, not the eventual output.

#### 13.4.4 `Model`

```ocaml
Model.call  : string -> json -> [ `Ok(json) | `Refused(string) | `Error(string) ] task
Model.spawn : string -> json -> string task
Model.call_text : string -> string -> [ `Ok(json) | `Refused(string) | `Error(string) ] task
Model.call_json : string -> json -> [ `Ok(json) | `Refused(string) | `Error(string) ] task
Model.spawn_text : string -> string -> string task
```

`Model.spawn` starts a host-managed background job and returns a stable job id.
The legacy conversation embedding reinjects a moderator internal event when a
job completes. Its completion event tags are:

- `Model_job_succeeded(job_id, recipe_name, result_json)`
- `Model_job_failed(job_id, recipe_name, message)`

Those completion tags belong to the legacy conversation embedding. The durable
session host records session-owned model jobs through its job service and follows
its retained completion contract. Do not infer restart or cancellation guarantees
from the generic callback or the string result type.

The string argument is a host-defined recipe name, not an unrestricted raw
provider/model identifier. `call_json` aliases `call`; the text helpers wrap
their input as JSON strings and do not unwrap successful JSON results. Returned
`Refused` or `Error` variants are values, not task failures; a missing recipe or
failed operation can instead fail the task. Model callbacks may perform external
work before commit, which rollback cannot undo.

#### 13.4.5 `Process`

```ocaml
Process.run : string -> string array -> string task
```

`Process.run` is a host-managed external operation. Hosts may reject or omit
this capability entirely. The standard shell adapter waits for completion and
returns stdout followed by stderr, without a separator; the result is not a job
ID. Structured argv, current shell permissions and the configured backend still
apply. A completed command cannot be undone by task rollback.

#### 13.4.6 `Schedule`

```ocaml
Schedule.after_ms : int -> 'e -> string task
Schedule.cancel   : string -> unit task
```

The generic `Schedule.after_ms` accepts a raw ChatML event value. The versioned
moderator operation accepts JSON and delivers `Internal_event(payload)` instead.
A negative delay fails. Timers and their retention depend on the installed host.

#### 13.4.7 `Runtime`

```ocaml
Runtime.emit               : 'e -> unit task
Runtime.request_compaction : unit -> unit task
Runtime.request_turn       : unit -> unit task
Runtime.end_session        : string -> unit task
```

`Runtime.emit` buffers a raw ChatML event for later enqueueing on
successful task completion.

`Runtime.request_turn()` requests that the host run **one more ordinary model turn**
after the current turn completes. The current default registry permits it in:

- `turn_end`
- `internal_event`
- `tool_observed`
- `tool_invoked`

The last two phases are used by the versioned moderator surface. Qualified
daemon hosts persist the request with the observation or handler result before scheduling it. Background handler requests
wait for their owning job to complete; cancelled or interrupted jobs discard them.
Handler and observation requests retain independent disposition, and an applied
request is not replayed on restart. General feature availability remains gated on
authoring-support qualification.

The host interprets this request after `turn_end` handling finishes; it does not
directly invoke a side model call.

Multiple `request_turn` effects emitted while handling one host event collapse to a
single continuation decision. `Runtime.end_session(...)` overrides `request_turn`.

#### 13.4.8 `Ui`

`Ui` is available only on `ui_moderator_surface`.

```ocaml
Ui.notify : string -> unit task
```

`Ui.notify` emits a host-local notice. It does not mutate canonical history
and does not append transcript items automatically.

#### 13.4.9 `Approval`

`Approval` is available only on `ui_moderator_surface`.

```ocaml
Approval.ask_text   : string -> string task
Approval.ask_choice : string -> string array -> string task
```

These operations suspend the current live script execution and later resume
that same execution with a validated response supplied by the host. They do
not append fake canonical user items automatically.

---

## 14. Surface syntax summary

This is not a full formal grammar, but it summarizes the implemented
surface syntax.

### 14.1 Statements

```ocaml
type t = type_expr
type 'a box = { value : 'a }
type ('a, 'e) outcome = [ `Ok('a) | `Error('e) ]
let x = expr
let (|x, y|) = expr
let (|x, y|) : int * string = expr
let f (|x, y|) step = expr
let x : type_expr = expr
let f x y = expr
let f () = expr
let rec f : type_expr = expr
let rec f x = expr and g y = expr
module M = struct stmts end
module M : sig signature_items end = struct stmts end
open M
expr
```

Signature items are transparent `type` declarations (including parameters), `val name : type_expr` (optionally
followed by `where equality('a)` constraints joined with `and`) and
`module M : sig signature_items end`. Optional `;;` separates statements and
signature items; a single `;` remains an expression separator.

### 14.2 Expressions

```ocaml
()
1
1.0
true
"x"
x
fun x -> expr
fun (|x, y|) -> expr
fun () -> expr
f(x)
if c then t else e
while c do body done
let x = e1 in e2
let (|x, y|) = e1 in e2
let x : t = e1 in e2
let rec f x = e1 in e2
let* x = e1 in e2
let+ x = e1 in e2
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
(|e1, e2|)
(|e1, (|e2, e3|)|)
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
(|p1, p2|)
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
Catalog.id
int box
(int, string) outcome
int * string
(int * string) * bool
fun() -> int
fun(unit) -> int
fun(int * string) -> bool
fun(int, string) -> bool
fun(int) -> fun(int) -> int
expr -> int
unit -> string
state task
task array
state task array
{ name : string; status : status }
[ `Pending | `Done | `Error(string) ]
```

---

## 15. Diagnostics

### 15.1 Structured failures and source ownership

Parser, typechecker and runtime failures use `Chatml_diagnostic.t`. Its validated
constructor preserves read-only message/span projections and an optional owned
site. A site holds the exact captured source identity, content digest and range;
it grants no execution authority and performs no live file lookup. An existing
site wins when a caller supplies fallback context. A legacy unowned span alone
does not establish source ownership.

Ranges are half-open byte ranges, with 1-based lines and 0-based byte columns.
LF, CR and CRLF are canonical line breaks; a position between CR and its
following LF remains on the preceding line until LF is consumed. Invalid or
stale coordinates reject instead of being clipped. Messages retain at most
16 KiB, and diagnostics retain at most eight related sites. Complete rendering
is also bounded to 16 KiB. Source labels and excerpts escape control bytes; caret
markers are omitted when escaping or multiple lines would misrepresent byte
columns. `render_legacy` uses explicitly supplied text only for unowned ranges;
an owned failure always renders its captured bytes.

Closures retain their defining source through direct calls, tail calls and
builtin callbacks. Task values retain their construction site, and continuation
failures preserve their defining source through persistent events and UI
resumption. Sites retain only the needed captured source, with no complete
inventory or control callback. Metadata introduces no semantic task step or
fuel charge. Diagnostic polling still checks cancellation, deadlines and active
scope lifetime. Eager and bind/map callback errors preserve their existing
`Task.catch` boundaries; plain task/host failure strings reach recovery unchanged
even when their diagnostic display is truncated.

### 15.2 Captured compiler and detailed host APIs

For OCaml embedders, `Chatml_parse.parse_captured` produces a source/AST pair.
`parse_captured_interface` pairs companion interface syntax with its exact captured
bytes. `Chatml_signature_check.check_captured` checks that interface against supplied
implementation exports; it does not discover or load companion files. Signature
claims use the interface source, while an alias expansion retains its defining
source. An alias-equivalence mismatch can include the implementation definition
as a related site without replacing the interface claim's primary location.

`Chatml_typechecker.check_captured_program_with_surface` and
`Chatml_resolver.resolve_captured_program` preserve that pairing; resolution
rejects annotations belonging to a different source identity or revision.
Per-unit type and module-visibility annotation tables are separate while inference
identities and weak type cells remain deliberately shared within the compilation.
Location-based queries require the owning source identity and content digest.
These tables are not deeply frozen value-query snapshots. Interface checking
validates source metadata before committing constraints to shared weak variables;
failed or cancelled checks do not publish a partial constraint update.

`Chatml_host_runtime.compile_captured_script` compiles exact captured bytes
without executing initializers. Its compilation diagnostic retains the stage,
message/span projections, rendered text and shared structured diagnostic.
`Chatml_compilation.compile_captured` and `compile_captured_with_policy` provide
the same captured compilation through the existing joined Eio domain, source-size
limits and cooperative cancellation/time policy. The caller supplies the chosen
capture; these APIs perform no project or file discovery.
The host's `instantiate_session_detailed`, `run_entrypoint_detailed`,
`handle_event_detailed`, `handle_next_queued_event_detailed` and
`resume_ui_request_detailed` return the shared diagnostic from the same
interpreter used by the string APIs. Existing single-string compilation and
string-returning host entrypoints remain rendering adapters. Compiler checkpoint
exceptions retain their original identity and backtrace; cancellation and
execution-control exceptions propagate after cleanup rather than becoming
ordinary task failures.

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
- regular recursive data preserving ordinary generalization and value restriction
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

- mutual recursive type declarations
- general expression ascription syntax
- ref type syntax
- open-row type syntax
- full user-facing algebraic datatype declarations beyond alias-style
  structural `type` declarations
- selective imports
- abstract signature types, module type declarations or functors
- ambient project discovery for submitted source strings
- layout-sensitive syntax
- Unicode identifiers
- full ML-style match usefulness analysis
- ad-hoc overloaded numeric operators

The language remains intentionally conservative and small.

---

## 18. Important implementation caveats

These are worth documenting because they shape current behavior.

### 18.1 Module and record operations retain their kind

The typechecker distinguishes modules from ordinary records, and the runtime
represents them as `VModule` and `VRecord` respectively. Projection and
`record_keys` support both. Record patterns, annotations and copy-updates require
ordinary records; `open` requires a module. Inferred aliases and higher-order
helpers retain these constraints. Public type aliases exist only in the static
module namespace, while signed runtime modules contain only selected value and
nested-module members.

### 18.2 Records get stronger row ergonomics than variants

Record projection and open patterns infer the fields a helper requires,
allowing extra fields at call sites. Exact annotations and closed patterns stay
exact. Variants use their own row constraints and may narrow through matches;
neither mechanism grants implicit conversion between incompatible branch
results.

### 18.3 Builtins remain a host-side facility

The richer builtin type language exists for host/runtime authors, not as a
complete user-facing type-annotation mechanism.

---

## 19. Recommended style

For the current language, the most ergonomic and robust style is:

- use records for script state
- use tuples for small positional groups or multiple results; use records when
  field names make the meaning clearer
- declare explicit recursive record/variant types when a script's data model
  is genuinely recursive
- annotate recursive helper functions against those declared types
- write small helpers over row-polymorphic state records
- use variants for finite event/state tags
- use modules to group helpers and transparent types; add inline signatures when
  a public member boundary helps
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
        if st.idx < length(st.tasks) then set_status(set_running(st, true), status_witness(1))
        else set_running(st, false)
      else st

  | `Fail(msg) ->
    if st.idx >= length(st.tasks) then set_running(st, false)
    else set_running(set_status(st, `Error(msg)), false)

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

This example expects a square adjacency matrix and valid start/goal indices.
The helper illustrates tuple matching; as an ordinary function, both of its
arguments are evaluated before it runs.

```ocaml
let and_ a b =
  match (|a, b|) with
  | (|true, true|) -> true
  | _ -> false

module Graph = struct
  (* adjacency matrix: g[u][v] = 1 if edge *)
  let neighbors g u = g[u]

  let bfs_distance g start goal =
    let n = length(g) in

    (* distances initialized to -1 (unvisited) *)
    let dist = Array.make(n, -1) in
    dist[start] <- 0;

    (* simple fixed-size queue of nodes *)
    let q = Array.make(n, 0) in
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
      (* e : { key : string; value : json } *)
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

### 20.11 Tuple results behind a checked module interface

A transparent generic alias can name a product without changing its shape.
A signature selects public members; an omitted helper remains private.
The exact function type below takes two arguments and returns one pair.

<!-- ochat-authoring-example: {"id":"reference.tuple-interface","surface":"one_off_v1","result":"count=7"} -->
```ocaml
module Labeled : sig
  type 'a t = string * 'a
  val make : fun(string, 'a) -> 'a t
end = struct
  type 'a t = string * 'a
  let separator = "="
  let make label value = (|label ++ separator, value|)
end

let pair : int Labeled.t = Labeled.make("count", 7)
let (|label, value|) = pair
let main input = Task.pure(`String(label ++ to_string(value)))
```

Type members are transparent aliases, not runtime fields. Generic signature
aliases must agree with their implementation for every parameter substitution.
For host JSON output, explicitly construct a `Json` value as above; tuples do
not implicitly become JSON arrays. Their session snapshot encoding is a separate
persistence format.

---

## 21. Task values and moderator-runtime embedding

This section documents the current implementation of ChatML task values and
the generic host-side moderator runtime. The unversioned moderator contract
and its legacy events are distinct from the statically checked Ochat
extensibility-v1 contracts summarized in section 13.2.2.2.

### 21.1 Task values in the core runtime

Tasks are first-class runtime values. The core value space therefore
includes:

```ocaml
VTask of task
```

with the internal task representation:

```ocaml
type task =
  { site : Chatml_diagnostic.Site.t option
  ; step : task_step
  }

and task_step =
  | TPure of value
  | TBind of task * value
  | TMap of task * value
  | TArray of Chatml_task_array_mode.t * value array * value
  | TFail of string
  | TCatch of task * value
  | TPerform of eff
  | TSpawn of eff
```

The ChatML evaluator does not interpret these task nodes directly. It
constructs and propagates them as values. Each task retains its optional
construction site for owned diagnostics; array traversal state belongs to one
interpretation, not to the reusable task description. Interpretation happens only in
host code, such as the moderator runtime.

### 21.2 Surface type convention

ChatML follows the existing postfix type-constructor style.

The type constructor is named:

- `task` in the type namespace
- `Task` in the module/value namespace

Examples:

```ocaml
state task
message array
state task array
```

The builtin helper functions therefore have shapes such as:

```ocaml
Task.pure : 'a -> 'a task
Task.bind : 'a task -> ('a -> 'b task) -> 'b task
```

### 21.3 Task module and surface syntax

The builtin `Task` module currently exports:

```ocaml
Task.pure  : 'a -> 'a task
Task.bind  : 'a task -> ('a -> 'b task) -> 'b task
Task.map   : 'a task -> ('a -> 'b) -> 'b task
Task.map_array : 'a array -> ('a -> 'b task) -> 'b array task
Task.iter_array : 'a array -> ('a -> unit task) -> unit task
Task.fail  : string -> 'a task
Task.catch : 'a task -> (string -> 'a task) -> 'a task
```

ChatML also supports monadic let sugar for tasks:

```ocaml
let* x = t1 in t2
let+ x = t1 in t2
```

desugaring to `Task.bind` and `Task.map` respectively. `let*` continues with
a task; `let+` computes a plain result value. Discarding a task does not interpret
it. Array task traversal is sequential and takes a fresh shallow input snapshot
at interpretation time, as described in section 13.2.3.

`Task.catch` handles deferred task/operation failures. It does not catch an eager
`fail(...)` while constructing the protected argument, static errors, host
cancellation or bounded-execution control failures. Pure callback evaluation failures
are not automatically converted to recoverable `Task.fail` values. Returned `Error`/`Refused` values
do not automatically fail a task.

### 21.4 Host-side function application

Because `TBind`, `TMap`, and `TCatch` store ChatML closures/builtins inside
task values, host runtimes need a way to apply those values.

The evaluator therefore exposes a public host-side API:

```ocaml
apply_value_result : value -> value list -> (value, runtime_error) result
apply_value_exn    : value -> value list -> value
```

These are used by the moderator runtime and may also be used by other host
embeddings.

### 21.5 Moderator script contract

The integrated moderator runtime uses a convention-based contract. This
self-contained example counts appended items and rejects every tool call.
The complete source is available as
[`moderator.chatml`](../examples/chatml/moderator.chatml):

```ocaml
type state = { appended_count : int }
type event =
  [ `Session_start
  | `Session_resume
  | `Turn_start
  | `Item_appended(item)
  | `Pre_tool_call(tool_call)
  | `Post_tool_response(tool_result)
  | `Turn_end
  ]

let initial_state = { appended_count = 0 }

let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Item_appended(item) ->
      Task.pure({ st with appended_count = st.appended_count + 1 })
    | `Pre_tool_call(call) ->
      let* () = Tool.reject("Tools are disabled by this moderator.") in
      Task.pure(st)
    | _ ->
      Task.pure(st)
```

`Item_appended` is not limited to user messages. The standard host events are
`Session_start`, `Session_resume`, `Turn_start`, `Item_appended(item)`,
`Pre_tool_call(call)`, `Post_tool_response(result)`, and `Turn_end`. Internal
events are host-defined variants, such as
`Model_job_succeeded(job_id, recipe, result)`, not a universal
`AsyncCompleted` event. See the [moderator event model](chatml-moderator-runtime.md#event-model).

Conceptually:

```ocaml
initial_state : state
on_event : context -> state -> event -> state task
```

This unversioned generic API checks entrypoint presence and callability when
instantiating the session; hosts may also supply `required_bindings` to
`compile_script` for static checks. Ochat's versioned `moderator_v1` and
`delegated_moderator_v1` compiler contracts also statically require the
`initial_state`/three-argument `on_event` state agreement and typed moderator
event/result contract. They have additional typed invocation, job, subscription
and internal events. Do not use this legacy event example as the complete
versioned event inventory; see [invocation context](chatml-invocation-context.md)
and [background work](chatml-authoring-background.md).

### 21.6 Structural runtime aliases exposed to moderator scripts

`moderator_surface` installs the following builtin type aliases:

```ocaml
type item =
  { id : string
  ; value : json
  }

type tool_desc =
  { name : string
  ; description : string
  ; input_schema : json
  }

type tool_call =
  { id : string
  ; name : string
  ; args : json
  }

type tool_result =
  { call_id : string
  ; name : string
  ; result : json
  }

type context =
  { session_id : string
  ; now_ms : int
  ; phase : string
  ; items : item array
  ; available_tools : tool_desc array
  ; session_meta : json
  }
```

These aliases are compile-time conveniences layered on top of ordinary
structural record/variant typing.

The following signature blocks use `option('a)` as reference shorthand for
`` [ `None | `Some('a) ] ``, not as an installed source type alias. These helpers
run immediately; they do not request host effects.

The moderator surface also installs an `Item` builtin module:

```ocaml
Item.create : string -> json -> item
Item.id : item -> string
Item.value : item -> json
Item.kind : item -> option(string)
Item.role : item -> option(string)
Item.text_parts : item -> string array
Item.text : item -> option(string)
Item.input_text_message : string -> string -> string -> item
Item.output_text_message : string -> string -> item
Item.user_text : string -> string -> item
Item.assistant_text : string -> string -> item
Item.system_text : string -> string -> item
Item.notice : string -> string -> item
Item.is_user : item -> bool
Item.is_assistant : item -> bool
Item.is_system : item -> bool
Item.is_tool_call : item -> bool
Item.is_tool_result : item -> bool
```

`Item.kind` inspects the serialized OpenAI response item `"type"` field when
present. `Item.role` and `Item.text_parts` provide convenient access to common
message-like item shapes without forcing scripts to hand-author raw JSON.
`Item.text` selects the first recognized text part or returns `None`. Message
constructors take a host item ID first; `input_text_message` additionally takes
a role before its text. `is_system` recognizes both system and developer roles.
`notice` builds a host notice shape, not an automatic transcript insertion.

`Tool_call` provides exact-name and shape-safe argument inspection:

```text
Tool_call.arg : tool_call -> string -> option(json)
Tool_call.arg_string : tool_call -> string -> option(string)
Tool_call.arg_bool : tool_call -> string -> option(bool)
Tool_call.arg_array : tool_call -> string -> option(json array)
Tool_call.is_named : tool_call -> string -> bool
Tool_call.is_one_of : tool_call -> string array -> bool
```

Typed accessors return `None` for missing fields or the wrong JSON kind.
`Context` selects from the supplied context's item and tool inventories:

```text
Context.last_item : context -> option(item)
Context.last_user_item : context -> option(item)
Context.last_assistant_item : context -> option(item)
Context.last_system_item : context -> option(item)
Context.last_tool_call : context -> option(item)
Context.last_tool_result : context -> option(item)
Context.find_item : context -> string -> option(item)
Context.items_since_last_user_turn : context -> item array
Context.items_since_last_assistant_turn : context -> item array
Context.items_by_role : context -> string -> item array
Context.find_tool : context -> string -> option(tool_desc)
Context.has_tool : context -> string -> bool
```

The `items_since_*` helpers include the latest matching item, or return all
items when none matches. `last_system_item` accepts system or developer roles;
`items_by_role` uses exact role equality. A tool description's presence does
not authorize execution. See [moderator helpers](chatml-moderator-runtime.md).

The `Turn` module exposes item-oriented mutation helpers:

```ocaml
Turn.prepend_system : string -> unit task
Turn.append_item : item -> unit task
Turn.replace_item : string -> item -> unit task
Turn.delete_item : string -> unit task
Turn.replace_or_append : option(string) -> item -> unit task
Turn.append_notice : string -> unit task
Turn.halt : string -> unit task
```

Legacy `Turn.append_message`, `Turn.replace_message`, and
`Turn.delete_message` names remain available as aliases.

### 21.7 Integrated host lifecycle

When a ChatMD prompt declares:

```xml
<script language="chatml" kind="moderator" id="main">
...
</script>
```

or:

```xml
<script language="chatml" kind="moderator" id="main" src="moderator.chatml" />
```

the legacy prompt-loading path treats that unversioned script as its single
moderation program. A versioned authored moderator uses the compiler contract
and selected captured program instead.

Integration rules:

- zero scripts means no moderation and preserves baseline driver behavior,
- one script enables the shared moderation manager,
- more than one script is a prompt validation error,
- the script is parsed and validated during ChatMD prompt loading,
- the script is compiled once per prompt load and instantiated per session,
- the script remains host-managed and is not converted into model-visible
  request history.

The shared moderation vocabulary defines these phase names:

- `session_start`
- `session_resume`
- `turn_start`
- `message_appended`
- `pre_tool_call`
- `post_tool_response`
- `turn_end`
- `internal_event`

The script-visible event constructors are:

- `` `Session_start ``
- `` `Session_resume ``
- `` `Turn_start ``
- `` `Item_appended(item) ``
- `` `Pre_tool_call(tool_call) ``
- `` `Post_tool_response(tool_result) ``
- `` `Turn_end ``
- and user-defined internal variants delivered through the internal-event path

Current built-in host paths emit:

- `session_start` for fresh moderated sessions,
- `session_resume` when restoring a persisted moderator snapshot,
- `turn_start` before each model request,
- `message_appended` after a canonical transcript item is appended by the
  streamed runtime,
- `pre_tool_call` before tool execution,
- `post_tool_response` after a tool output item is produced,
- `turn_end` after a streamed assistant turn finishes,
- `internal_event` while replaying queued emitted events.

### 21.8 Effect requests

Task effects are represented internally as:

```ocaml
type eff =
  { op : string
  ; args : value list
  }
```

with two execution modes:

- `TPerform eff` for host-performed operations
- `TSpawn eff` for host-spawned asynchronous operations

The concrete set of supported operation names is host-defined.

### 21.9 Host-side moderator runtime

The generic implementation lives in `chatml_host_runtime.ml`;
`chatml_moderator_runtime.ml` is its compatibility facade. It
supports:

- compile once:
  - parse
  - typecheck against a selected surface
  - resolve
- instantiate per session:
  - fresh environment
  - install chosen builtin surface
  - evaluate the program
  - load `initial_state` and `on_event`
- handle events:
  - call `on_event`
  - require a returned task
  - interpret that task
  - commit or discard local transactional outputs

The public entrypoints are:

```ocaml
compile_script
instantiate_session
handle_event
pending_ui_request
resume_ui_request
```

The repository's shared moderation integration layers this runtime under
`Chat_response.Moderation`, `Chat_response.Moderator_manager`, and the shared
drivers used by `chat_tui`, file-backed prompt execution, nested `run_agent`
calls, and MCP prompt-agent wrappers.

### 21.10 Operation registry and defaults

The moderator runtime interprets task effects through an operation registry:

```ocaml
type op_kind =
  | Local_transactional
  | Local_transactional_with_result of { rollback : value list -> unit }
  | External_sync
  | External_async
  | Diagnostic
```

Each operation definition provides:

- an operation name,
- its runtime kind,
- a phase check,
- and a host callback.

`Local_transactional_with_result` records a provisional reservation result with
its operation arguments and can release discarded reservations during
`Task.catch`. It must not start external work before the owning commit; rollback
only releases provisional resources. Hosts still own whole-transaction failure
and cancellation cleanup.

The runtime also ships with a standard default registry constructor:

```ocaml
default_operations
default_runtime_config
```

This default registry understands the standard moderator modules
(`Log`/`Turn`/`Tool`/`Model`/`Process`/`Schedule`/`Runtime`) and, when the
UI surface is installed, the `Ui` and `Approval` operations.

Current default behavior is intentionally conservative:

- diagnostic/log and local turn/tool-moderation handlers default to no-op
  success,
- `Runtime.emit` buffers an internal event,
- `Runtime.end_session` marks the session as ended on successful commit,
- external integrations such as `Tool.call`, `Model.call`, and
  `Schedule.after_ms` fail with a clear `"… is not configured"` error unless
  the host supplies handlers. 
- `Runtime.request_turn` is a local transactional operation and is phase-restricted in v1
(see above). It is surfaced to the host as a runtime request and interpreted by the
host conversation loop after `turn_end`, or by a qualified daemon at the persisted
handler/observation boundary described in section 13.4.7.

Named model recipes are host-defined. `Model.call("recipe", payload)` and
`Model.spawn("recipe", payload)` do not select arbitrary provider model names;
they route through the host capability registry under a recipe name chosen by
the embedder.

The current embedding registers at least one recipe:

- `agent_prompt_v1`

Recipe names are not provider model identifiers; they route to host-defined behavior.

### 21.11 Commit/rollback semantics

The moderator runtime currently buffers:

- the next script state,
- local transactional effect records,
- internal emitted events,
- pending end-session requests.

On successful completion of the returned task:

- the new script state is installed as committed runtime state,
- local transactional effects are committed in execution order,
- buffered emitted events are appended to the session queue,
- a pending end-session request halts the session.

In-memory runtime commit alone is not a disk durability guarantee. A durable host
validates and persists the prospective state/effects/queue through the borrowed
`prepare_transaction` boundary before the infallible runtime/host installers.
Concrete host persistence, owned-job publication and acknowledgement rules
remain separate from task construction.

If execution suspends on `Approval.ask_text` or `Approval.ask_choice`:

- the current handler does not commit,
- buffered local transactional effects remain uncommitted,
- buffered emitted events remain uncommitted,
- the session continues to expose the last committed state,
- a host-visible `pending_ui_request` becomes available,
- and ordinary `handle_event` progression remains blocked until the host calls
  `resume_ui_request`.

On task failure:

- the previous script state is kept,
- buffered local transactional effects are discarded,
- buffered emitted events are discarded,
- buffered end-session requests are discarded.

`TCatch` restores the local transactional buffers before running the handler
task.

Rollback covers these staged local buffers, not completed tool/model/process
operations, diagnostic output or arbitrary ref/array mutations. A generic host
using mutable script state must supply an appropriate `copy_state` boundary; the
default generic callback returns the original value rather than deep-copying it.
The integrated durable host validates/copies data-shaped state and commits through
its persistence owner. A callback failure before commit does not imply that
previously admitted external work never happened.

### 21.12 Overlay and tool moderation semantics

The shipped host integration interprets committed local effects through a
shared moderation layer rather than mutating canonical history directly.

`Turn.*` effects update a host-owned overlay that can:

- prepend synthetic developer messages,
- append synthetic messages,
- replace projected messages by stable host id,
- delete projected messages by stable host id,
- halt the session with a reason.

The host computes the effective moderated request history by applying that
overlay to projected canonical history before the next model turn.

Tool moderation effects are interpreted as:

- `Tool.approve()` – keep the tool call unchanged,
- `Tool.reject(reason)` – synthesize a denied tool output item and skip tool execution,
- `Tool.rewrite_args(json)` – keep the tool name but replace the payload,
- `Tool.redirect(name, json)` – replace both tool name and payload.

If one moderation event emits multiple conflicting tool moderation actions, the
host treats that as an error instead of guessing precedence.

### 21.13 Internal event replay

`Runtime.emit(event)` buffers a raw ChatML value. After any successful host
event handling, the moderation manager drains queued events FIFO and re-feeds
them through phase `internal_event`.

Current behavior:

- replay happens only after successful task completion,
- replay stops once the queue is empty,
- a safety limit prevents unbounded loops,
- end-session requests stop further replay for that host event.

### 21.14 Persistence boundary

The integrated host persists moderator state explicitly in `Session.t`.

The persisted snapshot contains:

- `script_id`
- `script_source_hash`
- serializable durable script state
- serializable queued internal events
- halted flag
- host overlay snapshot

Pending UI approval state is intentionally not part of this snapshot. A
partially suspended approval is live-session-only and cannot be restored from
persisted moderator state.

Only data-shaped ChatML values cross this boundary. The snapshot codec rejects
closures, refs, builtins, modules, and tasks.

Tuple snapshots preserve arity, order, and nesting. Session documents retain
unknown fields on tuple containers and their children through restore and
unrelated edits. As with arrays and variant payloads, changing tuple children
while retaining unknown child fields can make their association ambiguous; the
document encoder then returns `Extension_conflict` before writing. For example,
editing a session setting preserves an unknown field on the first tuple child,
while changing that child's known value rejects when its unknown field is still
retained. Child edits without retained unknown child fields succeed.

When a session is exported to ChatMD, canonical history stays canonical while
overlay-derived synthetic entries are materialized explicitly in the exported
transcript so resume/export semantics stay auditable.

### 21.15 Phase restrictions

The operation model includes runtime phase checks:

```ocaml
phase_check : string -> (unit, string) result
```

The current default registry provides helper constructors such as:

```ocaml
allow_all_phases
require_phases
```

Most generic default operations use `allow_all_phases`, but
`Runtime.request_turn` explicitly requires one of `turn_end`, `internal_event`,
`tool_observed` or `tool_invoked`. Host-specific operation sets can impose
additional phase, capability, schema and lifetime restrictions. A builtin's
availability during compilation is not permission to execute it in every phase.

### 21.16 No-script fallback

All public moderation integration is optional. If a ChatMD prompt declares no
moderator `<script>`, the shared drivers, `chat_tui`, export flow, and nested
agent execution all use the baseline non-moderated behavior.


### 21.17 Runtime requests and host continuation

Moderator scripts may emit host-visible runtime requests:

- `Runtime.request_turn()` → request one more ordinary turn after `turn_end`
- `Runtime.request_compaction()` → request compaction (embedding-defined)
- `Runtime.end_session(reason)` → request session termination

Host behavior is embedding-defined, but the shared in-memory conversation loop applies
the following precedence after a streamed turn ends:

1. `end_session` terminates immediately and overrides continuation.
2. Tool-driven follow-up (pending tool calls) continues.
3. `request_turn` continues.
4. Otherwise the loop stops.

Multiple `request_turn` requests emitted while handling one host event collapse to a
single continuation decision. `request_compaction` never forces another turn by itself.

## 22. Ochat shell-runtime embedding

In an ochat moderator host, `Process.run` is not a generic spawn primitive. It
is available only when `<moderator_runtime shell_runtime="..."/>` binds the
moderator to a compiled ChatMD shell runtime. Calls use structured argv and the
same resolution, effects, capability, policy, approval, sandbox, output, and
audit path as shell tools.

Ochat also embeds ChatML through purpose-built shell surfaces for matchers,
reviewers, before/after interceptors, effect analyzers, and audit filters. Each
surface imports versioned typed values and only the actions required by that
kind. It does not inherit moderator capability modules. Stateful instances use
transactional commit/rollback, Eio serialization, resource budgets, and
source/manifest-bound snapshots.

See [ChatMD shell extensions](chatmd-shell-extensions.md).
