# ChatML implementation architecture

This guide is for contributors working on the ChatML implementation.

It explains how the major pieces fit together and where to make changes for:

- syntax
- typing
- runtime behavior
- builtin library support
- performance-oriented local variable handling

It complements:

- `docs-src/guide/chatml-language-spec.md`
- `docs-src/guide/chatml-parsing-and-diagnostics.md`

---

## 1. High-level pipeline

ChatML runs through four main phases:

1. **Lexing / parsing**
2. **Type checking**
3. **Resolution / lowering**
4. **Evaluation**

These phases are deliberately split across modules.

---

## 2. Module map

### Syntax front-end

- `chatml_lexer.mll`
  - tokenizes source text
- `chatml_parser.mly`
  - parses tokens into the source AST
- `chatml_parse.ml`
  - structured parse wrapper with diagnostics

### Core language definitions

- `chatml_lang.ml`
  - source AST
  - parsed type-expression AST
  - resolved AST
  - runtime value types
  - shared helpers (pattern matching, environments, diagnostics)

### Type system

- `chatml_typechecker.ml`
  - Hindley–Milner inference
  - explicit recursive types (`Mu` / `Rec_var`)
  - type declaration elaboration
  - contractiveness checking
  - row polymorphism
  - match checks
  - builtin type import

### Lowering / runtime preparation

- `chatml_resolver.ml`
  - lexical-address resolution
  - slot selection
  - source AST → resolved AST lowering

### Runtime

- `frame_env.ml`
  - runtime local-frame representation
- `chatml_slot_layout.ml`
  - shared slot-selection policy
- `chatml_eval.ml`
  - evaluator for resolved AST only

### Builtins / standard library

- `chatml_builtin_spec.ml`
  - builtin type language
  - builtin definitions
- `chatml_builtin_modules.ml`
  - installs builtin runtime values into environments

---

## 3. Source AST vs resolved AST

This is one of the most important architectural boundaries.

### Source AST

The source AST is what the parser produces and the typechecker consumes.

It contains source-level constructs such as:

- `EVar`
- `ELambda`
- `ELetIn`
- `ELetRec`
- `EMatch`
- `EAnnot`
- `SType`

### Resolved AST

The resolved AST is what the evaluator consumes.

It contains lowered forms such as:

- `REVarGlobal`
- `REVarLoc`
- `RELambda`
- `RELetBlock`
- `RELetRec`
- `REMatch`

### Why this split exists

The split keeps concerns separated:

- parser and typechecker work on user-facing syntax
- type declarations and annotations are checked before runtime lowering
- resolver chooses lexical addresses and slot layouts
- evaluator only runs optimized/lowered forms

If you change source syntax, you usually touch:

- `chatml_parser.mly`
- `chatml_lang.ml`
- `chatml_typechecker.ml`
- `chatml_resolver.ml`

If you change runtime execution, you usually touch:

- `chatml_eval.ml`
- maybe `frame_env.ml`
- maybe `chatml_slot_layout.ml`

---

## 4. Parsing and parse diagnostics

### Raw parser

`chatml_parser.mly` exposes the Menhir parser entry point.

On its own, it is fairly low-level.

### Structured parse wrapper

Most code should use:

```ocaml
Chatml_parse.parse_program
```

This wrapper:

- produces a `Chatml_lang.program`
- attaches the original source text
- returns structured parse diagnostics

If you are adding new syntax, keep parser and wrapper diagnostics aligned.

---

## 5. Typechecker architecture

`chatml_typechecker.ml` is the semantic center of the front-end.

### Core responsibilities

- infer types for expressions/statements
- reject ill-typed programs
- elaborate parsed `type_expr` syntax into internal types
- maintain a separate compile-time type-declaration environment
- represent recursion explicitly with `Mu` / `Rec_var`
- validate recursive type contractiveness
- enforce the value restriction
- type records and variants with rows
- check match arms for some redundancy/exhaustiveness properties
- import builtin type schemes
- record principal types by source span

### Important design points

#### 5.1 Shared monomorphic structure where needed

The environment stores both:

- generalized schemes
- shared monomorphic types

This matters for record-heavy inference and mutation safety.

#### 5.2 Record reopening heuristic

Lambda parameters inferred to be record-shaped are reopened to open rows.

This is a deliberate ergonomic bias toward state-record scripting.

#### 5.3 Builtin types

Builtin schemes come from `chatml_builtin_spec.ml`.

If you add a new type form to the builtin type language, you must update:

- `Builtin_spec.ty`
- conversion in `chatml_typechecker.ml`

#### 5.4 Explicit recursive types

Recursive types are no longer represented by cyclic ordinary inference
variables. Instead, the checker uses explicit recursive type nodes:

- `Mu of name * typ`
- `Rec_var of name`

Ordinary HM inference variables are therefore acyclic again, and recursive
types are introduced only through explicit checked declarations.

#### 5.5 Recursive type declarations and annotations

The current user-facing recursive-type path is intentionally small:

- top-level `type` declarations
- binding annotations on `let`, `let rec`, and `let ... in`

Type declarations are alias-like and compile-time only. They live in a
separate type environment threaded through statement inference.

#### 5.6 Contractiveness and monomorphism

Recursive type declarations are validated for contractiveness before use.

Bindings whose type contains explicit recursive structure are stored
monomorphically; the implementation does not attempt polymorphic recursion.

---

## 6. Resolver architecture

`chatml_resolver.ml` performs lowering from source AST to resolved AST.

### Responsibilities

- replace local variables with lexical addresses
- keep globals/modules/builtins as global lookups
- choose slot layouts for bindings
- coalesce nested non-recursive lets into block layouts
- erase type-only surface constructs before runtime

### Current design

The resolver is now explicitly reentrant:

- it uses a `resolve_ctx`
- no global mutable type-lookup state remains

The context threads:

- `lookup_type`
- lexical frame-stack state

Type-only constructs have limited runtime impact:

- `EAnnot` is erased during resolution
- `SType` never appears in the resolved runtime AST

### If you add a new binding form

You likely need to update:

- lexical frame push/pop behavior
- slot selection for the new binding
- traversal into child expressions

---

## 7. Frame and slot model

This is the main runtime-performance subsystem.

### 7.1 Frames

`frame_env.ml` provides runtime local storage.

A frame stores:

- raw cells
- the packed slot layout used to allocate them

### 7.2 Slots

Current slot kinds:

- `SInt`
- `SBool`
- `SFloat`
- `SString`
- `SObj`

### 7.3 Validation

All frame reads/writes validate:

- index bounds
- expected slot vs actual allocated slot

This means slot-layout mismatches fail fast.

### 7.4 Shared slot policy

`chatml_slot_layout.ml` is the single place for:

- expression-shape fallback slot choice
- runtime value → slot mapping
- slot/value compatibility checks

If you add a new slot specialization, update this file and then the
resolver/evaluator consumers.

---

## 8. Evaluator architecture

`chatml_eval.ml` evaluates only resolved AST.

### Two runtime namespaces

The evaluator uses:

1. a mutable name environment for globals/modules/builtins
2. a frame stack for local lexical bindings

### Tail calls

The evaluator has:

- `eval_expr`
- internal `eval_expr_tail ~tail`
- `finish_eval`

Tail calls are only produced in genuine tail position.

If you change control-flow constructs, be careful to preserve correct tail
position threading through:

- `if`
- `match`
- let-blocks
- recursion
- sequencing

### Pattern matching

Runtime pattern matching is implemented in `chatml_lang.ml` helpers plus
frame allocation in `chatml_eval.ml`.

If you add a new pattern form, you will need to update both:

- static inference/checking
- runtime matching

---

## 9. Builtin library architecture

### 9.1 Builtin type language

`chatml_builtin_spec.ml` defines the host-side type language for builtins.

It supports:

- type variables
- primitive types
- arrays
- refs
- tuples
- row-based records
- row-based variants
- functions

### 9.2 Builtin runtime values

The same module also holds the builtin list:

- name
- scheme
- implementation

### 9.3 Installation

`chatml_builtin_modules.ml` installs builtins into a runtime environment.

### 9.4 Adding a builtin

Typical steps:

1. add the builtin scheme + implementation in `chatml_builtin_spec.ml`
2. ensure the type is expressible in the builtin type language
3. add tests in:
   - `test/chatml_runtime_test.ml`
   - `test/chatml_typechecker_test.ml`
4. update docs if the builtin is part of the documented prelude

---

## 10. Diagnostics layering

There are now three main structured diagnostic layers:

### Parse diagnostics

- module: `chatml_parse.ml`
- source: syntax/lexer problems

### Type diagnostics

- module: `chatml_typechecker.ml`
- source: inference/checking failures

### Runtime diagnostics

- module: `chatml_lang.ml` / `chatml_eval.ml`
- source: dynamic invalid states in user code

Keeping their formatting style aligned is intentional.

---

## 11. Where to change what

### Add syntax

Touch:

- `chatml_lexer.mll` if tokenization changes
- `chatml_parser.mly`
- `chatml_lang.ml`
- `chatml_typechecker.ml`
- `chatml_resolver.ml`
- maybe `chatml_eval.ml`

For type-only syntax such as `type` declarations or checked annotations,
the runtime often needs no direct semantic change, but the resolver still
usually needs to erase or skip the new forms.

### Add a builtin

Touch:

- `chatml_builtin_spec.ml`
- tests
- docs if public

### Change typing rule

Touch:

- `chatml_typechecker.ml`
- maybe docs/tests

### Change runtime semantics

Touch:

- `chatml_eval.ml`
- maybe `frame_env.ml`
- maybe `chatml_slot_layout.ml`
- tests/docs

### Change module/open behavior

Touch both:

- `chatml_typechecker.ml`
- `chatml_eval.ml`

because type-time and runtime behavior must stay aligned.

---

## 12. Main current invariants

Contributors should preserve these invariants:

- evaluator consumes resolved AST only
- ordinary inference variables are acyclic
- recursive types are represented explicitly via `Mu` / `Rec_var`
- recursive type declarations are contractive
- recursive types are not generalized
- recursive bindings are function-only
- local variable addresses are resolver-produced lexical locations
- frame slot accesses must match stored frame layout
- `open` must not silently shadow existing bindings
- equality restrictions must stay aligned between typechecker intent and
  runtime semantics
- conservative record joins for `if` / `match` remain independent from
  recursive type machinery

---

## 13. Recommended contributor workflow

When changing ChatML:

1. update implementation
2. add/update tests
3. update:
   - `chatml-language-spec.md`
   - any focused guide affected by the change

For nontrivial changes, add both:

- typechecker coverage
- runtime coverage

That is especially important for:

- builtins
- resolver changes
- evaluator control-flow changes
- row-polymorphism behavior
