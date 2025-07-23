# ChatML – Lightweight Runtime for Embedded Scripts

ChatML is a *tiny*, dynamically-typed scripting language embedded inside
this repository to support **prompt-time logic** (think "templating on
steroids") and unit-testing.  On the spectrum between Bash, OCaml, and
Lua it sits closest to Lua: a single file runtime (~1.4 kLOC), no I/O in
the core, and a small FFI of built-ins exposed by the host.

The language is *experimental* and not meant for end-users yet; the
primary goal today is to make the evaluator easy to reason about and thus
suited for writing deterministic test fixtures.

Table of contents

1. Why another DSL?
2. High-level architecture
3. Surface syntax  
   3.1  Expressions  
   3.2  Patterns & match  
   3.3  `let` / `let rec` blocks  
   3.4  Control-flow (`if`, `while`, sequencing)  
   3.5  Records & variants (row-polymorphic)
4. Static passes – resolver & typechecker
5. Runtime & tail-call trampoline
6. Built-in modules
7. Future roadmap

---

## 1  Why another DSL?

* **Self-contained** – no external interpreter dependency; runs directly in
  OCaml tests and CLI tools.
* **Deterministic evaluation** – no I/O, no global state beyond an
  explicit environment passed in.
* **Embeddable** – bytecode fits inside a single OCaml module; the host can
  expose functions as easily as `Frame_env.add_external`.

## 2  Architecture overview

```
┌──────────────┐    tokens   ┌────────────┐    AST     ┌────────────┐
│ chatml_lexer │ ──────────▶ │ chatml_par │ ──────────▶│ resolver   │
└──────────────┘             │ ser        │            └────────────┘
        ▲                    └────────────┘                    │
        │                     resolved AST                     ▼
        │                                                   ┌──────────┐
        │                         type errors               │ type-chk │
        └───────────────────────────────────────────────────▶└──────────┘
                                                                │
                                                   ill-typed ✘ │
                                                                ▼
                                                         ┌────────────┐
                                                         │ evaluator  │
                                                         └────────────┘
```

1. **Lexer / parser** – Menhir grammar closely resembles OCaml’s core
   syntax (subset).
2. **Resolver** – converts variable *names* to *lexical addresses* and
   assigns a *slot layout* per stack frame, enabling a flat array-based
   runtime instead of hash-tables.
3. **Typechecker** – Hindley-Milner-style with row-polymorphic records and
   variants.  Optional: the evaluator can run untyped programs for quick
   prototyping.
4. **Evaluator** – small-step, tail-recursive, uses a trampoline to avoid
   leaking OCaml stack frames on deep recursion.

## 3  Surface syntax (BNF excerpt)

```
expr ::= INT                       | BOOL
       | STRING                    | IDENT
       | expr expr                (* application           *)
       | fun IDENT* -> expr       (* lambda                *)
       | let IDENT = expr in expr
       | let rec IDENT = expr in expr
       | if expr then expr else expr
       | while expr do expr done
       | { field = expr; … }     (* records – open rows w/ _ *)
       | Variant expr*            (* polymorphic variant    *)
       | match expr with pattern -> expr | …

pattern ::= _ | IDENT | INT | BOOL | STRING
          | Variant pattern*       | { field = pattern; … [_] }
```

Whitespace and comments (`(* … *)`) follow OCaml rules.

### Record extension & update

```chatml
let p = { x = 1; y = 2 } in
let p' = { p with y = 3; z = 4 } in
p'.z                 (* ⇒ 4 *)
```

### Arrays & mutable references

Mutable state exists but is explicit:

```chatml
let r = ref 0 in
  r := !r + 1;
  !r                     (* ⇒ 1 *)
```

## 4  Static passes

* **Resolver** (`chatml_resolver.ml`) walks the AST replacing each variable
  occurrence with a `[depth, index]` pair pointing to a slot in a specific
  frame.  This eliminates string comparison at runtime.
* **Typechecker** (`chatml_typechecker.ml`) supports HM inference, `let
  polymorphism`, row polymorphic records & variants, and *escape* analysis
  for references.  The pass can be skipped; in that case the evaluator
  assumes the program is well-typed.

## 5  Runtime & tail-call trampoline

The evaluator (`chatml_lang.ml`) stores frames as `value array`s.  Every
call is either:

* a **direct tail call** – compiled into a loop by swapping the current
  frame pointer and expression pointer; or
* an **indirect tail call** – handled via an explicit `Cont` variant.

As a consequence the OCaml call-stack remains bounded even for programs
with unbounded recursion depth.

## 6  Built-in modules

`chatml_builtin_modules.ml` exposes a minimal standard library compiled at
start-up:

| Name       | Kind      | Description                              |
|------------|-----------|------------------------------------------|
| `Int`      | module    | `add`, `sub`, `mul`, `div`, `mod` …      |
| `Float`    | module    | `pi`, `sin`, `cos`, `pow`, comparisons   |
| `Array`    | module    | `make`, `length`, `get`, `set`, `map`    |
| `String`   | module    | `length`, `concat`, `split`              |
| `Debug`    | module    | `print` (hooks into OCaml `Format.printf`)|

Host applications can inject additional values via:

```ocaml
Frame_env.add_external env ~name:"now" (Value.Float (Eio.Time.now env_clock))
```

## 7  Future roadmap

* **Package system** – ability to `open` external modules.
* **Better error messages** – source-highlighting & hints.
* **Bytecode compiler** – turn AST into a compact vector of opcodes for
  faster cold-start.

---

*Last updated: {{date}}*

