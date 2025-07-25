# `Chatml_resolver`

Lexical-address resolver and slot allocator for the ChatML language.

---

## Overview

The **resolver** is the second static pass of the ChatML tool-chain,
sitting between the type-checker and the evaluator / compiler.
Its goal is to turn an *abstract* program – where variables are
identified by name and bindings know nothing about their run-time
layout – into a *concrete* one that can be executed in O(1) without any
name lookup or dynamic slot discovery.

It performs two independent but complementary tasks:

1. **Lexical-addressing** – replace every `EVar "x"` by
   `EVarLoc { depth; index; slot }` where:
   * `depth` = how many frames to pop (0 = current frame),
   * `index` = position inside that frame, and
   * `slot`   = a [`Frame_env.slot`](./frame_env.doc.md) describing the
     low-level OCaml representation.

2. **Slot selection** – pick the most specialised slot constructor
   (`SInt`, `SBool`, `SFloat`, `SString`, `SObj`) for every variable and
   function parameter.  The choice follows a small heuristic:
   * If the node has a principal type recorded by the
     type-checker → use it.
   * Otherwise fall back to a literal-based guess (`42` ⇒ `SInt`,
     `"foo"` ⇒ `SString`, …).

The pass is *purely structural*: the transformed AST is semantically
equivalent to the input.  Its only purpose is to give later passes
enough metadata to allocate frames once and to access them without
hash-tables or lists.

---

## Public API

### `resolve_program : Chatml_lang.program -> Chatml_lang.program`

* Accepts a parsed **and** type-checked program.
* Emits an equivalent program where:
  * variable occurrences are transformed into `EVarLoc`,
  * binding nodes carry a list of `packed_slot`,
  * consecutive non-recursive `let` bindings are merged into a single
    `ELetBlockSlots` so that the evaluator creates one frame instead of
    many.

```ocaml
open Chatml
open Chatml_resolver

let parsed : Chatml_lang.program = (* obtained from the parser *)
let typed  : Chatml_lang.program = Chatml_typechecker.type_check_program parsed in
let resolved = resolve_program typed in
Format.printf "%a@." Chatml_lang.pp_program resolved;
```

### `eval_program : Chatml_lang.env -> Chatml_lang.program -> unit`

One-liner that performs

```ocaml
Chatml_lang.eval_program env (resolve_program prog)
```

Use it when you do not need to inspect the resolved AST.

---

## How resolution works

1. **Frame-stack simulation** – the resolver keeps an explicit stack of
   [`Hashtbl`](https://ocaml.janestreet.com/ocaml-core/latest/doc/core/Core/Hashtbl/)
   whose lifetime mirrors the lexical scopes it visits.  Each hash-table
   maps variable names to `{ index; slot }` pairs.

2. **Traversal** – the core of the pass is the mutually-recursive
   `resolve_expr` / `resolve_stmt` functions that walk the AST,
   maintaining the stack, computing slots and populating the enriched
   nodes on the fly.

3. **Type lookup** – instead of peeking at the type-checker’s global
   table directly, the resolver asks the checker for a
   *pure* `span -> typ option` closure which it threads through the
   traversal.  The indirection avoids dependency cycles and keeps the
   checker’s mutable state private.

4. **Merging `let` blocks** – nested `let`–`in` chains that belong to
   the same source block are merged into one `ELetBlockSlots`.  The
   optimisation saves one `Frame_env.alloc`/`push`/`pop` trip per
   binding at run-time.

---

## Examples

### Simple `let` binding

```ocaml
let ast = parse "let x = 1 in x + 1" in
let ty  = Chatml_typechecker.type_check_program ast in
match resolve_program ty with
| ( [ { value = Chatml_lang.SExpr { value = expr; _ }; _ } ], _ ) ->
  Format.printf "%a@." Chatml_lang.pp_expr expr
| _ -> assert false
```

emits (simplified):

```text
ELetBlockSlots
  ([ "x", EInt 1 ], [ Slot SInt ],
   EVarLoc { depth = 0; index = 0; slot = Slot SInt })
```

### Lambda parameters

```ocaml
(* fun a b -> a +. b *)
```

is turned into

```
ELambdaSlots (["a"; "b"], [Slot SFloat; Slot SFloat], <body>)
```

where each float parameter gets its dedicated `SFloat` slot.

---

## Limitations & future work

* Row-polymorphic record patterns receive only `SObj` slots because the
  current heuristic does not inspect individual fields.
* Slot selection is *best-effort*.  If the type-checker cannot infer a
  monomorphic type the resolver falls back to `SObj`, which forces the
  evaluator to perform dynamic checks.
* Variant patterns do not propagate fine-grained slot information to
  their arguments yet.

---

## Internal helpers (for maintainers)

The code contains several local utilities that are not part of the
public surface:

* `frame_map` – alias for a `string -> slot_info` hash-table.
* `with_frame` – RAII-style wrapper that pushes a frame on entry and
  guarantees pop in the `ensure` clause.
* `choose_slot` – small heuristic that selects a slot using the principal
  type when available and falls back to a literal inspection.

They are documented inline and should remain private – do **not** rely
on them from outside the resolver.



