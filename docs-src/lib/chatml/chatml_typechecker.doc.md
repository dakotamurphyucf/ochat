# `Chatml_typechecker` – Hindley–Milner type-checker for ChatML

This document complements the inline odoc comments found in
`chatml_typechecker.ml`.  It is meant for *human* readers browsing the
repository who prefer Markdown over generated API docs.

---

## 1  Overview

`Chatml_typechecker` implements a classic Hindley–Milner (HM) type-inference
algorithm (Algorithm W) extended with row polymorphism for records and
variants.  It operates directly on the abstract-syntax tree produced by the
ChatML parser and resolver and can be executed independently from the
interpreter.

Running the checker:

* surfaces mistakes early (e.g. field/variant mismatches, arithmetic on
  strings);
* improves editor integration by providing on-hover type information; and
* enables smarter optimisation passes in the future.

---

## 2  Public API

### `infer_program`

```ocaml
val infer_program : program -> unit
```

1. Allocates invocation-local inference state (counters, level stack, span table).
2. Performs type inference on the supplied program.
3. Prints either `"Type checking succeeded!"` or a formatted error message
   that includes the faulty source excerpt.

Language type errors are formatted by this convenience entrypoint. Hosts should
use `check_program` or `check_program_with_surface` for structured diagnostics.
Inference state is local to a compilation; concurrent compilations do not share
mutable type variables or a global span table. Host checkpoint exceptions propagate.

### `type_lookup_for_program`

```ocaml
val type_lookup_for_program : program -> Source.span -> typ option
```

Creates a *snapshot* of the span-to-type mapping for the given program and
returns a lookup closure.  The closure is pure: further calls to
`infer_program` will not invalidate it.

This is the recommended entry-point for editor integrations.

---

## 3  Type system at a glance

| Feature                 | Syntax example          | Notes |
| ----------------------- | ----------------------- | ----- |
| Polymorphic functions   | `let id = fun x -> x`   | `id : 'a -> 'a` |
| Records                 | `{ foo = 1; bar = 2 }`  | Row polymorphic (open rows) |
| Variants                | `` `Some(3) ``           | Row polymorphic |
| Arrays                  | `[1, 2, 3]`              | Homogeneous |
| References              | `ref(42)`                | Mutable cell |

The built-in environment ({!init_env} in the source) contains a small set of
primitives such as arithmetic operators and a `print` function.  New
non-expansive bindings introduced by `let` can be generalised. The value
restriction prevents polymorphic mutable storage; bindings containing recursive
types remain monomorphic.

---

## 4  Example usage

```ocaml
let source = {|let double x = x + x
let answer = double(21)|}

let program = Chatml.Chatml_parse.parse_program_exn source

let () =
  match Chatml_typechecker.check_program program with
  | Ok _checked -> print_endline "Type checking succeeded"
  | Error diagnostic ->
    print_endline (Chatml_typechecker.format_diagnostic source diagnostic)
```

The checker accepts the parsed AST. The compiler subsequently uses its type
information during resolution; the evaluator executes the resolved AST.

---

## 5  Limitations & future work

* Exhaustiveness and redundancy checks are conservative; they do not prove
  arbitrary conditions or relationships between guards.
* **No effect tracking**: references are supported but the type system does
  not track mutability or aliasing.
* **Error messages** are decent but still lack hinting (e.g. missing
  record fields suggestions).

Contributions welcome!


### Host-required entrypoint types

`check_program_with_surface` accepts optional `required_bindings` as a list of
binding names and `Chatml_builtin_spec.ty` contracts. After normal inference it
checks the final bindings against those types, sharing type variables across the
entire contract. This lets a host relate `initial_state` to a handler's state
argument/result and enforce exact function arity without running source code.
Expected types come from the host, so source aliases or a later incompatible
binding cannot bypass the check. The generic host compiler forwards this option.

See [extension compiler surfaces](../../agent-server/extensibility-foundations.md#static-script-contracts)
for the one-off and standalone tool contracts and their current integration limits.

### Recursive types and cancellation

Explicit recursive aliases use `Mu` binders; inferred recursive data types use
cyclic type-variable graphs. Unification tracks active comparison pairs, including
their binder environments, to terminate when a recursive structure refers back to
an obligation already being checked. The assumptions are local to the current
path. Sibling fields still check their constraints after earlier fields bind
mutable type variables. Comparing different recursive payloads or different
enclosing binders continues to fail.

For example, a function returning `Null` or `Array` of recursive results can be
inferred and checked against the host's `json` contract without a result annotation:

```ocaml
let rec tree n =
  if n == 0 then `Null
  else let child = tree(n - 1) in `Array([child, child])
let main input = Task.pure(tree(2))
```

`check_program_with_surface` accepts an optional host `checkpoint`. Inference,
instantiation, generalization and unification invoke it periodically. With no
checkpoint, the typechecker imposes no resource policy. The Eio compiler service
uses it to observe caller cancellation and elapsed-time budgets; work between
checkpoints is still cooperative. Compilation never evaluates the function above.
