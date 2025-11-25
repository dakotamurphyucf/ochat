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

1. Resets all internal mutable state (counters, level stack, span table).
2. Performs type inference on the supplied program.
3. Prints either `"Type checking succeeded!"` or a formatted error message
   that includes the faulty source excerpt.

The checker never raises a user-visible exception — all errors are captured
and reported through the message channel instead.  The global span table is
populated as a side effect and can later be queried by IDE tooling.

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
| Variants                | `` `Some 3 ``            | Row polymorphic |
| Arrays                  | `[1;2;3]`               | Homogeneous |
| References              | `ref 42`                | Mutable cell |

The built-in environment ({!init_env} in the source) contains a small set of
primitives such as arithmetic operators and a `print` function.  New
bindings introduced by `let` are automatically generalised.

---

## 4  Example usage

```ocaml
open Chatml

(* 1.  Parse some ChatML source. *)
let src = """
  let double = fun x -> x + x in
  double 21
""" in

let prog =
  src
  |> Lexing.from_string
  |> Chatml_parser.program Chatml_lexer.read
  |> Chatml_resolver.resolve_program  (* optional resolver pass *)
in

(* 2.  Run the type-checker. *)
Chatml_typechecker.infer_program prog;

(* 3.  Query the type of an arbitrary span (here the whole program). *)
let lookup = Chatml_typechecker.type_lookup_for_program prog in
match lookup (snd prog) with
| Some ty -> Format.printf "Program type: %s@." (Chatml_typechecker.show_type ty)
| None    -> Format.printf "No type information available.@."
```

> **Note:** The example assumes that the resolver pass has already been run
> so that the AST carries slot information.  The type-checker itself does
> not require it but the subsequent interpreter does.

---

## 5  Limitations & future work

* **No exhaustiveness check**: `match` expressions are typed but not
  checked for completeness.
* **No effect tracking**: references are supported but the type system does
  not track mutability or aliasing.
* **Error messages** are decent but still lack hinting (e.g. missing
  record fields suggestions).

Contributions welcome!

