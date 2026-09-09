# `Chatml_lang` – ChatML interpreter core

This document complements the inline odoc comments found in
`chatml_lang.ml`.  It is meant for *human* readers browsing the repository
who prefer markdown over generated API docs.

> **TL;DR** – *Load the module, build an environment with a few built-ins,
> call [`eval_program`](#eval_program) on a parsed program, and inspect the
> mutated environment to retrieve results.*

---

## 1  Overview

`Chatml_lang` defines syntax, runtime values and environment helpers for
**ChatML**, a statically typed, expression-oriented language. The evaluator is
implemented separately in `Chatml_eval`; task effects are interpreted by the
owning host. Runtime closures and host functions are not serializable values.

The interpreter is split in several passes:

1. **Parsing** (`chatml_parser.ml`) and **lexing** (`chatml_lexer.mll`).
2. **Type-checker** (`chatml_typechecker.ml`).
3. **Resolver** (`chatml_resolver.ml`): resolves identifiers and pre-computes
   frame layouts using inferred types.
4. **Evaluation** (`chatml_eval.ml`).

This page describes the values and environments used by evaluation; refer to the
other `.doc.md` files for the compiler passes.

---

## 2  Key types

### `pattern`
The syntactic patterns accepted by `match` expressions.  Supported
constructs: literals, variables, wildcards, variants, records (open or
closed).

### `expr`
Untyped core language expressions.  The AST produced by the parser uses
variants such as `ELambda`, `EApp`, `EMatch`, …

Variants ending with `*Slots` are introduced by the resolver and embed the
exact slot layout (see below) needed by the evaluator.  Users never build
them manually.

### `value`
Runtime values handled by the interpreter.  The set mirrors OCaml’s core
types plus variants, records, arrays and references.

`VBuiltin` is a convenient escape hatch: wrap an OCaml function

```ocaml
(value list -> value)
```

to expose it as a first-class function inside ChatML.

### Frames and slots

Evaluating a `let` or a lambda allocates an **activation frame** whose size
and memory layout are dictated by a list of **slots** (`Frame_env.packed_slot`).
Slots enable unboxed storage of `int`, `bool`, `float`, `string` when the
shape of the value is known in advance.  When it is not, we fall back to
`SObj` which stores an `Obj.t` pointer.

---

## 3  Public API

### `create_env` – create a fresh module environment

```ocaml
val create_env : ?control:execution_control -> unit -> env
```

Allocates an environment containing a hash table of binding cells and an optional
host execution control. Use `define_var`, `find_var` and `update_var` to access
bindings. A fresh environment gives each program its own globals.

Execution control supplies checkpoints, allocation accounting, builtin admission,
value checks and host-effect boundaries. Before dispatch, the host runtime reports
the operation name and whether it is spawned; returned values pass the after-effect
hook before debug rendering or continuation use. With no control, the evaluator
installs no resource budgets.
These callbacks implement host policy; they do not grant tools or permissions.

### `copy_env` – shallow clone an environment

Copies the binding table while retaining existing binding cells and the same
execution control. Closures therefore preserve lexical sharing and the caller's
budget. This shallow copy is not isolation from mutations to shared cells.

<a id="eval_program"></a>

### `eval_program` – execute a ChatML module

```ocaml
val eval_program : env -> program -> unit
```

This function belongs to `Chatml_eval` and accepts a resolved program. After
running, the environment is updated with the bindings declared by the script.

---

## 4  Examples

### 4.1  Evaluating a simple expression

```ocaml
open Chatml

let () =
  let env = Chatml_lang.create_env () in
  (* Provide a print built-in *)
  Chatml_lang.define_var env "print" (VBuiltin (function
    | [ VString s ] -> print_endline s; VUnit | _ -> failwith "arity"));

  (* Parse & resolve *)
  let program =
    "print(\"Hello ChatML!\")"
    |> Chatml_parse.parse_program_exn
    |> Chatml_resolver.resolve_program
  in
  Chatml_eval.eval_program env program
```

Expected output:

```text
Hello ChatML!
```

### 4.2  Mutually-recursive functions

```ocaml
let source = {|
let rec even n = if n == 0 then true else odd(n - 1)
and odd n = if n == 0 then false else even(n - 1)
let result = if even(13) then "even" else "odd"
|}
```

Thanks to the resolver pass, `even` and `odd` live in the same frame and
see each other during evaluation.

---

## 5  Limitations and future work

1. **No GC interaction awareness** – storing `Obj.t` pointers means the
   collector cannot move values.  This is currently fine but prevents more
   exotic optimisations.
2. **No exception handling** – runtime errors abort the whole evaluation.
   Adding `try … with` support would require extending both the AST and the
   evaluator.
3. **No ahead-of-time optimisation** – evaluation is tree-walk.  A
   bytecode or LLVM backend could give nice speed-ups for heavy scripts.

Feel free to file issues or open PRs if you run into the above!

---

**Happy hacking 🦑**
