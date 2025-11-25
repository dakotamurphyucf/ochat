# `Chatml_lang` – ChatML interpreter core

This document complements the inline odoc comments found in
`chatml_lang.ml`.  It is meant for *human* readers browsing the repository
who prefer markdown over generated API docs.

> **TL;DR** – *Load the module, build an environment with a few built-ins,
> call [`eval_program`](#eval_program) on a parsed program, and inspect the
> mutated environment to retrieve results.*

---

## 1  Overview

`Chatml_lang` is the reference interpreter for the **ChatML** language – a
small, statically-typed, expression-oriented dialect used internally to
script prompts, smoke-test chat agents, and prototype new features.

Why roll our own language?  Because embedding snippets of OCaml or Lua in
prompt files proved too heavyweight for non-programmers while plain text
string interpolation was not expressive enough.  ChatML sits in the sweet
spot: it is easy to parse, trivially serialisable, and – thanks to this
module – runnable inside any OCaml program without C stubs or external
processes.

The interpreter is split in several passes:

1. **Parsing** (`chatml_parser.ml`) and **lexing** (`chatml_lexer.mll`).
2. **Resolver** (`chatml_resolver.ml`): resolves identifiers and pre-computes
   frame layouts.
3. **Type-checker** (`chatml_typechecker.ml`).
4. **Evaluation** – *this* file.

Only step 4 is documented here; refer to the other `.doc.md` files for the
remaining passes.

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
val create_env : unit -> env
```

Allocates an empty hash-table mapping identifiers to runtime values.
Passing distinct environments to independent scripts provides "module"
isolation.

### `copy_env` – shallow clone an environment

Useful when you need to evaluate code in a sandbox that should not mutate
the parent bindings.

### `eval_program` – execute a ChatML module

```ocaml
val eval_program : env -> program -> unit
```

See the extended example in the inline odoc comment attached to the
function definition.  After running, the environment is updated with every
binding declared by the script.

---

## 4  Examples

### 4.1  Evaluating a simple expression

```ocaml
open Chatochat.Chatml

let () =
  let env = Chatml_lang.create_env () in
  (* Provide a print built-in *)
  Hashtbl.set env ~key:"print" ~data:(VBuiltin (function
    | [ VString s ] -> print_endline s; VUnit | _ -> failwith "arity"));

  (* Parse & resolve *)
  let program =
    "print (\"Hello ChatML!\");"                      (* source *)
    |> Chatml_parser.parse_string                         (* stmt list *)
    |> Chatml_resolver.resolve_module "Main"             (* stmt list, name *)
  in
  Chatml_lang.eval_program env program
```

Expected output:

```text
Hello ChatML!
```

### 4.2  Mutually-recursive functions

```ocaml
let source = {|
let rec even n = if n = 0 then true else odd (n - 1)
and odd  n = if n = 0 then false else even (n - 1)
in
print (if even 13 then "even" else "odd");
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

