# dsl_script – Embedded ChatML sandbox

`dsl_script` is a **self-contained demonstration binary** that ships
with the Ochat tooling repository.  It compiles and executes a
hard-coded snippet of the *ChatML* DSL at start-up, showcasing how to
drive the interpreter pipeline from native OCaml code.

> **Status** : experimental – mainly used as a smoke-test during
> development; the interface is subject to change without notice.

---

## 1 Synopsis

```console
$ dsl_script            # runs the embedded ChatML program
```

The executable takes **no command-line flags** – its behaviour is fully
defined by the source string embedded in
[`bin/dsl_script.ml`](../../bin/dsl_script.ml).

---
•
## 2 What does it do?

1. Allocates a fresh `Chatml_lang.env` with `Chatml_lang.create_env`.
2. Registers the standard library modules via
   `Chatml_builtin_modules.add_global_builtins`.
3. Parses the embedded ChatML source string with the generated lexer
   and parser (`Chatml_lexer` / `Chatml_parser`).
4. Resolves names, type-checks the AST and evaluates it through
   `Chatml_resolver.eval_program`.

If the program prints values (it does, see below) they appear on
standard output; any uncaught exception aborts execution with a
non-zero exit code.

---

## 3 Embedded ChatML program (excerpt)

```chatml
let p = {name = "Alice"; age = 25}

let f p =
    let inc_age person =
        person.age <- person.age + 1;
        person
    in
    print(p.name);
    print(inc_age({p with age = 30 + p.age}))

f(p)

let a = `Some(1, 2)
let b = `None

match a with
| `Some(x, y) -> print([x, y])
| `None       -> print([0])
```

Running `dsl_script` therefore produces something similar to:

```console
Alice
{ name = "Alice"; age = 56 }
[1, 2]
```

---

## 4 Public API (programmatic use)

Although the binary itself is not configurable, the helper function
`Dsl_script.parse` *is* exported by the module and can be reused to
convert arbitrary ChatML source strings into an AST:

```ocaml
open Dsl_script   (* or Bin_dsl_script depending on your build *)

let ast : Chatml_lang.program =
  Dsl_script.parse "print(42)";;

(* You can now pass [ast] to Chatml_resolver.eval_program *)
```

---

## 5 Limitations & notes

* **Hard-coded code** – the binary is *not* a generic ChatML runner; it
  always evaluates the statically embedded snippet.
* **Development artefact** – consider it an example or test harness
  rather than a user-facing tool.
* **No I/O isolation** – all `print` calls in the ChatML program write
  directly to the process’ standard output.

---

## 6 Related modules

* [`Chatml_lang`](../lib/chatml/chatml_lang.doc.md) – core AST and
  interpreter runtime.
* [`Chatml_parser`](../lib/chatml/chatml_parser.doc.md) – Menhir-generated
  parser.
* [`Chatml_resolver`](../lib/chatml/chatml_resolver.doc.md) – name
  resolution, type-checking and evaluation driver.

