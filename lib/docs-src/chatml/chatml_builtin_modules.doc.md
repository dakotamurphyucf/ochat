# `Chatml_builtin_modules` – Standard Library Primitives for ChatML

> Location: `lib/chatml/chatml_builtin_modules.ml`

This module defines the *built-in* functions and operators that are
automatically made available to every ChatML program.  The helpers are
implemented as OCaml closures wrapped in the [`VBuiltin`] constructor
defined in `Chatml_lang`.  They perform dynamic checks at run-time and
raise `Failure _` with a descriptive error message when the arguments do
not satisfy the expected shape.

## Quick start

```ocaml
open Chatml.Chatml_lang

let env = create_env () in
Chatml.Chatml_builtin_modules.BuiltinModules.add_global_builtins env;

(* REPL-style interactions *)
let () =
  (* to_string *)
  match find_var env "to_string" with
  | Some (VBuiltin f) ->
      let VString s = f [ VInt 42 ] in
      assert (String.equal s "42")
  | _ -> assert false

  (* arithmetic – mixed int/float *)
  ; match find_var env "+" with
    | Some (VBuiltin f) ->
        let VFloat r = f [ VInt 1; VFloat 2.5 ] in
        assert (Float.equal r 3.5)
    | _ -> assert false

  (* printing *)
  ; match find_var env "print" with
    | Some (VBuiltin f) -> ignore (f [ VString "hello" ])
    | _ -> ()
```

## Exported symbols

| Name      | Arity                | Behaviour |
|-----------|----------------------|-----------|
| `print`   | variadic             | Prints each argument’s `to_string` representation separated by a space and appends `\n`; returns `()` |
| `to_string` | 1 | Converts a value to its textual representation |
| `sum`     | variadic – **int**   | Returns the sum of all integer arguments. Fails on non-integers. |
| `length`  | 1 – **array**        | Returns the size of the input array. |
| `+` `-` `*` `/` | 2 – numeric (`int`/`float`, mixed allowed) | Standard arithmetic. Division by zero fails. Mixed `int`/`float` promotes to `float`. |
| `<` `>` `<=` `>=` | 2 – numeric    | Numeric comparisons. |
| `==` `!=` | 2 – `int`, `float`, `bool`, `string` | Equality / inequality. |

## Known limitations

* **No polymorphism** – every helper performs explicit run-time checks
  and supports only the documented types.
* **`sum`** currently deals with integers only – floating-point support
  might be added in the future.
* **`print`** is implemented via `Printf.printf`, hence output is sent
  to `stdout` with no buffering guarantees.

## Implementation notes (for contributors)

`add_global_builtins` mutates the supplied environment.  Callers should
always allocate a fresh one to avoid accidental shadowing.  The function
is intentionally kept in a separate compilation unit so that it can be
linked by tooling (unit tests, REPL, etc.) without introducing a
dependency on the full interpreter pipeline.

The helper [`value_to_string`] centralises the textual conversion logic
and is used both by `print` and by unit tests across the code-base.

---

Generated automatically following the guidelines in
`<ocaml-documentation-guidelines>`.

