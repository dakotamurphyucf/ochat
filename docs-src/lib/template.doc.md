# `Template` — simple string templating & parsing for OCaml

`ochat.template` is a **minimal, dependency-light** helper for
string interpolation that plays nicely with OCaml’s type system.  Two
functors form the public interface:

* **`Make_Template`** – *render* a template (`{{variable}}` syntax)
  using any user-defined record type.
* **`Make_parser`** – *parse* the fully-rendered output back into a
  structured value, using user-supplied regular expressions.

Although the implementation is <100 lines, the abstractions make it
easy to scale from one-off substitutions to larger, nested
templates—all without reflection or fragile global state.

> **Quick-and-dirty helpers** — For ad-hoc needs the module also
> exposes three standalone functions, mirroring Mustache’s CLI:
>
> * `Template.of_string   : string -> string` – identity.
> * `Template.load        : ?search_dirs:string list -> string -> string` –
>   read a template file, searching `search_dirs` first (defaults to
>   `"."`).
> * `Template.render      : string -> (string * string) list -> string` –
>   substitute `{{key}}` placeholders directly, without going through
>   a `RENDERABLE` module.
>
> These helpers exist primarily for internal modules such as
> `Meta_prompting` but are convenient in scripts where defining a full
> record type would be over-kill.

---

## 1. High-level overview

1. Define a module that satisfies the `RENDERABLE` signature.
   ```ocaml
   module Person = struct
     type t =
       { name  : string
       ; age   : int
       }

     let to_key_value_pairs p =
       [ "name", p.name; "age", Int.to_string p.age ]
   end
   ```

2. Instantiate the functor:
   ```ocaml
   module Person_template = Template.Make_Template (Person)
   ```

3. Create / render:
   ```ocaml
   let t = Person_template.create
     "Hello, {{name}}!  You are {{age}} years old." in
   Person_template.render t { name = "Alice"; age = 42 };
   (* → "Hello, Alice!  You are 42 years old." *)
   ```

4. (Optional) recover the value back from the rendered string by
   providing a `PARSABLE` module and using `Make_parser`.

---

## 2. Public API

### 2.0 One-shot helpers

| Value | Description |
|-------|-------------|
| `of_string : string -> string` | Identity function (convenience wrapper). |
| `load : ?search_dirs:string list -> string -> string` | Read a template file from disk, trying each directory in `search_dirs` (default `"."`). Raises if the file is not found. |
| `render : string -> (string * string) list -> string` | Direct substitution on a raw string (no functor instantiation). |


### 2.1 `module type RENDERABLE`

| Value | Description |
|-------|-------------|
| `type t` | OCaml type representing the domain value. |
| `to_key_value_pairs : t -> (string * string) list` | Convert `t` into the key-value map used during substitution. |

### 2.2 `Make_Template (R : RENDERABLE)`

| Value | Description |
|-------|-------------|
| `type t` | Immutable representation of the raw template string. |
| `create : string -> t` | Wrap a literal string as a template. |
| `render : t -> R.t -> string` | Substitute each `{{key}}` with its value as returned by `R.to_key_value_pairs`. Missing keys are replaced by the empty string. |
| `to_string : t -> string` | Return the raw template string (no substitution). |

### 2.3 `module type PARSABLE`

| Value | Description |
|-------|-------------|
| `type t` | OCaml type reconstructed from the rendered text. |
| `parse_patterns : (string * string) list` | List of (`regex`, `key`) pairs. Each regex **must** contain a single capture group; the captured substring is associated with `key`. |
| `from_key_value_pairs : (string * string) list -> t` | Build the value of type `t` from the collected substrings. |

### 2.4 `Make_parser (P : PARSABLE)`

| Value | Description |
|-------|-------------|
| `parse : string -> P.t option` | Attempt to match *all* patterns.  Returns `Some v` if successful, otherwise `None`. |

---

## 3. Worked example (nested templates)

```ocaml
open Core

module Items = struct
  type t = string list
  let to_key_value_pairs xs =
    let body = List.map xs ~f:(sprintf "- %s") |> String.concat ~sep:"\n" in
    [ "items", body ]
end

module Items_template = Template.Make_Template (Items)

module Person = struct
  type t = {
    name : string;
    age  : int;
    items: string list;
  }

  let items_template = Items_template.create {|items\n-----------\n{{items}}|}

  let to_key_value_pairs p =
    [ "name",  p.name
    ; "age",   Int.to_string p.age
    ; "items", Items_template.render items_template p.items
    ]
end

module PT = Template.Make_Template (Person)

let () =
  let tpl = PT.create {|Hello, {{name}}!\nYour age is {{age}}.\nShopping:\n{{items}}|} in
  print_endline @@ PT.render tpl {
    name = "John"; age = 30; items = ["milk"; "bread"]; }
```

Prints:

```text
Hello, John!
Your age is 30.
Shopping:
items
-----------
- milk
- bread
```

---

## 4. Design decisions & limitations

* **Performance** — substitution relies on a single `Re2.replace_exn`
  call; for small templates this is negligible.  No streaming
  interface is provided.

* **Escaping / conditionals** — out-of-scope on purpose.  Use a full
  feature-rich engine (e.g. *mustache.ml*) if you require loops or
  conditionals.

* **Missing keys** — silently replaced by the empty string.  Wrap
  `render` if you need stricter behaviour.

---

## 5. Related work

* [`mustache`](https://github.com/rgrinberg/ocaml-mustache) — full
  Mustache implementation; heavier but more powerful.
* [`fmt`](https://erratique.ch/software/fmt) — combinator library for
  pretty-printing; not template-string based but worth a look for
  complex formatting tasks.

---

## 6. Changelog

* **v0.1.0** – initial release (render, parse, nested templates).

---

Happy templating! ✨

