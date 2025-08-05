# `Meta_prompting` – Prompt-generator Functor

> Part of the **ochat.meta_prompting** library

This module contains a single functor – **`Make`** – whose purpose is to turn
an arbitrary *task description* into a ready-to-send prompt.  The functor does
*no* LLM interaction on its own; instead it merely wraps the input in a
consistent Markdown skeleton and performs optional placeholder substitution
using the lightweight `{% raw %}{{variable}}{% endraw %}` syntax provided by
`Template`.

---

## 1 . High-level overview

```
            Task             +  (optional)  Template file
              │                     │
              ▼                     ▼
        to_markdown         load / render
              │                     │
              └───┬─ placeholder map ─┘
                  ▼
              Prompt.make
```

1.  The caller supplies a value of type `Task.t` and the functor uses
    `Task.to_markdown` to obtain a GitHub-flavoured Markdown fragment.
2.  If a template path is given the file is loaded (an `Eio` environment is
    required for filesystem access) and rendered using
    `Template.Make_Template`.  Placeholders are resolved using

    * the user-provided `params : (string * string) list`, followed by
    * a single pair `("TASK_MARKDOWN", <markdown>)` produced automatically.

   When no template is provided the module falls back to the raw Markdown and
   prepends a short default header that reminds the LLM of prompt-writing best
   practices.
3.  The final string becomes the body of the prompt constructed with
   `Prompt.make`.

---

## 2 . Public interface

```ocaml
module Make
  (Task   : sig type t val to_markdown : t -> string end)
  (Prompt : sig
     type t
     val make :
       ?header:string -> ?footnotes:string list ->
       ?metadata:(string * string) list -> body:string -> unit -> t
   end)
: sig
  val generate :
    ?env:< fs : _ Eio.Path.t ; .. > ->
    ?template:string ->
    ?params:(string * string) list ->
    Task.t -> Prompt.t
end
```

### Arguments

* **`Task`** – supplies the concrete task type and a single serialisation
  function.  The functor is polymorphic in `t`; any value that can be rendered
  as Markdown is acceptable.
* **`Prompt`** – abstract constructor for the resulting prompt.  It mirrors the
  type used throughout the *ochat* codebase but keeps the functor independent
  from the exact record definition.

### `generate` – parameters

| Parameter          | Default      | Description |
|--------------------|--------------|-------------|
| `env`              | *required if* `template` is set | An `Eio` *standard environment* providing at least the `fs` capability. |
| `template`         | `None`       | Path to a template file (`.md`, `.txt`, …). |
| `params`           | `[]`         | Extra placeholder bindings; later duplicates override earlier ones. |
| `task`             | —            | The task value to embed in the prompt. |

**Exceptions**

* `Invalid_argument` – raised when `template` is provided but `env` is `None`.

---

## 3 . Usage examples

### Basic usage (no template)

```ocaml
module T = struct
  type t = string
  let to_markdown s = s (* identity – already Markdown *)
end

module P = struct
  type t = { header : string option; body : string }
  let make ?header ~footnotes:_ ?metadata:_ ~body () = { header; body }
end

module G = Meta_prompting.Make (T) (P)

let prompt =
  G.generate "Write a haiku about OCaml";;

(* prompt.body now contains the default header followed by the task text. *)
```

### Using a template with placeholders

Assuming the file `my_template.md` contains

```md
### Task

{{TASK_MARKDOWN}}

### Context

User: {{USER}}
```


```ocaml
let prompt =
  G.generate
    ~env:my_eio_env
    ~template:"my_template.md"
    ~params:[ "USER", "alice@example.com" ]
    "Generate a regex that matches valid IPv4 addresses";;
```

The resulting body will have both `{{TASK_MARKDOWN}}` and `{{USER}}`
substituted.

---

## 4 . Known limitations

* Placeholder syntax is strictly `{{KEY}}`; there is no support for
  conditionals or loops.
* Template loading is blocking; large files may stall the calling fiber.
* The default header is fixed in the source and cannot be customised via the
  public API – pass an explicit template if you need control over the header.

---

## 5 . Related modules

* **`Template`** – minimal string templating with zero external dependencies.
* **`Prompts`** – concrete prompt record used throughout *ochat*.


