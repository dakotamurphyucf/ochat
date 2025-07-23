# `Gpt_function` – Registering function-calling **tools** for OpenAI

The `Gpt_function` module provides a *very* small abstraction that bridges the
gap between

*a) declarative* tool descriptions expected by the OpenAI Chat Completions API
and

*b) concrete* OCaml functions that execute the requested action.

It is the foundation used across the code-base to expose helpers such as
`read_file`, `apply_patch`, or `odoc_search` to the LLM.

---

## 1 .  Why does it exist?

OpenAI models understand **JSON schemas** and can ask the host program to call
one of the advertised *tools*.  A tool is identified by its `name` and is
described by three extra fields:

* `description` – plain-text summary shown to the model,
* `parameters`  – JSON schema that defines the expected arguments,
* `strict`      – whether extra properties are allowed (default: `true`).

When the model wants to invoke a tool it returns a structure:

```json
{
  "name": "read_file",
  "arguments": "{ \"file\": \"lib/gpt_function.ml\" }"
}
```

The host application is then responsible for 3 steps:

1. Look up the implementation by `name`.
2. Parse the `arguments` string into an OCaml value.
3. Execute the function and feed the *string* result back to the model.

`Gpt_function` makes the process trivial and type-safe.

---

## 2 .  Public API recap

### 2.1  `module type Def`

`Def` is a **declarative module** – it contains *only values* that describe the
tool.

```ocaml
module type Def = sig
  type input
  val name        : string
  val description : string option
  val parameters  : Jsonaf.t          (* JSON schema *)
  val input_of_string : string -> input
end
```

* `input` is the OCaml representation of the decoded arguments.
* `input_of_string` must turn the raw JSON string received from the model into
  an [`input`] value, raising on malformed input.


### 2.2  `create_function`

```ocaml
val create_function
  :  (module Def with type input = 'a)
  -> ?strict:bool
  -> ('a -> string)
  -> t
```

Couples the declarative [`Def`] with its OCaml implementation.  The optional
`~strict` flag maps to the OpenAI `strict` parameter (defaults to `true`).


### 2.3  `functions`

```ocaml
val functions
  :  t list
  -> Openai.Completions.tool list * (string, string -> string) Core.Hashtbl.t
```

Takes a list of registered tools and returns:

* the list to feed into `Openai.Completions.post_chat_completion ~tools`,
* a lookup table `name → implementation` for dispatch at runtime.


---

## 3 .  Full example – “echo”

```ocaml
open Core

module Echo : Gpt_function.Def with type input = string = struct
  type input = string

  let name = "echo"
  let description = Some "Return the given string unchanged"

  let parameters =
    `Object
      [ "type", `String "object"
      ; "properties", `Object [ "text", `Object [ "type", `String "string" ] ]
      ; "required", `Array [ `String "text" ]
      ]

  let input_of_string s =
    Jsonaf.of_string s |> Jsonaf.member_exn "text" |> Jsonaf.string_exn
end

let echo_impl (text : string) = text

let echo_tool   = Gpt_function.create_function (module Echo) echo_impl
let tools, tbl  = Gpt_function.functions [ echo_tool ]

(* later, after the model requests { name = "echo"; arguments = ... } *)
let result =
  let fn = Hashtbl.find_exn tbl "echo" in
  fn "{\"text\":\"Hello\"}"
(* ⇒ result = "Hello" *)
```

---

## 4 .  Implementation notes

* The record type [`t`] is **transparent** – you can access the `info` and
  `run` fields directly if needed (many internal modules do).
* The module purposefully supports *only* `string -> string` functions – this
  matches OpenAI’s requirement that tool outputs are plain strings.


---

## 5 .  Limitations & future work

1. Only a single return type (`string`) is supported.  Structured responses
   would require a second schema describing the *output*.
2. No built-in logging or error handling – implementations are expected to
   raise exceptions or return error strings themselves.
3. Schema validation is *not* performed at registration time; any JSON value
   can be supplied in [`parameters`].


---

