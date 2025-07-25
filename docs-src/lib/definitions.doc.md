# `Definitions`

> Catalogue of *tool* specifications exposed by the Ochat OCaml agent.

This module is **data-only** – it does *not* carry any behaviour.  Each
sub-module is an implementation of `Ochat_function.Def`, a small record
type that mirrors what the OpenAI Function-calling API expects:

```ocaml
module type Ochat_function.Def = sig
  type input

  val name            : string          (* Unique identifier *)
  val description     : string option   (* Shown to the LLM *)
  val parameters      : Jsonaf.t        (* JSON-schema of `input` *)
  val input_of_string : string -> input (* Decoder used by runtime *)
end
```

`Definitions` packages a **curated set** of such specs so higher-level
components – e.g. `Chat_completion` – can offer them to the language
model and later re-hydrate the call into a strongly-typed value.

---

## Quick start

1. Pick the sub-module that corresponds to the capability you want
   (e.g. `Definitions.Webpage_to_markdown`).
2. Implement the runtime logic:

   ```ocaml
   let run url = Webpage_markdown.fetch url in
   let tool = Ochat_function.create_function (module Definitions.Webpage_to_markdown) run
   ```
3. Aggregate the tools and pass their **metadata** to the OpenAI API:

   ```ocaml
   let function_info, dispatch_table = Ochat_function.functions [ tool ] in
   openai_request ~tools:function_info |> ignore;
   ```

4. When the API returns `{ "name": "webpage_to_markdown", "arguments": ... }`
   look up the entry in `dispatch_table` and execute it.

---

## Sub-modules

Below is a concise reference.  For the *exact* JSON schema consult the
`parameters` value in each sub-module.

| Tool | `input` OCaml type | Synopsis |
|------|--------------------|----------|
| **Get_contents** | `string` | Return the contents of a local file |
| **Odoc_search** | `(string * int option * string option * string)` | Semantic search over locally-indexed odoc docs |
| **Fork** | `{ command : string; arguments : string list }` | Spawn a fully-isolated helper agent |
| **Webpage_to_markdown** | `string` | Download an URL and convert it to Markdown |
| **Add_line_numbers** | `string` | Prefix each line of a text block with its index |
| **Get_url_content** | `string` | Fetch the raw body of a remote resource |
| **Index_ocaml_code** | `(string * string)` | Embed OCaml sources into a vector DB |
| **Query_vector_db** | `(string * string * int * string option)` | Search the vector DB for relevant snippets |
| **Apply_patch** | `string` | Apply a V4A diff/patch to the workspace |
| **Read_directory** | `string` | List entries of the given directory |
| **Make_dir** | `string` | Create a new directory |

---

## Known limitations

* This module assumes **well-formed** JSON input.  Each `input_of_string`
  raises if mandatory fields are missing; callers should catch
  exceptions and surface a graceful error to the user.
* The catalogue is opinionated and targets the needs of the Ochat
  agent in this repository.  Feel free to fork and extend.



