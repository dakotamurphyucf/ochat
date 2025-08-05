# ChatMarkdown prompt parsing (`prompt.ml`)

This document complements the inline **odoc** comments inside
`prompt.mli` / `prompt.ml`.  It provides a broader overview, usage
examples, and clarifications that fall outside the scope of API
documentation.

---

## 1. What the module does

* Parses a ChatMarkdown document (a constrained XML dialect) using
  `Chatmd_lexer` + `Chatmd_parser` (Menhir-generated).
* Expands `<import/>` directives recursively, keeping track of the base
  directory.
* Produces a strongly-typed OCaml representation (`top_level_elements
  list`).  Every record derives `jsonaf`, `sexp`, `bin_io`, `hash`, and
  `compare`.

Why that matters: you can pattern-match and transform the prompt safely
instead of juggling `((string * string) list) option` blobs.

---

## 2. Quick start

```ocaml
open Chatmd_prompt   (* public name: Chat_markdown inside prompt.ml *)

let env        = Eio.Stdenv.cwd env    (* anywhere in your app *)
let prompt_dir = Io.ensure_chatmd_dir ~cwd:env in

let raw        = Io.load_doc ~dir:prompt_dir "hello.chatmd" in
let messages   = Chat_markdown.parse_chat_inputs ~dir:prompt_dir raw in

List.iter messages ~f:(function
  | Chat_markdown.User      m -> printf "User: %s\n"  (Option.value ~default:"" m.content)
  | Chat_markdown.Assistant m -> printf "Assistant: ..."                (* etc. *)
  | _ -> ())
```

---

## 3. Selected type cheatsheet

| OCaml Type | ChatMarkdown construct |
|------------|-----------------------|
| `msg` / `user_msg` / `assistant_msg` | `<msg role="…">…</msg>`, `<user>`, `<assistant>` |
| `tool_call_msg` | `<tool_call id="…" function_name="…">JSON args</tool_call>` |
| `tool_response_msg` | `<tool_response id="…">JSON result</tool_response>` |
| `config` | `<config max_tokens="2048" temperature="0.2" />` |
| `tool` | `<tool name="grep" command="grep -nH {{pattern}} {{file}}" />` |
| `reasoning` / `summary` | openAI-style assistant reasoning blocks |

Every record field corresponds 1-to-1 to an attribute or piece of
content in the source document.  See the generated odoc pages for full
details.

---

## 4. `parse_chat_inputs` – algorithm in a glance

1. `Lexing` – leverages `Str` to recognise tags and attributes.
2. `Parsing` – Menhir builds an AST defined by `Chatmd_ast`.
3. `Import expansion` – recursive, duplicates prevented by the call
   stack (no explicit cycle detection yet).
4. `Normalisation` – shorthand tags remapped, stray text filtered.

Execution is {i synchronous}; wrap the call in a separate Eio domain if
the prompt is large (> 100 KB) and you need to keep the UI responsive.

---

## 5. Edge cases & limitations

* Cyclic imports yield a stack overflow (Menhir recursion) – to be fixed
  in a future release.
* Unknown XML fragments inside a recognised element are preserved as raw
  text but are **not** validated.
* Invalid UTF-8 is propagated as-is; ensure the source is normalised
  before feeding it to the parser.

---

## 6. Metadata helpers

`Chatmd_prompt.Metadata` is a lightweight in-process registry for
attaching arbitrary annotations to any value returned by
`parse_chat_inputs`.  Because storing the pairs externally keeps record
types unchanged, you can evolve metadata independently from the wire
format.

| Function | Purpose |
|----------|---------|
| `add elt ~key ~value` | Append a pair to `elt`’s metadata list. Multiple entries with the same key are allowed. |
| `get elt` | Retrieve the list currently associated with `elt` or `None` if none. |
| `set elt kvs` | Replace the list with `kvs`. |
| `clear ()` | Remove **all** metadata; call this between two independent requests in services that hold state. |

The functions mutate global state and therefore are {i not} thread-safe
by design.  If you use metadata from multiple Eio fibres, wrap the calls
in a mutex or reconsider your design.

---

## 7. Re-exported helpers

Although `prompt.ml` itself does not perform I/O, the typical workflow
relies on:

* `Io.load_doc` – load a file as a string.
* `Io.ensure_chatmd_dir` – lazily create the hidden `.chatmd` cache.

Refer to [`io.mli`](../io.mli) for details.


