Prompt_intf – Structured prompts for LLM calls
================================================

`Prompt_intf` is a tiny utility module that formalises what a *prompt* is in
the context of the **meta-prompting** pipeline shipped with OChat.  It gives
callers a *typed* representation instead of raw strings so that:

* individual sections can be manipulated independently (e.g. prepend a system
  directive, inject trace-ids, attach long examples as foot-notes …),
* the final assembly logic lives in one place, ensuring a consistent textual
  layout across the whole code-base.

Unlike `Prompt_session.t`—which stores past interactions—`Prompt_intf.t` is
dedicated to building the *next* single prompt to send to the model.

---

Record fields
-------------

````ocaml
type t = {
  header   : string option;          (* optional system / pre-amble section *)
  body     : string;                 (* main user / assistant content       *)
  footnotes: string list;            (* explanatory blocks, separated later *)
  metadata : (string * string) list; (* invisible key/value annotations     *)
}
````

Field details

* **header** – *Optional.* Copied verbatim at the start of the prompt followed
  by a newline.  Useful for ChatGPT-style `system` instructions.  Empty or
  whitespace-only strings are ignored.
* **body** – *Required.* The core instruction and/or conversation that the
  LLM should answer.  The module makes no assumption about the encoding – you
  can pass ChatMarkdown, plain text, JSON, …
* **footnotes** – Zero or more blocks appended after `body` and separated with
  the ASCII ruler `\n---\n`.  Perfect for large examples that would otherwise
  clutter the main message.
* **metadata** – Arbitrary key/value pairs turned into HTML comments at the
  very end of the prompt: `<!-- key: value -->`.  They are ignored by the
  model but extremely handy when debugging logs or storing provenance data in
  vector databases.

---

API reference
-------------

### `make`

```ocaml
val make :
  ?header:string ->
  ?footnotes:string list ->
  ?metadata:(string * string) list ->
  body:string ->
  unit ->
  t
```

Constructs a prompt record.  All optional parameters default to *empty* values
so the minimal invocation is:

```ocaml
let prompt = Prompt_intf.make ~body:"Hello, world!" ()
```

### `to_string`

```ocaml
val to_string : t -> string
```

Collapses a `t` into a single string suitable for transmission to an LLM.  The
layout is:

1. `header` (if any) followed by a newline
2. `body`
3. each item of `footnotes` list separated by `\n---\n`
4. `metadata` rendered as HTML comments, **in reverse insertion order**

Example:

```ocaml
let prompt =
  Prompt_intf.make
    ~header:"system: You are a helpful assistant."
    ~body:"Translate the following text to French: `Good morning`."
    ~footnotes:[ "# Glossary"; "Good morning → Bonjour" ]
    ~metadata:[ "request_id", "42" ]
    ()

let raw = Prompt_intf.to_string prompt in
print_endline raw
```

The printed prompt will be:

```
system: You are a helpful assistant.
Translate the following text to French: `Good morning`.
---
# Glossary
---
Good morning → Bonjour
<!-- request_id: 42 -->
```

### `add_metadata`

```ocaml
val add_metadata : t -> key:string -> value:string -> t
```

Returns a *fresh* value with the `(key, value)` pair inserted *at the front* of
the `metadata` list, which means it will appear *last* when rendered.

```ocaml
let prompt_with_ts =
  prompt
  |> Prompt_intf.add_metadata ~key:"timestamp" ~value:(Time.to_string (Time.now ()))
```

---

Known limitations
-----------------

* No automatic escaping of content – callers must ensure that header, body and
  foot-notes do not inadvertently break the formatting conventions.
* The ordering of metadata is reversed when converted to string (newest last).
  This is intentional but worth remembering.

---

Change log
----------

* **v0.2** (2025-08-05) – Rewrite documentation, clarify concatenation rules.
* **v0.1** – Initial release.

