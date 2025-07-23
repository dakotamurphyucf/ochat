# `Webpage_markdown.Tool`

Convert any publicly accessible web page to GitHub-flavoured Markdown and expose the action as an OpenAI *function-calling* tool.

---

## 1  Overview

`Webpage_markdown.Tool` is a thin adapter that pairs the declarative schema defined in
`Definitions.Webpage_to_markdown` with a concrete OCaml implementation.  The returned
`Gpt_function.t` value can be bundled with other tools when calling
`Openai.Completions.post_chat_completion`, enabling the language model to ask the host
application for dynamic content retrieval.

Internally the module relies on the following building blocks:

* **`Webpage_markdown.Driver.fetch_and_convert`** – downloads the document and converts
  it to Markdown.  The converter recognises GitHub _blob_ URLs and fetches the raw
  source file for a cleaner representation.
* **`Ttl_lru_cache`** – caches up to 128 recent conversions for 5 minutes to avoid
  re-downloading the same resource.

The cache is process-local and memory-resident – it is discarded when the
application terminates.

---

## 2  API

````ocaml
val register :
  env:Eio_unix.Stdenv.base  ->
  dir:_ Eio.Path.t          ->  (* currently unused *)
  net:_ Eio.Net.t           ->
  Gpt_function.t
````

`register` synthesises a ready-to-use tool instance.  Only `env` and
`net` are actively used; `dir` is kept for signature compatibility with sibling
factories.

When the model issues a call such as

```json
{
  "name": "webpage_to_markdown",
  "arguments": "{ \"url\": \"https://ocaml.org\" }"
}
```

OpenAI will provide the argument string to the OCaml side, which should in turn
execute

```ocaml
let markdown = dispatch "https://ocaml.org" in
(* … forward [markdown] back to the model … *)
```

If the same URL is requested again within five minutes the cached Markdown string
is returned instantly.

---

## 3  Examples

### 3.1  Minimal integration

```ocaml
open Eio.Std

let main env =
  let net  = Eio.Stdenv.net  env in
  let dir  = Eio.Path.cwd (Eio.Stdenv.fs env) in

  (* 1. Register the tool *)
  let wp_to_md = Webpage_markdown.Tool.register ~env ~dir ~net in

  (* 2. Prepare the list passed to OpenAI *)
  let tools_json, dispatch = Gpt_function.functions [ wp_to_md ] in

  (* 3. Use [tools_json] in the chat-completion request … *)
  (* 4. When OpenAI returns { name = "webpage_to_markdown"; arguments = "…" } *)
  let handle_tool_call url_json =
    let markdown = dispatch url_json in
    (* send [markdown] back to OpenAI *)
  in
  ()

let () = Eio_main.run main
```

### 3.2  Programmatic use

You can also bypass OpenAI and use the underlying conversion directly:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let net  = Eio.Stdenv.net env in
  Driver.fetch_and_convert ~env ~net "https://github.com/ocaml/ocaml/blob/trunk/README.md"
  |> print_endline
```

---

## 4  Error semantics

Any exception raised during download or conversion is caught and encoded as a
plain string of the form:

```
Error fetching <url>: <exception-message>
```

The caller is expected to forward that text back to the model or present it to
the user.

---

## 5  Limitations & notes

* The cache is *not* persistent.  Restarting the host process invalidates it.
* Pages that rely heavily on client-side rendering might need the additional
  Chrome-headless fallback implemented in `Driver`; long-running pages are
  aborted after 60 seconds.
* Markdown conversion is heuristic.  Very complex HTML may degrade into a raw
  code block.

---

© No licence or copyright notice required.

