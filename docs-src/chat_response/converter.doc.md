# Converter – From ChatMarkdown to OpenAI types

## Purpose

`Converter` transforms the *AST* produced by the ChatMarkdown parser into
values understood by the `openai` OCaml client.  It is the last
step before a request is sent to the OpenAI endpoint.

```
ChatMarkdown XML  ──► Prompt.Chat_markdown.t list  ──► Converter.to_items ──► Openai.Responses.Item.t list
```


## Key entry points

| Function | Description |
|----------|-------------|
| Function | Purpose |
|----------|---------|
| `to_items ~ctx ~run_agent els` | Public façade – converts the whole ChatMarkdown AST into a list of `Openai.Responses.Item.t`. Skips `<config/>` and `<tool/>` blocks which are processed earlier in the pipeline. |
| `string_of_items ~ctx ~run_agent items` | Internal helper used when an argument or a message body contains a list of inline content items (images, nested agents, …). Returns the concatenated textual representation. |


## Example

```ocaml
let ctx = Ctx.create ~env ~dir ~cache in

(* Forward to Chat_response.Driver to avoid module cycles. *)
let run_agent ~ctx prompt_xml inline_items =
  Driver.run_agent ~ctx prompt_xml inline_items

let items : Openai.Responses.Item.t list =
  Converter.to_items ~ctx ~run_agent parsed_elements
```


## Design notes

* **Pure translation** – the module allocates but owns no mutable state.
* **Caching** – repeated `<agent/>` inclusions are looked-up with
  `Cache.find_or_add` ensuring that large sub-prompts are only executed
  once per session.
* **No cross-module cycles** – the `run_agent` callback is injected by
  the caller so that `Converter` remains independent of `Driver`.


## Limitations

The current implementation does not support the complete OpenAI JSON
schema (images are limited to data-URIs, no video support, …).  Those
capabilities can be added incrementally as the need arises.


