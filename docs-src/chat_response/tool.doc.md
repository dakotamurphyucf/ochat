# Tool – Bridging ChatMarkdown and function calls

This document complements the in-code odoc comments.  It focuses on the
big picture and provides examples that are inconvenient to keep in the
source file.

## Overview
`Tool` converts a ChatMarkdown [`<tool …/>`](../chatmd/README.md) element
into a [`Gpt_function.t`](../../gpt_function/gpt_function.mli) – the
structure expected by the {i function-calling} variant of OpenAI’s
chat/completions endpoint.

Internally the helper recognises {b four} back-ends:

| Kind | XML snippet | Runtime representation |
|------|-------------|------------------------|
| Built-in | `<tool name="fork"/>` | OCaml function from {!module:Functions} |
| Shell wrapper | `<tool command="grep" name="grep"/>` | `Eio.Process.spawn` |
| Agent | `<tool agent="./sentiment.chatmd" name="sentiment"/>` | Recursively runs driver |
| MCP remote | `<tool mcp_server="https://tools.acme.com" name="sum"/>` | `Mcp_client` over HTTP |


## API cheatsheet
Function summary (see in-code docs for details):

| Function | Purpose |
|----------|---------|
| `convert_tools` | Thin copy between `Openai.Completions` and `Openai.Responses`. |
| `custom_fn` | Wrap an arbitrary shell command (timeout + output cap). |
| `agent_fn` | Execute a nested ChatMarkdown agent prompt. |
| `mcp_tool` | Discover remote tools and apply a TTL-LRU cache. |
| `of_declaration` | Front-door dispatcher used by `Driver` & `Converter`. |


## Usage example

### Converting `<tool>` declarations into a request payload

```ocaml
(* [decls] is a list of ChatMarkdown AST nodes extracted from the prompt *)
let gpt_fns =
  List.concat_map decls ~f:(Tool.of_declaration ~sw ~ctx ~run_agent)

(* Build the JSON payload for OpenAI.  The helper also returns a
   lookup table mapping function names to OCaml closures. *)
let comp_tools, _tbl = Gpt_function.functions gpt_fns in
let request_tools      = Tool.convert_tools comp_tools in

Responses.post_response ~model:"gpt-4o-mini" ~tools:request_tools body
```


## Cache invalidation strategy

MCP servers are polled at most every *[cache_ttl]* seconds.  However, the
server may push a `notifications/tools/list_changed` message when a tool
is added or removed.  The helper [`register_invalidation_listener`] keeps
an ear on that channel and flushes the TTL-LRU entry so that the next
prompt reflects the new tool list.


## Limitations & warnings
* **Security** – `custom_fn` executes arbitrary binaries with user input.
  Do not enable on a multi-tenant server.
* MCP tool discovery ignores pagination and assumes a single RPC call
  returns the full list.
* Timeout (60 s) and output cap (100 KiB) are hard-coded.

