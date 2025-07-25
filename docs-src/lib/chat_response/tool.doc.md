# Tool – Bridging ChatMarkdown and function calls

This document complements the in-code odoc comments.  It focuses on the
big picture and provides examples that are inconvenient to keep in the
source file.

## Overview
`Tool` converts a ChatMarkdown [`<tool …/>`](../chatmd/README.md) element
into a [`Ochat_function.t`](../../ochat_function/ochat_function.mli) – the
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
High-level summary (refer to the in-code odoc comments for the
canonical specification):

| Function | Role | Key parameters |
|----------|------|---------------|
| `convert_tools` | Convert the minimal `Openai.Completions.tool` records into the richer `Openai.Responses.Request.Tool.t` form expected by the *chat/completions* endpoint. | – |
| `custom_fn` | Wrap an arbitrary shell command so that it can be invoked through the function-calling API. | `env` – Eio standard environment; `command` – binary to execute; `name`/`description` – exposed to the model. |
| `agent_fn` | Run a nested ChatMarkdown agent prompt from within the current conversation. | `ctx` – shared execution context; `run_agent` – callback that starts a fresh driver. |
| `mcp_tool` | Convert an `<tool mcp_server="…"/>` declaration into one `Ochat_function.t` per remote tool, using a 5-minute TTL-LRU cache and passive invalidation via server notifications. | `sw` – parent switch; `ctx` – execution context; `mcp_server` – URI of the MCP endpoint. |
| `of_declaration` | Single front-door dispatcher that maps any `<tool …/>` element to its runtime implementation (may return several functions). | `sw`, `ctx`, `run_agent`, `decl`. |

The next section drills deeper into signatures, invariants, and
example invocations.

## Function reference

### `convert_tools`

```ocaml
val convert_tools : Openai.Completions.tool list -> Res.Request.Tool.t list
```

Pure field-by-field copy. Complexity O(n).

### `custom_fn`

```ocaml
val custom_fn : env:Eio.Stdenv.t -> CM.custom_tool -> Ochat_function.t
```

- Accepts JSON input `{ "arguments": string array }`.
- Hard timeout: **60 s** (configurable only via code change).
- Output capped at **100 KiB** – long output is truncated with `…truncated`.

Example:

```ocaml
let grep = custom_fn ~env { name="grep"; description=None; command="grep" } in
Ochat_function.call grep ["-n"; "pattern"; "file.txt"]
```

### `agent_fn`

```ocaml
val agent_fn :
  ctx:_ Ctx.t ->
  run_agent:(ctx:_ Ctx.t -> string -> CM.content list -> string) ->
  CM.agent_tool ->
  Ochat_function.t
```

Input schema `{ "input": string }`. The call spawns a new driver
instance, forwarding only the final answer back to the parent model.

### `mcp_tool`

```ocaml
val mcp_tool :
  sw:Eio.Switch.t ->
  ctx:_ Ctx.t ->
  CM.mcp_tool ->
  Ochat_function.t list
```

Queries the remote `/tools/list` endpoint (or reads from cache) and
converts the advertised metadata into `Ochat_function.t` values. Each
remote tool becomes an individual function.

### `of_declaration`

```ocaml
val of_declaration :
  sw:Eio.Switch.t ->
  ctx:_ Ctx.t ->
  run_agent:(ctx:_ Ctx.t -> string -> CM.content list -> string) ->
  CM.tool ->
  Ochat_function.t list
```

Central dispatcher used by {!module:chat_response.driver} and
{!module:chat_response.converter}.  See in-code docs for full details.


## Usage example

### Converting `<tool>` declarations into a request payload

```ocaml
(* [decls] is a list of ChatMarkdown AST nodes extracted from the prompt *)
let ochat_fns =
  List.concat_map decls ~f:(Tool.of_declaration ~sw ~ctx ~run_agent)

(* Build the JSON payload for OpenAI.  The helper also returns a
   lookup table mapping function names to OCaml closures. *)
let comp_tools, _tbl = Ochat_function.functions ochat_fns in
let request_tools      = Tool.convert_tools comp_tools in

Responses.post_response ~model:"ochat-4o-mini" ~tools:request_tools body
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

