# mcp_server – MCP registry wrapper binary

`mcp_server` launches an instance of the in-memory registry from
[`Mcp_server_core`](../lib/mcp/mcp_server_core.doc.md), registers a small set
of built-in tools and turns every `*.chatmd` prompt file in the *prompts*
folder into an agent-backed tool.  The registry is then exposed either over
standard I/O or over HTTP, depending on command-line flags.

---

## 1 Synopsis

```console
$ mcp_server [--http PORT] [--require-auth] [--client-id ID] [--client-secret SECRET]
```

When `--http` is not given, the program reads one JSON-RPC envelope per line
from **stdin** and writes the responses line-delimited to **stdout**.  With
`--http PORT` it instead binds a small HTTP server on `127.0.0.1:PORT` that
implements the transport described in [`Mcp_server_http`](../lib/mcp/mcp_server_http.doc.md).

## 2 Command-line flags

| Flag | Default | Description |
|------|---------|-------------|
| `--http PORT` | *(absent)* | Start a streamable HTTP endpoint on `PORT`. |
| `--require-auth` | `false` | Enable OAuth 2.1 bearer-token validation. |
| `--client-id ID` | *(absent)* | Static client ID accepted by the token endpoint. |
| `--client-secret SECRET` | *(absent)* | Static client secret accepted by the token endpoint. |

## 3 Built-in tools

| Name | Purpose | Source |
|------|---------|--------|
| `echo` | Returns the supplied text verbatim. | Internal demo helper |
| `apply_patch` | Apply a textual V4A diff/patch to the repository. | [`Functions.apply_patch`](../lib/functions/functions.doc.md) |
| `read_dir` | List the contents of a directory. | [`Functions.read_dir`](../lib/functions/functions.doc.md) |
| `get_contents` | Read a file and return its contents. | [`Functions.get_contents`](../lib/functions/functions.doc.md) |
| `webpage_to_markdown` | Download a web page and convert it to Markdown. | [`Functions.webpage_to_markdown`](../lib/functions/functions.doc.md) |
| `meta_refine` | Refine a *meta-prompt* using LLM-backed heuristics. | [`Functions.meta_refine`](../lib/functions/functions.doc.md) |

Every prompt file discovered under the directory referenced by
`$MCP_PROMPTS_DIR` (or `./prompts` when the variable is unset) is also
registered as **two** additional resources:

1. a *prompt* that users can select via `prompts/*` JSON-RPC calls;
2. a *tool* exposing the prompt via `tools/call`.

## 4 Stdio transport

In stdio mode the program expects exactly one JSON value per line.  Each
value is parsed and dispatched to [`Mcp_server_router`](../lib/mcp/mcp_server_router.doc.md).
All emitted JSON values are likewise terminated with a newline so that the
parent process can treat the stream as *line-delimited JSON* (LD-JSON).

```text
stdin        stdout
│            │
▼            ▲
[ JSON ]–––▶ [ JSON ]
```

The loop runs in the main Eio fibre and blocks indefinitely.

## 9 Internal helper functions

The executable defines several helper functions that, while not exported
by any public library, are useful to understand when extending the
server.  All of them reside in `bin/mcp_server.ml` and mutate the
in-memory registry passed as their first argument.

| Function | Purpose |
|----------|---------|
| `setup_tool_echo` | Register the trivial `echo` tool that returns the supplied text verbatim. |
| `register_builtin_apply_patch` | Expose the `apply_patch` tool backed by `Functions.apply_patch`. |
| `register_builtin_read_dir` | Register the `read_dir` tool backed by `Functions.read_dir`. |
| `register_builtin_get_contents` | Register the `get_contents` tool backed by `Functions.get_contents`. |
| `run_stdio` | Launch the line-delimited JSON stdio transport when `--http` is absent. |

Below is a condensed API reference.

### `setup_tool_echo`

```ocaml
val setup_tool_echo : Mcp_server_core.t -> unit
```

Registers the *echo* tool whose handler simply returns the `text` argument
unchanged.

### `register_builtin_apply_patch`

```ocaml
val register_builtin_apply_patch :
  Mcp_server_core.t -> dir:_ Eio.Path.t -> unit
```

Adds the `apply_patch` tool that expects a single `input` field holding a
Unified-V4A patch.  The return value is the patched text.

### `register_builtin_read_dir`

```ocaml
val register_builtin_read_dir :
  Mcp_server_core.t -> dir:_ Eio.Path.t -> unit
```

Registers the `read_dir` tool.  Arguments:

* `path` – file-system path; may be relative to the server’s working directory.

The response is a JSON array listing the directory contents.

### `register_builtin_get_contents`

```ocaml
val register_builtin_get_contents :
  Mcp_server_core.t -> dir:_ Eio.Path.t -> unit
```

Registers the `get_contents` tool.  Arguments:

* `file` – path to the text file.

The entire file contents are returned as a JSON string.

### `run_stdio`

```ocaml
val run_stdio : core:Mcp_server_core.t -> env:Eio.Stdenv.t -> unit
```

Starts the LD-JSON stdio transport.  The function never returns.  It is
only used when the `--http` flag is **not** supplied at start-up.

## 5 HTTP transport

When the `--http` flag is supplied the stdio loop is replaced by a call to
[`Mcp_server_http.run`](../lib/mcp/mcp_server_http.doc.md).  The function
binds a lightweight Piaf server that supports:

* **JSON-RPC 2.0** over `POST /mcp`;
* **Server-Sent Events** for push notifications via `GET /mcp`.

Internally the same registry instance is shared between all transports,
therefore a tool call performed over HTTP is immediately visible to stdio
clients and vice-versa.

## 6 Hot-reloading of prompts and resources

Two background fibres poll the prompts directory and the current working
directory every ten seconds:

* **Prompt polling** – newly added `*.chatmd` files are parsed and registered
  without restarting the server.  Deletions are *not* detected yet.
* **Resource polling** – emits `resources/list_changed` notifications when
  new regular files appear or disappear in the CWD.

Both fibres live under the same switch as the transport (HTTP or stdio), so
they terminate automatically when the main service shuts down.

## 7 Exit codes

| Code | Meaning |
|------|---------|
| `0` | Clean shutdown (Ctrl-C or programmatic stop). |
| `≠0` | Unhandled exception – inspect stderr for the stack trace. |

## 8 Limitations & notes

* Only one transport can be active at a time: specifying `--http` disables
  the stdio loop.
* The HTTP server binds to *localhost* exclusively.  Use a reverse proxy
  if external access is required.
* File-watching relies on cheap polling; a future iteration will switch to
  platform-specific watchers when exposed by Eio.

---

© The documentation is released into the public domain.  No warranties.

