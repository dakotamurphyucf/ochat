# `Mcp_types` – OCaml bindings for the Model Context Protocol

> *Version modelled: MCP draft 2025-03-26*

This module provides _value types only_ – no runtime logic – for talking to
an MCP-compatible server.  The intention is to cover the **Phase 1** client
implementation that uses a stdio transport.  The schema slice is kept
deliberately small: adding new fields later is a drop-in change because all
records derive [`ppx_jsonaf_conv`](https://github.com/janestreet/jsonaf) codecs
and tolerate unknown JSON keys.

## Overview of the data-model

| Area | Sub-module | What it models |
|------|------------|----------------|
| JSON-RPC 2.0 | `Jsonrpc` | Requests, responses, notifications, error objects |
| Capability negotiation | `Capability` | `tools`, `prompts`, `resources` keys |
| Tool registry | `Tool`, `Tools_list_result` | Metadata returned by `"tools/list"` |
| Tool execution | `Tool_result` | Wrapper around the content returned by a tool invocation |
| Resources API | `Resource` | Minimal metadata to implement `"resources/list"` and `"resources/read"` |

All sub-modules expose immutable record types along with automatically
generated `jsonaf_of_*` / `*_of_jsonaf` functions.  Where a choice had to be
made, the bindings favour **idiomatic OCaml naming** (`snake_case`) and rely
on `[@key "…"]` attributes to keep the on-wire names intact.

## Dependency surface

* **Base/Core** – fundamental container types and utility functions.
* **Jsonaf** – JSON representation (`Jsonaf.t`) and code-generation
  ppx (`jsonaf_conv`).
* **Bin_prot** – binary serialisation via `[@@deriving bin_io]` (used for
  tests and IPC).

These libraries ship with Jane Street’s standard distribution, so no exotic
opam packages are required.

## Sub-module reference

Below is a *brief* reminder of each public item.  See the `mcp_types.mli`
signature for authoritative types.

### `Jsonrpc`

```ocaml
module Jsonrpc : sig
  module Id : sig
    type t = String of string | Int of int
    val of_int    : int    -> t
    val of_string : string -> t
    val ( = )     : t -> t -> bool  (* alias for [Id.equal] *)
  end

  type request
  type response
  type notification
  type error_obj

  val make_request :
    ?params:Jsonaf.t -> id:Id.t -> method_:string -> unit -> request
  val ok     : id:Id.t -> Jsonaf.t -> response
  val error  :
    id:Id.t -> code:int -> message:string -> ?data:Jsonaf.t -> unit -> response
  val notify : ?params:Jsonaf.t -> method_:string -> unit -> notification
end
```

**Usage example – send a “ping” and wait for a “pong”:**

```ocaml
open Mcp_types.Jsonrpc

let id = Id.of_int 1 in
let ping = make_request ~id ~method_:"core/ping" () in

(* serialise *)
let json_string = Jsonaf.to_string (jsonaf_of_request ping) in
Eio.Buf_write.(write b json_string; newline b);

(* ... round-trip over stdio ... *)

(* deserialise *)
match request_of_jsonaf @@ Jsonaf.of_string json_string with
| { method_ = "core/pong"; _ } -> print_endline "latency OK"
| _ -> failwith "unexpected reply"
```

### `Capability`

```ocaml
type Capability.t = {
  tools     : Capability.tools_capability option;
  prompts   : Capability.prompts_capability option;
  resources : Capability.resources_capability option;
}
```

All booleans are optional; an absence means the feature is not supported (or
the server is too old to advertise it).

### `Tool` and `Tools_list_result`

```ocaml
type Tool.t = {
  name         : string;
  description  : string option;
  input_schema : Jsonaf.t;
}

type Tools_list_result.t = {
  tools       : Tool.t list;
  next_cursor : string option;
}
```

Paginate by repeatedly calling `"tools/list"` with the returned `next_cursor`.

### `Tool_result`

```ocaml
type Tool_result.content =
  | Text of string         (* simple string *)
  | Json of Jsonaf.t       (* any JSON value *)
  | Rich of Jsonaf.t       (* object with well-defined schema *)

type Tool_result.t = {
  content  : Tool_result.content list;  (* ordered segments *)
  is_error : bool;                      (* [true] if the tool failed *)
}
```

### `Resource`

```ocaml
type Resource.t = {
  uri         : string;         (* e.g. "urn:uuid:…" *)
  name        : string;
  description : string option;
  mime_type   : string option;  (* RFC 6838 compliant MIME type *)
  size        : int option;     (* bytes, when known *)
}
```

## Design notes

* **Snake-case OCaml fields.**  We do not leak camelCase into user code.
  `[@key "…"]` keeps the original on-wire spelling intact.
* **PPX-generated code ≠ API.**  Only a handful of helper functions are
  re-exported (`Jsonrpc.make_request`, `Jsonrpc.ok`, …).  Everything else –
  including conversion functions – can still be accessed via
  `Mcp_types.Jsonrpc.request_of_jsonaf` etc.  but is considered private
  implementation detail.
* **No polymorphism for now.**  All records are concrete; tailoring the tool
  payload to a specific schema should happen one layer above, once the
  runtime knows which tool is being called.

## Limitations / future work

1. The JSON-RPC layer has no batching support yet.
2. The `Resource` sub-module lacks wrappers for binary/blob content – only
   text resources are handled.
3. Capability negotiation is *advisory*; the client currently ignores
   everything except `tools.listChanged`.

PRs are welcome!  Please keep new fields optional to preserve
backward-compatibility.
