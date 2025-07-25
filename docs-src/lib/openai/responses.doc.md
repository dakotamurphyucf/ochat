# `Responses` – High-level wrapper for OpenAI’s `/v1/responses` API

---

## Overview

`Responses` offers a thin, type-safe layer over OpenAI’s JSON schema while
taking care of the following low-level chores:

* Building the request payload with optional streaming, tool-calling,
  reasoning controls, etc.
* Performing the HTTPS request via `cohttp-eio` with capability-safe
  network handles.
* Incrementally decoding the Server-Sent Events (SSE) stream when
  requested, invoking a user callback for every event.
* Converting the final JSON reply into rich OCaml records derived from
  the schema.

Everything is centred around a single function:

```ocaml
val post_response :
  'a response_type
  -> ?max_output_tokens:int
  -> ?temperature:float
  -> ?tools:Request.Tool.t list
  -> ?model:Request.model
  -> ?reasoning:Request.Reasoning.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> _ Eio.Net.t
  -> inputs:Item.t list
  -> 'a
```

### Blocking vs streaming modes

```ocaml
open Responses

(* 1 – blocking variant returning a single JSON object *)
let ({ Response.output; _ } : Response.t) =
  post_response Default ~dir net ~inputs

(* 2 – streaming variant.  The callback fires for every SSE event. *)
let print_delta = function
  | Response_stream.Output_text_delta { delta; _ } -> print_string delta
  | _ -> ()

let () =
  post_response (Stream print_delta) ~temperature:0.7 ~dir net ~inputs
```

The helper `response_type` GADT encodes the result type at the call site:

* `Default` → returns a fully-parsed `Response.t`.
* `Stream f` → returns `unit` after piping every SSE chunk through `f`.

## Building a request

Requests are heterogeneous lists of `Item.t`, where each variant mirrors a
possible entry in the JSON array sent to the server.  In practice you
rarely need more than user / assistant messages:

```ocaml
open Responses

open Responses.Input_message

let user : Input_message.t =
  { role = User
  ; content = [ Text { text = "Tell me a joke"; _type = "input_text" } ]
  ; _type = "message"
  }

let inputs = [ Item.Input_message user ]
```

### Tool calling

Adding support for function-calling or vector-store search is as simple as
populating the optional `~tools` parameter with a value constructed via
the nested `Request.Tool.*` helpers.  Refer to the type definitions in
`responses.mli` for details; every field corresponds 1:1 to the JSON
specification.

## Diagnostics

When `post_response` is invoked in streaming mode it writes the raw text
of each SSE line to `raw-openai-streaming-response.txt` in `~dir`.  This
is invaluable when debugging JSON decoding errors.

## Limitations & Caveats

* **Breaking API** – The remote endpoint is not stable.  New event types
  may appear without notice.
* **No automatic retry/back-off** – Any network error bubbles up to the
  caller.
* **TLS verification** – `tls-eio` is configured with no explicit root
  store.  Make sure your environment provides one if certificate
  pinning is a requirement.
* **Large payloads** – Neither request nor reply size is currently
  chunked; the whole JSON object is materialised in memory when
  `Default` is used.

## See also

* [`Completions`](./completions.doc.md) – Simpler chat API without tool
  calling.
* [`Embeddings`](./embeddings.doc.md) – Generate vector embeddings for
  semantic search.

---


