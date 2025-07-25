# `Embeddings` – OpenAI vector embeddings wrapper

This document complements the odoc inline comments found in
`embeddings.mli`.  It is intended for users who prefer Markdown and for
projects that render developer documentation outside of odoc (e.g.
GitHub Pages).

## Overview

`Embeddings` converts natural-language text into fixed-size numerical
vectors using OpenAI’s `/v1/embeddings` endpoint.  The function
`Embeddings.post_openai_embeddings` takes a capability-safe network
handle (`Eio.Net.t`) plus a batch of input sentences and returns the
resulting vectors as OCaml values.

Typical use-cases:

* **Semantic search** – index vectors with a nearest-neighbour data
  structure (see `Vector_db`) and query similar documents.
* **Clustering / visualisation** – feed the high-dimensional vectors
  into PCA, t-SNE, UMAP, etc.
* **Prompt engineering** – pass embeddings to other LLM pipelines that
  accept dense representation instead of raw text.

## API

```
val post_openai_embeddings :
  _ Eio.Net.t -> input:string list -> Embeddings.response
```

### Arguments

* `net` – Capability returned by `Eio.Stdenv.net env`.  Ensures the
  calling fiber is authorised to open outbound TLS connections.
* `input` – Non-empty list of UTF-8 strings.  Each string must be ≤8192
  tokens according to the tokenizer used by the chosen model.  Up to
  2048 items are accepted per request (OpenAI limit, subject to
  change).

### Return value

Record `{ data : embedding list }` where each `embedding` contains:

* `embedding` – `float list`, the raw dense vector.
* `index` – Which line of `input` this vector corresponds to.

### Exceptions

* `Invalid_argument` – `input` is empty.
* Any exception raised by `Io.Net.post` (network failures, non-2xx HTTP
  response, JSON parsing errors, …).  Wrap the call with `Io.to_res`
  if you prefer to propagate errors as `Result.t` values.

## Example

```ocaml
(* dune runtest ‑package ochat.openai *)

open Eio.Std

let main env =
  let net = Eio.Stdenv.net env in
  let input = [ "The quick brown fox jumps over the lazy dog" ] in
  let ({ data = [ { embedding; _ } ] } : Embeddings.response) =
    Embeddings.post_openai_embeddings net ~input
  in
  Format.printf "Vector length = %d\n" (List.length embedding)

let () = Eio_main.run main
```

## Implementation notes

* Vectors are delivered as `float list` instead of `float array` to
  avoid the overhead of allocating C-layout bigarrays when most users
  will immediately feed them to higher-level ML libraries that accept
  lists or require their own buffers.
* The model name is currently hard-coded to
  `text-embedding-3-large`.  Fork the module or submit a patch if you
  need configurability.

## Limitations

* The function performs no rate-limiting; you are responsible for
  backing off according to OpenAI’s usage policies.
* Only HTTPS is supported (TLS v1.2+).  Proxy settings are ignored.
* The module uses the permissive TLS configuration defined in
  `Io.Net.tls_config`, which disables certificate validation.  **Do
  not use in production environments.**

---

> Module maintained by the Ochat-OCaml community.  Contributions and
> bug-reports are welcome!

