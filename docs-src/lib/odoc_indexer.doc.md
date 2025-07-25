# `Odoc_indexer` – turn *odoc*-generated HTML into a search corpus

`Odoc_indexer` is the missing *"make index"* step in the OCaml documentation tool-chain:

```
      dune build @doc           odoc_indexer index
┌─────────────────────┐        ┌─────────────────────────────┐
│  _build/_doc/_html  │  ───▶  │ pkg/{vectors,bm25,*.md}.binio│
└─────────────────────┘        └─────────────────────────────┘
```

The module walks the HTML tree produced by `odoc`/`dune build @doc`, breaks the pages into token-bounded snippets, obtains OpenAI embeddings for each snippet and finally stores everything on disk in a format that [`Vector_db`](vector_db.doc.md) and [`Bm25`](bm25.doc.md) understand.

---

## Table of contents

1. [Quick start](#quick-start)
2. [Public API](#public-api)
3. [Processing pipeline](#processing-pipeline)
4. [Configuration knobs](#configuration-knobs)
5. [Operational details](#operational-details)
6. [Limitations](#limitations)

---

## Quick start

```ocaml
open Eio.Std
open Odoc_indexer

let () =
  Eio_main.run @@ fun env ->
    let root   = Eio.Path.(Eio.Stdenv.cwd env / "_build/default/_doc/_html") in
    let output = Eio.Path.(Eio.Stdenv.cwd env / "_doc_index") in
    index_packages
      ~env
      ~root
      ~output
      ~net:(Eio.Stdenv.net env)
      ()
```

After the function returns you will find, for each package *pkg*:

```
_doc_index/pkg/
├─ vectors.binio  (dense 1536-d embeddings)
├─ bm25.binio     (lexical index)
└─ <id>.md        (raw snippet bodies)
```

Loading the data back is a single line of code:

```ocaml
let vecs = Vector_db.Vec.read_vectors_from_disk Path.(cwd / "_doc_index/pkg/vectors.binio")
```

---

## Public API

```ocaml
val index_packages :
  ?filter:package_filter ->
  env:Eio_unix.Stdenv.base ->
  root:_ Eio.Path.t ->          (* _build/default/_doc/_html           *)
  output:_ Eio.Path.t ->        (* destination directory               *)
  net:#Eio.Net.t ->
  unit ->                       (* must be () for labelled arguments   *)
  unit
```

The call is synchronous – it returns only when **all** artefacts have been flushed to disk.

---

## Processing pipeline

| Stage | Implementation | Concurrency |
|-------|----------------|-------------|
| Crawl | `Odoc_crawler.crawl` | `Fiber.List.iter` (25 fibres) |
| Slice | `Odoc_snippet.slice` | domain pool (`Io.Task_pool`) |
| Embed | OpenAI *Embeddings* | single throttled fibre |
| Persist | `Vector_db` / `Bm25` | synchronous |

### Diagram

```text
┌──────────────┐  HTML/README ┌─────────────────┐   snippets   ┌────────────┐
│  Crawler     │ ───────────▶ │  Snippet slice  │ ───────────▶ │ Task pool  │
└──────────────┘              └─────────────────┘              │  workers   │
                                                               │  ▼         │
                                   batched HTTP                │ get_vectors│
                                   ┌────────────────────────────────────────┐
                                   │   OpenAI Embeddings endpoint           │
                                   └────────────────────────────────────────┘
                                                                  │
                                           (vec)                  ▼
                                       ┌──────────────┐  md text  ┌─────────┐
                                       │ Vector_db.Vec├──────────▶│  Bm25   │
                                       └──────────────┘           └─────────┘
```

---

## Configuration knobs

| Parameter | Purpose |
|-----------|---------|
| `filter` | Controls which packages are processed. Accepts the variants: `All` (default), `Include [pkgs]`, `Exclude [pkgs]`, or `Update (prev, pkgs)` for incremental updates |
| `rate_per_sec` | Hard-coded to *1000* requests/s; change in `Embed_service.create` |
| `min_tokens / max_tokens` | Chunk size boundaries, live in `Odoc_snippet.slice` |

---

## Operational details

* **Logging & tracing** – The module emits structured logs (`Log.emit`) and OpenTelemetry spans (`Log.with_span`); hook up a collector to see a flame-graph style visualisation.

* **Idempotency** – Each snippet is identified by the MD5 of its full Markdown body (incl. the meta header). Re-running the indexer on the same commit overwrites but never duplicates content.

* **Failure semantics** – Transient HTTP errors are retried up to three times. An unrecoverable error aborts the entire `Switch` and propagates to the caller.

---

## Limitations

1. **OpenAI dependency** – You need an `OPENAI_API_KEY`; usage may incur cost.
2. **No incremental updates** – The current implementation rebuilds the index from scratch. Incremental and delta indexing are on the roadmap.
3. **Memory footprint** – The embeddings corpus must fit in memory when you later query with `Vector_db`. For very large libraries consider sharding by package.

---

PRs, bug reports and ☕ donations are welcome!

