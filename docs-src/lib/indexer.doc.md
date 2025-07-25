# `Indexer` – Build a corpus from OCaml sources

> "If code isn't searchable it doesn't exist."  
> *— every developer after grepping for half an hour*

`Indexer` walks a directory tree, extracts **ocamldoc comments** from
every `*.ml` and `*.mli` file, and turns them into a hybrid **semantic +
lexical** search corpus:

* ***Dense vectors*** created with the
  [OpenAI *Embedding* API](https://platform.openai.com/docs/guides/embeddings)
  – perfect for fuzzy, intent-level queries.
* ***BM-25 indices*** computed with the in-house
  [`Bm25`](bm25.doc.md) module – ideal for exact keyword matches and
  scoring tie-breaks.

All artefacts are written to disk and can later be consumed by
[`Vector_db`](vector_db.doc.md), `Bm25`, or any downstream retrieval
pipeline.

---

## Table of contents

1. [Quick start](#quick-start)
2. [API overview](#api-overview)
3. [End-to-end pipeline](#end-to-end-pipeline)
4. [Configuration knobs](#configuration-knobs)
5. [Operational concerns](#operational-concerns)
6. [Limitations](#limitations)

---

## Quick start

```ocaml
open Eio
open Indexer

Eio_main.run @@ fun env ->
  let cwd = Stdenv.cwd env in
  (* The output will be written to ./_index/            *)
  (* and the code under ./lib/ will be analysed.        *)
  Switch.run @@ fun sw ->
    Indexer.index
      ~sw
      ~dir:cwd
      ~dm:(Stdenv.domain_mgr env)
      ~net:(Stdenv.net env)
      ~vector_db_folder:"_index"
      ~folder_to_index:"lib"
```

Once the function returns you will find:

```
_index/
├─ vectors.mli.binio   (≈ 400 KB)
├─ vectors.ml.binio    (≈ 1.2 MB)
├─ bm25.mli.binio      (≈ 110 KB)
└─ bm25.ml.binio       (≈ 350 KB)
```

You can now load the data:

```ocaml
let mli_vecs = Vector_db.Vec.read_vectors_from_disk (cwd / "_index/vectors.mli.binio") in
let ml_vecs  = Vector_db.Vec.read_vectors_from_disk (cwd / "_index/vectors.ml.binio")  in
```

---

## API overview

### [`index`](../indexer.mli)

Signature (simplified):

```ocaml
val index :
  sw:Switch.t ->                       (* cancellation context        *)
  dir:Eio.Path.t ->                    (* project root                *)
  dm:Eio.Domain_manager.t Resource.t ->(* optional multi-core pool    *)
  net:#Eio.Net.t ->                    (* HTTP capability             *)
  vector_db_folder:string ->           (* output directory            *)
  folder_to_index:string ->            (* input code base             *)
  unit
```

The function is synchronous: it returns only after all vectors and BM-25
files have been flushed to disk.

---

## End-to-end pipeline

```text
┌────────────┐    *.ml/*.mli     ┌─────────────────┐   snippets   ┌────────────┐
│  File walk │ ───────────────▶ │  Ocaml_parser   │ ───────────▶ │  Task_pool │
└────────────┘                  └─────────────────┘              │  workers   │
                                                                   │  ▼        │
                   (location meta + docs)                          │handle_job │
                                                                   └────┬──────┘
                                                                        │
                                            batched HTTP calls          ▼
                                    ┌────────────────────────────────────────────────┐
                                    │       OpenAI Embeddings endpoint              │
                                    └────────────────────────────────────────────────┘
                                                                        │
                                                  (embedding vec)       ▼
                                               ┌──────────────┐  doc text  ┌──────────────┐
                                               │ Vector_db.Vec├───────────▶│     Bm25     │
                                               └──────────────┘           └──────────────┘
```

1. **File walk** – [`collect_ocaml_files`](../indexer.ml) enumerates the
   source tree.
2. **Parsing** – [`Ocaml_parser`](ocaml_parser.doc.md) extracts doc
   strings and source locations.
3. **Chunking** – [`handle_job`](../indexer.ml#L22) merges consecutive
   doc strings into 64–320-token snippets.
4. **Embedding** – [`get_vectors`](../indexer.ml#L97) calls OpenAI; long
   documents are window/stride-sliced.
5. **Persistence** – vectors and BM-25 indices are written via
   [`Vector_db.Vec.write_vectors_to_disk`](../vector_db.mli) and
   [`Bm25.write_to_disk`](../bm25.mli).

---

## Configuration knobs

| Parameter                | Purpose                                                     |
|--------------------------|-------------------------------------------------------------|
| `vector_db_folder`       | Destination directory for output files.                    |
| `folder_to_index`        | Source code directory (recursively scanned).               |
| `min_tokens` / `max_tokens` | Chunk size boundaries (currently 64 / 320).             |
| `embedding_cap`          | Hard limit above which slicing kicks in (6 000 tokens).    |
| `window_tokens` / `stride_tokens` | Sliding-window parameters for long docs.         |

The last four constants live near the top of
[`indexer.ml`](../indexer.ml) – tweak and recompile.

---

## Operational concerns

* **Concurrency** – Parsing is CPU-bound and therefore offloaded to a
  `Domain_manager`; HTTP calls are naturally IO-bound and multiplexed by
  Eio.
* **Idempotency** – Snippets are identified by the *MD5* of their full
  text (incl. metadata).  Re-indexing the same commit will not create
  duplicates, but changes *do* invalidate IDs.
* **Cancellation** – Cancelling the parent [`Switch`] aborts all fibre
  groups and leaves no partial files thanks to `Eio.Path.save ~create:(
  `Or_truncate ...)` semantics.
* **API credentials** – The function expects the usual OpenAI
  environment variables to be set (`OPENAI_API_KEY`, etc.) as required
  by the [`openai`](https://github.com/…/openai-ocaml) library.

---

## Limitations

1. **No delta indexing** – The current implementation rebuilds the
   entire corpus from scratch.  Incremental updates are planned.
2. **Embeddings cost** – Each snippet incurs an OpenAI API call; budget
   accordingly.
3. **Transitive library references** – `indexer.ml` currently relies on
   transitive `dune` dependencies (`Io`, `Bm25`).  Future versions will
   list them explicitly.


