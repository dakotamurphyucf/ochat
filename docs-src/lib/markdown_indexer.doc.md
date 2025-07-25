# `Markdown_indexer`

Turns an arbitrary *directory tree of Markdown files* into a ready-to-use
vector database that can be queried via semantic similarity search
(`Vector_db.query` or the `ochat md-search` CLI helper).

Internally the pipeline glues together:

1. `Markdown_crawler` – enumerate files.
2. `Markdown_snippet` – slice each document into 64-320 token windows with
   20 % overlap.
3. `Embed_service` – obtain OpenAI embeddings (batched, retry-safe, rate-limited).
4. `Vector_db` – persist the dense vectors alongside the original Markdown
   snippet.
5. `Md_index_catalog` – keep a global catalogue mapping *index names* to their
   centroid vectors.

---

## Directory layout

```
.md_index/
└─ <index_name>/                <-- one logical index (e.g. "docs")
   ├─ vectors.binio            (Vector_db.Vec array)
   └─ snippets/
      └─ <snippet-id>.md       (original Markdown)
```

The *parent* directory holds `md_index_catalog.binio` containing the metadata
of every index created on the machine.

---

## Public API

```ocaml
val index_directory :
  ?vector_db_root:string ->
  env:Eio_unix.Stdenv.base ->
  index_name:string ->
  description:string ->
  root:_ Eio.Path.t ->
  unit
```

* `vector_db_root` – Parent folder holding one sub-directory per logical
  index (defaults to `.md_index`).
* `index_name` – Folder name below `vector_db_root` and primary key in the
  global catalogue (e.g. `"docs"`).
* `description` – Free-text summary shown in UIs.
* `root` – Directory to be indexed (recursively walked by
  `Markdown_crawler`).

### What the function does

1. Creates/updates the on-disk structure shown above.
2. Uploads *new* snippets only — the stable MD5-based identifier guarantees
   idempotency.  Existing embeddings are reused unchanged.
3. Computes the **centroid vector** of the index and writes/updates the global
   catalogue (`Md_index_catalog`).

All I/O is performed through Eio and therefore non-blocking.  The function is
fully cancel-safe and can be composed with other Eio-based components.

---

## Example (CLI)

Executable wrappers are provided under `bin/`:

```bash
# create/update index
ochat md-index \
  --root lib/docs-src \
  --index-name docs \
  --description "Library documentation"

# query the corpus with hybrid BM25 + vector search
ochat md-search --query "How to initialise?"
```

These are thin shims around the public API and are primarily used for manual
testing.

---

## Caveats & limitations

* Currently assumes **OpenAI embeddings**; plug-in support for
  alternative models is possible because `Embed_service` is factored out.
* Currently tied to **OpenAI embeddings**.  Swapping in a self-hosted model
  would require a thin adapter inside `Embed_service`.
* Corpus must fit into memory during embedding.  For millions of snippets a
  streaming approach would be required.

