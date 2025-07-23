# `Markdown_indexer`

High-level orchestration for turning a *directory of Markdown files* into a
vector database that can be searched with semantic similarity (`Markdown_search`).

The module glues together:

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

## API

```ocaml
val index_directory :
  ?vector_db_root:string ->
  env:Eio_unix.Stdenv.base ->
  index_name:string ->
  description:string ->
  root:_ Eio.Path.t ->
  unit
```

* `vector_db_root` – top-level folder (default: `.md_index`).
* `index_name`      – logical identifier (e.g. "docs" or "wiki").
* `description`     – one-line human summary stored in the catalogue.
* `root`            – directory that will be indexed (passed to
  `Markdown_crawler`).

### Behaviour

1. Creates/updates the on-disk structure shown above.
2. For *new* snippets (determined by stable MD5 id) calls OpenAI once and
   appends the resulting vectors; existing snippets skipped (idempotent).
3. Computes the **centroid vector** of the index and writes/updates the global
   catalogue (`Md_index_catalog`).

The entire operation runs inside `Eio_main.run`, therefore respects
cancellation and integrates with other Eio-based components.

---

## CLI helpers

Executable wrappers are provided under `bin/`:

* `gpt md-index --root lib/docs-src --index-name docs --description "Lib docs"`
* `gpt md-search --query "How to initialise?"`

These are thin shims around the public API and are primarily used for manual
testing.

---

## Caveats

* Currently assumes **OpenAI embeddings**; plug-in support for
  alternative models is possible because `Embed_service` is factored out.
* Extremely large directories may exhaust memory – slice & embed happen in
  memory before being flushed to disk.

