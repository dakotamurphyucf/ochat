# md_index – build a semantic index from Markdown files

`md_index` crawls a directory tree of **Markdown** documents, splits every
file into overlapping text windows, obtains *OpenAI embeddings* and writes
the resulting dense vectors to an on-disk [`Vector_db`](../../lib/vector_db.doc.md)
corpus.  A companion catalogue keeps track of the **centroid** vector of
each logical index so that tools like [`md_search`](./md_search.doc.md) can
quickly shortlist the closest corpora.

Internally the executable is nothing more than a command-line façade
around [`Markdown_indexer.index_directory`](../../lib/markdown_indexer.doc.md);
all heavy lifting happens in the library.

---

## 1 Synopsis

```console
$ md-index --root PATH [--name NAME] [--desc TEXT] [--out DIR]
```

Default values:

| Flag | Default | Description |
|------|---------|-------------|
| `--root PATH` | *(mandatory)* | Top-level folder that will be scanned **recursively**. |
| `--name NAME` | `docs` | Logical identifier used as both the sub-directory <br>`DIR/NAME` and the key in `md_index_catalog.binio`. |
| `--desc TEXT` | `Markdown documentation index` | One-liner shown by UIs. |
| `--out DIR`   | `.md_index` | Parent directory holding all vector DBs. |

---

## 2 Algorithm (delegated to the library)

1. **Discovery** – `Markdown_crawler` walks the file tree and yields
   `.md`, `.markdown` & `.mdx` files.
2. **Chunking** – Each document is split into 64–320-token windows with an
   overlap of 20 % (`Markdown_snippet`).
3. **Embedding** – Windows that are *not* present in the target index are
   sent to the OpenAI Embeddings API (batched, retry-safe).
4. **Persistence** – Vectors are appended to a memory-mapped file
   `vectors.binio`; the original Markdown chunks are stored under
   `snippets/ID.md`.
5. **Catalogue update** – The centroid vector of the index is computed and
   inserted/updated in `md_index_catalog.binio` so that other tools can
   discover it.

All I/O is executed via [Eio](https://github.com/ocaml-multicore/eio) and
therefore non-blocking.

---

## 3 Examples

Index the documentation of the current repository under the logical name
`docs`:

```console
$ md-index --root ./lib/docs-src --name docs \
           --desc "Library documentation" --out .md_index
Markdown indexing completed. Index name: docs – stored under .md_index
```

Creating a second index side-by-side:

```console
$ md-index --root ~/blog --name blog_posts --out .md_index
```

---

## 4 Exit codes

| Code | Meaning |
|------|---------|
| 0 | Index built/updated successfully. |
| 1 | Missing `--root` or invalid combination of flags. |

---

## 5 Limitations & future work

* **OpenAI-specific** – alternative embedding providers are possible but
  would require extending `Embed_service`.
* **In-memory chunking** – the entire document is read before being split;
  extremely large Markdown files (> 512 kB) may exhaust memory.
* **No incremental removal** – deleted source files are not yet purged
  from the index.

---

## 6 See also

* [`md_search`](./md_search.doc.md) – query Markdown indexes
* [`Markdown_indexer`](../../lib/markdown_indexer.doc.md) – library API
* [`Vector_db`](../../lib/vector_db.doc.md) – cosine similarity search
