# md_index – build a semantic index from Markdown files

`md_index` crawls a directory tree of **Markdown** documents, splits every
file into overlapping text windows, obtains *OpenAI embeddings* and writes
the resulting dense vectors to an on-disk [`Vector_db`](../lib/vector_db.doc.md)
corpus.  A companion catalogue keeps track of the **centroid** vector of
each logical index so that tools like [`md_search`](./md_search.doc.md) can
quickly shortlist the closest corpora.

Internally the executable is nothing more than a command-line façade
around [`Markdown_indexer.index_directory`](../lib/markdown_indexer.doc.md);
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
   `.md`, `.markdown` and `.mdown` files (case-sensitive), not `.mdx`.
2. **Chunking** – Each document is split into 64–320-token windows with an
   overlap of 20 % (`Markdown_snippet`).
3. **Embedding** – All discovered windows are embedded again on each run;
   there is no on-disk embedding reuse in this pipeline.
4. **Persistence** – Vectors replace the serialized file
   `vectors.binio`; the original Markdown chunks are stored under
   `snippets/ID.md`.
5. **Catalogue update** – The centroid vector of the index is computed and
   inserted/updated in `md_index_catalog.binio` so that other tools can
   discover it.

I/O uses Eio. The indexer accumulates snippets and results in memory; this is
not a streaming, atomic, or memory-mapped index update.

---

## 3 Examples

Index the documentation of the current repository under the logical name
`docs`:

```console
$ md-index --root ./docs-src --name docs \
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
| 1 | Empty/missing `--root`, explicitly rejected by the command. |
| Other non-zero | Argument-parser or runtime failure; no stable detailed error-code contract. |

---

## 5 Limitations & future work

* **OpenAI-specific** – alternative embedding providers are possible but
  would require extending `Embed_service`.
* **In-memory processing** – the crawler reads a file before rejecting it if
  larger than 10 MiB. The indexer also accumulates all accepted snippets and vectors.
* **Rebuild semantics** – a nonempty run replaces the vector set. Old snippet
  files may remain on disk, but are no longer retrieved unless their IDs remain
  in the vector set. If discovery is empty, the process exits successfully before
  replacing the old vectors/catalog; an empty run does not clear an old index.
* **Ignore rules** – root `.gitignore` matching is best-effort, not full Git
  semantics (no nested rules or negation handling). Symlinks are followed;
  use a curated input tree, not ignore rules as a confidentiality boundary.
* **Embeddings** – use the same `EMBEDDINGS_MODEL` and stub/live mode at query
  time. Missing credentials select test vectors, not useful semantic embeddings;
  see [search configuration](../guide/search-and-indexing.md#embedding-configuration).

---

## 6 See also

* [`md_search`](./md_search.doc.md) – query Markdown indexes
* [`Markdown_indexer`](../lib/markdown_indexer.doc.md) – library API
* [`Vector_db`](../lib/vector_db.doc.md) – cosine similarity search
