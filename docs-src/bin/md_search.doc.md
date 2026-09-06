# md_search – semantic search over Markdown snippet indexes

`md_search` is a small command-line program that lets you run
natural-language queries over one or more *Markdown indexes* produced
with the companion [`md_index`](./md_index.doc.md) tool.

Each index folder contains vectors and snippet files:

* `vectors.binio` – dense OpenAI embeddings of every snippet
* `snippets/ID.md` – the original snippet text under its indexer-generated ID.

The parent directory holds `md_index_catalog.binio`; the Markdown indexer does
not write a per-index `meta.json` file.

`md_search` loads the vectors in bulk, converts them into an
in-memory corpus with [`Vector_db`](../lib/vector_db.doc.md) and returns
the *k* closest snippets to your query.

---

## 1 Synopsis

```console
$ md-search --query TEXT [--index NAME|all] [--index-dir DIR] [-k INT]
```

The binary is installed under the same opam package as the library, so
`opam install ochat` will place it in your `$PATH`.

## 2 Command-line flags

| Flag | Default | Description |
|------|---------|-------------|
| `--query TEXT` | *(required)* | Natural-language query. Passed to the OpenAI embeddings endpoint. |
| `--index NAME` | `all` | Name of the index directory to search.  Use the special value `all` to automatically pick the five closest indexes based on their centroid vector. |
| `--index-dir DIR` | `.md_index` | Directory that stores the indexes as sub-folders. |
| `-k INT` | `5` | Number of snippets to print. |

## 3 Algorithm (high-level)

1. The query text is embedded with the configured model (default `text-embedding-3-large`)
   via `Openai.Embeddings.post_openai_embeddings`.
2. If `--index=all`, the program loads the global catalogue
   (`md_index_catalog.binio`) to locate the *five* indexes whose
   centroid vector is closest to the query (simple dot product).  If a
   specific name is supplied, the catalogue is skipped. If the catalogue is
   absent, the searcher falls back to all subdirectories, not a five-index cap.
3. All vectors from the selected indexes are read from disk and fed to
   `Vector_db.create_corpus`, which normalises them and builds an Owl
   matrix.
4. Cosine similarity (computed with a matrix–vector product) ranks the
   snippets; the top-*k* IDs are mapped back to their source files.
5. Each snippet file is printed to `stdout` with a separators (`---`).

The entire pipeline runs inside an [Eio] fibre; blocking I/O does not
freeze the process.

## 4 Examples

Searching for a documentation snippet that explains *tail-call
optimisation*:

```console
$ md-search --query "tail call optimisation" -k 3
[1] [ocaml_manual] c172be…
… tail-recursive function …

---

[2] [blog_posts] 6a7338…
… trampoline avoids growing the stack …

---

[3] [stackoverflow] 4c0a12…
… perform TCO by re-writing the call as a loop …
```

Restricting the search to a single index:

```console
$ md-search --query "finalisers" --index ocaml_manual -k 2
```

## 5 Exit codes

| Code | Meaning |
|------|---------|
| 0 | Completed, including no candidate indexes or no vectors (prints a diagnostic). |
| 1 | Empty/missing `--query`, explicitly rejected by the command. |
| Other non-zero | Argument-parser or uncaught runtime failure; no stable per-error taxonomy. |

Vector files that cannot be read are skipped. A zero exit status therefore does
not prove that every selected index was valid or that any results were found.
The query embedding is requested before index discovery, even for an empty index root.

## 6 Limitations & notes

* **Provider configuration** – real embeddings use `OPENAI_API_KEY`,
  `EMBEDDINGS_HOST` (default `api.openai.com`), and `EMBEDDINGS_MODEL` (default
  `text-embedding-3-large`). These are distinct from the agent's `API_URL`.
  Missing/empty API keys or a present `OPENAI_EMBEDDINGS_STUB` select deterministic
  128-dimensional test vectors instead; these are not meaningful semantic search.
* **Memory usage** – the vectors of every selected index are loaded at once. Very large indexes (>100k snippets) may exhaust memory.
* **Matching embeddings** – build and query with the same model and stub/live
  mode. Changing environment settings does not migrate an existing index.
* **Unix-only** – depends on `Eio.Posix`; does not compile to JavaScript or MirageOS.  
* **Preview length** – snippet preview is truncated to 8000 bytes.
