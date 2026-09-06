# `odoc-search`

Semantic search over an on-disk ODoc vector index

---

## 1  Purpose

`odoc-search` turns a short natural-language query into the most relevant OCaml
API fragments taken from package documentation.  It is the counterpart of
`odoc-index` and relies on the latter having pre-computed dense embeddings,
lexical BM25 indices and rendered Markdown snippets for each OPAM package.

Typical use-cases include:

* finding a function or module when only its behaviour is known;
* exploring unfamiliar libraries by asking conceptual questions (e.g. “how do
  I run fibres on multiple domains in Eio?”);
* locating examples faster than full-text search.

## 2  How it works

1. The query text is embedded with the configured model (default
   `text-embedding-3-large`). See [embedding configuration](../guide/search-and-indexing.md#embedding-configuration).
2. If `--package` is **not** provided the coarse
   [`package_index.binio`](../../lib/package_index.mli) is consulted to retain
   five packages whose blurbs are closest to the query, independently of `-k`. This avoids
   loading vectors for thousands of packages when only a handful are needed.
   If the coarse index is absent or returns no candidates, directory entries
   are considered instead.
3. For each selected package, `vectors.binio` is loaded; unreadable vector
   files are skipped.
4. All vectors are concatenated into a single corpus and queried with
   `Vector_db.query`. The active CLI path is dense-only; it does not load BM25.
5. Result identifiers are resolved to the Markdown bodies shipped by
   `odoc-index` and printed to *stdout* in the following format:

   ```text
   [rank] [package] <id>:
   <full snippet body>

   ---
   ```

## 3  Command-line reference

| Flag                | Description                                                 |
|---------------------|-------------------------------------------------------------|
| `--query STRING`    | Natural-language search text (mandatory).                   |
| `--package PKG`     | Limit search to a single OPAM package.                      |
| `--index DIR`       | Root directory of the index (default `.odoc_index`).        |
| `-k INT`            | Maximum number of hits to return (default 5).               |
| `--beta FLOAT`      | Parsed (default 0.25), but unused by the active dense-only CLI path. |

The source retains an unused hybrid helper. Passing `--beta 1` does not switch
the current command to lexical-only search.

## 4  Examples

### 4.1  Search the whole index

```bash
$ odoc-search --query "generate random uuid" -k 3
[1] [base] Uuid_unix.create:
(* returns e.g. "2b9cfbbc-0bb2-4fff-aac4-2e0e746c6a8e" *)

---

[2] [core] Uuid.of_string:
...
```

### 4.2  Restrict to a single package

```bash
$ odoc-search --query "lru cache" --package lru
```

Only snippets coming from the `lru` OPAM package are considered.

## 5  Return format

The tool writes plain text to *stdout* so that it can be piped into
interactive fuzzy finders such as `fzf` or redirected to a file.  Each result
is separated by a horizontal rule (`---`).

## 6  Environment variables

Real embeddings use `OPENAI_API_KEY`, `EMBEDDINGS_HOST`, and `EMBEDDINGS_MODEL`.
Missing/empty credentials or any present `OPENAI_EMBEDDINGS_STUB` select
128-dimensional test vectors instead of a provider call. They are not useful
semantic embeddings; do not mix them with a live-model index.

## 7  Known limitations

* **Memory footprint** – the whole embedding corpus is loaded into RAM.  Very
  large indexes (> 500 MB) may not fit on machines with limited memory.
* **Per-invocation work** – each CLI process embeds its query and loads vectors;
  it does not retain an in-memory cache for the next CLI invocation.
* **No incremental updates** – rebuilding the index is currently the only way
  to incorporate new packages or documentation.

An empty/missing query explicitly exits 1. No vectors prints a diagnostic and
returns success; parse/runtime errors may exit nonzero. Success does not imply
every selected index was readable or that there were any hits.

## 8  Related tools

* `odoc-index` – builds the vector + BM25 index consumed by this program.
* `md-search` – performs the same style of search over a Markdown-only index.

[OpenAI API]: https://platform.openai.com/docs/api-reference/embeddings
