# Search, indexing & code intelligence

Ochat ships **local indexing + retrieval** building blocks that let agents pull in *small, high-signal snippets* from:

- your repo’s Markdown docs (design notes, readmes, guides),
- your repo’s OCaml source (hybrid semantic + lexical search), and
- locally-generated odoc HTML (API docs for your project and packages you’ve indexed).

This is the fastest way to ground an agent in a codebase without pasting huge files into the prompt.

## Pick the right corpus (quick cheat sheet)

- “Why does the system work this way?” / “what’s the design?” → **Markdown docs** (`markdown_search`)
- “Where is this implemented?” / “show me the code pattern” → **OCaml source index** (`query_vector_db`)
- “What does this API guarantee?” / “what’s the signature?” → **odoc docs** (`odoc_search`)

---

## Index types and entrypoints

| Corpus | Indexer CLI | Search CLI | ChatMD indexing tool | ChatMD search tool | Key notes |
|---|---|---|---|---|---|
| Markdown docs | `md-index` | `md-search` | `index_markdown_docs` | `markdown_search` | Multi-index catalog; `all` auto-shortlists the top 5 likely indexes, then dense search. |
| OCaml source | `ochat index` | `ochat query` | `index_ocaml_code` | `query_vector_db` | Hybrid retrieval (dense + BM25). Prefer `index: "ml"` or `"mli"` explicitly. |
| Odoc HTML docs | `odoc-index` | `odoc-search` | *(none)* | `odoc_search` | Index is vectors+BM25 per package, plus package shortlist index; **main search path is dense-only today**. |

### The usual workflow

1. **Build indexes in batch** (locally, CI, or a “setup” step).
2. Keep the on-disk index directories (`.md_index`, `.odoc_index`, `./vector`) alongside your repo or in a cache/artifact store.
3. In ChatMD prompts, call the search tools to retrieve relevant snippets on demand.

> You *can* index from inside ChatMD (Markdown + OCaml source) — great for bootstrapping — but for most projects it’s better to index once and reuse the artifacts.

---

## Quick start: the 3 highest-value workflows

### 1) Docs RAG over your repo Markdown (fastest win)

Index your docs once, then let the agent pull in the best snippets.

```sh
# Build an index named "docs" under ./.md_index/docs
md-index --root docs-src --name docs --desc "Project docs" --out .md_index

# Query across all known markdown indexes (auto-shortlists top 5)
md-search --query "how does tool calling work?" --index all --index-dir .md_index -k 5
```

Why it’s great:
- Markdown docs often contain the “why” and the “intended architecture” that isn’t in code comments.
- Results come back as **snippets**, not entire files, keeping prompts small.

### 2) Hybrid retrieval over code (semantic + exact identifiers)

Build a code index (vectors + BM25), then query it.

```sh
# Index (default folder-to-index is ./lib, but be explicit for clarity)
ochat index -folder-to-index ./lib -vector-db-folder ./vector

# Query (CLI queries the ml corpus)
ochat query -vector-db-folder ./vector -query-text "tail-recursive map" -num-results 5
```

Why it’s great:
- Dense vectors handle fuzzy questions (“where does streaming cancellation happen?”).
- BM25 rescues exact symbol matches (module/type/function names).

### 3) Local odoc search (API truth without a browser)

Index odoc HTML, then query it.

```sh
# Typical odoc HTML root for a dune project:
odoc-index --root _build/default/_doc/_html --out .odoc_index

odoc-search --query "Eio.Switch.run usage" -k 5 --index .odoc_index
```

> Note: `odoc-index` and `odoc-search` work on a directory layout where **first-level directories are packages**.

---

## Examples: sample outputs for the search CLIs

Sometimes it’s easiest to understand what a command does by seeing *the shape of the results*.
These example files are **illustrative** (hand-written sample outputs) so you can quickly recognize what “good” looks like.

- [`md-search` example (Markdown docs search)](search-examples/md-search.md)
- [`odoc-search` example (odoc docs search)](search-examples/odoc-search.md)
- [`ochat query` example (hybrid code search)](search-examples/ochat-query.md)

---

## ChatMD tool reference (schemas + practical notes)

These are built-in ChatMD tools (declare via `<tool name="…"/>`) implemented in `lib/functions.ml` and defined in `lib/definitions.ml`.

### `index_markdown_docs`

Declare:
```xml
<tool name="index_markdown_docs"/>
```

Input:
```json
{
  "root": "docs-src",
  "index_name": "docs",
  "description": "Project docs",
  "vector_db_root": ".md_index"
}
```

Notes:
- Writes to `<vector_db_root>/<index_name>/…` (default `vector_db_root` is `.md_index`).
- Requires OpenAI embeddings (so `OPENAI_API_KEY` must be set).

### `markdown_search`

Declare:
```xml
<tool name="markdown_search"/>
```

Input:
```json
{
  "query": "streaming cancellation design",
  "k": 5,
  "index_name": "all",
  "vector_db_root": ".md_index"
}
```

Behavior:
- If `index_name` is omitted or `"all"`, ochat **shortlists ~5 likely indexes** using the catalog centroid vectors, then runs dense search inside those.
- Results are returned as snippet previews (first ~8000 chars).

### `index_ocaml_code`

Declare:
```xml
<tool name="index_ocaml_code"/>
```

Input:
```json
{
  "folder_to_index": "./lib",
  "vector_db_folder": "./vector"
}
```

Notes:
- Builds two corpora: **`ml`** and **`mli`**, each with vectors + BM25.

### `query_vector_db`

Declare:
```xml
<tool name="query_vector_db"/>
```

Input (recommended):
```json
{
  "vector_db_folder": "./vector",
  "query": "where do we parse tool declarations?",
  "num_results": 5,
  "index": "ml"
}
```

Important gotcha (worth calling out in prompts):
- The on-disk files written by the indexer are `vectors.ml.binio` / `vectors.mli.binio` and `bm25.ml.binio` / `bm25.mli.binio`.
- **In practice, you should always set `index` to `"ml"` or `"mli"`** so the tool loads the right files.

Ranking:
- Hybrid retrieval via `Vector_db.query_hybrid` (dense shortlist + BM25 re-rank).
- CLI `ochat query` uses `beta=0.1`; the ChatMD tool uses `beta=0.4` (results can differ).

### `odoc_search`

Declare:
```xml
<tool name="odoc_search"/>
```

Input:
```json
{
  "query": "Eio.Switch API",
  "package": "all",
  "k": 5,
  "index": ".odoc_index"
}
```

Notes:
- `package` is required; use `"all"` unless you know the exact package to scope to.
- Uses a coarse package shortlist index (`package_index.binio`) when available.
- Dense vector search is the main scoring path today (BM25 files exist but are not used by the main tool implementation).

---

## On-disk layouts (cheat sheet)

### Markdown docs: `.md_index/`

Produced by: `md-index` and `index_markdown_docs`.

```
.md_index/
  md_index_catalog.binio
  <index_name>/
    vectors.binio
    snippets/
      <id>.md
```

What it’s for:
- `md_index_catalog.binio` stores `(name, description, centroid vector)` so that `index_name="all"` can shortlist the most relevant indexes quickly.

What gets indexed (important for troubleshooting):
- Only files with extensions: `.md`, `.markdown`, `.mdown`
- Skips files larger than 10 MiB
- Best-effort root `.gitignore` support + built-in denylist (`_build/`, `node_modules/`, `.git/`, …)

### Odoc docs: `.odoc_index/`

Produced by: `odoc-index` (wraps `Odoc_indexer.index_packages`).

```
.odoc_index/
  package_index.binio
  <pkg>/
    vectors.binio
    bm25.binio
    <id>.md
```

Notes:
- The crawler expects an odoc HTML tree such as `_build/default/_doc/_html`.
- Hidden modules are skipped if the HTML contains: `This module is hidden.`
- READMEs in `_doc-dir/` are included as Markdown.

Important gotcha:
- The shipped `odoc-index` binary currently calls `Odoc_indexer.index_packages` with a **hard-coded filter policy** (curated include/exclude). If you expected “index everything under `--root`”, you may need to adjust `bin/odoc_index.ml` or call the library differently.

### OCaml source vector DB (commonly `./vector/`)

Produced by: `ochat index` and `index_ocaml_code`.

```
vector/
  vectors.ml.binio
  vectors.mli.binio
  bm25.ml.binio
  bm25.mli.binio
  <hash>          # snippet bodies (no extension)
```

Notes:
- Snippet bodies are stored as files named by a stable hash id.
- BM25 is built from the same snippet bodies.

---

## How chunking works (predictability matters)

### Markdown docs chunking
- Structure-aware chunking by headings/blank lines/code fences/tables **and** thematic breaks (`---`, `***`, `___`).
- Token windows: **64–320** with **64-token overlap**.
- Snippet IDs are stable hashes of the final snippet text.

### Odoc docs chunking
- Similar 64–320 with 64 overlap; code-fence/table/heading-aware.
- Derived from HTML → Markdown conversion; hidden modules are skipped.

### OCaml source chunking (indexer)
- Targets similar sizes, but uses a **whitespace token heuristic** (not Tikitoken) while chunking.
- Large snippets are sliced into overlapping windows before embedding to stay under embedding limits.

---

## Ranking behavior: dense vs hybrid (and why results differ)

- **Markdown search**: dense cosine similarity only.
- **Odoc search**: dense cosine similarity in the main path today; BM25 artifacts exist but aren’t used by the primary search tool path.
- **Code search**: hybrid:
  - cosine shortlist (dense),
  - BM25 scoring,
  - linear interpolation with `beta`.

Reproducibility note:
- CLI and ChatMD tool use different `beta` values for code search (so top results can differ).

---

## Performance notes (what’s cached)

In the ChatMD tool implementations:
- `markdown_search` caches:
  - embeddings per query (in-memory)
  - loaded vectors per `vectors.binio` path (in-memory)
- `odoc_search` caches:
  - embeddings per query
  - loaded vectors per package `vectors.binio` path

Practical guidance:
- Keep `k` small (≤10) unless you truly need breadth.
- Narrow scope when you can (`index_name` for Markdown; `package` for odoc; `index` `"ml"` vs `"mli"` for code).

---

## Troubleshooting (common “no results” failures)

- **“No Markdown indices found …”**
  - you haven’t run `md-index` / `index_markdown_docs`,
  - you’re pointing `vector_db_root` at the wrong directory,
  - the docs folder has no supported Markdown extensions.

- **“No vectors found …”**
  - index directory exists but `vectors.binio` is missing (partial/failed indexing),
  - wrong `index_name` / wrong index root.

- **`odoc_search` returns nothing**
  - you didn’t build `.odoc_index`,
  - your `--root` wasn’t an odoc HTML tree,
  - the package you expected wasn’t indexed (see the `odoc-index` hard-coded filter note above).

- **`query_vector_db` returns nothing / errors**
  - the vector db folder doesn’t contain the expected `vectors.ml.binio` / `bm25.ml.binio`,
  - you forgot to set `index: "ml"` or `"mli"` when calling the tool.

Environment requirements:
- `OPENAI_API_KEY` is required for embedding calls (indexing and queries).

---

## “Code intelligence” beyond retrieval (what exists today)

Ochat also contains OCaml-specific building blocks that are adjacent to search/indexing:

### Merlin integration (library)

`lib/merlin.ml` wraps the `ocamlmerlin` CLI and exposes:
- identifier **occurrences** (find ranges),
- **completions** (candidates + types + optional docs).

This is useful for editor-like tooling, but it is **not currently exposed as a built-in ChatMD tool**.

### Dune project introspection (library)

`lib/dune_describe.ml` shells out to `dune describe …` and parses structured output:
- local/external library dependencies,
- executable/module inventories,
- source directories and module file paths.

This can complement retrieval by helping you **scope** what to index/search (e.g. “only index these libraries” or “jump to the owning executable/library”).

