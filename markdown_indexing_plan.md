# Markdown Indexing and Semantic Search – Implementation Plan

This document describes how to add _generic_ Markdown indexing and search capabilities to the code-base.  The new feature must let an LLM agent ingest **any directory of `*.md` files** (e.g. `lib/docs-src`) and later answer natural-language questions over that content by calling a tool.

The design deliberately mirrors the existing **odoc documentation** pipeline so that most helper code (vector DB, OpenAI batching, task-pool, etc.) can be reused unchanged.

---

## 1 · High-level architecture

```
                 ┌─────────────────────────┐
                 │  markdown_crawler.ml    │  ① walk directory tree
                 └───────────┬─────────────┘
                             │ Markdown text
                             ▼
                 ┌─────────────────────────┐
                 │  markdown_snippet.ml    │  ② slice 64-320 token windows
                 └───────────┬─────────────┘
                             │ (meta,text) list
                             ▼
                 ┌─────────────────────────┐
                 │  markdown_indexer.ml    │  ③ orchestrate
                 └───────────┬─────────────┘
                             │ embeddings
                             ▼
   .md_index/<index_name>/ ──► vectors.binio
                     └─ snippets/<id>.md

Search side

  markdown_search (Gpt_function)         ④ vector retrieval
        ▲
        │ LLM agent tool call            ⑤ return top-k snippets
```

Pipeline steps:
1. **crawl** directory → raw Markdown text together with its *relative path* (no implicit grouping).
2. **slice** text into overlapping token-bounded windows, attach metadata.
3. **embed** slices (OpenAI) → write to disk under `.md_index/<index_name>` chosen by the user.
4. **query** vectors using cosine similarity (`Vector_db.query`).

---

## 2 · Key components

### 2.1 `Markdown_crawler`
* Recursively traverses a given root directory while respecting ignore patterns (`.gitignore` & optional additional list).
* Emits every file whose basename ends in one of `[".md"; ".markdown"; ".mdown"]` **and** is non-empty.
* **No attempt** is made to determine logical sub-packages – every document belongs to the single *index* specified by the caller.
* Signature mirrors `Odoc_crawler.crawl` but drops the `pkg` arg:

```ocaml
val crawl :
  root:Eio.Path.t ->
  f:(doc_path:string -> markdown:string -> unit) -> unit
```

Implementation notes
* Reuse the same concurrent traversal pattern (`Fiber.List.iter ~max_fibers:25`).
* Use `Eio.Path.read` to load the file as UTF-8; files >10 MiB are skipped for safety.
* **Gitignore support**: best-effort parsing of the nearest `.gitignore` – supports `*`, `?`, and trailing slash directory globs.  Falls back to a static deny-list (`.^`, `_build`, `dist`, `node_modules`, etc.) if the file is missing.

### 2.2 `Markdown_snippet`

Responsibilities identical to `Odoc_snippet` but tailored for “hand-written” markdown which may be less regular than odoc output.

Behaviour
* **Chunk size**: 64 ≤ tokens ≤ 320, 20 % overlap (≈64 tokens).
* **Block detection**: split on headings (`^#`), thematic breaks (`---`), blank lines, fenced code blocks and tables.
  * Re‐use (`include`) `Odoc_snippet.Chunker`; extend pattern for thematic breaks if needed.
* **Token counting** uses the shared LRU‐cached wrapper around `tikitoken`.

Data type (almost the same as `Odoc_snippet.meta`):

```ocaml
type meta = {
  id         : string;   (* md5(meta+body)          *)
  index      : string;   (* user-supplied index name *)
  doc_path   : string;   (* path relative to root   *)
  title      : string option; (* first H1/H2 if any *)
  line_start : int;
  line_end   : int;
} [@@deriving sexp, bin_io, compare, hash]
```

`Markdown_snippet.slice` → `(meta * string) list` (with header prepended exactly like in odoc version).

### 2.3 `Markdown_indexer`

Entry-point replaces the “package” concept with a user-supplied **index name** and **description**.

```ocaml
val index_directory :
  ?vector_db_root:string    (* default ".md_index"       *) ->
  index_name:string         (* logical id e.g. "docs"    *) ->
  description:string        (* one-line blurb            *) ->
  root:Eio.Path.t           (* folder to crawl           *) ->
  unit
```

Process:
1. Create `Task_pool` for CPU-intensive slicing (one domain/core).
2. Use `Markdown_crawler.crawl` to feed all docs into the pool; gather slices.
3. Batch ≤300 snippets, call OpenAI embeddings with retry/back-off (reuse `Embed_service`).
4. Persist under `.md_index/<index_name>/`:
   * `vectors.binio`    – dense vectors.
   * `snippets/<id>.md` – raw markdown.
5. Update the **index catalogue** (`md_index_catalog.binio`) containing `(name, description, centroid_vector)` so the search tool can shortlist relevant indexes.

### 2.4 `Markdown_search` (tool)

Implemented as a `Gpt_function` (see `functions.ml`).  Inputs:

```json
{
  "query":            "string",
  "k":                5  (default),
  "index_name":       "string"   ("all" allowed),
  "vector_db_root":   "string"   (default ".md_index")
}
```

Algorithm:
1. Embed user query (cached).
2. Determine candidate indexes:
   * If `index_name = "all"`, shortlist top-N (default 5) using the index catalogue; otherwise use the explicitly named index.
3. For each candidate, lazy-load the associated `vectors.binio` (kept in an in-memory cache).
4. Run `Vector_db.query` for each database (e.g. k = 10).  Merge results into a global ranking table keyed by cosine similarity.
5. Return an array of JSON objects sorted by score, each containing `index`, `score`, a truncated `snippet`, and `source` ("path#Lstart-Lend").

---

## 3 · Supporting data structures

* **Vector store layout** (per index)

```
.md_index/<index_name>/
  ├─ vectors.binio          (Vector_db.Vec array)
  └─ snippets/
      └─ <id>.md
```

* **Index catalogue** – `md_index_catalog.binio` with the same wire-format as `Package_index`, storing `(name, description, centroid_vector)` for each created index.

---

## 4 · Tool-chain integration

`chat_response/tool.ml` is the single source of truth for registering functions exposed to the agent.  We will:

1. Define `Definitions.Index_markdown_docs` & `Definitions.Markdown_search` modules mirroring the signatures of existing ones.
2. Provide runtime wrappers in `functions.ml` that capture `~dir` (current working directory) and any shared caches.
3. Edit `lib/chat_response/tool.ml` to include these in the lookup list, right next to `odoc_search`.  **No** modifications to prompt templates are required – ChatMD will automatically advertise the tools declared here.

---

## 5 · Implementation steps

1. **Scaffolding**:  create empty modules + dune stanzas.
2. Implement `Markdown_crawler` (≈150 loc).
3. Implement `Markdown_snippet` (~400 loc but 90 % copy from `Odoc_snippet`).
4. Refactor common logic in `odoc_indexer` & `indexer.ml` into `embed_service.ml` (shared), if beneficial.
5. Implement `Markdown_indexer` reusing shared helpers.
6. Add new `Gpt_function` definitions and register in runtime.
7. Add CLI executable `bin/md_index.ml` for manual use: `gpt md-index --root lib/docs-src` and `gpt md-search --query "…"`.
8. Unit tests (Alcotest):
   * `chunking` – ensure a 1 000-token sample produces ≥ 4 snippets all within the token bounds.
   * `index_roundtrip` – index the bundled fixture docs, then query with a unique phrase; assert the top hit covers that phrase.
   * `catalogue_lookup` – create two dummy indexes with orthogonal descriptions; assert the catalogue shortlist picks the correct one.
9. Documentation in `lib/docs-src/markdown_indexer.doc.md` explaining usage &  design.

---

## 6 · Potential challenges & mitigations

| Challenge | Mitigation |
|-----------|-----------|
| Very large Markdown (>1 MiB) | Abort with warning; encourage authors to split docs. |
| Non-UTF-8 files | On encoding error, skip and log. |
| Rate limits on embeddings | Reuse `Embed_service` with back-off and global semaphore. |
| Memory during indexing | Stream per-package; flush vectors periodically. |
| Multiple indexes with similar content | Catalogue scoring ensures most relevant indexes are considered; user can override by specifying `index_name`. |

---

## 7 · Libraries / third-party dependencies

* **`omd`** – lightweight CommonMark parser (used only for heading extraction if we want structural accuracy; current heuristic splitter already works without it so keep as optional compile-time dependency).
* **`tikitoken`**, `owl`, `core`, `eio`, `soup` – already in tree.
* **`ocaml-gitignore`** – optional, for Gitignore parsing (fallback to heuristic if absent).

No additional system dependencies are required.

---

## 8 · Backwards compatibility & extensibility

* The design keeps `.odoc_index` and `.md_index` **separate** – no risk of collision.
* If desired both pipelines could later be unified under a generic `Doc_kind` GADT.
* Snippet header format stays identical to odoc version so downstream viewers (GitHub UI, ChatTUI) can render either without changes.

---

End of plan – ready for implementation.

---

## TODO List (living document)

| Task | State |
|------|-------|
| Scaffold new modules (`markdown_crawler.ml`, `markdown_snippet.ml`, `markdown_indexer.ml`) & update `dune` | completed |
| Implement `Markdown_crawler` with `.gitignore` support & 10 MiB cap | completed |
| Implement `Markdown_snippet` (chunker reuse + token logic) | completed |
| Refactor shared embedding logic into `embed_service.ml`  | completed |
| Implement `Markdown_indexer` pipeline incl. centroid computation & persistence | completed |
| Build index catalogue (`md_index_catalog.binio`) writer/loader | completed |
| Add `Definitions.Index_markdown_docs` & `Definitions.Markdown_search` | pending |
| Wire wrappers in `functions.ml` | pending |
| Register tools in `lib/chat_response/tool.ml` | pending |
| Provide CLI wrappers `bin/md_index.ml` & `bin/md_search.ml` | pending |
| Add optional `ocaml-gitignore` dependency and fallback matcher | pending |
| Unit tests: `chunking`, `index_roundtrip`, `catalogue_lookup` | pending |
| Developer documentation (`markdown_indexer.doc.md`) | pending |
| Expect tests for `markdown_crawler` and other new modules | pending |
| Developer documentation (`markdown_crawler.doc.md`) and for other new modules | pending |

> Follow the Task States & Management rules when updating this table during implementation.

