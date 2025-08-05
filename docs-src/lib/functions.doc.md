# `Functions` – Curated toolbox exposed to the LLM agent

The `Functions` module bundles **ready-made, production-hardened
[Ochat_function] registrations** that can be advertised to an OpenAI model and
executed on demand.  Each value – `get_contents`, `apply_patch`,
`odoc_search`, … – is a *self-contained* record combining a declarative JSON
schema with an OCaml implementation.

<br/>

---

## Table of contents

1. [Quick start](#quick-start)
2. [Available tools](#available-tools)
3. [Design notes](#design-notes)
4. [Limitations](#limitations)

---

## Quick start

```ocaml
open Functions

Eio_main.run @@ fun env ->
  let cwd  = Eio.Stdenv.cwd  env
  and net  = Eio.Stdenv.net  env in

  (* Pick a subset of tools and hand them to the model *)
  let tools, dispatch =
    Ochat_function.functions
      [ get_contents    ~dir:cwd
      ; apply_patch     ~dir:cwd
      ; odoc_search     ~dir:cwd ~net
      ]

  (* tools → OpenAI;  dispatch → your inference loop *)
  |> ignore
```

Each call to `Functions.<tool>` returns a fresh `Ochat_function.t`.  The helper
constructs can therefore be instantiated multiple times with different
capabilities (e.g. sandboxed directories).

---

## Available tools

| Tool                               | JSON `name`      | Category    | Synopsis |
|------------------------------------|------------------|-------------|----------|
| `get_contents`                     | `read_file`      | filesystem  | Return the UTF-8 contents of a given file. |
| `apply_patch`                      | `apply_patch`    | filesystem  | Apply an Ochat diff to the workspace. |
| `append_to_file`                   | `append_to_file` | filesystem  | Append text to a file (creates it if missing). |
| `find_and_replace`                 | `find_and_replace` | filesystem | In-place string substitution (first or all matches). |
| `read_dir`                         | `read_directory` | filesystem  | List immediate children of a directory. |
| `mkdir`                            | `make_dir`       | filesystem  | Create a sub-directory (idempotent). |
| `get_url_content`                  | `get_url_content`| web         | Fetch a URL, strip HTML, return plain text. |
| `webpage_to_markdown`              | `webpage_to_markdown` | web   | Convert a remote page to Markdown. |
| `index_ocaml_code`                 | `index_ocaml_code`| indexing   | Crawl a folder and build a hybrid vector + BM25 index. |
| `query_vector_db`                  | `query_vector_db`| search      | Query the index built by *index_ocaml_code*. |
| `index_markdown_docs`              | `index_markdown_docs` | indexing | Build a semantic index over a folder of Markdown files. |
| `markdown_search`                  | `markdown_search`| search      | Query a Markdown index created with *index_markdown_docs*. |
| `odoc_search`                      | `odoc_search`    | search      | Semantic search over locally-indexed OCaml docs. |
| `meta_refine`                      | `meta_refine`    | misc        | Refine a raw prompt using Recursive Meta-Prompting. |
| `fork`                             | `fork`           | misc        | *Stub* – reserved for future agent-forking support. |

> ℹ️  All tools return *plain strings* – exactly what OpenAI expects today.

### 1 . `get_contents`

Read the specified file relative to the capability directory supplied during
registration.

```ocaml
let read = Functions.get_contents ~dir in
(* JSON arguments expected from the model *)
{"file": "lib/bm25.ml"}
```

### 2 . `get_url_content`

Performs an HTTP `GET`, decompresses gzip-encoded payloads with `Ezgzip`,
parses the HTML using `LambdaSoup`, and returns a single string containing the
visible text blocks.

### 3 . `index_ocaml_code`

Delegates the heavy lifting to [`Indexer.index`] which extracts embeddings
(via OpenAI), tokenises files, builds a BM-25 bag-of-words index, and stores
everything under `vector_db_folder/`.

### 4 . `query_vector_db`

Combines cosine similarity (dense vectors) with BM-25 (lexical) according to
the formula explained in [`Vector_db.query_hybrid`].  The optional
`index=<suffix>` argument lets you shard large corpora.

### 5 . `apply_patch`

Thin wrapper around [`Apply_patch.process_patch`].  Supports multi-file
add/update/delete/move operations using the *Ochat diff* syntax.

### 6 . `odoc_search`

Embeds the natural-language query with OpenAI and runs a vector search over
the pre-computed snippet embeddings stored in `.odoc_index/`.  Results are
rendered as a Markdown list identical to the command-line utility shipped with
this repository.

### 7 . `index_markdown_docs`

Chunks a directory tree of Markdown files into token-bounded snippets, embeds
them with OpenAI, and writes the resulting vectors under
`vector_db_root/<index_name>/`.  The helper is a thin wrapper around
[`Markdown_indexer.index_directory`] and therefore inherits the same
heuristics (extension filter, `.gitignore` support, context window sizing).

```ocaml
let register =
  Functions.index_markdown_docs
    ~env                     (* capability: network & clock *)
    ~dir                     (* capability: workspace root *)

(* JSON expected from the model *)
{"root": "docs", "index_name": "project_docs", "description": "Project documentation"}
```

### 8 . `markdown_search`

Semantic retrieval over one – or several – Markdown indices generated with
`index_markdown_docs`.  Candidate indices are shortlisted using cosine
similarity on catalogue vectors before the selected stores are queried for the
top-`k` snippets.

```ocaml
let search = Functions.markdown_search ~dir ~net in

(* JSON arguments *)
{"query": "how to configure dune for js_of_ocaml", "k": 3, "index_name": "project_docs"}
```

### 9 . `append_to_file`

Append arbitrary text to the end of a file.  If the target file does not yet
exist it will be created.  The helper prefixes the payload with a newline so
that multiple calls result in clean paragraph breaks.

```ocaml
let append = Functions.append_to_file ~dir in

(* JSON expected from the model *)
{"file": "CHANGELOG.md", "content": "\n## 0.2.0 – 2025-08-05\n* Add new CLI flags"}
```

### 10 . `find_and_replace`

Search–replace convenience wrapper.  Receives a four-tuple
`(file, search, replace, all)` and rewrites the file in-place using
`String.substr_replace_*`:

* When `all = false` only the **first** occurrence is changed.  An error is
  returned if multiple matches are present – in that case you should fall back
  to the more explicit `apply_patch` tool.
* When `all = true` all non-overlapping matches are replaced.

```ocaml
let sub = Functions.find_and_replace ~dir in

(* Replace absolute imports with ppxlib qualified ones *)
{"file": "lib/parser.ml", "search": "open Ast", "replace": "open Ppxlib.Ast", "all": true}
```

### 11 . `meta_refine`

Performs *Recursive Meta-Prompting* ([paper link](https://arxiv.org/abs/2302.00000))
on an input prompt.  Useful for auto-improving user instructions before they
are forwarded to another agent or LLM.

```ocaml
let refine = Functions.meta_refine in

refine """You are ChatGPT, a large language model trained by OpenAI."""
(* ⇒ returns a better structured system prompt *)
```

---

## Design notes

* **Capability-oriented** – no ambient authority.  Filesystem and network
  access are supplied explicitly.
* **Stateless** – each tool is a pure function `string -> string`; long-running
  side effects (like indexing) are performed inside `Eio.Switch.run` to ensure
  clean-up on cancellation.
* **Thread-safe caches** – `odoc_search` maintains small in-memory caches for
  embeddings and vector blobs, protected by an `Eio.Mutex`.

---

## Limitations

1. Return type is fixed to `string`; structured results require manual JSON
   encoding.
2. `get_url_content` performs no readability heuristics; large pages may blow
   the context window.
3. `fork` is a placeholder awaiting a full multi-agent orchestrator.

---

© 2025 – Ochat example documentation

