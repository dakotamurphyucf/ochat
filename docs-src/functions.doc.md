# `Functions` – Curated toolbox exposed to the LLM agent

The `Functions` module bundles **ready-made, production-hardened
[Gpt_function] registrations** that can be advertised to an OpenAI model and
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
    Gpt_function.functions
      [ get_contents    ~dir:cwd
      ; apply_patch     ~dir:cwd
      ; odoc_search     ~dir:cwd ~net
      ]

  (* tools → OpenAI;  dispatch → your inference loop *)
  |> ignore
```

Each call to `Functions.<tool>` returns a fresh `Gpt_function.t`.  The helper
constructs can therefore be instantiated multiple times with different
capabilities (e.g. sandboxed directories).

---

## Available tools

| Tool                               | JSON `name`      | Category    | Synopsis |
|------------------------------------|------------------|-------------|----------|
| `get_contents`                     | `read_file`      | filesystem  | Return the UTF-8 contents of a given file. |
| `get_url_content`                  | `get_url_content`| web         | Fetch a URL, strip HTML, return plain text. |
| `index_ocaml_code`                 | `index_ocaml_code`| indexing   | Crawl a folder and build a hybrid vector + BM25 index. |
| `query_vector_db`                  | `query_vector_db`| search      | Query the index built by *index_ocaml_code*. |
| `apply_patch`                      | `apply_patch`    | filesystem  | Apply a ChatGPT diff to the workspace. |
| `read_dir`                         | `read_directory` | filesystem  | List immediate children of a directory. |
| `mkdir`                            | `make_dir`       | filesystem  | Create a sub-directory (idempotent). |
| `odoc_search`                      | `odoc_search`    | search      | Semantic search over locally-indexed OCaml docs. |
| `webpage_to_markdown`              | `webpage_to_markdown` | web   | Convert a remote page to Markdown. |
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
add/update/delete/move operations using the *ChatGPT diff* syntax.

### 6 . `odoc_search`

Embeds the natural-language query with OpenAI and runs a vector search over
the pre-computed snippet embeddings stored in `.odoc_index/`.  Results are
rendered as a Markdown list identical to the command-line utility shipped with
this repository.

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

© 2025 – ChatGPT example documentation

