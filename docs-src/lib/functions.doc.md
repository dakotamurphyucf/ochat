# `Functions` – Curated toolbox exposed to the LLM agent

The `Functions` module bundles ready-made
`Ochat_function` registrations that can be advertised to a model and
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
let registrations env =
  let dir = Eio.Stdenv.cwd env in
  Ochat_function.functions
    [ Functions.get_contents ~dir
    ; Functions.apply_patch ~dir
    ; Functions.odoc_search ~dir ~net:(Eio.Stdenv.net env)
    ]
```

Each call to `Functions.<tool>` returns a fresh `Ochat_function.t`.  The helper
constructs can therefore be instantiated multiple times with different
capabilities. The returned dispatch runners require an invocation and return
`Openai.Responses.Tool_output.Output.t`. See [custom tools](gpt_function.doc.md)
for a compiled example. Registering a directory does not universally sandbox
every tool: use each tool's documented path and authorization contract.

---

## Available tools

| Tool                               | JSON `name`      | Category    | Synopsis |
|------------------------------------|------------------|-------------|----------|
| `get_contents`                     | `read_file`      | filesystem  | Return (part of) a UTF-8 file relative to one capability directory. |
| `get_contents_scoped`              | `read_file`      | filesystem  | Read regular text files confined to named canonical directory roots. |
| `apply_patch`                      | `apply_patch`    | filesystem  | Apply an Ochat diff to the workspace. |
| `append_to_file`                   | `append_to_file` | filesystem  | Append text to a file (creates it if missing). |
| `find_and_replace`                 | `find_and_replace` | filesystem | In-place string substitution (first or all matches). |
| `read_dir`                         | `read_directory` | filesystem  | List immediate children of a directory. |
| `mkdir`                            | `mkdir`          | filesystem  | Create a sub-directory (idempotent). |
| `get_url_content`                  | `get_url_content`| web         | Fetch a URL, strip HTML, return plain text. |
| `webpage_to_markdown`              | `webpage_to_markdown` | web   | Convert a remote page to Markdown. |
| `index_ocaml_code`                 | `index_ocaml_code`| indexing   | Crawl a folder and build a hybrid vector + BM25 index. |
| `query_vector_db`                  | `query_vector_db`| search      | Query the index built by *index_ocaml_code*. |
| `index_markdown_docs`              | `index_markdown_docs` | indexing | Build a semantic index over a folder of Markdown files. |
| `markdown_search`                  | `markdown_search`| search      | Query a Markdown index created with *index_markdown_docs*. |
| `odoc_search`                      | `odoc_search`    | search      | Semantic search over locally-indexed OCaml docs. |
| `meta_refine`                      | `meta_refine`    | misc        | Refine a raw prompt using Recursive Meta-Prompting. |
| `fork`                             | `fork`           | agent       | Declaration stub intercepted by the host's implemented nested-fork driver; do not call this registration directly. |
| `import_image`                     | `import_image`   | filesystem  | Return an image as a data-URI suitable for image-input tool outputs. |

> ℹ️  Most tools return a plain string (`Tool_output.Output.Text`). The exception
> is `import_image`, which returns a structured image part
> (`Tool_output.Output.Content`) when the file exists.

### 1 . `get_contents`

Read the specified file relative to the capability directory supplied during
registration.

```ocaml
let read_example dir =
  let tool = Functions.get_contents ~dir in
  tool.run {|{"file":"lib/bm25.ml","offset":0,"line_count":200}|}
```

Notes:

- The JSON decoder also accepts `"path"` as an alias for `"file"`.
- The returned text includes a ripgrep-like header:

  ```
  lib/bm25.ml:1-200:
  [total_lines=1234]
  ...
  ```

- Binary files are rejected (best-effort heuristic).

For ChatMD-style named roots, construct `read_file_root` values and register
`get_contents_scoped`:

```ocaml
let scoped_reader env ~project_dir ~package_docs =
  let project =
    Functions.read_file_root ~id:"project" ~path:project_dir
      ~description:"Project source" ()
  in
  let packages =
    Functions.read_file_root ~id:"packages" ~path:package_docs
      ~description:"Installed package documentation" ()
  in
  Functions.get_contents_scoped
    ~fs:(Eio.Stdenv.fs env)
    ~dir:(Eio.Stdenv.cwd env)
    ~roots:[ project; packages ]
    ~description:"Prefer packages for dependency questions."
    ()
```

The scoped schema requires `file`, accepts optional `root`, `offset`, and
`line_count`, and enumerates the valid root IDs. Its description lists each
root's resolved absolute native path and optional description, followed by the
caller-supplied description. Without `root`, a relative path resolves from
`dir` but must still be inside one root. With `root`, `file` must be relative
to that root. Absolute paths are accepted only without `root` and only inside
a root.

Roots and targets are canonicalized before confinement checks, preventing
`..` and symlink escapes. Roots must be existing directories; targets must be
existing regular, non-binary-like files. `offset` and `line_count` must be
non-negative. The simpler `get_contents ~dir` remains available to library
callers and the standalone MCP server; ChatMD root declarations are lowered
to `get_contents_scoped` by `Chat_response.Tool`.

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

Wraps [Apply_patch](apply_patch.doc.md). Supports multi-file add/update/delete/move
operations using the Ochat patch syntax. Writes/deletes are sequential, not an
atomic transaction; inspect the working tree after failure before retrying.

### 6 . `odoc_search`

Embeds the natural-language query with OpenAI and runs a vector search over
the pre-computed snippet embeddings stored in `.odoc_index/`.  Results are
rendered as Markdown snippets. This registration searches all package directories
when `package="all"`; the standalone CLI's shortlist and other options differ.
See [search behavior](../guide/search-and-indexing.md).

### 7 . `index_markdown_docs`

Chunks a directory tree of Markdown files into token-bounded snippets, embeds
them with OpenAI, and writes the resulting vectors under
`vector_db_root/<index_name>/`.  The helper is a thin wrapper around
[`Markdown_indexer.index_directory`] and therefore inherits the same
heuristics (extension filter, `.gitignore` support, context window sizing).

```ocaml
let register env dir =
  Functions.index_markdown_docs
    ~env
    ~dir
```

Example model arguments:

```json
{"root": "docs", "index_name": "project_docs", "description": "Project documentation"}
```

### 8 . `markdown_search`

Semantic retrieval over one – or several – Markdown indices generated with
`index_markdown_docs`.  Candidate indices are shortlisted using cosine
similarity on catalogue vectors before the selected stores are queried for the
top-`k` snippets.

```ocaml
let search_example dir net =
  let tool = Functions.markdown_search ~dir ~net in
  tool.run {|{"query":"configure dune for js_of_ocaml","k":3,"index_name":"project_docs"}|}
```

### 9 . `append_to_file`

Append arbitrary text to the end of a file.  If the target file does not yet
exist it will be created.  The helper prefixes the payload with a newline so
that multiple calls result in clean paragraph breaks.

```ocaml
let append_example dir =
  let tool = Functions.append_to_file ~dir in
  tool.run {|{"path":"CHANGELOG.md","content":"Document the new CLI flags."}|}
```

### 10 . `find_and_replace`

Search–replace convenience wrapper.  Receives a four-tuple
`(path, find, replace, all)` and rewrites the file in-place using
`String.substr_replace_*`:

* When `all = false` only the **first** occurrence is changed.  An error is
  returned if multiple matches are present – in that case you should fall back
  to the more explicit `apply_patch` tool.
* When `all = true` all non-overlapping matches are replaced.

```ocaml
let replace_example dir =
  let tool = Functions.find_and_replace ~dir in
  tool.run {|{"path":"lib/parser.ml","find":"open Ast","replace":"open Ppxlib.Ast","all":true}|}
```

### 11 . `meta_refine`

Runs the [meta-prompting flow](meta_prompting.doc.md) on a prompt and task.
An empty prompt selects generation; a nonempty prompt selects updating.
Execution can make provider requests; constructing the registration does not.

```ocaml
let refine_example env =
  let tool = Functions.meta_refine ~env in
  tool.run {|{"prompt":"Review code carefully.","task":"Review OCaml changes for correctness."}|}
```

---

## Design notes

* **Explicit capabilities** – callers supply directory/network/environment
  handles. This is not a promise that a broad handle confines every operation.
* **Effectful runners** – registrations decode serialized input and return typed
  output. Running them may change files, start work, use caches or call providers;
  they are not pure functions. Some older tools report failures as output text
  instead of raising, so callers must respect the individual contract.
* **Thread-safe caches** – `odoc_search` maintains small in-memory caches for
  embeddings and vector blobs, protected by an `Eio.Mutex`.

---

## Limitations

1. Results can be text or structured text/image content. Individual tools still
   determine their error convention; generic registration adds no validation,
   secret redaction, transaction rollback or retry policy.
2. `get_url_content` performs no readability heuristics; large pages may blow
   the context window.
3. `Functions.fork.run` is only a declaration stub. The host intercepts it and
   uses the [fork runtime](chat_response/fork.doc.md) for nested execution.

---

© 2025 – Ochat example documentation
