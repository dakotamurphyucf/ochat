# Search, indexing & code-intelligence

Ochat ships several indexers and searchers that work together to give agents
fast, cheap access to documentation and source code. You can drive them from
the command line or directly from ChatMD via built-in tools.

## Index types and entrypoints

| Corpus            | Indexer CLI   | Search CLI    | ChatMD indexing tools | ChatMD search tools   | Notes |
|-------------------|---------------|---------------|-----------------------|-----------------------|-------|
| OCaml docs (odoc) | `odoc-index`  | `odoc-search` | – CLI only            | `odoc_search`         | odoc HTML → Markdown snippets, vectors + BM25 per package; search currently uses dense vectors only. |
| Markdown docs     | `md-index`    | `md-search`   | `index_markdown_docs` | `markdown_search`     | Slices Markdown into overlapping 64–320 token windows; pure vector search over one or more indexes. |
| OCaml source      | `ochat index` | `ochat query` | `index_ocaml_code`    | `query_vector_db`     | Parses modules and docstrings, builds vectors + BM25; hybrid vector + BM25 ranking. |

The usual pattern is:

1. Run the CLI indexers in CI or as a batch job to build or refresh
   your indices.
2. Commit or ship the resulting `.md_index`, `.odoc_index` or `vector`
   directories alongside your code.
3. From ChatMD prompts, call the corresponding search tools to retrieve
   relevant context at run time instead of pasting large blobs into the
   prompt.

## On-disk layout (cheat sheet)

### Markdown docs (`.md_index`)

`md-index` and the `index_markdown_docs` tool both build the same layout:

- Root directory (defaults to `.md_index`)
  - `md_index_catalog.binio` – catalogue of logical index names with
    descriptions and centroid vectors.
  - `<index_name>/`
    - `vectors.binio` – array of snippet vectors.
    - `snippets/<id>.md` – Markdown body for each snippet.

`md-search` and `markdown_search` read this layout. When you search across
"all" indexes, they first shortlist a few promising indexes using the
catalogue, then run dense vector search inside those.

### Odoc docs (`.odoc_index`)

`odoc-index` builds a per-package layout under (by default) `.odoc_index`:

- Root directory
  - `package_index.binio` – coarse index of packages for initial
    shortlisting.
  - `<pkg>/`
    - `vectors.binio` – array of snippet vectors for that package.
    - `bm25.binio` – BM25 index built from the same snippets.
    - `<id>.md` – Markdown body for each snippet.

`odoc-search` and the `odoc_search` tool expect this directory structure. They
use the package index plus dense vectors today; the BM25 files exist but are
not part of the main scoring path.

### OCaml source vector DB

`ochat index` and the `index_ocaml_code` tool both write into a vector
database directory (often `./vector`):

- Root directory
  - `vectors.ml.binio` / `vectors.mli.binio` – vectors for implementation and
    interface snippets.
  - `bm25.ml.binio` / `bm25.mli.binio` – BM25 indices over the same snippets.
  - `<hash>` – individual snippet bodies named by a stable hash.

`ochat query` and the `query_vector_db` tool load these files and use hybrid
vector + BM25 scoring. The CLI always queries the `ml` corpus; the tool lets
you choose between `ml` and `mli` via its `index` parameter.

## Using search from ChatMD

In ChatMD prompts you typically declare the search tools you want and let the
model choose how to combine them. For example:

```xml
<config model="gpt-4o" temperature="0" />

<tool name="odoc_search" />
<tool name="markdown_search" />
<tool name="query_vector_db" />

<user>
  1. Search OCaml API docs for `Eio.Switch` usage.
  2. Search our markdown design docs for "streaming".
  3. Search the vector DB for "tail-recursive map".
  4. Combine the findings into one explanation.
</user>
```

The assistant can decide which tool to call first, stitch the results
together and keep the transcript small by referring to indexed snippets
instead of pasting entire files. When you also expose
`index_markdown_docs` and `index_ocaml_code` as tools, the model can even
bootstrap or refresh indices as part of longer-running workflows.

