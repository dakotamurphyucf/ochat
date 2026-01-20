# `ochat query` example (hybrid code search)

This page is a **placeholder example** showing the *shape* of `ochat query` results.
The output below is illustrative — your snippet ids/content will differ.

`ochat query` runs **hybrid retrieval** over a code index created by `ochat index`:
- dense similarity for semantic matches
- BM25 for exact identifiers and token matches

## Prerequisites

Index your code:

```sh
ochat index -folder-to-index ./lib -vector-db-folder ./vector
```

## Example query

```sh
ochat query -vector-db-folder ./vector -query-text "dispatch tool declarations to implementations" -num-results 3
```

## Example output (illustrative)

`ochat query` prints code snippets (wrapped as fenced `ocaml` blocks).

````text
**Result 1:**

```ocaml
match decl with
| CM.Builtin name ->
  (match name with
   | "apply_patch" -> ...
   | "markdown_search" -> ...
   | "query_vector_db" -> ...
   | other -> failwithf "Unknown built-in tool: %s" other ())
| CM.Custom c -> ...
| CM.Agent agent_spec -> ...
| CM.Mcp mcp -> ...
```

**Result 2:**

```ocaml
let query_vector_db ~dir ~net : Ochat_function.t =
  let f (vector_db_folder, query, num_results, index) =
    ...
    Vector_db.query_hybrid corpus ~bm25 ~beta:0.4 ~embedding:query_vector ~text:query ~k:num_results
```

**Result 3:**

```ocaml
let markdown_search ~dir ~net : Ochat_function.t =
  ...
  let idxs = Vector_db.query db query_mat k in
```
````

## Tips for better results

- Prefer queries that include one “anchor” identifier plus a short description:
  - “tool dispatcher of_declaration” + “built-in mapping”
  - “vector_db query_hybrid” + “bm25”
- If you need interface-only search, you likely want the `"mli"` corpus via the ChatMD tool (`query_vector_db` with `index:"mli"`).

