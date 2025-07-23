# `Odoc_snippet` – turn Markdown into LLM-friendly chunks

`Odoc_snippet` is a tiny yet critical piece of the documentation
indexing pipeline: it splits a single Markdown file – usually obtained
from `odoc`-generated HTML – into **token-bounded snippets** that can
be embedded and searched efficiently.

```
HTML  ──▶  html-to-md  ──▶  Odoc_snippet.slice  ──▶  (meta, text) list
```

Each resulting snippet:

* contains between **64** and **320** BPE tokens;  
  a 64-token overlap with the previous chunk provides continuity;
* begins with an ocamldoc header that records package, module path and
  line range;  
  this allows consumers (e.g. `Odoc_indexer`) to render *“open in
  browser”* links;
* is identified by a **stable MD5 hash**, making de-duplication and
  incremental indexing straightforward.

---

## Table of contents

1. [Quick start](#quick-start)
2. [API reference](#api-reference)
3. [Chunking algorithm](#chunking-algorithm)
4. [Performance notes](#performance-notes)
5. [Limitations](#limitations)

---

## Quick start

```ocaml
open Core

let markdown = In_channel.read_all "_build/_doc/_html/Eio/Switch/index.md" in

let snippets =
  Odoc_snippet.slice
    ~pkg:"eio"
    ~doc_path:"Eio/Switch/index.html"
    ~markdown
    ~tiki_token_bpe:"gpt-4"
    ()
in

printf "generated %d chunks\n" (List.length snippets);

(* Dump the first header *)
let meta, body = List.hd_exn snippets in
printf "%s\n" body;
```

Typical output:

```
generated 14 chunks
(** Package:eio Module:Eio.Switch Lines:1-37 *)

# Switch – Structured cancellation

The switch controls …
```

---

## API reference

### Type [`meta`](../odoc_snippet.mli)

| Field | Meaning |
|-------|---------|
| `id` | MD5 of `[header ^ body]` |
| `pkg` | opam package name |
| `doc_path` | Relative HTML path (for hyperlinks) |
| `title` | First markdown heading, if present |
| `line_start` / `line_end` | Inclusive 1-based line range |


### `slice`

```ocaml
val slice :
  pkg:string ->
  doc_path:string ->
  markdown:string ->
  tiki_token_bpe:string ->
  unit ->
  (meta * string) list
```

The function is *deterministic* – given the same inputs it always
returns identical `id`s and snippet boundaries, allowing repeated runs
to update embeddings in-place without invalidating downstream caches.

---

## Chunking algorithm

1. **Block detection** – the Markdown is first split into *blocks* by
   `Chunker.chunk_by_heading_or_blank`:
   * headings (`#`, `##`, …);
   * fenced code segments (` ``` … ``` `);
   * table rows (`| a | b |`);
   * paragraphs separated by blank lines.
2. **Token counting** – each block is assigned a token estimate:
   * for blocks < 2 kB: exact BPE count (`Tikitoken.encode`);
   * for larger blocks: an `O(1)` length heuristic.
3. **Sliding window** – blocks are greedily accumulated until the
   window would exceed `max_tokens` (320).
4. **Overlap** – before starting the next window, the algorithm rolls
   back until the last 64 tokens (`overlap_tokens`).

The resulting windows respect paragraph/code boundaries, minimising
the risk of cutting syntax in half while keeping the implementation
fast and allocation-friendly.

---

## Performance notes

* **LRU cache** – BPE counts for blocks ≤ 10 KB are stored in an
  in-memory LRU (`max_size = 5 000`) to speed up repeated runs.
* **Linear time** – apart from token counting the algorithm is
  `O(n)` in the number of characters.
* **Parallel-friendly** – no global mutability leaks outside the cache
  guarded by an `Eio.Mutex`; calling `slice` from multiple domains is
  safe.

---

## Limitations

1. **Markdown-specific** – the heuristic block splitter is tuned for
   *odoc* output; exotic Markdown constructs may confuse it.
2. **No incremental HTML parsing** – the module assumes HTML →
   Markdown conversion to be done upfront.
3. **Heuristic token counter** – the fallback `length / 4` estimate for
   large ASCII blocks can be off by ~10 % but is good enough for
   overlap sizing.

---

*Happy chunking!* :sparkles:

