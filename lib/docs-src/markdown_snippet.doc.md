# `Markdown_snippet`

Break Markdown documents into *token-bounded* slices that can be sent to a
language-model embedding API.

The implementation is almost a copy of `Odoc_snippet` with changes for the
less regular structure of hand-written docs.

---

## Chunking rules

* Target window: **64 – 320 tokens** (inclusive).
* Overlap: **64 tokens** (≈20 %).
* Split boundaries:
  * ATX headings (`#`, `##`, …​).
  * Thematic breaks (`---`, `***`, `___`).
  * Blank lines.
  * Fenced code blocks (`` ``` ``) – kept intact.
  * GFM tables (rows starting with `|`).

The logic lives in the internal `Chunker` module and can be reused by callers.

---

## Metadata

```ocaml
type meta = {
  id         : string;        (* md5(meta+body)          *)
  index      : string;        (* index name supplied by user *)
  doc_path   : string;        (* relative path *)
  title      : string option; (* first H1/H2 heading *)
  line_start : int;
  line_end   : int;
}
```

`id` serves as the primary key for persistence and deduplication.

---

## Public function

```ocaml
val slice :
  index_name:string ->
  doc_path:string ->
  markdown:string ->
  tiki_token_bpe:string ->    (* contents of Tikitoken BPE file *)
  unit ->
  (meta * string) list        (* header prepended to body *)
```

The resulting list can be fed directly into `Embed_service`.

---

## Performance notes

* Token counting uses an **LRU cache** (~5000 entries) protected by an
  `Eio.Mutex` – safe to call from multiple fibres.
* When `tikitoken` fails (e.g. malformed UTF-8) a heuristic word-count fallback
  keeps the pipeline running.

