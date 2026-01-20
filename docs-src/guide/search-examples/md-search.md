# `md-search` example (Markdown docs search)

This page is a **placeholder example** showing the *shape* of `md-search` results.
The output below is illustrative — your snippet ids and content will differ.

## Prerequisites

Build at least one Markdown index:

```sh
md-index --root docs-src --name docs --desc "Project docs" --out .md_index
```

## Example query

```sh
md-search \
  --query "how does tool calling work?" \
  --index all \
  --index-dir .md_index \
  -k 3
```

## Example output (illustrative)

`md-search` prints a ranked list. Each item includes:
- the index name,
- a snippet id,
- a Markdown preview of the snippet body.

```text
[1] [docs] 9b3e4a1d6a0a3c9c8e7f1b2c3d4e5f6a
(** Package:docs Doc:docs-src/overview/tools.md Lines:120-180 *)

Tools are opt-in: the model can only call what your prompt declares via <tool .../>.
...

---

[2] [docs] 1a2b3c4d5e6f77889900aabbccddeeff
(** Package:docs Doc:docs-src/overview/chatmd-language.md Lines:40-95 *)

ChatMarkdown is a Markdown + XML dialect...
...

---

[3] [docs] deadbeefdeadbeefdeadbeefdeadbeef
(** Package:docs Doc:README Lines:200-260 *)

Tools & capabilities (quick tour) ...
...
```

## Tips for better results

- Use a short, information-dense query (“tool declaration schema”, “ChatMD tool dispatch”, “MCP tool caching”).
- If you have multiple indexes, prefer `--index <name>` when you know which one you want, and use `--index all` for exploration.

