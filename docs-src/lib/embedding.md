# Embedding the libraries & caching

Every public binary is a thin wrapper over libraries available under `lib/`.
You can reuse the same pieces in your own code both for ChatMD conversations
and for building search indices.

## Driving ChatMD conversations from OCaml

`Chat_response.Driver.run_completion_stream_in_memory_v1` is the main entry
point when you want to execute a ChatMarkdown conversation entirely in memory
without touching a `.chatmd` file on disk:

```ocaml
let run ~env ~history =
  Chat_response.Driver.run_completion_stream_in_memory_v1
    ~env
    ~history
    ~tools:None
    ~model:`Gpt4o
    ()
```

The function takes an `Eio_unix.Stdenv.base` `env` and an
`Openai.Responses.Item.t list` `history`, streams tokens as they arrive, and
returns the updated history. It handles tool discovery and caching for you and
persists a shared agent cache under a `.chatmd/cache.bin` directory chosen by
the caller (CLI, TUI, tests).

If you want to manage that cache yourself, use `Chat_response.Cache`:

```ocaml
let cache_file = Path.(cwd / "cache.bin") in
let cache = Chat_response.Cache.load ~file:cache_file ~max_size:1000 () in

(* use [cache] through Ctx.of_env and the Chat_response helpers *)

Chat_response.Cache.save ~file:cache_file cache
```

## Embedding indices

The same embedding stack underpins all the indexers:

- `Markdown_indexer.index_directory` (driven by the `md-index` CLI or the
  `index_markdown_docs` ChatMD tool).
- `Odoc_indexer.index_packages` (driven by the `odoc-index` CLI).
- `Indexer.index` for OCaml source (driven by `ochat index` or the
  `index_ocaml_code` ChatMD tool).

Markdown and odoc indexers use `Embed_service` internally to batch requests
and respect rate limits; the code indexer calls `Openai.Embeddings` directly.


## Caching in practice

Most higher-level helpers share a TTL-LRU cache so expensive work is only done
once per input.

Agent executions use `Chat_response.Cache`, which is implemented on top of
`Ttl_lru_cache` and exposes `create`, `find_or_add`, `load` and `save`. The
cache lives in memory while your program runs and can be saved and restored
across runs with `Chat_response.Cache.save` and
`Chat_response.Cache.load`.

Other parts of the system reuse the same caching building blocks:

- `Webpage_markdown.Tool` keeps a small TTL-LRU of URL-to-Markdown
  conversions.
- `Markdown_snippet` and `Odoc_snippet` use `Lru_cache` to memoise token
  counts.
- The TUI and CLI entry points both initialise a shared `Chat_response.Cache`
  under a `.chatmd/cache.bin` directory so repeated agents stay fast.

Need to embed docs for a project? `Odoc_indexer.index_packages` and
`Markdown_indexer.index_directory` are the main entry points; combine them
with the search tools (`odoc_search`, `markdown_search`, `query_vector_db`) to
build your own RAG workflows.

