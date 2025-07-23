# Cache module

In-memory **TTL-based LRU** cache used across the *chat_response* library to
memoise expensive agent executions.

The module maps a complete
`Prompt.Chat_markdown.agent_content` value – which
uniquely identifies an `<agent/>` inclusion in a ChatMarkdown document – to the
assistant answer returned by running that agent.

Key design decisions:

* **Capacity bound** – the number of live entries never exceeds the limit
  provided at creation time.  Insertions beyond that point evict the least
  recently used entry.
* **Per-entry TTL** – every cache line additionally expires after an
  individual time-to-live span.  This ensures that even rarely accessed data
  eventually refreshes.
* **Binary persistence** – the whole cache can be flushed to or restored from a
  single file using deterministic `Bin_prot` serialization.  The helpers
  [`Cache.load`] and [`Cache.save`] hide the underlying mechanics.

---

## API overview

| Function | Description |
|----------|-------------|
| `create ~max_size ()` | Create an empty cache with a maximum number of entries. |
| `find_or_add cache key ~ttl ~default` | Return the cached value for `key` or   compute / store a fresh one if the entry is missing or stale. |
| `load ~file ~max_size ()` | Load a cache from `file` if present else fall back   to `create`. |
| `save ~file cache` | Persist the whole cache to disk. |


---

## Examples

### 1.  One-off lookup

```ocaml
let result =
  Cache.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
    run_agent ~ctx prompt items)
```


### 2.  Persistent cache across runs

```ocaml
let cache_file = Path.(cwd / "cache.bin") in

(* Load existing state or start fresh *)
let cache = Cache.load ~file:cache_file ~max_size:1000 () in

(* … program logic … *)

Cache.save ~file:cache_file cache
```

---

## Limitations / notes

* The current implementation serialises the *whole* cache at once.  For very
  large caches this may incur noticeable I/O latency – *future work* could
  switch to incremental checkpoints.
* The module does not expose fine-grained control over TTL refresh behaviour –
  all calls go through `find_or_add`.  Should other access patterns appear we
  may extend the public API accordingly.

