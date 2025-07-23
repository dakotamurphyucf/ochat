# `Embed_service`

Concurrency-friendly wrapper around the OpenAI *Embeddings* HTTP endpoint.

It batches snippet texts, enforces a global **rate-limit** (requests/second),
handles retry/back-off on transient failures and converts results straight
into `Vector_db.Vec` records.

---

## Why a dedicated service?

Indexing often needs to embed thousands of snippets.  Doing this naïvely in a
single blocking loop is wasteful – we want to:

1. **Pipeline** work from multiple producer fibres (Markdown/Odoc indexers).
2. **Cap throughput** to stay within model rate-limits.
3. **Retry** failed requests transparently.

`Embed_service.create` returns a *function* that you call with a list of
(`meta`, `text`) pairs.  Under the hood these requests are sent on a stream
to a background daemon that serialises actual HTTP calls.

---

## API

```ocaml
val create :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  codec:Tikitoken.codec ->
  rate_per_sec:int ->
  get_id:('meta -> string) ->
  ('meta * string) list -> ('meta * string * Vector_db.Vec.t) list
```

* The returned function is **thread-safe** (can be called from any fibre).
* Vector length is computed locally using `token_count` to avoid an extra API
  call.

---

## Implementation details

* Uses `Stream` (bounded) as the producer-consumer queue.
* Retry logic sleeps `1.0` seconds between attempts (max 3 tries).
* The last call timestamp is tracked to respect `rate_per_sec`.

