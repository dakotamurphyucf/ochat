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

### Parameters

* `sw` – parent switch used to supervise the background worker fibre.
* `clock` – wall clock (from `Eio.Time`) for throttling and back-off.
* `net` – `Eio.Net.t` capability for issuing HTTPS requests.
* `codec` – `Tikitoken.codec` to count tokens locally (avoids an extra API call).
* `rate_per_sec` – hard cap on outgoing requests; must be **> 0**.
* `get_id` – pure function converting the caller-supplied metadata into a
  stable identifier (stored in `Vector_db.Vec.id`).

### Behaviour

* Calls from arbitrary fibres enqueue work on a bounded stream and return a
  promise that resolves when the HTTP call completes.
* The service enforces `rate_per_sec` globally; if necessary it sleeps the
  worker fibre before issuing the next request.
* Each request is retried up to **three** times (1 s back-off) on transient
  failures (network hiccups, 5xx responses). The third failure is re-raised to
  the caller.

### Example

```ocaml
open Eio.Std

let () = Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let embed =
    Embed_service.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~net:(Eio.Stdenv.net env)
      ~codec:Tikitoken.Cl100k_base.codec
      ~rate_per_sec:10
      ~get_id:Digest.string
  in
  let snippets = [ ("README.md#intro", "OpenAI provides powerful models …") ] in
  match embed snippets with
  | [ (_meta, _text, vec) ] ->
      Format.printf "Vector dim = %d\n" (Array.length vec.vector)
  | _ -> assert false
```

---

## Known limitations

* Only a single worker fibre is spawned; peak throughput is therefore capped
  at `rate_per_sec` (no parallelism beyond that).
* `rate_per_sec` is applied per process – if you spin up multiple processes
  you must enforce the global limit yourself.
* The service does not split inputs automatically; the caller must ensure that
  the batch of texts fits below the model's context window.

* The returned function is **thread-safe** (can be called from any fibre).
* Vector length is computed locally using `token_count` to avoid an extra API
  call.

---

## Implementation details

* Uses `Stream` (bounded) as the producer-consumer queue.
* Retry logic sleeps `1.0` seconds between attempts (max 3 tries).
* The last call timestamp is tracked to respect `rate_per_sec`.

