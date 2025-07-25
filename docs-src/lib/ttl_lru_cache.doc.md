# `Ttl_lru_cache` – An expiring, least-recently-used cache

## Overview

`Ttl_lru_cache` layers *time-to-live* (TTL) semantics on top of the
high-performance [`Lru_cache`](./lru_cache.doc.md) implementation already
present in the codebase:

* Every binding is stored together with its **absolute expiration timestamp** –
  a `Core.Time_ns.Span.t` measured since the Unix epoch.
* Read operations (`find`, `mem`, `find_and_remove`) treat **expired bindings
  as cache misses** and *atomically* purge them, so that future look-ups do not
  pay the cost again.
* A maintenance helper, `remove_expired`, walks the whole cache and removes all
  stale bindings in one pass.  It is O(length t) and therefore intended for
  infrequent, housekeeping scenarios (e.g. a cron-like job).

Apart from the TTL logic, the module re-exports (and delegates to) the full
[`Lru_cache`] API; hence **all operations remain amortised O(1)** with the sole
exception of `remove_expired`.

> **Thread-safety** – The implementation inherits the *single-thread* nature of
> `Lru_cache`.  Use a mutex or another synchronisation primitive when sharing a
> cache between domains/fibres.

## Quick-reference

```ocaml
module Make (K : Lru_cache.H) : sig
  type key = K.t
  type 'a t

  val create         : max_size:int -> unit -> 'a t

  (* TTL helpers *)
  val set_with_ttl   : 'a t -> key:key -> data:'a -> ttl:Time_ns.Span.t -> unit
  val remove_expired : _ t -> unit

  (* Reads with implicit eviction of stale bindings *)
  val find           : 'a t -> key -> 'a option
  val mem            : _ t -> key -> bool
  val find_and_remove: 'a t -> key -> 'a option

  (* All regular LRU operations and statistics *)
  val set            : 'a t -> key:key -> data:'a entry -> unit
  val length         : _ t -> int
  val hit_rate       : _ t -> float
  (* …see full signature for the rest… *)
end
```

## Detailed behaviour

### TTL calculation

`set_with_ttl t ~key ~data ~ttl` stores a binding that expires at

```
Time_ns.Span.since_unix_epoch () + ttl
```

where `ttl` is any (possibly fractional) duration.  A **negative** `ttl` means
the binding is already stale and will be removed on the next access.  Using a
TTL of [`Time_ns.Span.zero`] is allowed but rarely useful.

### Read path and lazy eviction

`find`, `mem`, and `find_and_remove` first look up the binding in the internal
hash-queue.  If it exists **and** has not expired they behave as in a normal
LRU cache, promoting the entry to *most-recently-used* and updating the hit
statistics.  Otherwise they:

1. Remove the stale binding.
2. Count the access as a *miss*.
3. Return `None` / `false`.

This strategy keeps reads O(1) while guaranteeing that a stale object is
removed at most once.

### `remove_expired` – eager cleanup

In scenarios where the cache may stay inactive for long periods, expirations
pile up and the first unlucky lookup afterwards would have to iterate until it
hits a fresh entry.  To avoid *that* latency spike, schedule
`remove_expired cache` periodically (e.g. every minute).  The function walks
the **whole cache** once, so its cost is proportional to the current size.

### Example – small JSON document cache

```ocaml
module String_cache = Ttl_lru_cache.Make (String)

let cache : Yojson.Safe.t String_cache.t =
  String_cache.create ~max_size:128 ()

let load_json path = Yojson.Safe.from_file path

let get_json path =
  String_cache.find_or_add cache path
    ~default:(fun () -> load_json path)
    ~ttl:(Time_ns.Span.of_min 30.)

let () =
  let doc = get_json "config.json" in
  (* …use JSON document… *)
  ()
```

The first call loads the file from disk (`default`) and caches it for
30 minutes.  Subsequent calls within the TTL window hit the cache and avoid I/O
latency.  After the TTL expires, the next caller refreshes the entry.

## Limitations

* **O(length) cleanup:** `remove_expired` is linear.  For very large caches
  consider a different strategy (lazy removal might be acceptable, or a more
  sophisticated data structure that indexes expirations).
* **Clock dependency:** Expiration is based on the local monotonic clock as
  reported by `Time_ns.Span.since_unix_epoch ()`.  Skew adjustments or manual
  clock changes will affect perceived lifetimes.
* **No per-entry refresh:** The module does not automatically extend the TTL on
  access.  If you need that behaviour, re-insert the binding with a new TTL.

## Internal invariants

* `length t ≤ max_size t` – inherited from `Lru_cache`.
* A binding is physically present in the cache *iff* it is fresh.


