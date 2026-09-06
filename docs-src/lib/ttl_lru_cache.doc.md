# Ttl_lru_cache — expiring LRU cache

[Interface](../../lib/ttl_lru_cache.mli) · [implementation](../../lib/ttl_lru_cache.ml).

## Overview

The wrapper stores `{ data; expires_at }` on top of [Lru_cache](lru_cache.doc.md).
Expiration uses wall-clock time since the Unix epoch, represented as a
Core.Time_ns.Span.t; it is **not a monotonic deadline**.

## Quick-reference

`Make (Key)` exposes create, set_with_ttl, is_expired, find, mem,
find_and_remove, find_or_add, remove_expired and underlying diagnostics/mutations.
Low-level `set` accepts an entry with an absolute expires_at value; it does not
validate freshness.

## Detailed behaviour

### TTL calculation

`set_with_ttl t ~key ~data ~ttl` stores
`Time_ns.Span.since_unix_epoch () + ttl`. At or beyond that timestamp the
entry is expired. Zero/negative durations expire immediately. Access does not
extend TTL. Wall-clock adjustments can shorten or lengthen perceived lifetime.

### Read path and lazy eviction

Find/mem first perform an underlying LRU lookup, which counts/promotes a present
entry, then test expiration. An expired entry is removed and the public result
is None/false. Consequently **an expired public miss can count as an LRU hit**
in hit_rate. Find_and_remove also counts a present stale entry as a base hit,
although the TTL result is None. Find_or_add follows find, computing/storing its
default when no fresh value is available.

### remove_expired – eager cleanup

This explicitly scans all entries and removes expired ones. There is no timer
or periodic cleanup. Length, is_empty, stats and to_alist expose physical
contents and can include stale entries until access or cleanup. A lookup checks
only its requested key, not a scan of other stale entries.

### Example – short-lived text cache

```ocaml
let example () =
  let module Cache = Ttl_lru_cache.Make (Core.String) in
  let cache = Cache.create ~max_size:2 () in
  Cache.set_with_ttl cache ~key:"expired" ~data:"old"
    ~ttl:(Core.Time_ns.Span.of_sec (-1.));
  assert (Cache.length cache = 1);
  assert (Core.Option.is_none (Cache.find cache "expired"));
  assert (Cache.length cache = 0);
  assert (Core.Float.equal (Cache.hit_rate cache) 1.)
```

## Limitations

Point access has the underlying cache's expected cost; traversal, clear and
shrink operations can be linear. Default/destructor work is additional.
No synchronization, monotonic-clock guarantee, automatic refresh or background
eviction is provided.

## Internal invariants

Physical length is bounded by capacity, **not** by the number of fresh entries.
Only freshness-aware reads exclude expired payloads. To serialize live entries
only, clean up first or filter to_alist using is_expired.

