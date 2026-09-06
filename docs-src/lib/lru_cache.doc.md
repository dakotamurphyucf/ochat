# Lru_cache — bounded least-recently-used cache

[Public interface](../../lib/lru_cache_intf.ml) · [implementation](../../lib/lru_cache.ml).

## Overview

`Lru_cache.Make (Key)` uses a Core hash queue; Key supplies hashing, comparison,
sexp conversion and an invariant. Lookups and writes promote present entries.
It is not synchronized across domains. Sharing among fibers is safe only while
operations/callbacks do not yield or reenter without coordination.

## API Quick-reference (simplified)

`create ?destruct ~max_size ()`, `find`, `mem`, `find_or_add`,
`find_and_remove`, `set`, `remove`, `clear`, `set_max_size`,
`length`, `max_size`, `is_empty`, `hit_rate`, `to_alist`, `stats`
and `invariant` are exposed. There is no `find_exn` convenience method.

## Detailed semantics

### Creation

Capacity must be nonnegative; invalid sizes raise a sexp-backed exception
(not specifically Invalid_argument). Zero capacity immediately evicts inserted
entries. A destructor receives batches of removed key/value pairs in a Core Queue.

### Lookup & usage tracking

`find` returns an option; `mem` delegates to find. `find_and_remove` removes
the binding, invokes its destructor if present, and returns its former value.
If the destructor releases a resource, that returned resource is already released.
`find_or_add` calls find, so it also contributes to hit statistics.

### Mutations & eviction

Eviction removes LRU entries first. `set` removes an existing value before
installing its replacement and calls its destructor; even if that destructor
raises, the replacement is installed and the exception re-raised.
Callbacks run after the relevant removal, with valid cache state, but not
necessarily after an entire compound set operation. Avoid reentrant callbacks.

`clear` and shrinking `set_max_size` return the number dropped.
`remove` returns the polymorphic variant `` `Ok `` or `` `No_such_key ``.
Destructor exceptions propagate.

### Hit-rate

Underlying successful lookups divided by all lookups, or 0 before any.
This includes indirect lookups through mem/find_or_add. It does not track
whether an external TTL wrapper subsequently rejects the value.

## Examples

```ocaml
let example () =
  let module Cache = Lru_cache.Make (Core.Int) in
  let cache = Cache.create ~max_size:2 () in
  Cache.set cache ~key:1 ~data:"one";
  Cache.set cache ~key:2 ~data:"two";
  assert (Core.Option.equal String.equal (Cache.find cache 1) (Some "one"));
  Cache.set cache ~key:3 ~data:"three";
  assert (not (Cache.mem cache 2))
```

Use an Eio-owned resource's close callback as a destructor when appropriate;
cache membership is not a substitute for switch ownership.

## Limitations

Point lookups/inserts/removals are amortized O(1), excluding callbacks.
`to_alist`, stats and invariant traversal are linear. Clearing and shrinking
can process many entries. Callback cost is caller-defined.
No locking, transactional rollback, or serialization of arbitrary cached resources.

## Internal invariants (guaranteed)

With non-reentrant, well-behaved keys/callbacks, length does not exceed capacity.
`to_alist` returns a least-to-most-recent ordering snapshot.
