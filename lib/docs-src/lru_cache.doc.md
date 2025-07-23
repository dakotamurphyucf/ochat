# `Lru_cache` – A bounded least-recently-used cache

## Overview

`Lru_cache` implements a fixed-capacity, key-value store that evicts the
least-recently-used (LRU) bindings first.  A *use* is any operation that reads or
writes a binding (`mem`, `find`, `find_and_remove`, `set`, *etc.*).  The module
is functorised so that you can plug any key type that satisfies
`Hashtbl.Key_plain` and `Invariant.S`.

All operations are *amortised `O(1)`* thanks to an internal hash-queue from
`Core`.  The implementation is **not thread-safe** – wrap calls in a mutex if
you need to share a cache between domains.

## API Quick-reference (simplified)

```
module Lru_cache : sig
  module Make (K : Key) : sig
    type key   = K.t
    type 'a t

    val create        : ?destruct:((key * 'a) Queue.t -> unit)
                       -> max_size:int -> unit -> 'a t

    (* read-only *)
    val mem           : _ t -> key -> bool
    val find          : 'a t -> key -> 'a option
    val find_and_remove : 'a t -> key -> 'a option

    (* write *)
    val set           : 'a t -> key:key -> data:'a -> unit
    val find_or_add   : 'a t -> key -> default:(unit -> 'a) -> 'a
    val remove        : _ t -> key -> [ `Ok | `No_such_key ]
    val clear         : _ t -> [ `Dropped of int ]
    val set_max_size  : _ t -> max_size:int -> [ `Dropped of int ]

    (* diagnostics *)
    val length        : _ t -> int
    val max_size      : _ t -> int
    val hit_rate      : _ t -> float
    val to_alist      : 'a t -> (key * 'a) list
    val stats         : ?sexp_of_key:(key -> Sexp.t) -> _ t -> Sexp.t
  end
end
```

## Detailed semantics

### Creation

```
val create
  :  ?destruct:((key * 'a) Queue.t -> unit)
  -> max_size:int
  -> unit
  -> 'a t
```

* `max_size` – non-negative capacity.  `0` disables caching entirely (all write
  operations immediately evict their binding).
* `destruct` – optional callback invoked with every batch of evicted bindings
  **after** the internal state has been updated.  The bindings are provided in
  LRU → MRU order inside a `Core.Queue`.  Exceptions escape to the caller of
  the mutating function that caused the eviction.


### Lookup & usage tracking

| Function             | Returns                | Usage counted? |
| -------------------- | ---------------------- | -------------- |
| `mem    t k`         | `bool`                 | ✔︎             |
| `find   t k`         | `'a option`            | ✔︎             |
| `find_and_remove t k`| `'a option` (and eject)| ✔︎             |

Each of the above promotes the binding to the most-recently-used position if it
exists.  The promotion affects future eviction order and hit-rate statistics.

### Mutations & eviction

Mutations (`set`, `remove`, `clear`, `set_max_size`) may evict bindings.  The
policy is always:

1. Perform the requested change.
2. Evict from the *front* of the internal queue (the LRU end) until
   `length t ≤ max_size t`.
3. Execute the `destruct` callback once, if provided.

The number of dropped bindings is always returned when it matters (`clear`,
`set_max_size`).

### Hit-rate

`hit_rate t` = `(#successful look-ups) / (#total look-ups)` since the cache was
created.  Only `mem`, `find`, and `find_and_remove` count as look-ups.  The
ratio is `0.` when no look-ups have been performed.

## Examples

### Basic usage

```ocaml
module Int_cache = Lru_cache.Make (struct
  type t = int [@@deriving sexp]
  let hash = Int.hash
  let compare = Int.compare
  let invariant _ = ()
end)

let () =
  let cache = Int_cache.create ~max_size:3 () in

  (* Fill the cache *)
  List.iteri [10; 11; 12] ~f:(fun i v -> Int_cache.set cache ~key:i ~data:v);

  assert (Int_cache.length cache = 3);

  (* Access key 0 so it becomes MRU. *)
  assert (Int_cache.find_exn cache 0 = 10);

  (* Insert a new binding – key 1 (LRU) is evicted. *)
  Int_cache.set cache ~key:3 ~data:13;

  assert (not (Int_cache.mem cache 1));
  assert (Int_cache.length cache = 3);
  printf "%f\n" (Int_cache.hit_rate cache);               (* 0.5 *)
```

### Cleaning up resources on eviction

```ocaml
module String_cache = Lru_cache.Make (String)

let () =
  let cleanup evicted =
    Queue.iter evicted ~f:(fun (key, chan) ->
      printf "Closing %s\n" key;
      Out_channel.close chan)
  in
  let cache = String_cache.create ~destruct:cleanup ~max_size:2 () in
  let open Out_channel in
  String_cache.set cache ~key:"file-a" ~data:(create "a.txt");
  String_cache.set cache ~key:"file-b" ~data:(create "b.txt");
  String_cache.set cache ~key:"file-c" ~data:(create "c.txt");
  (* "file-a" handle has been closed by [cleanup]. *)
```

## Limitations

* **Concurrency:** the implementation is not thread-safe – protect it if you
  share the cache between parallel fibers/threads/domains.
* **Negative capacities:** disallowed; an [`Invalid_argument`] exception is
  raised.
* **Serialization:** no direct bin-io / sexp conversion for the *data* type.

## Internal invariants (guaranteed)

* `length t ≤ max_size t` at all times.
* `max_size t ≥ 0`.
* All calls to `invariant` (from `Invariant.S1`) succeed unless the user breaks
  the key or data invariants in their own code.

---

*Module version automatically derived from the source tree*.

