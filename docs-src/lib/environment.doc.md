# `Environment` – string-keyed maps as compiler environments

`Environment` is a very small convenience wrapper around OCaml’s standard
`Map` data-structure.  It specialises the map to `string` keys and re-exports
the complete `Map.S` interface, so you can use all the usual
`empty` / `add` / `find` / `union` operations you already know.  On top of
that it provides two helpers that are particularly handy when you model a
"compiler environment" – a mapping from identifiers to arbitrary payloads:

* `of_list` – build an environment directly from a list of bindings,
  with the **last** occurrence of a key winning (left-to-right fold).
* `merge`   – left-biased union that preserves the left-hand side when a
  key is present in both maps.

The module lives in `lib/environment.{ml,mli}` and is published as
`ochat.environment`.

---

## Quick example

```ocaml
open! Core

let example () =
  let base = Environment.of_list [ "x", 1; "y", 2 ] in
  let extra = Environment.of_list [ "y", 0; "z", 3 ] in
  let merged = Environment.merge base extra in
  assert (Option.equal Int.equal (Environment.find_opt "x" merged) (Some 1));
  assert (Option.equal Int.equal (Environment.find_opt "y" merged) (Some 2));
  assert (Option.equal Int.equal (Environment.find_opt "z" merged) (Some 3))
;;
```

---

## API reference (summary)

### `type 'a Environment.t`
The `Stdlib.Map.Make(String)` value type `'a t`. Complexity follows
`Stdlib.Map` – look-ups are logarithmic in the number of bindings.

### `val of_list : (string * 'a) list -> 'a t`
Convert a list of `(key, value)` pairs to an environment.  Later duplicates
override earlier ones.

### `val merge : 'a t -> 'a t -> 'a t`
Left-biased union.  Keeps every binding from the first argument and adds the
bindings from the second argument whose keys were not already present.

---

## Design notes

Why not expose all of `Stdlib.Map.Make(String)` directly?  Doing so would
force users to work with a private sub-module, e.g. `Env.Map`.  Re-exporting
the signature straight from the outer module keeps call-sites terse:

```ocaml
Environment.find "x" env   (* instead of Environment.Map.find *)
```

---

## Known limitations

* The implementation picks the default `Stdlib` map; if you rely on the
  `Core` or `Base` map APIs, convert explicitly. Their types and argument
  conventions are not interchangeable with this compatibility module.
* No custom merge strategy besides the left-biased one is provided – you can
  of course roll your own with `Map.union`.

---

## Implementation at a glance

The full code fits on a postcard:

```ocaml
module M = Stdlib.Map.Make(Stdlib.String)

include M  (* re-export the whole Map.S API *)

let of_list lst =
  Stdlib.List.fold_left (fun acc (k,v) -> add k v acc) empty lst

let merge lhs rhs =
  fold (fun k v acc -> if mem k acc then acc else add k v acc) rhs lhs
```

The helper functions are linear in the number of bindings they traverse and
therefore O(n · log n) due to the internal map operations.
