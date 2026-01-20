# `odoc-search` example (odoc docs search)

This page is a **placeholder example** showing the *shape* of `odoc-search` results.
The output below is illustrative — your snippet ids/content will differ.

## Prerequisites

1) Generate odoc HTML for a dune project:

```sh
dune build @doc
```

2) Index the generated HTML:

```sh
odoc-index --root _build/default/_doc/_html --out .odoc_index
```

## Example query

```sh
odoc-search --query "Eio.Switch.run usage" --index .odoc_index -k 3
```

## Example output (illustrative)

Each result shows:
- rank,
- package,
- snippet id,
- the full snippet body (Markdown) for that hit.

```text
[1] [eio] 9f1c2d3e4b5a69788796a5b4c3d2e1f0:
(** Package:eio Module:Eio.Switch Lines:120-180 *)

val run : (Switch.t -> 'a) -> 'a

Create a switch, run the callback, then...
...


---


[2] [eio] 11223344556677889900aabbccddeeff:
(** Package:eio Module:Eio.Fiber Lines:40-90 *)

Switch and cancellation are used to...
...


---


[3] [core] 0f1e2d3c4b5a69788796a5b4c3d2e1f0:
(** Package:core Module:Core.List Lines:10-60 *)

val iter : 'a t -> f:('a -> unit) -> unit
...
```

## Tips for better results

- If you already know the package, scoping can reduce noise:
  ```sh
  odoc-search --query "Switch.run" --package eio --index .odoc_index -k 5
  ```
- Remember: `odoc-search` is searching **what you indexed locally**, not the live web.

