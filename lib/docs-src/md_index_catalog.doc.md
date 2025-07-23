# `Md_index_catalog`

Persistent catalogue that maps *Markdown index names* (e.g. `"docs"`) to
their centroid embeddings.  The catalogue allows `Markdown_search` to quickly
narrow down which on-disk index folders are relevant for a user query.

---

## Storage format

Binary serialised (`bin_prot`) array of entries saved as
`md_index_catalog.binio` inside the **parent directory** of individual index
folders (default: `.md_index`).

```ocaml
type entry = {
  name        : string;        (* logical id – folder name *)
  description : string;        (* human readable blurb *)
  vector      : float array;   (* L2-normalised centroid *)
}
```

---

## API highlights

```ocaml
val load  : dir:_ Eio.Path.t -> t option
val save  : dir:_ Eio.Path.t -> t -> unit
val add_or_update :
  dir:_ Eio.Path.t ->
  name:string ->
  description:string ->
  vector:float array ->
  unit
```

`add_or_update` is idempotent – existing entries with the same `name` are
replaced in-place.

---

## Normalisation

All vectors are L2-normalised on write (`Entry.normalize`) so that subsequent
dot-product ranking becomes cosine similarity.

---

## Failure model

* The module never raises on I/O failure – callers should wrap operations in
  `Option.value` / fall back to defaults.

