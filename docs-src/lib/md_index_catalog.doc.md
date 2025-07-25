# Md_index_catalog

Persistent on-disk catalogue that maps *Markdown index folders* to the
centroid embeddings of their snippets.  The catalogue is the very first
stop for tools such as `Markdown_indexer` and `Markdown_search`: instead
of opening every index folder and reading thousands of vectors, they can
look up the catalogue, compute a quick cosine-similarity between the
catalogue vectors and a query embedding, and immediately focus on the
K most relevant indexes.

---

## Storage format and location

* **Filename:** `md_index_catalog.binio`  
* **Directory:** parent directory that contains the individual index
  folders (defaults to `.md_index`).

The file contains a `bin_prot`-serialised array of entries:

```ocaml
type entry = {
  name        : string ;       (* logical id – e.g. "docs"          *)
  description : string ;       (* free-text label shown to the user *)
  vector      : float array ;  (* L2-normalised centroid embedding  *)
}

type t = entry array
```

Serialisation uses `Bin_prot.Utils.bin_dump ~header:true`, which prefixes
the data with its byte length and therefore allows append-friendly I/O
and safe robustness checking on read.

---

## Public API

```ocaml
val load :
  dir:Eio.Fs.dir_ty Eio.Path.t -> t option
(** Read the catalogue from [dir].  Returns [None] when the file is
    absent or cannot be decoded (e.g. version mismatch). *)

val save :
  dir:Eio.Fs.dir_ty Eio.Path.t -> t -> unit
(** Atomically rewrite the catalogue in [dir] with the given value. *)

val add_or_update :
  dir:Eio.Fs.dir_ty Eio.Path.t ->
  name:string ->
  description:string ->
  vector:float array ->
  unit
(** Insert a new entry or replace the existing one with matching
    [name].  The function L2-normalises [vector] before persisting so
    that subsequent dot-products correspond to cosine similarity.*)
```

---

## Usage examples

All functions are non-blocking and must run inside an Eio fibre.  The
examples below assume a running fibre, as provided by `Eio_main.run`:

```ocaml
let ( / ) = Eio.Path.( / )

let demo env =
  let dir = Eio.Stdenv.cwd env / ".md_index" in

  (* 1.  Create or update an entry *)
  Md_index_catalog.add_or_update
    ~dir
    ~name:"docs"
    ~description:"Official documentation markdowns"
    ~vector:[| 0.12; 0.87; 0.48 |] ;

  (* 2.  Read the full catalogue *)
  match Md_index_catalog.load ~dir with
  | None -> Format.printf "Catalogue missing@."
  | Some cat ->
      Array.iter cat ~f:(fun e ->
        Format.printf "%-10s  %s@." e.name e.description)

let () =
  Eio_main.run @@ fun env ->
  demo env
```

---

## Implementation notes

1. **Normalisation.** `Entry.normalize` divides every vector by its
   L2-norm using `Owl.Mat.vecnorm'`.  Zero vectors are left untouched.
2. **Idempotency.** `add_or_update` filters out an existing entry with
   the same `name` before re-inserting, making repeated calls safe.
3. **Failure handling.** The module never raises when the catalogue is
   missing or unreadable – callers receive `None` and can fall back to
   an empty catalogue.

---

## Known limitations

* **No concurrency control.** Multiple fibres / processes writing the
  catalogue simultaneously will race and may interleave writes.
* **Whole-file rewrite.** `save` rewrites the whole array; the file size
  grows linearly with the number of indexes.  The design is adequate
  for the expected tens-of-indexes scale. For larger deployments a
  key/value store may be more appropriate.

---

### See also

* [`Markdown_indexer`](./markdown_indexer.doc.md) – builds per-folder
  indexes and computes centroid vectors.
* [`Vector_db`](./vector_db.doc.md) – underlying vector search engine.

