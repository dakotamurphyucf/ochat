# `Vector_db` – In-memory vector database for Chatmd


This document complements the inline `odoc` comments of
`vector_db.{mli,ml}` with a more discursive description, a FAQ-style
collection of examples, and implementation notes that do **not** belong
in the API reference.

Table of contents
-----------------

1. High-level overview
2. Data model
3. Public API walk-through
4. Usage examples
5. Internals & performance notes
6. Known limitations / future work

------------------------------------------------------------------------

1  High-level overview
---------------------

`Vector_db` turns a bag of *document → embedding* pairs into an
in-memory index that supports three operations:

* **nearest-neighbour search** via cosine similarity
* **hybrid ranking**:
  cosine ⊕ BM25 (useful when you care about exact token matches as well)
* **lazy retrieval** of the raw document bodies once the indices of the
  best matches are known.

The implementation is intentionally minimalistic – no partitioning, no
OPQ, no product quantisation.  It therefore works best for corpora of
moderate corpora that fit into RAM. Owl.Mat uses float64: 100,000 × 1,536
entries need approximately 1.14 GiB for the corpus matrix alone, before raw
embedding arrays, construction temporaries, metadata or query allocations.


2  Data model
-------------

```text
┌───────────────────────────────────────────────────────────┐
│                    Vec.t array (on disk)                 │
│  id, length, float[]                                      │
└──────────┬────────────────────────────────────────────────┘
           │  initialize / create_corpus
           ▼
┌───────────────────────────────────────────────────────────┐
│ vector_db.t (in memory)                                   │
│   corpus : float  (d × n  Owl.Mat)  – *normalised*        │
│   index  : int ↦ (id, token_len)                          │
└───────────────────────────────────────────────────────────┘
```

Important invariants:

* `Mat.col_num corpus = Hashtbl.length index`
* each column has unit L2-norm only for finite, nonzero, equally sized inputs;
  the implementation does not reject zero vectors before dividing by their norm


3  Public API walk-through
-------------------------

### Building a snapshot

```ocaml
val create_corpus : Vec.t array -> t
```

Normalises every embedding and returns an immutable snapshot.  Use this
when you already hold the embeddings in memory.

```ocaml
val initialize : path -> t
```

Reads the `Vec.t` array from disk (Bin_prot format, see below) and calls
`create_corpus`.

### Querying

```ocaml
val query : t -> Mat.mat -> int -> int array
```

Classic nearest-neighbour search in cosine space.  The query embedding
must be an `n × 1` Owl matrix whose L2-norm is 1.

```ocaml
val query_hybrid
  :  t
  -> bm25:Bm25.t
  -> beta:float
  -> embedding:Mat.mat
  -> text:string
  -> k:int -> int array
```

Interpolates vector and lexical signals within the top `20*k` dense shortlist.
BM25 independently returns its global top `20*k`; only overlapping IDs receive
lexical scores, normalized by the largest BM25 score in the dense shortlist.
`beta=1` is lexical ranking **within that dense shortlist**, not a global
BM25-only search. Keep beta in [0,1] and k nonnegative; no beta validation is
performed. BM25 IDs must match corpus column indices.

### Incremental updates

```ocaml
val add_doc : Mat.mat -> float array -> Mat.mat
```

Convenience helper that appends a new (normalised) column to an existing
matrix.  Note that *only* the matrix is extended – *not* the
`index` table (you have to take care of that yourself).

### Serialisation helpers

```ocaml
module Vec.Io
```

The functor `Bin_prot_utils_eio.With_file_methods` provides a `File`
sub-module with the usual `read`, `write`, etc. helpers that work inside
an Eio fibre.  `vector_db` merely re-exports the resulting module as
`Vec.Io`.


4  Usage examples
----------------

> The examples below assume OCaml 5.1+, `owl`, `eio` and `ochat.*`
> libraries are available.

### 4.1 Creating an index from scratch

```ocaml
open Core
open Eio.Std

let build_snapshot ~cwd ~embeddings_file ~docs =
  (* 1.  Encode documents with your favourite model – here we mock it *)
  let vecs : Vector_db.Vec.t array =
    Array.mapi docs ~f:(fun id text ->
        let embedding = if id mod 2 = 0 then [| 1.; 0. |] else [| 0.; 1. |] in
        { Vector_db.Vec.id = Int.to_string id
        ; len = String.length text (* dummy *)
        ; vector = embedding })
  in

  (* 2.  Persist the raw embeddings *)
  Vector_db.Vec.write_vectors_to_disk vecs (cwd / embeddings_file);

  (* 3.  Build the in-memory snapshot *)
  let db = Vector_db.create_corpus vecs in
  db

(* Usage *)

let () =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let _db =
    build_snapshot
      ~cwd
      ~embeddings_file:"embeddings.bin"
      ~docs:[|"hello"; "world"|]
  in
  ()
```

### 4.2 Running a hybrid query

```ocaml
let hybrid_search env query_text query_embedding =
  let cwd   = Eio.Stdenv.cwd env in
  let db    = Vector_db.initialize (cwd / "embeddings.bin") in
  let bm25  = Bm25.read_from_disk (cwd / "bm25.idx") in

  let idxs =
    Vector_db.query_hybrid
      db ~bm25 ~beta:0.4 ~embedding:query_embedding ~text:query_text ~k:5
  in
  Vector_db.get_docs cwd db idxs
```


5  Internals & performance notes
--------------------------------

* **cosine similarity** – implemented as `embeddingᵀ × corpus` with
  BLAS‐optimised matrix multiplication from Owl (≈ O(d·n)).
* **shortlisting in `query_hybrid`** – a dense shortlist is intersected with
  an independently computed global BM25 top list. BM25 evaluation is not
  restricted to the dense shortlist. Both ranking stages sort candidates.
* **length penalty** – the `apply_length_penalty` helper slightly
  down-weights embeddings whose token length is far away from the
  192-token heuristic target. Ochat itself is not a trained model. The penalty is currently
  *disabled* when computing the final score in `query` (open a PR if
  you want to experiment with this heuristic).


6  Known limitations / future work
---------------------------------

1.  No ANN structures – every query scans the whole matrix.  Consider
    HNSW or ScaNN for large (>10⁶) corpora.
2.  `add_doc` is purely functional and returns a new matrix; this is
    memory-heavy.  A mutable corpus representation would be more
    efficient.
3.  `query` assumes the embedding is already normalised.  Detecting and
    fixing non-unit vectors in debug builds could prevent user errors.
4.  The BM25 component relies on the stop-word list shipped with
    `Bm25.tokenize` which is currently empty.

------------------------------------------------------------------------
