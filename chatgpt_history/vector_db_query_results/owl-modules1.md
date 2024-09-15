Querying indexed OCaml code with text: **module decleration**
Using vector database data from folder: **./vector-owl**
Returning top **20** results

**Result 1:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_vocabulary.mli", line 10, characters 0-6
Module Path: Owl_nlp_vocabulary
OCaml Source: Interface
*)

(**
 Type of vocabulary (or dictionary).  *)
type t
```

**Result 2:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_corpus.mli", line 10, characters 0-6
Module Path: Owl_nlp_corpus
OCaml Source: Interface
*)

(**
 Type of a text corpus.  *)
type t
```

**Result 3:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 291, characters 0-214
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 Record type contains the result of a hypothesis test.  *)
type hypothesis =
  { reject : bool
  ; (* reject null hypothesis if [true] *)
    p_value : float
  ; (* p-value of the hypothesis test *)
    score : float (* score has different meaning in different tests *)
  }
```

**Result 4:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda.mli", line 16, characters 0-10
Module Path: Owl_nlp_lda
OCaml Source: Interface
*)

(**
 Type of LDA model.  *)
type model
```

**Result 5:**
```ocaml
(** 
Location: File "src/owl/cblas/owl_cblas.mli", line 16, characters 0-38
Module Path: Owl_cblas
OCaml Source: Interface
*)

(**
 Side type  *)
type uplo = Owl_cblas_basic.cblas_uplo
```

**Result 6:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_tfidf.mli", line 21, characters 0-6
Module Path: Owl_nlp_tfidf
OCaml Source: Interface
*)

(**
 Type of a TFIDF model  *)
type t
```

**Result 7:**
```ocaml
(** 
Location: File "src/owl/cblas/owl_cblas.mli", line 10, characters 0-47
Module Path: Owl_cblas
OCaml Source: Interface
*)

(**
 The default type is Bigarray's Genarray.  *)
type ('a, 'b) t = ('a, 'b, c_layout) Genarray.t
```

**Result 8:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 2395, characters 0-55
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val uniform_ : ?a:'a -> ?b:'a -> out:('a, 'b) t -> unit
```

**Result 9:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 3328, characters 0-63
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 Refer to :doc:`owl_dense_matrix_generic`  *)
type area =
  { a : int
  ; b : int
  ; c : int
  ; d : int
  }
```

**Result 10:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 35, characters 0-43
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**

Type of the ndarray, e.g., Bigarray.Float32, Bigarray.Complex64, and etc.
  *)
type ('a, 'b) kind = ('a, 'b) Bigarray.kind
```

**Result 11:**
```ocaml
(** 
Location: File "src/owl/sparse/owl_sparse_ndarray_generic.mli", line 10, characters 0-43
Module Path: Owl_sparse_ndarray_generic
OCaml Source: Interface
*)

(**
 Type of [kind].  *)
type ('a, 'b) kind = ('a, 'b) Bigarray.kind
```

**Result 12:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 306, characters 0-87
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 Pretty printer of hypothesis type  *)
val pp_hypothesis : Format.formatter -> hypothesis -> unit
  [@@ocaml.toplevel_printer]
```

**Result 13:**
```ocaml
(** 
Location: File "src/owl/linalg/owl_linalg_generic.mli", line 23, characters 0-53
Module Path: Owl_linalg_generic
OCaml Source: Interface
*)

(**

Matrix type, a special case of N-dimensional array.
  *)
type ('a, 'b) t = ('a, 'b) Owl_dense_matrix_generic.t
```

**Result 14:**
```ocaml
(** 
Location: File "src/owl/cblas/owl_cblas_basic.mli", line 18, characters 0-63
Module Path: Owl_cblas_basic
OCaml Source: Interface
*)

(**
 The default type is Bigarray's [Array1.t].  *)
type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
```

**Result 15:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke.mli", line 10, characters 0-47
Module Path: Owl_lapacke
OCaml Source: Interface
*)

(**
 Default data type  *)
type ('a, 'b) t = ('a, 'b, c_layout) Genarray.t
```

**Result 16:**
```ocaml
(** 
Location: File "src/owl/sparse/owl_sparse_ndarray_generic.mli", line 203, characters 0-30
Module Path: Owl_sparse_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val print : ('a, 'b) t -> unit
```

**Result 17:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_vocabulary.mli", line 145, characters 0-73
Module Path: Owl_nlp_vocabulary
OCaml Source: Interface
*)

(**
 Pretty printer for vocabulary type.  *)
val pp_vocab : Format.formatter -> t -> unit
  [@@ocaml.toplevel_printer]
```

**Result 18:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_matrix_generic.mli", line 31, characters 0-47
Module Path: Owl_dense_matrix_generic
OCaml Source: Interface
*)

(**

N-dimensional array type, i.e. Bigarray Genarray type.
  *)
type ('a, 'b) t = ('a, 'b, c_layout) Genarray.t
```

**Result 19:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_matrix_generic.mli", line 2223, characters 0-55
Module Path: Owl_dense_matrix_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val uniform_ : ?a:'a -> ?b:'a -> out:('a, 'b) t -> unit
```

**Result 20:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 30, characters 0-47
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**

N-dimensional array type, i.e. Bigarray Genarray type.
  *)
type ('a, 'b) t = ('a, 'b, c_layout) Genarray.t
```
