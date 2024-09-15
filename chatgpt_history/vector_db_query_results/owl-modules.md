Querying indexed OCaml code with text: **ocaml modules**
Using vector database data from folder: **./vector-owl**
Returning top **50** results

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

**Result 3:**
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

**Result 4:**
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

**Result 5:**
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

**Result 6:**
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

**Result 7:**
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

**Result 8:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 170, characters 0-26
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 Inverse function of [cosh].  *)
val acosh : float -> float
```

**Result 9:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 562, characters 0-24
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [is_odd x] returns [true] exactly if [x] is odd.  *)
val is_odd : int -> bool
```

**Result 10:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 152, characters 0-25
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [cosh x] returns :math:`\cosh(x)`.  *)
val cosh : float -> float
```

**Result 11:**
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

**Result 12:**
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
Location: File "src/owl/maths/owl_maths.mli", line 140, characters 0-25
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 Inverse function of [cot].  *)
val acot : float -> float
```

**Result 15:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 590, characters 0-37
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [mulmod a b m] computes (a*b) mod m.  *)
val mulmod : int -> int -> int -> int
```

**Result 16:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 158, characters 0-25
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [coth x] returns :math:`\coth(x)`.  *)
val coth : float -> float
```

**Result 17:**
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

**Result 18:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke.mli", line 353, characters 0-124
Module Path: Owl_lapacke
OCaml Source: Interface
*)

(**

Refer to `Intel MKL C Reference <https://software.intel.com/en-us/mkl-developer-reference-c-lapack-routines>`_
  *)
val ormql
  :  side:char
  -> trans:char
  -> a:(float, 'a) t
  -> tau:(float, 'a) t
  -> c:(float, 'a) t
  -> (float, 'a) t
```

**Result 19:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 565, characters 0-25
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [is_even x] returns [true] exactly if [x] is even.  *)
val is_even : int -> bool
```

**Result 20:**
```ocaml
(** 
Location: File "src/owl/sparse/owl_sparse_matrix_generic.mli", line 697, characters 0-47
Module Path: Owl_sparse_matrix_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val scalar_mul : 'a -> ('a, 'b) t -> ('a, 'b) t
```

**Result 21:**
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

**Result 22:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 66, characters 0-24
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [exp x] exponential.  *)
val exp : float -> float
```

**Result 23:**
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

**Result 24:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 838, characters 0-60
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 TODO  *)
val vonmises_cdf : float -> mu:float -> kappa:float -> float
```

**Result 25:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_corpus.mli", line 62, characters 0-49
Module Path: Owl_nlp_corpus
OCaml Source: Interface
*)

(**
 Map all the documents in a corpus into another array. The index (line number) is passed in.  *)
val mapi : (int -> string -> 'a) -> t -> 'a array
```

**Result 26:**
```ocaml
(** 
Location: File "src/owl/cblas/owl_cblas_basic.mli", line 429, characters 0-196
Module Path: Owl_cblas_basic
OCaml Source: Interface
*)

(**
 Computes a matrix-matrix product with general matrices.  *)
val gemm
  :  cblas_layout
  -> cblas_transpose
  -> cblas_transpose
  -> int
  -> int
  -> int
  -> 'a
  -> ('a, 'b) t
  -> int
  -> ('a, 'b) t
  -> int
  -> 'a
  -> ('a, 'b) t
  -> int
  -> unit
```

**Result 27:**
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

**Result 28:**
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

**Result 29:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_tfidf.mli", line 38, characters 0-38
Module Path: Owl_nlp_tfidf
OCaml Source: Interface
*)

(**
 Return the corpus contained in TFIDF model  *)
val get_corpus : t -> Owl_nlp_corpus.t
```

**Result 30:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 647, characters 0-48
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**

[map f x] is similar to [mapi f x] except the index is not passed.
  *)
val map : ('a -> 'a) -> ('a, 'b) t -> ('a, 'b) t
```

**Result 31:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 859, characters 0-60
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 TODO  *)
val lomax_cdf : float -> shape:float -> scale:float -> float
```

**Result 32:**
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

**Result 33:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 841, characters 0-63
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 TODO  *)
val vonmises_logcdf : float -> mu:float -> kappa:float -> float
```

**Result 34:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 853, characters 0-60
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 TODO  *)
val lomax_pdf : float -> shape:float -> scale:float -> float
```

**Result 35:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 176, characters 0-26
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 Inverse function of [coth].  *)
val acoth : float -> float
```

**Result 36:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 832, characters 0-60
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 TODO  *)
val vonmises_pdf : float -> mu:float -> kappa:float -> float
```

**Result 37:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_corpus.mli", line 65, characters 0-49
Module Path: Owl_nlp_corpus
OCaml Source: Interface
*)

(**
 Map all the tokenised ocuments in a corpus into another array. The index (line number) is passed in.  *)
val mapi_tok : (int -> 'a -> 'b) -> t -> 'b array
```

**Result 38:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke.mli", line 331, characters 0-124
Module Path: Owl_lapacke
OCaml Source: Interface
*)

(**

Refer to `Intel MKL C Reference <https://software.intel.com/en-us/mkl-developer-reference-c-lapack-routines>`_
  *)
val ormlq
  :  side:char
  -> trans:char
  -> a:(float, 'a) t
  -> tau:(float, 'a) t
  -> c:(float, 'a) t
  -> (float, 'a) t
```

**Result 39:**
```ocaml
(** 
Location: File "src/owl/sparse/owl_sparse_ndarray_generic.mli", line 246, characters 0-48
Module Path: Owl_sparse_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val mul : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
```

**Result 40:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 600, characters 0-55
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 TODO  *)
val exponential_logcdf : float -> lambda:float -> float
```

**Result 41:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 48, characters 0-25
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [ceil x] returns the smallest integer :math:`\geq x`.  *)
val ceil : float -> float
```

**Result 42:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 36, characters 0-24
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [abs x] returns :math:`|x|`.  *)
val abs : float -> float
```

**Result 43:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 69, characters 0-25
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [exp2 x] exponential.  *)
val exp2 : float -> float
```

**Result 44:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 299, characters 0-27
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [ellipe m] complete elliptic integral of the second kind.  *)
val ellipe : float -> float
```

**Result 45:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke.mli", line 342, characters 0-124
Module Path: Owl_lapacke
OCaml Source: Interface
*)

(**

Refer to `Intel MKL C Reference <https://software.intel.com/en-us/mkl-developer-reference-c-lapack-routines>`_
  *)
val ormqr
  :  side:char
  -> trans:char
  -> a:(float, 'a) t
  -> tau:(float, 'a) t
  -> c:(float, 'a) t
  -> (float, 'a) t
```

**Result 46:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 116, characters 0-24
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [cos x] returns :math:`\cos(x)`.  *)
val cos : float -> float
```

**Result 47:**
```ocaml
(** 
Location: File "src/owl/sparse/owl_sparse_matrix_generic.mli", line 708, characters 0-44
Module Path: Owl_sparse_matrix_generic
OCaml Source: Interface
*)

(**
 TODO: not implemented, just a place holder.  *)
val mpow : ('a, 'b) t -> float -> ('a, 'b) t
```

**Result 48:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 64, characters 0-44
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 TODO  *)
val concordant : 'a array -> 'b array -> int
```

**Result 49:**
```ocaml
(** 
Location: File "src/owl/stats/owl_stats.mli", line 862, characters 0-63
Module Path: Owl_stats
OCaml Source: Interface
*)

(**
 TODO  *)
val lomax_logcdf : float -> shape:float -> scale:float -> float
```

**Result 50:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.mli", line 134, characters 0-25
Module Path: Owl_maths
OCaml Source: Interface
*)

(**
 [acos x] returns :math:`\arccos(x)`.  *)
val acos : float -> float
```
