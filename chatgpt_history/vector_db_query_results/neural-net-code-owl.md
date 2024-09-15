Querying indexed OCaml code with text: **deep neural net code**
Using vector database data from folder: **./vector-owl**
Returning top **50** results

**Result 1:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 2040, characters 0-33
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)


(** {5 Neural network related} *)
```

**Result 2:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_a.mli", line 8, characters 0-106
Module Path: Owl_dense_ndarray_a
OCaml Source: Interface
*)


type 'a arr =
  { mutable shape : int array
  ; mutable stride : int array
  ; mutable data : 'a array
  }
```

**Result 3:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_d.mli", line 6, characters 0-13
Module Path: Owl_dense_ndarray_d
OCaml Source: Interface
*)


open Bigarray
```

**Result 4:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_z.mli", line 16, characters 0-54
Module Path: Owl_dense_ndarray_z
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.NN with type arr := arr
```

**Result 5:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_d.mli", line 16, characters 0-54
Module Path: Owl_dense_ndarray_d
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.NN with type arr := arr
```

**Result 6:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_z.mli", line 18, characters 0-125
Module Path: Owl_dense_ndarray_z
OCaml Source: Interface
*)


include
  Owl_dense_ndarray_intf.Complex
    with type elt := elt
     and type arr := arr
     and type cast_arr := cast_arr
```

**Result 7:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_z.mli", line 6, characters 0-13
Module Path: Owl_dense_ndarray_z
OCaml Source: Interface
*)


open Bigarray
```

**Result 8:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_c.mli", line 18, characters 0-125
Module Path: Owl_dense_ndarray_c
OCaml Source: Interface
*)


include
  Owl_dense_ndarray_intf.Complex
    with type elt := elt
     and type arr := arr
     and type cast_arr := cast_arr
```

**Result 9:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_c.mli", line 6, characters 0-13
Module Path: Owl_dense_ndarray_c
OCaml Source: Interface
*)


open Bigarray
```

**Result 10:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_c.mli", line 16, characters 0-54
Module Path: Owl_dense_ndarray_c
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.NN with type arr := arr
```

**Result 11:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_s.mli", line 16, characters 0-54
Module Path: Owl_dense_ndarray_s
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.NN with type arr := arr
```

**Result 12:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda.mli", line 6, characters 0-22
Module Path: Owl_nlp_lda
OCaml Source: Interface
*)


(** NLP: LDA module *)
```

**Result 13:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_s.mli", line 6, characters 0-13
Module Path: Owl_dense_ndarray_s
OCaml Source: Interface
*)


open Bigarray
```

**Result 14:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_a.mli", line 6, characters 0-14
Module Path: Owl_dense_ndarray_a
OCaml Source: Interface
*)


open Owl_types
```

**Result 15:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 26, characters 0-14
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)


open Owl_types
```

**Result 16:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_d.mli", line 12, characters 0-78
Module Path: Owl_dense_ndarray_d
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.Common with type elt := elt and type arr := arr
```

**Result 17:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_z.mli", line 14, characters 0-78
Module Path: Owl_dense_ndarray_z
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.Common with type elt := elt and type arr := arr
```

**Result 18:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_corpus.mli", line 6, characters 0-25
Module Path: Owl_nlp_corpus
OCaml Source: Interface
*)


(** NLP: Corpus module *)
```

**Result 19:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke_generated.mli", line 129, characters 0-266
Module Path: Owl_lapacke_generated
OCaml Source: Interface
*)


val dbdsvdx
  :  layout:int
  -> uplo:char
  -> jobz:char
  -> range:char
  -> n:int
  -> d:float ptr
  -> e:float ptr
  -> vl:float
  -> vu:float
  -> il:int
  -> iu:int
  -> ns:int32 ptr
  -> s:float ptr
  -> z:float ptr
  -> ldz:int
  -> superb:int32 ptr
  -> int
```

**Result 20:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_d.mli", line 14, characters 0-76
Module Path: Owl_dense_ndarray_d
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.Real with type elt := elt and type arr := arr
```

**Result 21:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 25, characters 0-13
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)


open Bigarray
```

**Result 22:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke_generated.mli", line 1171, characters 0-207
Module Path: Owl_lapacke_generated
OCaml Source: Interface
*)


val dgeev
  :  layout:int
  -> jobvl:char
  -> jobvr:char
  -> n:int
  -> a:float ptr
  -> lda:int
  -> wr:float ptr
  -> wi:float ptr
  -> vl:float ptr
  -> ldvl:int
  -> vr:float ptr
  -> ldvr:int
  -> int
```

**Result 23:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_matrix_d.mli", line 6, characters 0-13
Module Path: Owl_dense_matrix_d
OCaml Source: Interface
*)


open Bigarray
```

**Result 24:**
```ocaml
(** 
Location: File "src/owl/cblas/owl_cblas_generated.mli", line 1146, characters 0-222
Module Path: Owl_cblas_generated
OCaml Source: Interface
*)


val dgemm
  :  order:int
  -> transa:int
  -> transb:int
  -> m:int
  -> n:int
  -> k:int
  -> alpha:float
  -> a:float ptr
  -> lda:int
  -> b:float ptr
  -> ldb:int
  -> beta:float
  -> c:float ptr
  -> ldc:int
  -> unit
```

**Result 25:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 3171, characters 0-143
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val dilated_conv2d_backward_kernel_
  :  out:('a, 'b) t
  -> ('a, 'b) t
  -> ('a, 'b) t
  -> int array
  -> int array
  -> ('a, 'b) t
  -> unit
```

**Result 26:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_d.mli", line 8, characters 0-16
Module Path: Owl_dense_ndarray_d
OCaml Source: Interface
*)


type elt = float
```

**Result 27:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 6, characters 0-116
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)


(**
N-dimensional array module: including creation, manipulation, and various
vectorised mathematical operations.
*)
```

**Result 28:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_z.mli", line 10, characters 0-58
Module Path: Owl_dense_ndarray_z
OCaml Source: Interface
*)


type arr = (Complex.t, complex64_elt, c_layout) Genarray.t
```

**Result 29:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_z.mli", line 8, characters 0-20
Module Path: Owl_dense_ndarray_z
OCaml Source: Interface
*)


type elt = Complex.t
```

**Result 30:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 3161, characters 0-142
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val dilated_conv2d_backward_input_
  :  out:('a, 'b) t
  -> ('a, 'b) t
  -> ('a, 'b) t
  -> int array
  -> int array
  -> ('a, 'b) t
  -> unit
```

**Result 31:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke_generated.mli", line 10121, characters 0-198
Module Path: Owl_lapacke_generated
OCaml Source: Interface
*)


val dtfsm
  :  layout:int
  -> transr:char
  -> side:char
  -> uplo:char
  -> trans:char
  -> diag:char
  -> m:int
  -> n:int
  -> alpha:float
  -> a:float ptr
  -> b:float ptr
  -> ldb:int
  -> int
```

**Result 32:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_c.mli", line 14, characters 0-78
Module Path: Owl_dense_ndarray_c
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.Common with type elt := elt and type arr := arr
```

**Result 33:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_c.mli", line 8, characters 0-20
Module Path: Owl_dense_ndarray_c
OCaml Source: Interface
*)


type elt = Complex.t
```

**Result 34:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 2290, characters 0-114
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val max_pool2d_backward
  :  padding
  -> ('a, 'b) t
  -> int array
  -> int array
  -> ('a, 'b) t
  -> ('a, 'b) t
```

**Result 35:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 3191, characters 0-143
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val dilated_conv3d_backward_kernel_
  :  out:('a, 'b) t
  -> ('a, 'b) t
  -> ('a, 'b) t
  -> int array
  -> int array
  -> ('a, 'b) t
  -> unit
```

**Result 36:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 2197, characters 0-127
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val dilated_conv2d_backward_input
  :  ('a, 'b) t
  -> ('a, 'b) t
  -> int array
  -> int array
  -> ('a, 'b) t
  -> ('a, 'b) t
```

**Result 37:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_d.mli", line 10, characters 0-52
Module Path: Owl_dense_ndarray_d
OCaml Source: Interface
*)


type arr = (float, float64_elt, c_layout) Genarray.t
```

**Result 38:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_a.mli", line 20, characters 0-54
Module Path: Owl_dense_ndarray_a
OCaml Source: Interface
*)


val init_nd : int array -> (int array -> 'a) -> 'a arr
```

**Result 39:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke_generated.mli", line 27, characters 0-208
Module Path: Owl_lapacke_generated
OCaml Source: Interface
*)


val dbdsdc
  :  layout:int
  -> uplo:char
  -> compq:char
  -> n:int
  -> d:float ptr
  -> e:float ptr
  -> u:float ptr
  -> ldu:int
  -> vt:float ptr
  -> ldvt:int
  -> q:float ptr
  -> iq:int32 ptr
  -> int
```

**Result 40:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_c.mli", line 10, characters 0-58
Module Path: Owl_dense_ndarray_c
OCaml Source: Interface
*)


type arr = (Complex.t, complex32_elt, c_layout) Genarray.t
```

**Result 41:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_vocabulary.mli", line 6, characters 0-29
Module Path: Owl_nlp_vocabulary
OCaml Source: Interface
*)


(** NLP: Vocabulary module *)
```

**Result 42:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_s.mli", line 14, characters 0-76
Module Path: Owl_dense_ndarray_s
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.Real with type elt := elt and type arr := arr
```

**Result 43:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_s.mli", line 12, characters 0-78
Module Path: Owl_dense_ndarray_s
OCaml Source: Interface
*)


include Owl_dense_ndarray_intf.Common with type elt := elt and type arr := arr
```

**Result 44:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 1969, characters 0-26
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)


(** {5 Tensor Calculus} *)
```

**Result 45:**
```ocaml
(** 
Location: File "src/owl/fftpack/owl_fft_d.mli", line 7, characters 0-30
Module Path: Owl_fft_d
OCaml Source: Interface
*)


open Owl_dense_ndarray_generic
```

**Result 46:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 3114, characters 0-120
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val conv2d_backward_kernel_
  :  out:('a, 'b) t
  -> ('a, 'b) t
  -> ('a, 'b) t
  -> int array
  -> ('a, 'b) t
  -> unit
```

**Result 47:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_matrix_z.mli", line 18, characters 0-13
Module Path: Owl_dense_matrix_z
OCaml Source: Interface
*)


open Bigarray
```

**Result 48:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 2317, characters 0-114
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val avg_pool2d_backward
  :  padding
  -> ('a, 'b) t
  -> int array
  -> int array
  -> ('a, 'b) t
  -> ('a, 'b) t
```

**Result 49:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 2340, characters 0-179
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)


(**
The following functions are helper functions for some other functions in
both Ndarray and Ndview modules. In general, you are not supposed to use
these functions directly.
 *)
```

**Result 50:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray_generic.mli", line 3105, characters 0-119
Module Path: Owl_dense_ndarray_generic
OCaml Source: Interface
*)

(**
 TODO  *)
val conv2d_backward_input_
  :  out:('a, 'b) t
  -> ('a, 'b) t
  -> ('a, 'b) t
  -> int array
  -> ('a, 'b) t
  -> unit
```
