Querying indexed OCaml code with text: **module decleration**
Using vector database data from folder: **./vector-owl-ml**
Returning top **50** results

**Result 1:**
```ocaml
(** 
Location: File "src/owl/neural/owl_neural_parallel.ml", line 13, characters 0-653
Module Path: Owl_neural_parallel
OCaml Source: Implementation
*)


module type EngineSig = sig
  type param_context

  type barrier =
    | ASP
    | BSP
    | SSP
    | PSP

  (* functions of parameter server engine *)

  val get : 'a -> 'b * int

  val set : 'a -> 'b -> unit

  val worker_num : unit -> int

  val start : ?barrier:barrier -> string -> string -> unit

  val register_barrier : (param_context ref -> int * string list) -> unit

  val register_schedule : ('a list -> ('a * ('b * 'c) list) list) -> unit

  val register_pull : (('a * 'b) list -> ('a * 'c) list) -> unit

  val register_push : ('a -> ('b * 'c) list -> ('b * 'c) list) -> unit

  val register_stop : (param_context ref -> bool) -> unit
end
```

**Result 2:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_corpus.ml", line 8, characters 0-618
Module Path: Owl_nlp_corpus
OCaml Source: Implementation
*)


type t =
  { mutable uri : string
  ; (* path of the binary corpus *)
    mutable bin_ofs : int array
  ; (* index of the string corpus *)
    mutable tok_ofs : int array
  ; (* index of the tokenised corpus *)
    mutable bin_fh : in_channel option
  ; (* file descriptor of the binary corpus *)
    mutable tok_fh : in_channel option
  ; (* file descriptor of the tokenised corpus *)
    mutable vocab : Owl_nlp_vocabulary.t option
  ; (* vocabulary of the corpus *)
    mutable minlen : int
  ; (* minimum length of document to save *)
    mutable docid : int array (* document id, can refer to original data *)
  }
```

**Result 3:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda0.ml", line 24, characters 0-1034
Module Path: Owl_nlp_lda0
OCaml Source: Implementation
*)


type model =
  { mutable n_d : int
  ; (* number of documents *)
    mutable n_k : int
  ; (* number of topics *)
    mutable n_v : int
  ; (* number of vocabulary *)
    mutable alpha : float
  ; (* model hyper-parameters *)
    mutable beta : float
  ; (* model hyper-parameters *)
    mutable alpha_k : float
  ; (* model hyper-parameters *)
    mutable beta_v : float
  ; (* model hyper-parameters *)
    mutable t_dk : dsmat
  ; (* document-topic table: num of tokens assigned to each topic in each doc *)
    mutable t_wk : spmat
  ; (* word-topic table: num of tokens assigned to each topic for each word *)
    mutable t__k : dsmat
  ; (* number of tokens assigned to a topic: k = sum_w t_wk = sum_d t_dk *)
    mutable t__z : int array array
  ; (* table of topic assignment of each token in each document *)
    mutable iter : int
  ; (* number of iterations *)
    mutable data : int array array
  ; (* training data, tokenised *)
    mutable vocb : (string, int) Hashtbl.t (* vocabulary, or dictionary if you prefer *)
  }
```

**Result 4:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda.ml", line 18, characters 0-1064
Module Path: Owl_nlp_lda
OCaml Source: Implementation
*)


type model =
  { mutable n_d : int
  ; (* number of documents *)
    mutable n_k : int
  ; (* number of topics *)
    mutable n_v : int
  ; (* number of vocabulary *)
    mutable alpha : float
  ; (* model hyper-parameters *)
    mutable beta : float
  ; (* model hyper-parameters *)
    mutable alpha_k : float
  ; (* model hyper-parameters *)
    mutable beta_v : float
  ; (* model hyper-parameters *)
    mutable t_dk : float array array
  ; (* document-topic table: num of tokens assigned to each topic in each doc *)
    mutable t_wk : float array array
  ; (* word-topic table: num of tokens assigned to each topic for each word *)
    mutable t__k : float array
  ; (* number of tokens assigned to a topic: k = sum_w t_wk = sum_d t_dk *)
    mutable t__z : int array array
  ; (* table of topic assignment of each token in each document *)
    mutable iter : int
  ; (* number of iterations *)
    mutable data : Owl_nlp_corpus.t
  ; (* training data, tokenised*)
    mutable vocb : (string, int) Hashtbl.t (* vocabulary, or dictionary if you prefer *)
  }
```

**Result 5:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_tfidf.ml", line 19, characters 0-480
Module Path: Owl_nlp_tfidf
OCaml Source: Implementation
*)


type t =
  { mutable uri : string
  ; (* file path of the model *)
    mutable tf_typ : tf_typ
  ; (* function to calculate term freq *)
    mutable df_typ : df_typ
  ; (* function to calculate doc freq *)
    mutable offset : int array
  ; (* record the offset each document *)
    mutable doc_freq : float array
  ; (* document frequency *)
    mutable corpus : Owl_nlp_corpus.t
  ; (* corpus type *)
    mutable handle : in_channel option (* file descriptor of the tfidf *)
  }
```

**Result 6:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp.ml", line 9, characters 0-30
Module Path: Owl_nlp
OCaml Source: Implementation
*)


module Corpus = Owl_nlp_corpus
```

**Result 7:**
```ocaml
(** 
Location: File "owl.ml", line 30, characters 0-30
Module Path: Owl
OCaml Source: Implementation
*)


module Optimise = Owl_optimise
```

**Result 8:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda0.ml", line 297, characters 0-74
Module Path: Owl_nlp_lda0
OCaml Source: Implementation
*)


module LightLDA = struct
  let init _m = ()

  let sampling _m _d = ()
end
```

**Result 9:**
```ocaml
(** 
Location: File "src/owl/optimise/owl_regression_generic_sig.ml", line 6, characters 0-864
Module Path: Owl_regression_generic_sig
OCaml Source: Implementation
*)


module type Sig = sig
  module Optimise : Owl_optimise_generic_sig.Sig

  open Optimise.Algodiff

  (** {5 Type definition} *)

  type arr = A.arr
  (** Type of ndarray values. *)

  type elt = A.elt
  (** Type of scalar values. *)

  (** {5 Regression models} *)

  val ols : ?i:bool -> arr -> arr -> arr array
  (** TODO *)

  val ridge : ?i:bool -> ?alpha:float -> arr -> arr -> arr array
  (** TODO *)

  val lasso : ?i:bool -> ?alpha:float -> arr -> arr -> arr array
  (** TODO *)

  val elastic_net : ?i:bool -> ?alpha:float -> ?l1_ratio:float -> arr -> arr -> arr array
  (** TODO *)

  val svm : ?i:bool -> ?a:float -> arr -> arr -> arr array
  (** TODO *)

  val logistic : ?i:bool -> arr -> arr -> arr array
  (** TODO *)

  val exponential : ?i:bool -> arr -> arr -> elt * elt * elt
  (** TODO *)

  val poly : arr -> arr -> int -> arr
  (** TODO *)
end
```

**Result 10:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray.ml", line 14, characters 0-153
Module Path: Owl_dense_ndarray
OCaml Source: Implementation
*)


module Generic = struct
  include Owl_dense_ndarray_generic
  include Operator

  (* inject function aliases *)

  let mpow = Owl_linalg_generic.mpow
end
```

**Result 11:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_uniop.ml", line 83, characters 0-234
Module Path: Owl_ext_uniop
OCaml Source: Implementation
*)


module C = struct
  module M = Complex

  let re x = F M.(x.re)

  let im x = F M.(x.im)

  let abs x = F M.(norm x)

  let abs2 x = F M.(norm2 x)

  let conj x = C M.(conj x)

  let neg x = C M.(neg x)

  let reci x = C M.(inv x)
end
```

**Result 12:**
```ocaml
(** 
Location: File "src/owl/neural/owl_neural.ml", line 16, characters 0-143
Module Path: Owl_neural
OCaml Source: Implementation
*)


module D = struct
  include Owl_neural_generic.Make (Owl_algodiff_primal_ops.D)

  (* module Parallel = Owl_neural_parallel.Make (Graph) *)
end
```

**Result 13:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_uniop.ml", line 621, characters 0-425
Module Path: Owl_ext_uniop
OCaml Source: Implementation
*)


module DMZ = struct
  module M = Owl_ext_dense_matrix.Z

  let re x = M.re x

  let im x = M.im x

  let inv x = M.inv x

  let trace x = M.trace x

  let sum' x = M.sum' x

  let prod' x = M.prod' x

  let abs x = M.abs x

  let abs2 x = M.abs2 x

  let conj x = M.conj x

  let neg x = M.neg x

  let reci x = M.reci x

  let l1norm' x = M.l1norm' x

  let l2norm' x = M.l2norm' x

  let l2norm_sqr' x = M.l2norm_sqr' x
end
```

**Result 14:**
```ocaml
(** 
Location: File "src/owl/neural/owl_neural.ml", line 8, characters 0-143
Module Path: Owl_neural
OCaml Source: Implementation
*)


module S = struct
  include Owl_neural_generic.Make (Owl_algodiff_primal_ops.S)

  (* module Parallel = Owl_neural_parallel.Make (Graph) *)
end
```

**Result 15:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp.ml", line 11, characters 0-24
Module Path: Owl_nlp
OCaml Source: Implementation
*)


module Lda = Owl_nlp_lda
```

**Result 16:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda0.ml", line 112, characters 2-18
Module Path: Owl_nlp_lda0.SimpleLDA
OCaml Source: Implementation
*)


let init _m = ()
```

**Result 17:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda.ml", line 65, characters 2-18
Module Path: Owl_nlp_lda.SimpleLDA
OCaml Source: Implementation
*)


let init _m = ()
```

**Result 18:**
```ocaml
(** 
Location: File "src/owl/algodiff/owl_algodiff_primal_ops.ml", line 10, characters 2-186
Module Path: Owl_algodiff_primal_ops.S
OCaml Source: Implementation
*)


module Mat = struct
    let eye = Owl_dense_matrix.S.eye

    let tril = Owl_dense_matrix.S.tril

    let triu = Owl_dense_matrix.S.triu

    let diagm = Owl_dense_matrix.S.diagm
  end
```

**Result 19:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_uniop.ml", line 423, characters 0-1486
Module Path: Owl_ext_uniop
OCaml Source: Implementation
*)


module DMD = struct
  module M = Owl_ext_dense_matrix.D

  let min' x = M.min' x

  let max' x = M.max' x

  let minmax' x = M.minmax' x

  let min_i x = M.min_i x

  let max_i x = M.max_i x

  let minmax_i x = M.minmax_i x

  let inv x = M.inv x

  let trace x = M.trace x

  let sum' x = M.sum' x

  let prod' x = M.prod' x

  let abs x = M.abs x

  let abs2 x = M.abs2 x

  let neg x = M.neg x

  let reci x = M.reci x

  let signum x = M.signum x

  let sqr x = M.sqr x

  let sqrt x = M.sqrt x

  let cbrt x = M.cbrt x

  let exp x = M.exp x

  let exp2 x = M.exp2 x

  let expm1 x = M.expm1 x

  let log x = M.log x

  let log10 x = M.log10 x

  let log2 x = M.log2 x

  let log1p x = M.log1p x

  let sin x = M.sin x

  let cos x = M.cos x

  let tan x = M.tan x

  let asin x = M.asin x

  let acos x = M.acos x

  let atan x = M.atan x

  let sinh x = M.sinh x

  let cosh x = M.cosh x

  let tanh x = M.tanh x

  let asinh x = M.asinh x

  let acosh x = M.acosh x

  let atanh x = M.atanh x

  let floor x = M.floor x

  let ceil x = M.ceil x

  let round x = M.round x

  let trunc x = M.trunc x

  let erf x = M.erf x

  let erfc x = M.erfc x

  let logistic x = M.logistic x

  let relu x = M.relu x

  let softplus x = M.softplus x

  let softsign x = M.softsign x

  let softmax x = M.softmax x

  let sigmoid x = M.sigmoid x

  let log_sum_exp' x = M.log_sum_exp' x

  let l1norm' x = M.l1norm' x

  let l2norm' x = M.l2norm' x

  let l2norm_sqr' x = M.l2norm_sqr' x
end
```

**Result 20:**
```ocaml
(** 
Location: File "src/owl/linalg/owl_linalg_generic.ml", line 17, characters 0-241
Module Path: Owl_linalg_generic
OCaml Source: Implementation
*)


module M = struct
  include Owl_dense_matrix_generic
  include Owl_operator.Make_Basic (Owl_dense_matrix_generic)
  include Owl_operator.Make_Extend (Owl_dense_matrix_generic)
  include Owl_operator.Make_Matrix (Owl_dense_matrix_generic)
end
```

**Result 21:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.ml", line 14, characters 0-16
Module Path: Owl_maths
OCaml Source: Implementation
*)


let mul = ( *. )
```

**Result 22:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_dense_ndarray.ml", line 11, characters 0-248
Module Path: Owl_ext_dense_ndarray
OCaml Source: Implementation
*)


module type PackSig = sig
  type arr

  type elt

  type cast_arr

  val pack_box : arr -> ext_typ

  val unpack_box : ext_typ -> arr

  val pack_elt : elt -> ext_typ

  val unpack_elt : ext_typ -> elt

  val pack_cast_box : cast_arr -> ext_typ
end
```

**Result 23:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths_special.ml", line 213, characters 0-62
Module Path: Owl_maths_special
OCaml Source: Implementation
*)


external mulmod : int -> int -> int -> int = "owl_stub_mulmod"
```

**Result 24:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_matrix.ml", line 15, characters 0-188
Module Path: Owl_dense_matrix
OCaml Source: Implementation
*)


module Generic = struct
  include Owl_dense_matrix_generic
  include Operator

  (* inject function aliases *)

  let inv = Owl_linalg_generic.inv

  let mpow = Owl_linalg_generic.mpow
end
```

**Result 25:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_matrix.ml", line 63, characters 0-164
Module Path: Owl_dense_matrix
OCaml Source: Implementation
*)


module Z = struct
  include Owl_dense_matrix_z
  include Operator

  (* inject function aliases *)

  let inv = Owl_linalg_z.inv

  let mpow = Owl_linalg_z.mpow
end
```

**Result 26:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda.ml", line 250, characters 0-147
Module Path: Owl_nlp_lda
OCaml Source: Implementation
*)


module LightLDA = struct
  let init _m = failwith "LightLDA: not implemented"

  let sampling _m _d _doc = failwith "LightLDA: not implemented"
end
```

**Result 27:**
```ocaml
(** 
Location: File "src/owl/maths/owl_maths.ml", line 355, characters 0-49
Module Path: Owl_maths
OCaml Source: Implementation
*)


let mulmod a b m = Owl_maths_special.mulmod a b m
```

**Result 28:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_types.ml", line 86, characters 0-115
Module Path: Owl_ext_types
OCaml Source: Implementation
*)


type ('a, 'b) box =
  | DM : (dns, mat) box
  | DA : (dns, mat) box
  | SM : (sps, arr) box
  | SA : (sps, arr) box
```

**Result 29:**
```ocaml
(** 
Location: File "src/owl/algodiff/owl_algodiff_primal_ops.ml", line 36, characters 2-186
Module Path: Owl_algodiff_primal_ops.D
OCaml Source: Implementation
*)


module Mat = struct
    let eye = Owl_dense_matrix.D.eye

    let tril = Owl_dense_matrix.D.tril

    let triu = Owl_dense_matrix.D.triu

    let diagm = Owl_dense_matrix.D.diagm
  end
```

**Result 30:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_dense_matrix.ml", line 11, characters 0-248
Module Path: Owl_ext_dense_matrix
OCaml Source: Implementation
*)


module type PackSig = sig
  type mat

  type elt

  type cast_mat

  val pack_box : mat -> ext_typ

  val unpack_box : ext_typ -> mat

  val pack_elt : elt -> ext_typ

  val unpack_elt : ext_typ -> elt

  val pack_cast_box : cast_mat -> ext_typ
end
```

**Result 31:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_binop.ml", line 2330, characters 0-813
Module Path: Owl_ext_binop
OCaml Source: Implementation
*)


module DMZ_DMD = struct
  module M = Owl_ext_dense_matrix.Z

  let lift = Owl_ext_lifts.DMD_DMZ.lift

  let ( + ) x y = M.add x (lift y)

  let ( - ) x y = M.sub x (lift y)

  let ( * ) x y = M.mul x (lift y)

  let ( / ) x y = M.div x (lift y)

  let ( *@ ) x y = M.dot x (lift y)

  let ( = ) x y = M.equal x (lift y)

  let ( <> ) x y = M.not_equal x (lift y)

  let ( > ) x y = M.greater x (lift y)

  let ( < ) x y = M.less x (lift y)

  let ( >= ) x y = M.greater_equal x (lift y)

  let ( <= ) x y = M.less_equal x (lift y)

  let ( =. ) x y = M.elt_equal x (lift y)

  let ( <>. ) x y = M.elt_not_equal x (lift y)

  let ( >. ) x y = M.elt_greater x (lift y)

  let ( <. ) x y = M.elt_less x (lift y)

  let ( >=. ) x y = M.elt_greater_equal x (lift y)

  let ( <=. ) x y = M.elt_less_equal x (lift y)
end
```

**Result 32:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda0.ml", line 14, characters 0-68
Module Path: Owl_nlp_lda0
OCaml Source: Implementation
*)


type lda_typ =
  | SimpleLDA
  | FTreeLDA
  | LightLDA
  | SparseLDA
```

**Result 33:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda.ml", line 12, characters 0-68
Module Path: Owl_nlp_lda
OCaml Source: Implementation
*)


type lda_typ =
  | SimpleLDA
  | FTreeLDA
  | LightLDA
  | SparseLDA
```

**Result 34:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext.ml", line 12, characters 0-156
Module Path: Owl_ext
OCaml Source: Implementation
*)


module Dense = struct
  module Ndarray = struct
    include Owl_ext_dense_ndarray
  end

  module Matrix = struct
    include Owl_ext_dense_matrix
  end
end
```

**Result 35:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke_generated.ml", line 22096, characters 0-129
Module Path: Owl_lapacke_generated
OCaml Source: Implementation
*)


let clascl ~layout ~typ ~kl ~ku ~cfrom ~cto ~m ~n ~a:(CI.CPointer a) ~lda =
  lapacke_clascl layout typ kl ku cfrom cto m n a lda
```

**Result 36:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray.ml", line 50, characters 0-135
Module Path: Owl_dense_ndarray
OCaml Source: Implementation
*)


module Z = struct
  include Owl_dense_ndarray_z
  include Operator

  (* inject function aliases *)

  let mpow = Owl_linalg_z.mpow
end
```

**Result 37:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke.ml", line 53, characters 0-66
Module Path: Owl_lapacke
OCaml Source: Implementation
*)


let _lapacke_diag = function
  | NonUnit -> 131
  | Unit    -> 132
```

**Result 38:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke_generated.ml", line 22389, characters 0-233
Module Path: Owl_lapacke_generated
OCaml Source: Implementation
*)


let dormhr
    ~layout
    ~side
    ~trans
    ~m
    ~n
    ~ilo
    ~ihi
    ~a:(CI.CPointer a)
    ~lda
    ~tau:(CI.CPointer tau)
    ~c:(CI.CPointer c)
    ~ldc
  =
  lapacke_dormhr layout side trans m n ilo ihi a lda tau c ldc
```

**Result 39:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_uniop.ml", line 17, characters 0-952
Module Path: Owl_ext_uniop
OCaml Source: Implementation
*)


module F = struct
  module M = Owl_maths

  let abs x = F M.(abs x)

  let abs2 x = F (x *. x)

  let neg x = F M.(neg x)

  let reci x = F M.(reci x)

  let signum x = F M.(signum x)

  let sqr x = F (x *. x)

  let sqrt x = F M.(sqrt x)

  let exp x = F M.(exp x)

  let expm1 x = F M.(expm1 x)

  let log x = F M.(log x)

  let log10 x = F M.(log10 x)

  let log2 x = F M.(log2 x)

  let log1p x = F M.(log1p x)

  let sin x = F M.(sin x)

  let cos x = F M.(cos x)

  let tan x = F M.(tan x)

  let asin x = F M.(asin x)

  let acos x = F M.(acos x)

  let atan x = F M.(atan x)

  let sinh x = F M.(sinh x)

  let cosh x = F M.(cosh x)

  let tanh x = F M.(tanh x)

  let asinh x = F M.(asinh x)

  let acosh x = F M.(acosh x)

  let atanh x = F M.(atanh x)

  let floor x = F M.(floor x)

  let ceil x = F M.(ceil x)

  let round x = F M.(round x)

  let trunc x = F M.(trunc x)

  let relu x = F M.(relu x)

  let sigmoid x = F M.(sigmoid x)
end
```

**Result 40:**
```ocaml
(** 
Location: File "src/owl/dense/owl_dense_ndarray.ml", line 23, characters 0-135
Module Path: Owl_dense_ndarray
OCaml Source: Implementation
*)


module S = struct
  include Owl_dense_ndarray_s
  include Operator

  (* inject function aliases *)

  let mpow = Owl_linalg_s.mpow
end
```

**Result 41:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_binop.ml", line 1020, characters 0-733
Module Path: Owl_ext_binop
OCaml Source: Implementation
*)


module DMZ_C = struct
  module M = Owl_ext_dense_matrix.Z

  let ( + ) x a = M.add_scalar x a

  let ( - ) x a = M.sub_scalar x a

  let ( * ) x a = M.mul_scalar x a

  let ( / ) x a = M.div_scalar x a

  let ( = ) x a = M.equal_scalar x a

  let ( <> ) x a = M.not_equal_scalar x a

  let ( < ) x a = M.less_scalar x a

  let ( > ) x a = M.greater_scalar x a

  let ( <= ) x a = M.less_equal_scalar x a

  let ( >= ) x a = M.greater_equal_scalar x a

  let ( =. ) x a = M.elt_equal_scalar x a

  let ( <>. ) x a = M.elt_not_equal_scalar x a

  let ( <. ) x a = M.elt_less_scalar x a

  let ( >. ) x a = M.elt_greater_scalar x a

  let ( <=. ) x a = M.elt_less_equal_scalar x a

  let ( >=. ) x a = M.elt_greater_equal_scalar x a
end
```

**Result 42:**
```ocaml
(** 
Location: File "src/owl/algodiff/owl_algodiff_primal_ops.ml", line 6, characters 0-401
Module Path: Owl_algodiff_primal_ops
OCaml Source: Implementation
*)


module S = struct
  include Owl_dense_ndarray.S
  module Scalar = Owl_maths

  module Mat = struct
    let eye = Owl_dense_matrix.S.eye

    let tril = Owl_dense_matrix.S.tril

    let triu = Owl_dense_matrix.S.triu

    let diagm = Owl_dense_matrix.S.diagm
  end

  module Linalg = struct
    include Owl_linalg.S

    let qr a =
      let q, r, _ = qr a in
      q, r


    let lq x = lq x
  end
end
```

**Result 43:**
```ocaml
(** 
Location: File "src/owl/lapacke/owl_lapacke_generated.ml", line 21902, characters 0-261
Module Path: Owl_lapacke_generated
OCaml Source: Implementation
*)


let clarfb
    ~layout
    ~side
    ~trans
    ~direct
    ~storev
    ~m
    ~n
    ~k
    ~v:(CI.CPointer v)
    ~ldv
    ~t:(CI.CPointer t)
    ~ldt
    ~c:(CI.CPointer c)
    ~ldc
  =
  lapacke_clarfb layout side trans direct storev m n k v ldv t ldt c ldc
```

**Result 44:**
```ocaml
(** 
Location: File "src/owl/sparse/owl_sparse_matrix_generic.ml", line 12, characters 0-241
Module Path: Owl_sparse_matrix_generic
OCaml Source: Implementation
*)


type ('a, 'b) t =
  { mutable m : int
  ; (* number of rows *)
    mutable n : int
  ; (* number of columns *)
    mutable k : ('a, 'b) kind
  ; (* type of sparse matrices *)
    mutable d : ('a, 'b) eigen_mat (* point to eigen struct *)
  }
```

**Result 45:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda0.ml", line 291, characters 0-74
Module Path: Owl_nlp_lda0
OCaml Source: Implementation
*)


module FTreeLDA = struct
  let init _m = ()

  let sampling _m _d = ()
end
```

**Result 46:**
```ocaml
(** 
Location: File "src/owl/linalg/owl_linalg.ml", line 16, characters 0-44
Module Path: Owl_linalg
OCaml Source: Implementation
*)


module D = struct
  include Owl_linalg_d
end
```

**Result 47:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_binop.ml", line 1408, characters 0-813
Module Path: Owl_ext_binop
OCaml Source: Implementation
*)


module DMZ_DMC = struct
  module M = Owl_ext_dense_matrix.Z

  let lift = Owl_ext_lifts.DMC_DMZ.lift

  let ( + ) x y = M.add x (lift y)

  let ( - ) x y = M.sub x (lift y)

  let ( * ) x y = M.mul x (lift y)

  let ( / ) x y = M.div x (lift y)

  let ( *@ ) x y = M.dot x (lift y)

  let ( = ) x y = M.equal x (lift y)

  let ( <> ) x y = M.not_equal x (lift y)

  let ( > ) x y = M.greater x (lift y)

  let ( < ) x y = M.less x (lift y)

  let ( >= ) x y = M.greater_equal x (lift y)

  let ( <= ) x y = M.less_equal x (lift y)

  let ( =. ) x y = M.elt_equal x (lift y)

  let ( <>. ) x y = M.elt_not_equal x (lift y)

  let ( >. ) x y = M.elt_greater x (lift y)

  let ( <. ) x y = M.elt_less x (lift y)

  let ( >=. ) x y = M.elt_greater_equal x (lift y)

  let ( <=. ) x y = M.elt_less_equal x (lift y)
end
```

**Result 48:**
```ocaml
(** 
Location: File "src/owl/nlp/owl_nlp_lda0.ml", line 298, characters 2-18
Module Path: Owl_nlp_lda0.LightLDA
OCaml Source: Implementation
*)


let init _m = ()
```

**Result 49:**
```ocaml
(** 
Location: File "src/owl/sparse/owl_sparse_ndarray.ml", line 20, characters 0-71
Module Path: Owl_sparse_ndarray
OCaml Source: Implementation
*)


module D = struct
  include Owl_sparse_ndarray_d
  include Operator
end
```

**Result 50:**
```ocaml
(** 
Location: File "src/owl/ext/owl_ext_binop.ml", line 2020, characters 0-882
Module Path: Owl_ext_binop
OCaml Source: Implementation
*)


module DMZ_F = struct
  module M = Owl_ext_dense_matrix.Z

  let lift = Owl_ext_lifts.F_C.lift

  let ( + ) x a = M.add_scalar x (lift a)

  let ( - ) x a = M.sub_scalar x (lift a)

  let ( * ) x a = M.mul_scalar x (lift a)

  let ( / ) x a = M.div_scalar x (lift a)

  let ( = ) x a = M.equal_scalar x (lift a)

  let ( <> ) x a = M.not_equal_scalar x (lift a)

  let ( < ) x a = M.less_scalar x (lift a)

  let ( > ) x a = M.greater_scalar x (lift a)

  let ( <= ) x a = M.less_equal_scalar x (lift a)

  let ( >= ) x a = M.greater_equal_scalar x (lift a)

  let ( =. ) x a = M.elt_equal_scalar x (lift a)

  let ( <>. ) x a = M.elt_not_equal_scalar x (lift a)

  let ( <. ) x a = M.elt_less_scalar x (lift a)

  let ( >. ) x a = M.elt_greater_scalar x (lift a)

  let ( <=. ) x a = M.elt_less_equal_scalar x (lift a)

  let ( >=. ) x a = M.elt_greater_equal_scalar x (lift a)
end
```
