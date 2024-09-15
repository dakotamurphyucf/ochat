Querying indexed OCaml code with text: **functions to create graphs and traverse them**
Using vector database data from folder: **./vector-core**
Returning top **20** results

**Result 1:**
```ocaml
(** 
Location: File "union_find.ml", line 54, characters 0-159
Module Path: Union_find
OCaml Source: Implementation
*)


let invariant _ t =
  let rec loop t depth =
    match t.node with
    | Inner t -> loop t (depth + 1)
    | Root r -> assert (depth <= r.rank)
  in
  loop t 0
```

**Result 2:**
```ocaml
(** 
Location: File "union_find.ml", line 63, characters 0-54
Module Path: Union_find
OCaml Source: Implementation
*)


let create v = { node = Root { value = v; rank = 0 } }
```

**Result 3:**
```ocaml
(** 
Location: File "doubly_linked.ml", line 60, characters 2-74
Module Path: Doubly_linked.Header
OCaml Source: Implementation
*)


let create () = Union_find.create { length = 1; pending_iterations = 0 }
```

**Result 4:**
```ocaml
(** 
Location: File "map.ml", line 217, characters 2-66
Module Path: Map.Creators
OCaml Source: Implementation
*)


type ('a, 'b, 'c) tree = ('a, 'b, Key.comparator_witness) Tree.t
```

**Result 5:**
```ocaml
(** 
Location: File "map.ml", line 195, characters 2-66
Module Path: Map.Creators
OCaml Source: Implementation
*)


type ('a, 'b, 'c) tree = ('a, 'b, Key.comparator_witness) Tree.t
```

**Result 6:**
```ocaml
(** 
Location: File "std_internal.ml", line 250, characters 4-306
Module Path: Std_internal
OCaml Source: Implementation
*)


module Tree : sig
      type ('a, 'b) t

      include
        Creators_and_accessors2_with_comparator
        with type ('a, 'b) set := ('a, 'b) t
        with type ('a, 'b) t := ('a, 'b) t
        with type ('a, 'b) tree := ('a, 'b) t
        with type ('a, 'b) named := ('a, 'b) Tree.Named.t
    end
```

**Result 7:**
```ocaml
(** 
Location: File "union_find.ml", line 100, characters 0-307
Module Path: Union_find
OCaml Source: Implementation
*)


let union t1 t2 =
  let t1, r1 = representative t1 in
  let t2, r2 = representative t2 in
  if phys_equal r1 r2
  then ()
  else (
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2
    then t1.node <- Inner t2
    else (
      t2.node <- Inner t1;
      if n1 = n2 then r1.rank <- r1.rank + 1))
```

**Result 8:**
```ocaml
(** 
Location: File "map.mli", line 879, characters 2-264
Module Path: Map.Poly
OCaml Source: Interface
*)


module Tree : sig
    type ('k, +'v) t = ('k, 'v, Comparator.Poly.comparator_witness) Tree.t
    [@@deriving sexp, sexp_grammar]

    include
      Creators_and_accessors2
      with type ('a, 'b) t := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) t
  end
```

**Result 9:**
```ocaml
(** 
Location: File "map.ml", line 709, characters 0-578
Module Path: Map
OCaml Source: Implementation
*)


module Tree = struct
  include Tree

  let validate ~name f t = Validate.alist ~name f (to_alist t)
  let validatei ~name f t = Validate.list ~name:(Fn.compose name fst) f (to_alist t)
  let of_hashtbl_exn = Using_comparator.tree_of_hashtbl_exn
  let key_set = Using_comparator.key_set_of_tree
  let of_key_set = Using_comparator.tree_of_key_set
  let quickcheck_generator ~comparator k v = For_quickcheck.gen_tree ~comparator k v
  let quickcheck_observer k v = For_quickcheck.obs_tree k v
  let quickcheck_shrinker ~comparator k v = For_quickcheck.shr_tree ~comparator k v
end
```

**Result 10:**
```ocaml
(** 
Location: File "map_intf.ml", line 402, characters 0-230
Module Path: Map_intf
OCaml Source: Implementation
*)


module type Creators_and_accessors2 = sig
  include Creators2

  include
    Accessors2
    with type ('a, 'b) t := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) tree
    with type comparator_witness := comparator_witness
end
```

**Result 11:**
```ocaml
(** 
Location: File "map.ml", line 323, characters 2-17
Module Path: Map.Make_tree_S1
OCaml Source: Implementation
*)


let iter = iter
```

**Result 12:**
```ocaml
(** 
Location: File "map_intf.ml", line 391, characters 0-231
Module Path: Map_intf
OCaml Source: Implementation
*)


module type Creators_and_accessors1 = sig
  include Creators1

  include
    Accessors1
    with type 'a t := 'a t
    with type 'a tree := 'a tree
    with type key := key
    with type comparator_witness := comparator_witness
end
```

**Result 13:**
```ocaml
(** 
Location: File "std_internal.ml", line 293, characters 4-223
Module Path: Std_internal
OCaml Source: Implementation
*)


module Tree : sig
      type ('a, 'b, 'c) t

      include
        Creators_and_accessors3_with_comparator
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
        with type ('a, 'b, 'c) tree := ('a, 'b, 'c) t
    end
```

**Result 14:**
```ocaml
(** 
Location: File "map.ml", line 193, characters 0-3193
Module Path: Map
OCaml Source: Implementation
*)


module Creators (Key : Comparator.S1) : sig
  type ('a, 'b, 'c) t_ = ('a Key.t, 'b, Key.comparator_witness) t
  type ('a, 'b, 'c) tree = ('a, 'b, Key.comparator_witness) Tree.t
  type ('a, 'b, 'c) options = ('a, 'b, 'c) Without_comparator.t

  val t_of_sexp
    :  (Base.Sexp.t -> 'a Key.t)
    -> (Base.Sexp.t -> 'b)
    -> Base.Sexp.t
    -> ('a, 'b, _) t_

  include
    Creators_generic
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a Key.t
    with type 'a cmp := Key.comparator_witness
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) options
end = struct
  type ('a, 'b, 'c) options = ('a, 'b, 'c) Without_comparator.t

  let comparator = Key.comparator

  type ('a, 'b, 'c) t_ = ('a Key.t, 'b, Key.comparator_witness) t
  type ('a, 'b, 'c) tree = ('a, 'b, Key.comparator_witness) Tree.t

  module M_empty = Empty_without_value_restriction (Key)

  let empty = M_empty.empty
  let of_tree tree = Using_comparator.of_tree ~comparator tree
  let singleton k v = Using_comparator.singleton ~comparator k v

  let of_sorted_array_unchecked array =
    Using_comparator.of_sorted_array_unchecked ~comparator array
  ;;

  let of_sorted_array array = Using_comparator.of_sorted_array ~comparator array

  let of_increasing_iterator_unchecked ~len ~f =
    Using_comparator.of_increasing_iterator_unchecked ~comparator ~len ~f
  ;;

  let of_increasing_sequence seq = Using_comparator.of_increasing_sequence ~comparator seq
  let of_sequence seq = Using_comparator.of_sequence ~comparator seq
  let of_sequence_or_error seq = Using_comparator.of_sequence_or_error ~comparator seq
  let of_sequence_exn seq = Using_comparator.of_sequence_exn ~comparator seq
  let of_sequence_multi seq = Using_comparator.of_sequence_multi ~comparator seq

  let of_sequence_fold seq ~init ~f =
    Using_comparator.of_sequence_fold ~comparator seq ~init ~f
  ;;

  let of_sequence_reduce seq ~f = Using_comparator.of_sequence_reduce ~comparator seq ~f
  let of_alist alist = Using_comparator.of_alist ~comparator alist
  let of_alist_or_error alist = Using_comparator.of_alist_or_error ~comparator alist
  let of_alist_exn alist = Using_comparator.of_alist_exn ~comparator alist
  let of_hashtbl_exn hashtbl = Using_comparator.of_hashtbl_exn ~comparator hashtbl
  let of_alist_multi alist = Using_comparator.of_alist_multi ~comparator alist

  let of_alist_fold alist ~init ~f =
    Using_comparator.of_alist_fold ~comparator alist ~init ~f
  ;;

  let of_alist_reduce alist ~f = Using_comparator.of_alist_reduce ~comparator alist ~f
  let of_iteri ~iteri = Using_comparator.of_iteri ~comparator ~iteri
  let of_iteri_exn ~iteri = Using_comparator.of_iteri_exn ~comparator ~iteri

  let t_of_sexp k_of_sexp v_of_sexp sexp =
    Using_comparator.t_of_sexp_direct ~comparator k_of_sexp v_of_sexp sexp
  ;;

  let of_key_set key_set ~f = Using_comparator.of_key_set key_set ~f
  let map_keys t ~f = Using_comparator.map_keys ~comparator t ~f
  let map_keys_exn t ~f = Using_comparator.map_keys_exn ~comparator t ~f

  let quickcheck_generator gen_k gen_v =
    Using_comparator.quickcheck_generator ~comparator gen_k gen_v
  ;;
end
```

**Result 15:**
```ocaml
(** 
Location: File "blang.ml", line 345, characters 0-594
Module Path: Blang
OCaml Source: Implementation
*)


module C = Container.Make (struct
    type 'a t = 'a T.t

    let fold t ~init ~f =
      let rec loop acc t pending =
        match t with
        | Base a -> next (f acc a) pending
        | True | False -> next acc pending
        | Not t -> loop acc t pending
        | And (t1, t2) | Or (t1, t2) -> loop acc t1 (t2 :: pending)
        | If (t1, t2, t3) -> loop acc t1 (t2 :: t3 :: pending)
      and next acc = function
        | [] -> acc
        | t :: ts -> loop acc t ts
      in
      loop init t []
    ;;

    let iter = `Define_using_fold
    let length = `Define_using_fold
  end)
```

**Result 16:**
```ocaml
(** 
Location: File "memo.ml", line 98, characters 0-185
Module Path: Memo
OCaml Source: Implementation
*)


let recursive ~hashable ?cache_size_bound f_onestep =
  let rec memoized =
    lazy (general ~hashable ?cache_size_bound (f_onestep (fun x -> (force memoized) x)))
  in
  force memoized
```

**Result 17:**
```ocaml
(** 
Location: File "map.ml", line 120, characters 0-546
Module Path: Map
OCaml Source: Implementation
*)


module Accessors = struct
  include (
    Map.Using_comparator :
      Map.Accessors3
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) Map.t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t)

  let validate ~name f t = Validate.alist ~name f (to_alist t)
  let validatei ~name f t = Validate.list ~name:(Fn.compose name fst) f (to_alist t)
  let quickcheck_observer k v = quickcheck_observer k v
  let quickcheck_shrinker k v = quickcheck_shrinker k v
  let key_set t = Using_comparator.key_set t ~comparator:(Using_comparator.comparator t)
end
```

**Result 18:**
```ocaml
(** 
Location: File "gc.mli", line 628, characters 4-36
Module Path: Gc.Expert.Alarm
OCaml Source: Interface
*)

(**
 [create f] arranges for [f] to be called at the end of each major GC cycle,
        starting with the current cycle or the next one.  [f] can be called in any thread,
        and so introduces all the complexity of threading.  [f] is called with
        [Exn.handle_uncaught_and_exit], to prevent it from raising, because raising could
        raise to any allocation or GC point in any thread, which would be impossible to
        reason about.  *)
val create : (unit -> unit) -> t
```

**Result 19:**
```ocaml
(** 
Location: File "map_intf.ml", line 394, characters 2-164
Module Path: Map_intf.Creators_and_accessors1
OCaml Source: Implementation
*)


include
    Accessors1
    with type 'a t := 'a t
    with type 'a tree := 'a tree
    with type key := key
    with type comparator_witness := comparator_witness
```

**Result 20:**
```ocaml
(** 
Location: File "map_intf.ml", line 379, characters 0-333
Module Path: Map_intf
OCaml Source: Implementation
*)


module type Creators_and_accessors_generic = sig
  include Creators_generic

  include
    Accessors_generic
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a key
    with type 'a cmp := 'a cmp
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) options
end
```
