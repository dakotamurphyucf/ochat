open Core
module RM = Meta_prompting.Recursive_mp

(* Helper to fully evaluate a [Recursive_mp.t] to its contained value by
   iteratively applying [bind] until we reach a [Return] constructor.  This
   is sufficient for the finite values produced in the generators below. *)
let rec run : _ RM.t -> _ = function
  | RM.Return x -> x
  | RM.Bind (m, f) -> run (RM.bind m f)
;;

let map ~f = function
  | RM.Return x -> RM.Return (f x)
  | RM.Bind (m, g) -> RM.Bind (m, fun x -> g (f x))
;;

(* -------------------------------------------------------------------------- *)
(* Generators                                                                  *)
(* -------------------------------------------------------------------------- *)

let rm_return_gen : int RM.t Base_quickcheck.Generator.t =
  Quickcheck.Generator.map Int.quickcheck_generator ~f:RM.return
;;

(* Produce a simple [Bind] node that adds a constant offset. *)
let rm_bind_gen : int RM.t Base_quickcheck.Generator.t =
  let open Quickcheck.Generator in
  let open Let_syntax in
  let%bind x = Int.quickcheck_generator in
  let%map offset = Int.quickcheck_generator in
  RM.Bind (RM.return x, fun y -> RM.return (y + offset))
;;

let rm_gen : int RM.t Base_quickcheck.Generator.t =
  Quickcheck.Generator.weighted_union [ 3., rm_return_gen; 1., rm_bind_gen ]
;;

(* -------------------------------------------------------------------------- *)
(*  Properties for [join]                                                       *)
(* -------------------------------------------------------------------------- *)

let%test_unit "join_left_identity" =
  Quickcheck.test rm_gen ~f:(fun m ->
    let lhs = run (RM.join (RM.return m)) in
    let rhs = run m in
    [%test_result: int] lhs ~expect:rhs)
;;

(* Associativity: join ∘ join = join ∘ fmap join *)
let%test_unit "join_associativity" =
  (* Generate three integers to build a nested structure with two Bind levels
     so that the associativity property exercises the [Bind] case. *)
  let nested_gen =
    let open Quickcheck.Generator in
    let open Let_syntax in
    let%bind x = Int.quickcheck_generator in
    let%bind y = Int.quickcheck_generator in
    let%map z = Int.quickcheck_generator in
    x, y, z
  in
  Quickcheck.test nested_gen ~f:(fun (x, y, z) ->
    (* Construct a value of type [int RM.t RM.t RM.t] with nested binds to
         exercise non-trivial flattening behaviour. *)
    let m : int RM.t RM.t RM.t =
      RM.return (RM.return (RM.Bind (RM.return x, fun u -> RM.return (u + y + z))))
    in
    let lhs = run (RM.join (RM.join m)) in
    let rhs = run @@ RM.join (RM.Bind (RM.join m, RM.return)) in
    [%test_result: int] lhs ~expect:rhs)
;;
