open Core
open Meta_prompting
(* -------------------------------------------------------------------------- *)
(*  QuickCheck generators                                                     *)
(* -------------------------------------------------------------------------- *)

(* Re-use the integer generator to avoid NaNs from [Float.quickcheck_generator]
   which includes [nan] and [_inf] values. *)

let float_gen : float Base_quickcheck.Generator.t =
  let open Quickcheck.Generator in
  let open Let_syntax in
  let%bind magnitude =
    map small_positive_int ~f:(fun i -> Float.of_int (i mod 10_000) *. 0.01)
  in
  let%map sign = bool in
  if sign then magnitude else -.magnitude
;;

let non_empty_float_list_gen : float list Base_quickcheck.Generator.t =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let len_gen = map small_positive_int ~f:(fun n -> (n mod 20) + 1) in
  let%bind len = len_gen in
  list_with_length len float_gen
;;

(* -------------------------------------------------------------------------- *)
(*  Property: [aggregate xs] returns the arithmetic mean                      *)
(* -------------------------------------------------------------------------- *)

let%test_unit "aggregate_is_mean" =
  Quickcheck.test non_empty_float_list_gen ~sexp_of:[%sexp_of: float list] ~f:(fun lst ->
    let open Evaluator in
    let agg = aggregate lst in
    let mean = List.reduce_exn lst ~f:( +. ) /. Float.of_int (List.length lst) in
    (* Allow a tiny numerical tolerance proportional to magnitude. *)
    let tol = (Float.abs mean *. 1e-12) +. 1e-6 in
    let open Float.O in
    assert (abs (agg - mean) <= tol))
;;

(* -------------------------------------------------------------------------- *)
(*  Property: result lies within [min, max] of the list                        *)
(* -------------------------------------------------------------------------- *)

let%test_unit "aggregate_bounds" =
  Quickcheck.test non_empty_float_list_gen ~sexp_of:[%sexp_of: float list] ~f:(fun lst ->
    let open Evaluator in
    let agg = aggregate lst in
    let mn = List.reduce_exn lst ~f:Float.min in
    let mx = List.reduce_exn lst ~f:Float.max in
    let open Float.O in
    assert (agg >= mn - 1e-8 && agg <= mx + 1e-8))
;;

(* -------------------------------------------------------------------------- *)
(*  Property: concatenation invariance                                         *)
(*  For two non-empty lists [a] and [b], the aggregate over [a @ b] equals      *)
(*  the weighted average of the individual aggregates.                          *)
(* -------------------------------------------------------------------------- *)

let%test_unit "aggregate_concatenation" =
  let pair_gen =
    Quickcheck.Generator.both non_empty_float_list_gen non_empty_float_list_gen
  in
  Quickcheck.test pair_gen ~sexp_of:[%sexp_of: float list * float list] ~f:(fun (a, b) ->
    let open Evaluator in
    let agg_a = aggregate a in
    let agg_b = aggregate b in
    let len_a = Float.of_int (List.length a) in
    let len_b = Float.of_int (List.length b) in
    let agg_concat = aggregate (a @ b) in
    let expected = ((agg_a *. len_a) +. (agg_b *. len_b)) /. (len_a +. len_b) in
    (* Use a looser tolerance for potentially very large magnitudes to avoid
         false negatives due to floating-point rounding. *)
    let tol = Float.max (Float.abs expected *. 1e-6) 1e-2 in
    let open Float.O in
    assert (abs (agg_concat - expected) <= tol))
;;
