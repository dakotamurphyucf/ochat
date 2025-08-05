open Core
open Meta_prompting
(* Helpers ------------------------------------------------------------- *)

module RM = Recursive_mp

let extract_return = function
  | RM.Return x -> Some x
  | _ -> None
;;

(* -------------------------------------------------------------------- *)

let%test_unit "monad_left_identity" =
  Quickcheck.test Int.quickcheck_generator ~f:(fun x ->
    let f y = RM.return (y + 1) in
    match RM.bind (RM.return x) f |> extract_return, f x |> extract_return with
    | Some a, Some b -> [%test_result: int] ~expect:b a
    | _ -> failwith "Unexpected non-Return variant")
;;

let%test_unit "monad_right_identity" =
  Quickcheck.test Int.quickcheck_generator ~f:(fun x ->
    let m = RM.return x in
    match RM.bind m RM.return |> extract_return, extract_return m with
    | Some a, Some b -> [%test_result: int] ~expect:b a
    | _ -> failwith "Unexpected non-Return variant")
;;

let%test_unit "monad_associativity" =
  Quickcheck.test Int.quickcheck_generator ~f:(fun x ->
    let m = RM.return x in
    let f y = RM.return (y + 1) in
    let g y = RM.return (y * 2) in
    let lhs = RM.bind (RM.bind m f) g |> extract_return in
    let rhs = RM.bind m (fun x -> RM.bind (f x) g) |> extract_return in
    match lhs, rhs with
    | Some a, Some b -> [%test_result: int] ~expect:b a
    | _ -> failwith "Unexpected non-Return variant")
;;

(* -------------------------------------------------------------------- *)

module Ev = Evaluator

let%test_unit "mock_judge_returns_constant" =
  let candidate = "whatever" in
  let score = Ev.evaluate Ev.default candidate in
  [%test_result: float] ~expect:0.5 score
;;

(* Use [Int] generators and convert to float to avoid overflow/NaN issues with
   extreme [Float.quickcheck_generator] values. *)

let float_of_int i = Float.of_int i |> ( *. ) 0.1
let float_gen = Quickcheck.Generator.map Int.quickcheck_generator ~f:float_of_int

let%test_unit "aggregate_singleton" =
  Quickcheck.test float_gen ~sexp_of:Float.sexp_of_t ~f:(fun v ->
    let got = Ev.aggregate [ v ] in
    (* allow tiny rounding error proportional to magnitude *)
    assert (Float.(abs (got - v) <= (Float.abs v *. 1e-12) +. 1e-6)))
;;

let%test_unit "aggregate_constant_list" =
  Quickcheck.test float_gen ~sexp_of:Float.sexp_of_t ~f:(fun v ->
    let lst = List.init 10 ~f:(fun _ -> v) in
    let agg = Ev.aggregate lst in
    assert (Float.(abs (agg - v) <= (Float.abs v *. 1e-12) +. 1e-6)))
;;

(* -------------------------------------------------------------------- *)

(* Functor identity property: generating a prompt from a task should embed the
   task's Markdown verbatim in the prompt body.  We provide minimal Task and
   Prompt implementations to exercise [Meta_prompting.Make]. *)

module Simple_task = struct
  type t = string

  let to_markdown t = t
end

module Simple_prompt = struct
  type t = string

  let make ?header:_ ?footnotes:_ ?metadata:_ ~body () = body
end

module Gen = Meta_prompting.Meta_prompt.Make (Simple_task) (Simple_prompt)

let%test_unit "functor_identity" =
  Quickcheck.test String.quickcheck_generator ~f:(fun s ->
    let p = Gen.generate s in
    [%test_result: string] ~expect:s p)
;;

(* -------------------------------------------------------------------- *)
