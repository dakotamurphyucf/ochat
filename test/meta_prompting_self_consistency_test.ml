open Core
open Meta_prompting

module Cancelled_judge : Evaluator.Judge = struct
  let name = "cancelled"
  let evaluate ?env:_ _ = raise (Eio.Cancel.Cancelled Exit)
end

let%expect_test "self consistency and evaluator preserve cancellation" =
  let judge =
    Evaluator.wrap_self_consistency_judge
      ~k:3
      ~strategy:Evaluator.Mean
      (module Cancelled_judge)
  in
  let evaluator = Evaluator.create ~judges:[ Evaluator.Judge judge ] () in
  let cancelled =
    try
      ignore (Evaluator.evaluate evaluator "cancel" : float);
      false
    with
    | Eio.Cancel.Cancelled _ -> true
  in
  print_s [%sexp (cancelled : bool)];
  [%expect {| true |}]
;;

(** A deterministic pseudo-random judge that alternates between a high and
    low score on subsequent calls.  This allows us to simulate stochastic
    LLM behaviour in a reproducible fashion. *)

module Flippy_judge : Evaluator.Judge = struct
  let name = "flippy"
  let counter = ref 0

  let evaluate ?env:_ _candidate =
    incr counter;
    if !counter mod 2 = 1 then 0.9 else 0.1
  ;;
end

let%expect_test "self_consistency_majority" =
  let sc_judge =
    Evaluator.wrap_self_consistency_judge
      ~k:5
      ~strategy:Evaluator.Majority
      (module Flippy_judge)
  in
  let ev = Evaluator.create ~judges:[ Evaluator.Judge sc_judge ] () in
  let score = Evaluator.evaluate ev "dummy" in
  printf "%.1f" score;
  [%expect {|1.0|}]
;;

let%expect_test "self_consistency_mean" =
  let sc_judge =
    Evaluator.wrap_self_consistency_judge
      ~k:5
      ~strategy:Evaluator.Mean
      (module Flippy_judge)
  in
  let ev = Evaluator.create ~judges:[ Evaluator.Judge sc_judge ] () in
  let score = Evaluator.evaluate ev "dummy" in
  (* Expected mean of scores given the continuing counter state: pattern
     0.1,0.9,0.1,0.9,0.1 → 0.42. *)
  printf "%.2f" score;
  [%expect {|0.42|}]
;;
