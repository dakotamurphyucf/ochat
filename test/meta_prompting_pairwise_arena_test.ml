open! Core
open Meta_prompting

let%expect_test "pairwise arena has no implicit offline model fallback" =
  let module J = (val Evaluator.pairwise_arena_judge : Evaluator.Pairwise_judge) in
  let required =
    try
      ignore (J.evaluate ~incumbent:"answer A" ~challenger:"answer B" () : float);
      false
    with
    | Evaluator.Configuration_required -> true
  in
  print_s [%sexp (required : bool)];
  [%expect {| true |}]
;;
