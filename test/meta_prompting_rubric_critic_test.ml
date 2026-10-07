open! Core
open Meta_prompting

let%expect_test "requested rubric critic requires selected inference" =
  let module J = (val Evaluator.rubric_critic_judge : Evaluator.Judge) in
  let required =
    try
      ignore (J.evaluate "Some arbitrary answer." : float);
      false
    with
    | Evaluator.Configuration_required -> true
  in
  print_s [%sexp (required : bool)];
  [%expect {| true |}]
;;
