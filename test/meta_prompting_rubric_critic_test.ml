open Core
open Meta_prompting

(* We explicitly avoid providing an Eio environment and make sure the
   [Rubric_critic_judge] falls back to the deterministic 0.5 score
   when the OpenAI API key is absent. *)

let%expect_test "rubric_critic offline fallback" =
  let module J = (val Evaluator.rubric_critic_judge : Evaluator.Judge) in
  let score = J.evaluate "Some arbitrary answer." in
  printf "%.1f" score;
  [%expect "0.5"]
;;
