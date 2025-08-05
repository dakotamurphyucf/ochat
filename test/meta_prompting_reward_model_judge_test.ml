open Core
open Meta_prompting

(* Ensure that the [Reward_model_judge] falls back to a deterministic
   0.5 score when no OpenAI API key is available (offline test
   scenario). *)

let%expect_test "reward_model_judge offline fallback" =
  let module J = (val Evaluator.prompt_reward_model_judge : Evaluator.Judge) in
  let score = J.evaluate "Arbitrary answer for testing." in
  printf "%.1f" score;
  [%expect "0.5"]
;;
