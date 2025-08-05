open Core
open Meta_prompting

let%expect_test "offline_tie_returns_half" =
  let module J = (val Evaluator.pairwise_arena_judge : Evaluator.Pairwise_judge) in
  let score1 = J.evaluate ~incumbent:"answer A" ~challenger:"answer B" () in
  let score2 = J.evaluate ~incumbent:"answer A" ~challenger:"answer B" () in
  (* Without an API key the judge degrades to a deterministic tie. The
     win-probability for the challenger therefore equals 0.5 on every
     invocation. *)
  printf "%0.1f\n%0.1f" score1 score2;
  [%expect
    {|
0.5
0.5
|}]
;;
