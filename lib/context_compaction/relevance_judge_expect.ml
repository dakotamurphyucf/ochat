open! Core

let prompt = "Unit test prompt for relevance scoring"

let%expect_test "relevance judge – default config" =
  let cfg = Context_compaction.Config.default in
  let score = Context_compaction.Relevance_judge.score_relevance cfg ~prompt in
  let relevant = Context_compaction.Relevance_judge.is_relevant cfg ~prompt in
  printf !"Score: %.3f  Relevant: %b\n" score relevant;
  [%expect "Score: 0.500  Relevant: true"]
;;

let%expect_test "relevance judge – high threshold" =
  let cfg = { Context_compaction.Config.default with relevance_threshold = 0.8 } in
  let score = Context_compaction.Relevance_judge.score_relevance cfg ~prompt in
  let relevant = Context_compaction.Relevance_judge.is_relevant cfg ~prompt in
  printf !"Score: %.3f  Relevant: %b\n" score relevant;
  [%expect "Score: 0.500  Relevant: false"]
;;
