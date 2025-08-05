open! Core

let%expect_test "default config" =
  let cfg = Context_compaction.Config.default in
  print_s
    [%sexp
      { context_limit : int = cfg.context_limit
      ; relevance_threshold : float = cfg.relevance_threshold
      }];
  [%expect {| ((context_limit 20000) (relevance_threshold 0.5)) |}]
;;
