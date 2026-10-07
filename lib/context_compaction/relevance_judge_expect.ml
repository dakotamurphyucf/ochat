open! Core

let%expect_test
    "relevance preserves three samples and rejects malformed or out-of-range scores"
  =
  let samples = ref [ Ok "0.9"; Ok "1.1"; Ok "0.2 extra" ] in
  let calls = ref 0 in
  let score =
    Context_compaction.Relevance_judge.For_testing.score_samples ~sample:(fun () ->
      incr calls;
      let result = List.hd_exn !samples in
      samples := List.tl_exn !samples;
      result)
  in
  printf "calls=%d score=%.3f\n" !calls score;
  [%expect {| calls=3 score=0.633 |}]
;;

let%expect_test "expected inference failures use the existing neutral relevance fallback" =
  let score =
    Context_compaction.Relevance_judge.For_testing.score_samples ~sample:(fun () ->
      Error (Outcome (Failed (Authentication Missing))))
  in
  printf "score=%.3f\n" score;
  [%expect {| score=0.500 |}]
;;

exception Observer_failed

let%expect_test "relevance does not swallow observer failure or cancellation" =
  let propagated exn =
    try
      ignore
        (Context_compaction.Relevance_judge.For_testing.score_samples ~sample:(fun () ->
           raise exn)
         : float);
      false
    with
    | Observer_failed | Eio.Cancel.Cancelled _ -> true
  in
  printf
    "observer=%b cancellation=%b\n"
    (propagated Observer_failed)
    (propagated (Eio.Cancel.Cancelled (Failure "cancelled")));
  [%expect {| observer=true cancellation=true |}]
;;
