open! Core
open Meta_prompting

let%expect_test "requested reward judge requires explicit selected inference" =
  let module J = (val Evaluator.prompt_reward_model_judge : Evaluator.Judge) in
  let required =
    try
      ignore (J.evaluate "Arbitrary answer for testing." : float);
      false
    with
    | Evaluator.Configuration_required -> true
  in
  assert required;
  let evaluator = Evaluator.create ~judges:[ Judge (module J) ] () in
  let required =
    try
      ignore (Evaluator.evaluate evaluator "candidate" : float);
      false
    with
    | Evaluator.Configuration_required -> true
  in
  assert required;
  (* The ordinary explicit offline/default heuristic still has its former value. *)
  printf
    "selected inference required; offline heuristic %.1f"
    (Evaluator.evaluate_default "offline");
  [%expect {| selected inference required; offline heuristic 0.5 |}]
;;

let%expect_test "score parser requires a complete finite bounded number" =
  List.iter
    [ "0.5"
    ; "0"
    ; "1"
    ; " 1e-1 "
    ; "nan"
    ; "Infinity"
    ; "-0.1"
    ; "1.1"
    ; "0.5 followed by text"
    ; "\"0.5\""
    ; "1e999"
    ; "01"
    ; "1."
    ; "1e"
    ; ".5"
    ]
    ~f:(fun text ->
      print_s
        [%sexp (text : string), (Evaluator.Score.of_string text ~max:1. : float option)]);
  [%expect
    {|
    (0.5 (0.5))
    (0 (0))
    (1 (1))
    (" 1e-1 " (0.1))
    (nan ())
    (Infinity ())
    (-0.1 ())
    (1.1 ())
    ("0.5 followed by text" ())
    ("\"0.5\"" ())
    (1e999 ())
    (01 ())
    (1. ())
    (1e ())
    (.5 ())
  |}]
;;
