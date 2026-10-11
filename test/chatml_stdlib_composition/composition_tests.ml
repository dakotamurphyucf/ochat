open Core
open Chatml.Chatml_lang
module Composition = Chatml.Chatml_builtin_composition

let run source =
  let env = Chatml_builtin_modules.create_default_env () in
  match
    Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
  with
  | Error (Type_diagnostic diagnostic) -> failwith diagnostic.message
  | Error (Runtime_diagnostic diagnostic) -> failwith diagnostic.message
  | Ok () ->
    (match find_var env "result" with
     | Some (VInt value) -> print_s [%sexp (value : int)]
     | _ -> failwith "expected integer result")
;;

let%expect_test "composition selects callbacks and preserves typed payloads" =
  List.iter
    [ {|let calls = ref(0)
        let convert n = let ignored = calls := !calls + 1 in to_string(n)
        let yes = Outcome.map(`Ok(3), convert)
        let no = Outcome.map(`Error("kept"), convert)
        let a = match yes with | `Ok(s) -> String.length(s) | `Error(_) -> 0
        let b = match no with | `Error(s) -> String.length(s) | `Ok(_) -> 0
        let result = !calls * 100 + a * 10 + b|}
    ; {|let calls = ref(0)
        let step n = let ignored = calls := !calls + 1 in `Error(to_string(n))
        let first = Outcome.bind(`Ok(7), step)
        let second = Outcome.bind(first, fun n -> `Ok(n + 1))
        let result = !calls * 10 +
          (match second with | `Error(s) -> String.length(s) | `Ok(_) -> 0)|}
    ; {|let convert s = { message = s; count = String.length(s) }
        let mapped = Outcome.map_error(`Error("bad"), convert)
        let skipped = Outcome.map_error(`Ok(4), fun ignored -> fail("must not run"))
        let result =
          (match mapped with | `Error(e) -> e.count | `Ok(_) -> 0) +
          (match skipped with | `Ok(n) -> n | `Error(_) -> 0)|}
    ; {|let calls = ref(0)
        let convert n = let ignored = calls := !calls + 1 in n + 1
        let yes = Option.map(`Some(3), convert)
        let no = Option.map(`None, convert)
        let result = !calls * 100 + Option.get_or(yes, 0) * 10 + Option.get_or(no, 5)|}
    ; {|let yes = Option.bind(`Some(3), fun n -> `Some(n + 1))
        let none = Option.bind(`Some(3), fun ignored -> `None)
        let skipped = Option.bind(`None, fun ignored -> fail("must not run"))
        let result = Option.get_or(yes, 0) + Option.get_or(none, 5) + Option.get_or(skipped, 6)|}
    ; {|let order = ref(0)
        let values = [1, 2, 3]
        let kept = Array.filter_map(values, fun n ->
          let ignored = order := !order * 10 + n in
          if n == 2 then `None else `Some(n * 10))
        let ignored = values[0] <- 99
        let result = !order + kept[0] + kept[1]|}
    ; {|let result = match Json.parse_result("{\"count\":3}") with
        | `Ok(value) -> String.length(Json.stringify(value))
        | `Error(error) -> match error with | `Invalid_syntax(diagnostic) -> 0|}
    ; {|let result = match Json.parse_result("{") with
        | `Ok(_) -> 0
        | `Error(error) -> match error with | `Invalid_syntax(diagnostic) ->
          if String.length(diagnostic.message) > 0 then 1 else 0|}
    ]
    ~f:run;
  [%expect
    {|
    114
    11
    7
    145
    15
    163
    11
    1
    |}]
;;

let%expect_test "composition rejects incompatible callbacks statically" =
  List.iter
    [ "let result = Option.map(`Some(1), fun a b -> a + b)"
    ; "let result = Option.bind(`Some(1), fun n -> n + 1)"
    ; "let result = Outcome.bind(`Error(1), fun n -> `Error(\"wrong\"))"
    ; "let result = Outcome.map(`Ok(1), fun s -> String.length(s))"
    ; "let result = Array.filter_map([1], fun n -> n + 1)"
    ; "let result = Json.parse_result(3)"
    ]
    ~f:(fun source ->
      match
        Chatml_typechecker.check_program (Chatml.Chatml_parse.parse_program_exn source)
      with
      | Ok _ -> failwith "invalid callback accepted"
      | Error diagnostic -> [%test_eq: bool] true (Option.is_some diagnostic.span));
  print_endline "six invalid programs rejected with source spans";
  [%expect {| six invalid programs rejected with source spans |}]
;;

let%expect_test "callback failures remain failures and stop traversal" =
  let calls = ref 0 in
  let fail =
    VBuiltin
      (fun _ ->
        incr calls;
        failwith "callback failed")
  in
  List.iter
    [ (fun () -> Composition.Outcome.map (VVariant ("Ok", [ VInt 1 ])) ~f:fail)
    ; (fun () -> Composition.Outcome.bind (VVariant ("Ok", [ VInt 1 ])) ~f:fail)
    ; (fun () -> Composition.Outcome.map_error (VVariant ("Error", [ VInt 1 ])) ~f:fail)
    ; (fun () -> Composition.Option.map (VVariant ("Some", [ VInt 1 ])) ~f:fail)
    ; (fun () -> Composition.Option.bind (VVariant ("Some", [ VInt 1 ])) ~f:fail)
    ; (fun () -> VArray (Composition.filter_map [| VInt 1; VInt 2 |] ~f:fail))
    ]
    ~f:(fun f ->
      match f () with
      | _ -> failwith "callback failure was swallowed"
      | exception Failure message -> [%test_eq: string] "callback failed" message);
  print_s [%sexp (!calls : int)];
  [%expect {| 6 |}]
;;

let%expect_test "malformed runtime variants and callback results are rejected" =
  let invalid = VBuiltin (fun _ -> VInt 3) in
  List.iter
    [ (fun () -> Composition.Option.map (VVariant ("None", [ VInt 1 ])) ~f:invalid)
    ; (fun () -> Composition.Option.bind (VVariant ("Some", [ VInt 1 ])) ~f:invalid)
    ; (fun () -> Composition.Outcome.map (VVariant ("Ok", [])) ~f:invalid)
    ; (fun () -> Composition.Outcome.bind (VVariant ("Ok", [ VInt 1 ])) ~f:invalid)
    ; (fun () -> VArray (Composition.filter_map [| VInt 1 |] ~f:invalid))
    ]
    ~f:(fun f ->
      match f () with
      | _ -> failwith "malformed runtime value accepted"
      | exception Failure message -> print_endline message);
  [%expect
    {|
    Option.map: expected `None or `Some(_)
    Option.bind: expected `None or `Some(_)
    Outcome.map: expected `Ok(_) or `Error(_)
    Outcome.bind: expected `Ok(_) or `Error(_)
    Array.filter_map: expected `None or `Some(_)
    |}]
;;

let%expect_test "cancellation is not translated into an optional or error result" =
  let cancel = VBuiltin (fun _ -> raise (Eio.Cancel.Cancelled Exit)) in
  List.iter
    [ (fun () -> Composition.Option.bind (VVariant ("Some", [ VInt 1 ])) ~f:cancel)
    ; (fun () -> Composition.Outcome.bind (VVariant ("Ok", [ VInt 1 ])) ~f:cancel)
    ; (fun () -> VArray (Composition.filter_map [| VInt 1; VInt 2 |] ~f:cancel))
    ]
    ~f:(fun f ->
      match f () with
      | _ -> failwith "cancellation was swallowed"
      | exception Eio.Cancel.Cancelled Exit -> ());
  print_endline "cancellation propagated";
  [%expect {| cancellation propagated |}]
;;

let%expect_test "filter_map agrees with a bounded ordered reference model" =
  let callback =
    VBuiltin
      (function
        | [ VInt n ] ->
          if n % 3 = 0 then VVariant ("Some", [ VInt (n * n) ]) else VVariant ("None", [])
        | _ -> failwith "expected integer")
  in
  for length = 0 to 128 do
    let input = Array.init length ~f:(fun i -> VInt (i - 64)) in
    let expected =
      List.range (-64) (length - 64)
      |> List.fold_right ~init:[] ~f:(fun n acc ->
        if n % 3 = 0 then (n * n) :: acc else acc)
    in
    let actual =
      Composition.filter_map input ~f:callback
      |> Array.to_list
      |> List.map ~f:(function
        | VInt n -> n
        | _ -> failwith "expected integer")
    in
    [%test_eq: int list] expected actual
  done;
  print_endline "129 ordered cases passed, including empty input";
  [%expect {| 129 ordered cases passed, including empty input |}]
;;

let%expect_test "filter_map has fresh outer storage, shared payloads and live input" =
  let shared = ref (VInt 1) in
  let input = [| VRef shared; VInt 2 |] in
  let calls = ref 0 in
  let callback =
    VBuiltin
      (function
        | [ value ] ->
          incr calls;
          input.(1) <- VInt 9;
          VVariant ("Some", [ value ])
        | _ -> failwith "expected one argument")
  in
  let output = Composition.filter_map input ~f:callback in
  [%test_eq: bool] false (phys_equal input output);
  input.(0) <- VInt 7;
  shared := VInt 5;
  let observed =
    Array.map output ~f:(function
      | VRef { contents = VInt n } | VInt n -> n
      | _ -> failwith "unexpected value")
  in
  print_s [%sexp (observed : int array), (!calls : int)];
  [%expect {| ((5 9) 2) |}]
;;
