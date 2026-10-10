open! Core
open Chatml
module T = Chatml_typechecker
module B = Chatml_builtin_spec

let check code = T.check_program (Chatml_parse.parse_program_exn code)

let accepted code =
  match check code with
  | Ok _ -> ()
  | Error diagnostic -> failwith (T.format_diagnostic code diagnostic)
;;

let rejected code =
  match check code with
  | Ok _ -> failwith "incorrect constructor arity accepted"
  | Error diagnostic -> print_endline diagnostic.message
;;

let execute code =
  let env = Chatml_builtin_modules.create_default_env () in
  match Chatml_resolver.eval_program env (Chatml_parse.parse_program_exn code) with
  | Ok () -> ()
  | Error diagnostic -> failwith (T.format_diagnostic code diagnostic)
;;

let%expect_test "nullary, unary unit and multiple arguments retain runtime arity" =
  execute
    {|
      let a = `A in
      print(match a with | `A -> "nullary")
      let b = `A(()) in
      print(match b with | `A(x) -> to_string(x))
      print(match `A(()) with | `A(()) -> "unit pattern")
      print(match `Pair((), "second") with | `Pair((), text) -> text)
      type cases = [ `Empty | `One(unit) | `Pair(unit, string) ]
      let classify value =
        let value : cases = value in
        match value with
        | `Empty -> "empty"
        | `One(()) -> "one"
        | `Pair((), text) -> text
      print(classify(`Empty))
      print(classify(`One(())))
      print(classify(`Pair((), "pair")))
    |};
  [%expect
    {|
    nullary
    ()
    unit pattern
    second
    empty
    one
    pair
    |}]
;;

let%expect_test "mismatched arity is rejected before evaluation" =
  List.iter
    [ {| match `A(()) with | `A -> 0 |}
    ; {| match `A with | `A(()) -> 0 |}
    ; {| match `Pair((), 1) with | `Pair(x) -> 0 |}
    ; {| match `A(()) with | `A((), x) -> 0 |}
    ; {| type case = [ `A ] let value : case = `A(()) |}
    ; {| type case = [ `A(unit) ] let value : case = `A |}
    ; {| if true then `A else `A(()) |}
    ]
    ~f:rejected;
  [%expect
    {|
    Variant constructor arity mismatch
    Variant constructor arity mismatch
    Variant constructor arity mismatch
    Variant constructor arity mismatch
    Variant constructor arity mismatch
    Variant constructor arity mismatch
    Variant constructor arity mismatch
    |}]
;;

let%expect_test "coverage keeps unary unit and boolean component patterns" =
  accepted {| match `A(()) with | `A(value) -> value |};
  accepted {| match `A(()) with | `A(()) -> () |};
  accepted {| match `Pair((), true) with | `Pair((), _) -> () |};
  rejected
    {| type cases = [ `Empty | `One(unit) ]
       let classify value = let value : cases = value in match value with | `Empty -> 0 |};
  rejected {| match `A(()) with | `A(()) -> 0 | `A(_) -> 1 |};
  rejected {| match `Pair((), true) with | `Pair((), true) -> 0 |};
  [%expect
    {|
    Non-exhaustive variant match: missing case '`One(_)'
    Redundant match arm '`A(...)': previous arms already cover variant case '`A(_)'
    Non-exhaustive variant match: missing case '`Pair(_, _)'
    |}]
;;

let%expect_test "recursive aliases retain component counts through inference" =
  execute
    {|
      type tree = [ `Leaf(unit) | `Node(tree, tree) ]
      let rec count tree =
        let tree : tree = tree in
        match tree with
        | `Leaf(()) -> 1
        | `Node(left, right) -> count(left) + count(right)
      print(count(`Node(`Leaf(()), `Leaf(()))))
    |};
  rejected
    {| type tree = [ `Leaf(unit) | `Node(tree, tree) ]
       let tree : tree = `Leaf |};
  [%expect
    {|
    2
    Variant constructor arity mismatch
    |}]
;;

let%expect_test "builtin Option and JSON cases preserve zero and unary unit" =
  execute
    {|
      print(match Option.none() with | `None -> "none" | `Some(value) -> "some")
      print(match Option.some(()) with | `None -> "none" | `Some(()) -> "unit")
      print(Option.get_or(Option.some(()), ()))
      print(Json.stringify(`Null))
    |};
  rejected {| match Option.some(()) with | `None -> () | `Some -> () |};
  rejected {| Json.stringify(`Null(())) |};
  [%expect
    {|
    none
    unit
    ()
    null
    Variant constructor arity mismatch
    Variant constructor arity mismatch
    |}]
;;

let case arguments =
  B.TVariant (B.TVariant_row_extend ([ "A", arguments ], B.TVariant_row_empty))
;;

let%expect_test "required entrypoints preserve builtin constructor argument lists" =
  let compile arguments source =
    match
      Chatml_host_runtime.compile_script
        ~required_bindings:[ "main", B.TFun ([ B.TUnit ], case arguments) ]
        ~source
        ()
    with
    | Ok _ -> print_endline "accepted"
    | Error message ->
      if String.is_substring message ~substring:"Variant constructor arity mismatch"
      then print_endline "arity rejected"
      else failwith message
  in
  compile [] {| let main input = `A |};
  compile [ B.TUnit ] {| let main input = `A(()) |};
  compile [ B.TUnit; B.TInt ] {| let main input = `A((), 1) |};
  compile [] {| let main input = `A(()) |};
  compile [ B.TUnit ] {| let main input = `A |};
  compile [ B.TUnit; B.TInt ] {| let main input = `A(()) |};
  [%expect
    {|
    accepted
    accepted
    accepted
    arity rejected
    arity rejected
    arity rejected
    |}]
;;

let%expect_test "rendering and internal validation preserve explicit payloads" =
  let ty arguments = T.typ_of_builtin_ty (case arguments) in
  List.iter
    [ []; [ B.TUnit ]; [ B.TUnit; B.TInt ] ]
    ~f:(fun args ->
      let ty = ty args in
      print_endline (T.show_type ty);
      [%test_eq: string]
        (T.show_type ty)
        (T.show_type_for_diagnostic (T.create_state ()) ty);
      T.check_contractive ty |> Result.ok_or_failwith);
  List.iter
    [ T.Variant_payload [ T.Unit ]
    ; T.Record (T.Row (T.Env.singleton "a" (T.Variant_payload []), T.Empty_row))
    ; T.Variant (T.Row (T.Env.singleton "A" T.Unit, T.Empty_row))
    ]
    ~f:(fun ty ->
      match T.check_contractive ty with
      | Ok () -> failwith "invalid constructor payload placement accepted"
      | Error message -> print_endline message);
  [%expect
    {|
    [`A]
    [`A(unit)]
    [`A(unit, int)]
    Variant payload is only valid in a variant row
    Variant payload is only valid in a variant row
    Variant row fields must retain constructor payload arity
    |}]
;;
