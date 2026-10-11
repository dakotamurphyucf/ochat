open Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

let check source = T.check_program (Chatml.Chatml_parse.parse_program_exn source)

let%test_unit "exact lambda constraints survive generalization and containers" =
  List.iter
    [ {|let f r = match r with | { x = x } -> x
        let result = f({ x = 1; extra = true })|}
    ; {|let f r = let exact : { x : int } = r in exact.x
        let result = f({ x = 1; extra = true })|}
    ; {|let f r = match r.inner with | { x = x } -> x
        let result = f({ inner = { x = 1; extra = true }; outer = 2 })|}
    ; {|let f r = match !r with | { x = x } -> x
        let result = f(ref({ x = 1; extra = true }))|}
    ; {|let f r = match r[0] with | { x = x } -> x
        let result = f([{ x = 1; extra = true }])|}
    ; {|let make () = fun r -> match r with | { x = x } -> x
        let f = make()
        let result = f({ x = 1; extra = true })|}
    ]
    ~f:(fun source ->
      match check source with
      | Ok _ -> failwith "exact record constraint was lost"
      | Error diagnostic ->
        [%test_eq: bool] true (Option.is_some diagnostic.span);
        [%test_eq: string] "Row does not contain label 'extra'" diagnostic.message)
;;

let run source =
  let env = Chatml_builtin_modules.create_default_env () in
  match
    Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
  with
  | Error (Type_diagnostic diagnostic) -> failwith diagnostic.message
  | Error (Runtime_diagnostic diagnostic) -> failwith diagnostic.message
  | Ok () ->
    (match L.find_var env "result" with
     | Some (VInt value) -> print_s [%sexp (value : int)]
     | _ -> failwith "unexpected result")
;;

let%expect_test "ordinary inferred record helpers retain open rows and result sharing" =
  List.iter
    [ {|let f r = r.x
        let result = f({ x = 1 }) + f({ x = 2; extra = true })|}
    ; {|let f r = match r with | { x = x; _ } -> x
        let result = f({ x = 3; extra = true })|}
    ; {|let f r = match r.inner with | { x = x; _ } -> x
        let result = f({ inner = { x = 3; extra = true }; outer = 2 })|}
    ; {|let f r = { r with x = r.x + 1 }
        let result = f({ x = 2; extra = 5 }).extra|}
    ; {|let f r = { r with added = 3 }
        let result = f({ extra = 5 }).extra|}
    ; {|let f r = let ignored = r.x in r
        let result = f({ x = 2; extra = 5 }).extra|}
    ; {|let f r = (!r).x
        let result = f(ref({ x = 3; extra = true }))|}
    ; {|let f r = r[0].x
        let result = f([{ x = 3; extra = true }])|}
    ; {|let make () = fun r -> r.x
        let result = make()({ x = 3; extra = true })|}
    ]
    ~f:run;
  [%expect
    {|
    3
    3
    3
    5
    5
    5
    3
    3
    3
    |}]
;;

let%expect_test "conditional overrides constrain shared fields without erasing extras" =
  List.iter
    [ {|let maybe flag state = if flag then { state with enabled = true } else state
        let a = maybe(true, { enabled = false; count = 3 })
        let b = maybe(false, { enabled = false; count = 4; extra = "kept" })
        let result = a.count + b.count + (if a.enabled then 10 else 0)|}
    ; {|let maybe tag state = match tag with
          | `Keep -> state
          | `Enable -> { state with enabled = true }
        let result = maybe(`Keep, { enabled = false; count = 7 }).count|}
    ; {|let choose flag state =
          if flag then { state with enabled = true } else { state with enabled = false }
        let result = choose(true, { count = 7 }).count|}
    ; {|let maybe flag state =
          if flag then { state with enabled = true } else { state with count = 4 }
        let result = maybe(false, { enabled = false; count = 3; extra = 7 }).extra|}
    ]
    ~f:run;
  [%expect
    {|
    17
    7
    7
    7
    |}]
;;

let%expect_test "a conditional override rejects a missing or incompatible base field" =
  List.iter
    [ {|let maybe flag state = if flag then { state with enabled = true } else state
        let result = maybe(false, { enabled = 3 })|}
    ; {|let maybe flag state = if flag then { state with enabled = true } else state
        let result = maybe(true, { count = 3 })|}
    ; {|let maybe tag state = match tag with
          | `Keep -> state
          | `Enable -> { state with enabled = true }
        let result = maybe(`Enable, { enabled = 3 })|}
    ]
    ~f:(fun source ->
      match check source with
      | Ok _ -> failwith "unsafe conditional override accepted"
      | Error diagnostic ->
        [%test_eq: bool] true (Option.is_some diagnostic.span);
        print_endline diagnostic.message);
  [%expect
    {|
    Cannot unify bool with int
    Row does not contain label 'enabled'
    Cannot unify bool with int
    |}]
;;

let%expect_test
    "standalone copies infer a new type independently of the original annotation"
  =
  run
    {|let original : { enabled : int; count : int } = { enabled = 3; count = 7 }
      let updated = { original with enabled = true; extra = 4 }
      let result = original.enabled + updated.count + updated.extra +
        (if updated.enabled then 10 else 0)|};
  [%expect {| 24 |}]
;;

let%expect_test "explicit copy result annotations reject changed types and added fields" =
  List.iter
    [ {|let original = { enabled = 3 }
        let updated : { enabled : int } = { original with enabled = true }|}
    ; {|let original = { count = 3 }
        let updated : { count : int } = { original with enabled = true }|}
    ]
    ~f:(fun source ->
      match check source with
      | Ok _ -> failwith "incompatible result annotation accepted"
      | Error diagnostic -> print_endline diagnostic.message);
  [%expect
    {|
    Cannot unify bool with int
    Row does not contain label 'enabled'
    |}]
;;

let%expect_test
    "copy updates create a new type; conditional updates preserve the input shape"
  =
  let source =
    {|let original : {enabled : int; count : int} = {enabled = 3; count = 7}
      let copy = {original with enabled = true; note = "new"}
      let maybe_enable flag state = if flag then {state with enabled = true} else state
      let skipped = maybe_enable(false, {enabled = false; count = 7; note = "kept"})
      let enabled = maybe_enable(true, skipped)|}
  in
  let env = Chatml_builtin_modules.create_default_env () in
  (match
     Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
   with
   | Error _ -> failwith "documented example failed"
   | Ok () -> ());
  List.iter [ "original"; "copy"; "skipped"; "enabled" ] ~f:(fun name ->
    let value = L.find_var env name |> Option.value_exn in
    print_endline (name ^ " = " ^ Chatml.Chatml_builtin_spec.value_to_string value));
  [%expect
    {|
    original = { count = 7; enabled = 3 }
    copy = { count = 7; enabled = true; note = new }
    skipped = { count = 7; enabled = false; note = kept }
    enabled = { count = 7; enabled = true; note = kept }
    |}]
;;
