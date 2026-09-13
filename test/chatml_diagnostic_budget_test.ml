open Core
module T = Chatml_typechecker

let dag depth =
  let rec loop remaining ty =
    match remaining with
    | 0 -> ty
    | _ -> loop (remaining - 1) (T.Con ("pair", [ ty; ty ]))
  in
  loop depth T.TInt
;;

let%expect_test "small shared type graphs demonstrate exponential full display" =
  List.iter [ 4; 8; 12 ] ~f:(fun depth ->
    print_s [%sexp (depth : int), (String.length (T.show_type (dag depth)) : int)]);
  [%expect
    {|
    (4 168)
    (8 2808)
    (12 45048)
    |}]
;;

let%expect_test "real unification errors bound graph expansion, depth and names" =
  let deep_row =
    List.fold (List.init 10_000 ~f:Fn.id) ~init:T.Empty_row ~f:(fun row i ->
      T.Row (T.Env.singleton (Int.to_string i) T.TInt, row))
  in
  let wide_row =
    T.Row
      (T.Env.of_list (List.init 10_000 ~f:(fun i -> Int.to_string i, T.TInt)), T.Empty_row)
  in
  List.iter
    [ "shared", dag 80
    ; "deep-row", T.Record deep_row
    ; "wide-row", T.Record wide_row
    ; "large-name", T.Con (String.make 100_000 'x', [])
    ; ( "recursive-row-expansion"
      , T.Record (T.Mu ("r", T.Row (T.Env.singleton "value" (dag 80), T.Rec_var "r"))) )
    ]
    ~f:(fun (name, ty) ->
      match T.unify (T.create_state ()) ty T.Boolean with
      | () -> failwith "unexpected unification success"
      | exception T.Type_error message ->
        print_s
          [%sexp
            (name : string)
          , (String.is_prefix message ~prefix:"Cannot unify " : bool)
          , (String.is_suffix message ~suffix:"... with bool" : bool)
          , (String.length message < 16_384 : bool)]);
  [%expect
    {|
    (shared true true true)
    (deep-row true true true)
    (wide-row true true true)
    (large-name true true true)
    (recursive-row-expansion true true true)
    |}]
;;

let%expect_test "diagnostic traversal preserves caller cancellation" =
  let exception Cancelled_by_host in
  let polls = ref 0 in
  let state =
    T.create_state
      ~checkpoint:(fun () ->
        Int.incr polls;
        if !polls = 2 then raise Cancelled_by_host)
      ()
  in
  (match T.unify state (dag 80) T.Boolean with
   | () -> failwith "unexpected unification success"
   | exception Cancelled_by_host -> print_endline "caller cancellation propagated");
  [%expect {| caller cancellation propagated |}]
;;

let%expect_test "unification cancellation reaches occurs checks and recursive unfolding" =
  let exception Cancelled_by_host in
  let row =
    List.fold (List.init 1000 ~f:Fn.id) ~init:T.Empty_row ~f:(fun rest i ->
      T.Row (T.Env.singleton (Int.to_string i) T.TInt, rest))
  in
  List.iter
    [ "occurs", T.Var (ref (T.Free ("fresh", 0))), dag 80
    ; "contractiveness", T.Mu ("r", dag 80), T.Boolean
    ; "row-merge", row, T.Row (T.Env.singleton "0" T.TInt, T.Empty_row)
    ]
    ~f:(fun (name, lhs, rhs) ->
      let polls = ref 0 in
      let state =
        T.create_state
          ~checkpoint:(fun () ->
            Int.incr polls;
            if !polls = 2 then raise Cancelled_by_host)
          ()
      in
      match T.unify state lhs rhs with
      | () -> failwith "unification finished without polling"
      | exception Cancelled_by_host -> print_endline (name ^ ": cancelled"));
  [%expect
    {|
    occurs: cancelled
    contractiveness: cancelled
    row-merge: cancelled
    |}]
;;

let%expect_test "ordinary structured and recursive diagnostics retain their display" =
  let row =
    T.Row (T.Env.of_list [ "value", T.TInt; "next", T.Rec_var "node" ], T.Empty_row)
  in
  let cyclic = ref (T.Free ("a", 0)) in
  cyclic
  := T.Bound (T.Record (T.Row (T.Env.singleton "next" (T.Var cyclic), T.Empty_row)));
  let cases =
    [ T.Mu ("node", T.Record row)
    ; T.Var cyclic
    ; T.Array (T.Fun ([ T.String ], T.Con ("task", [ T.Boolean ])))
    ; T.Variant
        (T.Row
           ( T.Env.of_list [ "None", T.Unit; "Pair", T.Tuple [ T.TInt; T.String ] ]
           , T.Generic "rest" ))
    ; T.Record
        (T.Row
           (T.Env.singleton "a" T.TInt, T.Row (T.Env.singleton "a" T.String, T.Empty_row)))
    ]
  in
  List.iter cases ~f:(fun ty ->
    let full = T.show_type ty in
    [%test_eq: string] full (T.show_type_for_diagnostic (T.create_state ()) ty);
    print_endline full);
  [%expect
    {|
    mu node. {next: node; value: int}
    {next: 'rec}
    ((string -> bool task)) array
    [`None | `Pair(int, string) | ...]
    {a: int}
    |}]
;;

let%expect_test
    "compiler retains source location and bounded type errors without initialization"
  =
  let definitions =
    List.init 10 ~f:(fun i ->
      Printf.sprintf "let value%i = { left = value%i; right = value%i }" (i + 1) i i)
  in
  let source =
    String.concat
      ~sep:"\n"
      ([ "let poison = fail(\"must not initialize\")"; "let value0 = 0" ]
       @ definitions
       @ [ "let main input = value10 + true" ])
  in
  Eio_main.run (fun env ->
    match Chatml_compilation.compile ~env ~target:One_off_v1 ~source () with
    | Ok _ -> failwith "invalid program compiled"
    | Error error ->
      let diagnostic = Option.value_exn error.diagnostic in
      let span = Option.value_exn diagnostic.span in
      print_s
        [%sexp
          (error.code : string)
        , (diagnostic.stage : Chatml_host_runtime.compilation_stage)
        , (span.left.line : int)
        , (String.is_substring diagnostic.message ~substring:"..." : bool)
        , (String.length error.message <= 16_384 : bool)]);
  [%expect {| (chatml.invalid_handler Typecheck 13 true true) |}]
;;
