open! Core
module L = Chatml.Chatml_lang

let run source =
  let env = L.create_env () in
  Chatml_builtin_modules.BuiltinModules.add_global_builtins env;
  let program = Chatml.Chatml_parse.parse_program_exn source in
  match Chatml_resolver.run_program env program with
  | Ok () -> L.find_var env "result" |> Option.value_exn
  | Error (Type_diagnostic error) -> failwith ("type: " ^ error.message)
  | Error (Runtime_diagnostic error) -> failwith ("runtime: " ^ error.message)
;;

let integers = function
  | L.VArray values ->
    Array.to_list values
    |> List.map ~f:(function
      | L.VInt value -> value
      | _ -> failwith "expected integer element")
  | _ -> failwith "expected array"
;;

let string = function
  | L.VString value -> value
  | _ -> failwith "expected string"
;;

let reject source =
  let program = Chatml.Chatml_parse.parse_program_exn source in
  match Chatml_typechecker.check_program program with
  | Error _ -> ()
  | Ok _ -> failwith ("unexpectedly accepted: " ^ source)
;;

let array_source values =
  "[" ^ String.concat ~sep:"," (List.map values ~f:Int.to_string) ^ "]"
;;

let%expect_test "overflowing slice lengths remain language runtime errors" =
  List.iter [ "String.slice(\"x\", 1, "; "Array.sub([1], 1, " ] ~f:(fun call ->
    let source = "let result = " ^ call ^ Int.to_string Int.max_value ^ ")" in
    let env = L.create_env () in
    Chatml_builtin_modules.BuiltinModules.add_global_builtins env;
    match
      Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
    with
    | Error (Runtime_diagnostic error) ->
      let span = Option.value_exn error.span in
      [%test_eq: int] 1 span.left.line;
      print_endline error.message
    | Error (Type_diagnostic error) -> failwith error.message
    | Ok () -> failwith "overflowing range accepted");
  [%expect
    {|
    String.slice: start+len out of bounds
    Array.sub: start+len out of bounds
    |}]
;;

let%expect_test "finite array reference model covers copying mutation and windows" =
  let cases = ref 0 in
  List.iter (List.range 0 9) ~f:(fun count ->
    let values = List.init count ~f:(fun i -> (i * 7) - 11) in
    let literal = array_source values in
    [%test_eq: int list]
      (List.rev values)
      (run ("let result = Array.reverse(" ^ literal ^ ")") |> integers);
    [%test_eq: int list]
      values
      (run
         ("let a = "
          ^ literal
          ^ "\nlet b = Array.copy(a)\nArray.reverse_in_place(b)\nlet result = a")
       |> integers);
    [%test_eq: int list]
      (List.rev values)
      (run ("let a = " ^ literal ^ "\nArray.reverse_in_place(a)\nlet result = a")
       |> integers);
    List.iter
      (List.range 0 (count + 1))
      ~f:(fun start ->
        List.iter
          (List.range 0 (count - start + 1))
          ~f:(fun len ->
            let expected = List.take (List.drop values start) len in
            [%test_eq: int list]
              expected
              (run (sprintf "let result = Array.sub(%s, %d, %d)" literal start len)
               |> integers);
            Int.incr cases));
    List.iter (List.range 0 count) ~f:(fun index ->
      let expected =
        List.mapi values ~f:(fun i value -> if i = index then 100 else value)
      in
      [%test_eq: int list]
        expected
        (run (sprintf "let a = %s\nArray.set(a, %d, 100)\nlet result = a" literal index)
         |> integers)));
  print_s [%sexp (!cases : int)];
  [%expect {| 165 |}]
;;

(* A byte scanner is independent of the runtime's substring/search helpers. *)
let byte_window text start len =
  String.to_list text
  |> List.filteri ~f:(fun index _ -> index >= start && index < start + len)
  |> String.of_char_list
;;

let find_bytes text pattern =
  List.find
    (List.range 0 (String.length text + 1))
    ~f:(fun start ->
      start + String.length pattern <= String.length text
      && String.equal (byte_window text start (String.length pattern)) pattern)
;;

let%expect_test
    "finite string byte windows and first-match search have independent oracle"
  =
  let cases = ref 0 in
  List.iter [ ""; "a"; "aba"; "aabaaba"; "\195\169x" ] ~f:(fun text ->
    let literal = "\"" ^ text ^ "\"" in
    List.iter
      (List.range 0 (String.length text + 1))
      ~f:(fun start ->
        List.iter
          (List.range 0 (String.length text - start + 1))
          ~f:(fun len ->
            [%test_eq: string]
              (byte_window text start len)
              (run (sprintf "let result = String.slice(%s, %d, %d)" literal start len)
               |> string);
            Int.incr cases));
    List.iter [ ""; "a"; "ab"; "ba"; "z" ] ~f:(fun pattern ->
      let actual = run (sprintf "let result = String.find(%s, %S)" literal pattern) in
      let actual =
        match actual with
        | L.VVariant ("None", []) -> None
        | L.VVariant ("Some", [ L.VInt index ]) -> Some index
        | _ -> failwith "invalid find result"
      in
      [%test_eq: int option] (find_bytes text pattern) actual));
  print_s [%sexp (!cases : int)];
  [%expect {| 60 |}]
;;

let%expect_test "builtin schemes reject wrong arguments and mutable polymorphism" =
  List.iter
    [ "let result = String.slice(1, 0, 0)"
    ; "let result = String.find(\"x\", false)"
    ; "let result = Array.get([1], true)"
    ; "let a = [1]\nlet result = Array.set(a, 0, false)"
    ; "let result = Array.append([1], [false])"
    ; "let a = [fun x -> x]\nlet n = a[0](1)\nlet result = a[0](false)"
    ; "let r = ref(fun x -> x)\nlet n = (!r)(1)\nlet result = (!r)(false)"
    ; "let rec value = 1\nlet result = value"
    ; "let rec f x = g(x) and g x = x + 1\nlet result = f(false)"
    ; "let consume r = r.count + 1\nlet result = consume({count=false})"
    ]
    ~f:reject;
  print_endline "10 negative contracts rejected before evaluation";
  [%expect {| 10 negative contracts rejected before evaluation |}]
;;

let%expect_test "recursive groups rows and lexical mutation preserve independent results" =
  let cases =
    [ ( "let rec even n = if n == 0 then true else odd(n - 1)\n\
         and odd n = if n == 0 then false else even(n - 1)\n\
         let result = if even(14) then 1 else 0"
      , 1 )
    ; ( "let consume r = r.count + 1\n\
         let a = consume({count=2; label=\"a\"})\n\
         let result = a + consume({count=4; enabled=true})"
      , 8 )
    ; ( "let r = ref(1)\nlet read () = !r\nlet r = ref(100)\nr := 101\nlet result = read()"
      , 1 )
    ; ( "let make initial = let cell = ref(initial) in\n\
         fun delta -> cell := !cell + delta; !cell\n\
         let first = make(1)\n\
         let second = make(10)\n\
         let a = first(2)\n\
         let b = second(5)\n\
         let result = a + b + first(4)"
      , 25 )
    ]
  in
  List.iter cases ~f:(fun (source, expected) ->
    match run source with
    | L.VInt actual -> [%test_eq: int] expected actual
    | _ -> failwith "expected integer");
  print_endline "recursive and lexical reference cases agree";
  [%expect {| recursive and lexical reference cases agree |}]
;;

let%expect_test
    "concurrent compiler domains preserve local aliases runtime state and spans"
  =
  Eio_main.run (fun env ->
    let module C = Chatml_compilation in
    let module X = Chatml_execution in
    let module R = Chatml_host_runtime in
    let sources =
      [ ( "type local = int\n\
           let cell = ref(0)\n\
           let helper : local -> local = fun x -> cell := !cell + x; !cell\n\
           let main input = Task.pure(`Bool(helper(3) == 3))"
        , `True )
      ; ( "type local = string\n\
           let helper : local -> local = fun x -> x ++ \"!\"\n\
           let main input = Task.pure(`String(helper(\"ok\")))"
        , `String "ok!" )
      ]
    in
    let execute source expected =
      let program =
        C.compile ~env ~target:One_off_v1 ~source ()
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let config : R.runtime_config =
        { surface = Chatml.Chatml_extension_surface.one_off_v1
        ; operations = R.default_operations ()
        }
      in
      (* Reusing one compiled program must still instantiate fresh mutable state. *)
      List.iter [ 0; 1 ] ~f:(fun _ ->
        let actual =
          X.run
            ~env
            ~config
            ~program
            ~entrypoint:"main"
            ~arguments:[ L.VVariant ("Null", []) ]
            ()
          |> Result.map_error ~f:(fun error -> error.X.message)
          |> Result.ok_or_failwith
          |> Chatml.Chatml_value_codec.value_to_jsonaf_result
          |> Result.ok_or_failwith
        in
        assert (Jsonaf.exactly_equal expected actual))
    in
    List.iter sources ~f:(fun (source, expected) -> execute source expected);
    Eio.Fiber.all
      (List.init 12 ~f:(fun index () ->
         if index mod 3 = 2
         then (
           let source = "type local = bool\nlet main input =\n  missing(input)" in
           match C.compile ~env ~target:One_off_v1 ~source () with
           | Ok _ -> failwith "unknown binding accepted"
           | Error error ->
             let diagnostic = Option.value_exn error.diagnostic in
             let span = Option.value_exn diagnostic.span in
             [%test_eq: int] 3 span.left.line;
             [%test_eq: int] 2 span.left.column)
         else (
           let source, expected = List.nth_exn sources (index mod 3) in
           execute source expected)));
    print_endline
      "serial/concurrent alias contracts and fresh state agree; errors anchored");
  [%expect {| serial/concurrent alias contracts and fresh state agree; errors anchored |}]
;;

let%expect_test "higher order builtins agree with list oracle and preserve callback order"
  =
  List.iter (List.range 0 9) ~f:(fun count ->
    let values = List.init count ~f:(fun index -> index - 4) in
    let source = array_source values in
    [%test_eq: int list]
      (List.map values ~f:(fun value -> value * value))
      (run (sprintf "let result = Array.map(%s, fun x -> x * x)" source) |> integers);
    [%test_eq: int list]
      (List.filter values ~f:(fun value -> value >= 0))
      (run (sprintf "let result = Array.filter(%s, fun x -> x >= 0)" source) |> integers);
    let expected = List.fold values ~init:17 ~f:(fun acc value -> (acc * 3) + value) in
    (match
       run (sprintf "let result = Array.fold(%s, 17, fun acc x -> acc * 3 + x)" source)
     with
     | L.VInt actual -> [%test_eq: int] expected actual
     | _ -> failwith "expected integer");
    (* A callback's writes expose both traversal order and its exact call count. *)
    let expected = List.mapi values ~f:(fun index value -> value + index + 1) in
    [%test_eq: int list]
      expected
      (run
         (sprintf
            "let calls = ref(0)\n\
             let result = Array.map(%s, fun x -> calls := !calls + 1; x + !calls)"
            source)
       |> integers));
  print_endline "36 reference comparisons passed";
  [%expect {| 36 reference comparisons passed |}]
;;

let%expect_test "failed builtin mutations are atomic and retain call-site diagnostics" =
  List.iter
    [ "Array.set(a, 2, 9)"; "Array.swap(a, 0, 2)"; "Array.set(a, -1, 9)" ]
    ~f:(fun operation ->
      let source = "let a = [1, 2]\n" ^ operation in
      let env = L.create_env () in
      Chatml_builtin_modules.BuiltinModules.add_global_builtins env;
      let program = Chatml.Chatml_parse.parse_program_exn source in
      (match Chatml_resolver.run_program env program with
       | Error (Runtime_diagnostic error) ->
         let span = Option.value_exn error.span in
         [%test_eq: int] 2 span.left.line
       | Error (Type_diagnostic error) -> failwith error.message
       | Ok () -> failwith "invalid index accepted");
      [%test_eq: int list] [ 1; 2 ] (L.find_var env "a" |> Option.value_exn |> integers));
  print_endline "3 invalid mutations preserve original array and line 2";
  [%expect {| 3 invalid mutations preserve original array and line 2 |}]
;;

let%expect_test
    "simultaneous binders reject duplicates while sequential rebinding remains"
  =
  List.iter
    [ "lambda", "let f x x = x\nlet result = f(1, true)", 1
    ; ( "annotated lambda"
      , "let f : int -> int -> int = fun x x -> x\nlet result = f(1, 2)"
      , 1 )
    ; "recursive group", "let rec f x = 1\nand f x = 2\nlet result = f(0)", 2
    ; "local recursive group", "let result = let rec f x = 1\nand f x = 2 in f(0)", 2
    ]
    ~f:(fun (name, source, line) ->
      match
        Chatml_typechecker.check_program (Chatml.Chatml_parse.parse_program_exn source)
      with
      | Ok _ -> failwith ("duplicate accepted: " ^ name)
      | Error diagnostic ->
        let span = Option.value_exn diagnostic.span in
        [%test_eq: int] line span.left.line;
        print_s [%sexp (name : string), (diagnostic.message : string)]);
  List.iter
    [ "let value = 1\nlet value = true\nlet result = if value then 3 else 0"
    ; "let rec f x = x + 1\nlet rec f x = x + 2\nlet result = f(1)"
    ; "let f x = let x = x + 1 in x\nlet result = f(2)"
    ]
    ~f:(fun source ->
      match run source with
      | L.VInt actual -> [%test_eq: int] 3 actual
      | _ -> failwith "rebinding changed");
  [%expect
    {|
    (lambda "Duplicate lambda parameter 'x'")
    ("annotated lambda" "Duplicate lambda parameter 'x'")
    ("recursive group" "Duplicate recursive binding 'f'")
    ("local recursive group" "Duplicate recursive binding 'f'")
    |}]
;;
