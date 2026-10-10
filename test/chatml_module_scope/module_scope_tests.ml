open Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

let%expect_test "module self-reference is rejected even inside deferred functions" =
  List.iter
    [ "let first = M.later let later = 42"
    ; "let earlier = 42 let second = M.earlier"
    ; "let read () = M.later let later = 42"
    ; "let read () = M.later let first = read() let later = 42"
    ; "let alias = M let later = 42"
    ; "module Nested = struct let read () = M.later end let later = 42"
    ]
    ~f:(fun body ->
      let source = "module M = struct " ^ body ^ " end" in
      match T.check_program (Chatml.Chatml_parse.parse_program_exn source) with
      | Ok _ -> failwith "module self-reference accepted"
      | Error diagnostic ->
        [%test_eq: bool] true (Option.is_some diagnostic.span);
        print_endline diagnostic.message);
  [%expect
    {|
    Unknown variable 'M'
    Unknown variable 'M'
    Unknown variable 'M'
    Unknown variable 'M'
    Unknown variable 'M'
    Unknown variable 'M'
    |}]
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

let%expect_test "nonrecursive module scope preserves ordinary lexical bindings" =
  List.iter
    [ {|module M = struct let x = 42 let read () = x end let result = M.read()|}
    ; {|module M = struct
          let rec count n = if n == 0 then 0 else 1 + count(n - 1)
        end let result = M.count(3)|}
    ; {|module M = struct let x = 1 let read () = x let x = 2 end
        let result = M.read() * 10 + M.x|}
    ; {|module Earlier = struct let x = 7 end
        module M = struct let read () = Earlier.x end let result = M.read()|}
    ; {|module M = struct let x = 7 end
        module M = struct let previous () = M.x let x = 8 end
        let result = M.previous() * 10 + M.x|}
    ; {|module M = struct
          module M = struct let x = 3 end
          let value = M.x
        end let result = M.value + Array.length(record_keys(M))|}
    ]
    ~f:run;
  [%expect
    {|
    42
    3
    12
    7
    78
    5
    |}]
;;

let%expect_test "an initializer failure leaves no newly installed module" =
  let env = Chatml_builtin_modules.create_default_env () in
  let source =
    {|let count = ref(0)
      module M = struct let first = count := 1 let failed = 1 / 0 end|}
  in
  (match
     Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
   with
   | Error (Runtime_diagnostic _) -> ()
   | Error (Type_diagnostic diagnostic) -> failwith diagnostic.message
   | Ok () -> failwith "initializer unexpectedly succeeded");
  print_s [%sexp (Option.is_none (L.find_var env "M") : bool)];
  (match L.find_var env "count" with
   | Some (VRef { contents = VInt count }) -> print_s [%sexp (count : int)]
   | _ -> failwith "preceding mutation was lost");
  [%expect
    {|
    true
    1
    |}]
;;
