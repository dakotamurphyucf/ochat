open! Core
module Parse = Chatml.Chatml_parse
module Checker = Chatml_typechecker
module L = Chatml.Chatml_lang

let check source = Checker.check_program (Parse.parse_program_exn source)

let%expect_test "a pure JSON wrapper has independently polymorphic callback results" =
  let source =
    {|let decode value f = f(Json.tag(value))
      let number = decode(`Null, fun tag -> 7)
      let text = decode(`Null, fun tag -> "checked")|}
  in
  let env = L.create_env () in
  Chatml_builtin_modules.add_global_builtins env;
  (match Chatml_resolver.run_program env (Parse.parse_program_exn source) with
   | Ok () -> ()
   | Error _ -> assert false);
  (match L.find_var env "number", L.find_var env "text" with
   | Some (VInt number), Some (VString text) ->
     printf "number: %d\ntext: %s\n" number text
   | _ -> assert false);
  [%expect
    {|
    number: 7
    text: checked
    |}]
;;

let%test_unit "recursive data does not generalize captured or expansive weak storage" =
  List.iter
    [ {|let values = []
        let read value = let ignored = Json.tag(value) in values[0]
        let number = read(`Null) + 1
        let text = read(`Null) ++ "x"|}
    ; {|let decode value f = f(Json.tag(value))
        let stored = ref(decode)
        let number = (!stored)(`Null, fun tag -> 1)
        let text = (!stored)(`Null, fun tag -> "x")|}
    ; {|let decode value f = f(Json.tag(value))
        let stored = [decode]
        let number = stored[0](`Null, fun tag -> 1)
        let text = stored[0](`Null, fun tag -> "x")|}
    ; {|let capture value =
          let shared = [] in
          let read json = let ignored = Json.tag(json) in shared[0] in
          (|read, read|)
        let (|first, second|) = capture(0)
        let number = first(`Null) + 1
        let text = second(`Null) ++ "x"|}
    ]
    ~f:(fun source -> assert (Result.is_error (check source)))
;;

let%test_unit
    "equality, function arity and monomorphic recursive calls still constrain JSON \
     helpers"
  =
  List.iter
    [ {|let same value a b = let ignored = Json.tag(value) in a == b
        let bad = same(`Null, fun x -> x, fun x -> x)|}
    ; {|let decode value f = f(Json.tag(value))
        let bad = decode(`Null, fun tag -> 1, true)|}
    ; {|let rec loop value item =
          let ignored = Json.tag(value) in
          if true then item else loop(value, [item])|}
    ]
    ~f:(fun source -> assert (Result.is_error (check source)))
;;
