open! Core
module L = Chatml.Chatml_lang

let show source =
  Support.run source |> Chatml.Chatml_builtin_spec.value_to_string |> print_endline
;;

let%expect_test "nested let destructuring ignores leaves and evaluates its old scope once"
  =
  show
    {|let count = ref(0)
      let left = 10
      let next () = count := !count + 1; (|left + 1, (|false, "label"|)|)
      let (|left, (|_, label|)|) = next()
      let result = (|left, label, !count|)|};
  [%expect {| (|11, label, 1|) |}]
;;

let%expect_test "a tuple parameter consumes one argument independently of its leaves" =
  show
    {|let add (|a, (|_, b|)|) increment = a + b + increment
      let ordinary a b = a + b
      let ignore _ = 9
      let result = (|add((|2, (|false, 3|)|), 4), ordinary(2, 3), ignore((|1, 2|))|)|};
  [%expect {| (|9, 5, 9|) |}]
;;

let%test_unit "whole RHS restriction and projected schemes preserve weak/equality sharing"
  =
  Support.require
    {|let (|identity, _|) = (|fun x -> x, "pure"|)
      let result = (|identity(7), identity(true)|)|}
    ~accepted:true;
  Support.require
    {|let (|identity, items|) = (|fun x -> x, []|)
      let a = identity(7)
      let b = identity(true)|}
    ~accepted:false;
  Support.require
    {|let make () = let cell = ref([]) in (|cell, cell|)
      let (|a, b|) = make()
      a := [7]
      b := ["wrong"]|}
    ~accepted:false;
  (match
     Support.run
       {|let make () = let cell = ref([]) in (|cell, cell|)
        let (|a, b|) = make()
        a := [7]
        let result = (!b)[0]|}
   with
   | L.VInt n -> [%test_eq: int] n 7
   | _ -> failwith "Mutable tuple children were copied");
  Support.require
    {|let (|equal, _|) = (|fun a b -> a == b, ()|)
      let result = equal(ref(7), ref(7))|}
    ~accepted:false;
  Support.require
    {|let same (|left, right|) = if true then left else right
      let (|a, b|) = same((|(|{x = 1}, true|), (|{x = "wrong"}, true|)|))|}
    ~accepted:false;
  let recursive_rhs =
    "let preserve : json -> json = fun value -> value\n\
     let (|identity, recursive|) = (|fun x -> x, preserve|)\n\
     let a = identity(7)"
  in
  Support.require recursive_rhs ~accepted:true;
  (* A recursive sibling does not constrain the independent pure identity.
     The expansive whole-RHS and shared weak-cell cases above still restrict. *)
  Support.require (recursive_rhs ^ "\nlet b = identity(true)") ~accepted:true
;;

let%test_unit "product aliases, annotations and signatures admit all leaf exports" =
  Support.require
    {|module P : sig
        type 'a pair = 'a * string
        val number : int
        val label : string
        val take : fun(int pair, bool) -> int
      end = struct
        type 'a pair = 'a * string
        let (|number, label|) : int pair = (|7, "x"|)
        let take : fun(int pair, bool) -> int = fun (|n, _|) flag -> n
      end
      let (|n, text|) : int P.pair = (|P.number, P.label|)
      let result = P.take((|n, text|), true)|}
    ~accepted:true;
  Support.require
    "let (|n, (|flag, _|)|) : int * (bool * string) = (|7, (|true, \"x\"|)|)"
    ~accepted:true;
  Support.require "let (|n, flag|) : int * bool = (|7, \"wrong\"|)" ~accepted:false;
  Support.require "let add (|a, b|) = a + b\nlet result = add(2, 3)" ~accepted:false;
  Support.require "let add a b = a + b\nlet result = add((|2, 3|))" ~accepted:false
;;

let%test_unit "duplicate binders identify the exact second leaf and wildcards may repeat" =
  List.iter
    [ "let (|x, (|_, x|)|) = (|1, (|false, 2|)|)"; "let f (|x, _|) x = x" ]
    ~f:(fun source ->
      match Chatml_typechecker.check_program (Support.parse source) with
      | Error { span = Some span; message; _ } ->
        assert (String.is_substring message ~substring:"Duplicate");
        [%test_eq: string]
          (String.sub
             source
             ~pos:span.left.offset
             ~len:(span.right.offset - span.left.offset))
          "x";
        let prefix = String.prefix source span.left.offset in
        assert (String.is_substring prefix ~substring:"x")
      | _ -> failwith "Expected located duplicate binder error");
  Support.require
    "let (|_, _|) = (|7, true|)\nlet f _ _ = ()\nlet result = f(7, true)"
    ~accepted:true;
  Support.require
    "let x = 1\nlet (|x, _|) = (|x + 1, true|)\nlet result = x"
    ~accepted:true;
  List.iter
    [ "()"; "7"; "true"; "`Some(x)"; "{x = x}"; "(|x|)"; "(|x, y,|)" ]
    ~f:(fun head ->
      assert (
        Result.is_error (Chatml.Chatml_parse.parse_program ("let " ^ head ^ " = ()"))));
  List.iter
    [ "7"; "true"; "`Some(x)"; "{x = x}"; "(|x|)"; "(|x, y,|)"; "(|(), _|)" ]
    ~f:(fun head ->
      assert (
        Result.is_error
          (Chatml.Chatml_parse.parse_program ("let result = fun " ^ head ^ " -> ()"))))
;;

let%test_unit "recursive tuple parameters keep call arity and tail frame layout" =
  (match
     Support.run
       {|let rec loop (|n, total|) _ =
          if n == 0 then total else loop((|n - 1, total + 1|), true)
        let result = loop((|2000, 0|), false)|}
   with
   | L.VInt n -> [%test_eq: int] n 2000
   | _ -> failwith "Tuple tail call lost its flattened leaf layout");
  [%test_eq: string]
    (Support.run
       {|let result = let x = 1 in let (|x, text|) = (|x + 1, "x"|) in to_string(x) ++ text|}
     |> Chatml.Chatml_builtin_spec.value_to_string)
    "2x"
;;
