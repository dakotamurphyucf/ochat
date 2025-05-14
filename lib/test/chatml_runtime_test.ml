open Core
open Chatml
module L = Chatml_lang
open Chatml_builtin_modules

(** Small parsing helper identical to the one used in the type-checker tests. *)
let parse (str : string) : L.program =
  let lexbuf = Lexing.from_string str in
  try Chatml_parser.program Chatml_lexer.token lexbuf, str with
  | Chatml_parser.Error -> failwith "Parse error"
;;

(** Convenience helper that prepares an evaluation environment with the
    standard built-ins, then executes the ChatML [code] using the full
    pipeline (type-checker → resolver → interpreter).  We purposely
    thread the program through [Chatml_resolver.eval_program] so that we
    exercise *all* optimisation passes that the production runtime uses. *)
let eval code =
  let env = L.create_env () in
  BuiltinModules.add_global_builtins env;
  let prog = parse code in
  Chatml_resolver.eval_program env prog
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 1.  Recursion & arithmetic                                            *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "recursive factorial" =
  let code =
    {|      
      let rec fact n =
        match n with
        | 0 -> 1
        | _ -> n * fact(n - 1) in
      print(fact(5))
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    120 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 2.  Arrays: get / set / length                                        *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "array mutation and length" =
  let code =
    {|      
      let arr = [1, 2, 3]
      arr[1] <- 5
      print(arr[1])
      print(arr[2])
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    5 
    3 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 3b.  Sequential let-bindings (ELetBlock)                               *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "sequential let block" =
  let code =
    {|      
      let result =
        let a = 1 in
        let b = a + 2 in
        let c = b + 3 in
        c in
      print(result)
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    6 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 3c.  If/then/else                                                     *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "simple if branch" =
  let code =
    {|      
      print(if true then 1 else 2)
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    1 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 3.  Records: field update & access                                     *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "record field update" =
  let code =
    {|      
      let person = {name = "Bob"; age = 20}
      person.age <- person.age + 1
      print(person.age)
      print(person.name)
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    21 
    Bob 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 4.  Variants & pattern matching                                        *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "variant pattern match" =
  let code =
    {|      
      let v = `Pair(3, 4) in
      match v with
      | `Pair(x, y) -> print([x, y])
      | _ -> print([0])
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    [|3, 4|] 
    |}]
;;

let%expect_test "variant none match" =
  let code =
    {|      
      let v = `None in
      match v with
      | `None -> print("none")
      | _ -> print("other")
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    none 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 4b. Mutual recursion (let rec … and)                                   *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "mutual recursion even/odd" =
  let code =
    {|      
      let rec is_even n =
        match n with
        | 0 -> true
        | _ -> is_odd(n - 1) and is_odd m =
        match m with
        | 0 -> false
        | _ -> is_even(m - 1) in
      print(is_even(4));
      print(is_even(5));
      print(is_odd(3))
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    true 
    false 
    true 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 5.  References                                                          *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "mutable reference update" =
  let code =
    {|      
      let r = ref(0)
      r := !r + 10
      print(!r)
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    10 
    |}]
;;

(* while loop using a boolean flag to exercise loop semantics *)

let%expect_test "while toggle flag" =
  let code =
    {|      
      let flag = ref(true)
      let counter = ref(0)
      while !flag do
        counter := !counter + 1;
        flag := false
      done
      print(!counter)
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    1 
    |}]
;;

(* additional while loop to test mutable updates in loop *)

(* ───────────────────────────────────────────────────────────────────── *)
(* 6.  Modules & open                                                     *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "module definition and open" =
  let code =
    {|      
      module M = struct
        let square x = x * x
        let two = 2
      end

      open M
      print(square(two))
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    4 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 7. Higher-order functions & compose                                    *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "higher-order compose" =
  let code =
    {|      
      let compose f g x = f(g(x))
      let inc x = x + 1
      let double x = x * 2
      print(compose(inc, double, 3))
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    7 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 8.  Comparison operators and division                                *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "numeric comparisons and division" =
  let code =
    {|      
      print(10 / 2)
      print(3 < 5)
      print(6 > 9)
      print(4 <= 4)
      print(5 >= 10)
      print(7 == 7)
      print(8 != 8)
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    5 
    true 
    false 
    true 
    false 
    true 
    false 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 9.  Record extension (ERecordExtend)                                  *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "record extension overwrite and add" =
  let code =
    {|      
      let person = {name = "Alice"; age = 30}
      let employee = { person with age = 31; id = 123 }
      print(employee.name)
      print(employee.age)
      print(employee.id)
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    Alice 
    31 
    123 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 10. Built-ins: length & to_string                                     *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "array length builtin" =
  let code =
    {|      
      let arr = [10, 20, 30, 40]
      print(length(arr))
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    4 
    |}]
;;

let%expect_test "generic to_string builtin" =
  let code =
    {|      
      print(to_string(42))
      print(to_string(true))
      print(to_string([1, 2]))
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    42 
    true 
    [|1, 2|] 
    |}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 11.  Lambdas and map_in_place                                         *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "lambda values and map_in_place" =
  let code =
    {|      
      (* anonymous lambda bound to a variable and invoked *)
      let add10 = fun x -> x + 10
      print(add10(5))

      (* map_in_place: applies f to every element of the array in place *)
      let map_in_place arr f =
        let len = length(arr) in
        let idx = ref(0) in
        while !idx < len do
          arr[!idx] <- f(arr[!idx]);
          idx := !idx + 1
        done
      

      let nums = [1, 2, 3]
      map_in_place(nums, fun n -> n * 2)
      print(nums[0])
      print(nums[1])
      print(nums[2])
    |}
  in
  eval code;
  [%expect
    {|
    Type checking succeeded!
    15 
    2 
    4 
    6 
    |}]
;;
