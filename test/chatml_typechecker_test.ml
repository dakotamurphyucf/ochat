open Core
open Chatml
module L = Chatml_lang
open Chatml_builtin_modules

let parse (str : string) : L.program =
  let lexbuf = Lexing.from_string str in
  try Chatml_parser.program Chatml_lexer.token lexbuf, str with
  | Chatml_parser.Error -> failwith "Parse error"
;;

let%expect_test "variant with tuple args" =
  let code =
    {|    
      let p = `Pair(1, "two") in
      match p with
      | `Pair(a, b) -> b
      | _ -> ""
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "well-typed" =
  let code =
    {|
      let p = {name = "Alice"}
      let f person = print([person.name])
      f(p)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "const function is polymorphic on second argument" =
  let code =
    {|    
      let const x y = x
      const(1, "a")
      const("hello", 3.14)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "alias of polymorphic primitive remains polymorphic" =
  let code =
    {|
      let k = print
      k("hello")
      k([1,2])
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "polymorphic higher-order compose" =
  let code =
    {|
      let compose f g x = f(g(x))
      let id x = x
      let to_str n = num2str(n)
      compose(id, id, "ok")
      compose(to_str, id, 1)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "ill-typed (missing field)" =
  let code =
    {|
      let p = {name = "Alice"}
      print([p.age])
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 3, characters 13-18:
    3|    p.age
          ^^^^^

    Type error: Row does not contain label 'age'
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "record field set ok" =
  let code =
    {|
      let p = {name = "Bob"; age = 20}
      p.age <- p.age + 1
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "record field set wrong type" =
  let code =
    {|
      let p = {name = "Bob"; age = 20}
      p.age <- "old"
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 3, characters 6-20:
    3|    p.age <- "old"
          ^^^^^^^^^^^^^^

    Type error: Cannot unify int with string
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "function arity mismatch" =
  let code =
    {|
      let id x = x
      id(1, 2)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 3, characters 6-14:
    3|    id(1, 2)
          ^^^^^^^^

    Type error: Function arity mismatch
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "array heterogeneous types" =
  let code =
    {|
      print([1, "two"])
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 2, characters 12-22:
    2|    [1, "two"]
          ^^^^^^^^^^

    Type error: Cannot unify string with int
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "print is polymorphic" =
  let code =
    {|
      print([1, 2])
      print("done")
      print({a = 1; b = 2})
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "let rec mutual recursion ok" =
  let code =
    {|
      let rec f x = g(x) and g y = f(y)
      f(1)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "if condition not bool" =
  let code =
    {|
      if 1 then 2 else 3
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 2, characters 6-24:
    2|    if 1 then 2 else 3
          ^^^^^^^^^^^^^^^^^^

    Type error: Cannot unify int with bool
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "unknown variable" =
  let code =
    {|
      let y = x
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 2, characters 14-15:
    2|    x
          ^

    Type error: Unknown variable 'x'
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "array index not number" =
  let code =
    {|
      let arr = [1,2,3] in arr["a"]
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 2, characters 27-35:
    2|    arr["a"]
          ^^^^^^^^

    Type error: Cannot unify string with int
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "parametric polymorphism on records" =
  let code =
    {|
      let f p = p.name
      let tmp = f({name = "Bob"}) in
      f({name = "Bob"; age = 10})
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "row polymorphism success" =
  let code =
    {|
      let get_name p = p.name
      get_name({name = "Ann"; age = 42})
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "calling non-function value" =
  let code =
    {|
      let v = 3
      v(1)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 3, characters 6-10:
    3|    v(1)
          ^^^^

    Type error: Cannot unify int with (int -> 'a)
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "while condition not bool" =
  let code =
    {|
      while 1 do print([1]) done
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 2, characters 6-32:
    2|    while 1 do print([1]) done
          ^^^^^^^^^^^^^^^^^^^^^^^^^^

    Type error: Cannot unify int with bool
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "array set wrong element type" =
  let code =
    {|
      let a = [1,2]
      a[0] <- "hello"
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 3, characters 6-21:
    3|    a[0] <- "hello"
          ^^^^^^^^^^^^^^^

    Type error: Cannot unify int with string
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "array set element success" =
  let code =
    {|
      let a = [1,2]
      a[0] <- 3
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "fresh monomorphic variables for each call-site of id" =
  let code =
    {|
      let id x = x
      id(1)
      id("str")
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "module basic access" =
  let code =
    {|
      module M = struct
        let x = 1
        let id y = y
      end

      M.id(M.x)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "module open and use" =
  let code =
    {|
      module Math = struct
        let one = 1
        let add a b = a + b
      end

      open Math
      add(one, 1)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "module missing field error" =
  let code =
    {|
      module A = struct
        let foo = 1
      end

      print([A.bar])
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 6, characters 13-18:
    6|    A.bar
          ^^^^^

    Type error: Row does not contain label 'bar'
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "variant match simple" =
  let code =
    {|    
      let v = `Some(1) in
      match v with
      | `Some(x) -> x
      | _ -> 0
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "match literals and wildcard" =
  let code =
    {|      
      let f n =
        match n with
        | 0 -> 0
        | 1 -> 10
        | _ -> 99 in
      f(1);
      f(2)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "variant polymorphic is_none" =
  let code =
    {|      
      let is_none v =
        match v with
        | `None -> true
        | _ -> false in
      is_none(`None);
      is_none(`Some(1))
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "variant payload type error" =
  let code =
    {|      
      let v = `Some("hi") in
      match v with
      | `Some(x) -> x + 1
      | _ -> 0
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 4, characters 20-25:
    4|    x + 1
          ^^^^^

    Type error: Cannot unify number with string
    |}]
;;

(* ------------------------------------------------------------------ *)
let%expect_test "record extension update ok" =
  let code =
    {|
      let p = {name = "Bob"; age = 20}
      let p2 = { p with age = p.age + 1 }
      print([p2.age])
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "record extension add new field age" =
  let code =
    {|
      let p = {name = "Ann"}
      let q = { p with age = 18 }
      print(q.age + 1)
      print(q.name)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "record extension override type" =
  let code =
    {|
      let p = {name = "Bob"; age = 20}
      let p2 = { p with age = "old" }
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "record pattern subset ok" =
  let code =
    {|
      let r = {name = "Jim"; age = 35}
      match r with
      | {name = n; _} -> print([n])
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "record pattern missing field" =
  let code =
    {|
    let r = {name = "Sue"}
    match r with | {age = a} -> a
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 3, characters 4-33:
    3|    match r with | {age = a} -> a
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Type error: Row does not contain label 'age'
    |}]
;;

let%expect_test "record pattern missing field" =
  let env = Chatml_lang.create_env () in
  BuiltinModules.add_global_builtins env;
  let code =
    {|
    (* Type checking succeeded! Uncaught exception: (Failure "No field 'age' in record") *)
    (* --------------------------------------- *)
    let p = {name = "Alice"; age = 25}
    let f p =
        let inc_age person =
            person.age <- person.age + 1;
            person
        in
        print(p.name);
        print(inc_age({p with age = 30 + p.age}))

    
    f(p)
    let a = `Some(1, 2)
     let b = `None
     match a with
     | `Some(x, y) -> print([x, y])
     | `None -> print([0])


  |}
  in
  let ast = parse code in
  Chatml_resolver.eval_program env ast;
  [%expect
    {|
    Type checking succeeded!            
    Alice 
    { age = 56; name = Alice } 
    [|1, 2|]
    |}]
;;
