open Core
open Chatml
module L = Chatml_lang
open Chatml_builtin_modules

let parse (str : string) : L.program =
  let lexbuf = Lexing.from_string str in
  try Chatml_parser.program Chatml_lexer.token lexbuf, str with
  | Chatml_parser.Error -> failwith "Parse error"
;;

let check_program_result (code : string) =
  let prog = parse code in
  Chatml_typechecker.check_program prog
;;

let check_program_formatted (code : string) =
  match check_program_result code with
  | Ok _ -> "ok"
  | Error diagnostic -> Chatml_typechecker.format_diagnostic code diagnostic
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
      let to_str n = to_string(n)
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
let%expect_test "record copy update ok" =
  let code =
    {|
      let p = {name = "Bob"; age = 20}
      let q = {p with age = p.age + 1}
      q.age
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "record copy update may change field type" =
  let code =
    {|
      let p = {name = "Bob"; age = 20}
      let q = {p with age = "old"}
      q.age
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
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

let%expect_test "print is unary" =
  let code =
    {|
      print(1, 2)
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 2, characters 6-17:
    2|    print(1, 2)
          ^^^^^^^^^^^

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

let%test_unit "value restriction rejects polymorphic ref update" =
  let code =
    {|
      let r = ref(fun x -> x) in
      r := (fun x -> x + 1);
      (!r)(true)
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected value restriction to reject polymorphic ref use"
  | Error _ -> ()
;;

let%test_unit "value restriction rejects polymorphic array update" =
  let code =
    {|
      let a = [fun x -> x] in
      a[0] <- (fun x -> x + 1);
      a[0](true)
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected value restriction to reject polymorphic array use"
  | Error _ -> ()
;;

let%test_unit "value restriction rejects aliasing expansive binding" =
  let code =
    {|
      let r = ref(fun x -> x)
      let alias = r
      alias := (fun x -> x + 1)
      (!r)(true)
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected weakly-typed alias to remain monomorphic"
  | Error _ -> ()
;;

let%test_unit "value restriction preserves polymorphic immutable record field" =
  let code =
    {|
      let r = {id = fun x -> x}
      r.id(1)
      r.id("s")
    |}
  in
  match check_program_result code with
  | Ok _ -> ()
  | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic)
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

let%test_unit "module can use outer bindings during definition" =
  let code =
    {|
      let x = 1
      module M = struct
        let y = x
      end
      M.y
    |}
  in
  match check_program_result code with
  | Ok _ -> ()
  | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic)
;;

let%test_unit "module does not export outer bindings implicitly" =
  let code =
    {|
      let x = 1
      module M = struct
        let y = x
      end
      M.x
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected module field lookup to reject implicit outer export"
  | Error _ -> ()
;;

let%test_unit "open inside module does not re-export imported names" =
  let code =
    {|
      module N = struct
        let x = 1
      end
      module M = struct
        open N
        let y = x
      end
      M.x
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected opened names inside module to stay unexported"
  | Error _ -> ()
;;

let%test_unit "duplicate binder in variant pattern is rejected" =
  let code =
    {|
      let v = `Pair(1, 2) in
      match v with
      | `Pair(x, x) -> x
      | _ -> 0
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected duplicate binder in pattern to be rejected"
  | Error _ -> ()
;;

let%test_unit "duplicate binder across record pattern fields is rejected" =
  let code =
    {|
      let r = {left = 1; right = 2} in
      match r with
      | {left = x; right = x} -> x
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected duplicate record-pattern binder to be rejected"
  | Error _ -> ()
;;

let%test_unit "catch-all match arm makes later arms redundant" =
  let code =
    {|
      match 1 with
      | _ -> 0
      | 1 -> 1
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected arm after wildcard to be rejected as redundant"
  | Error _ -> ()
;;

let%test_unit "variable match arm makes later arms redundant" =
  let code =
    {|
      match 1 with
      | x -> x
      | 1 -> 0
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected arm after variable pattern to be rejected as redundant"
  | Error _ -> ()
;;

let%test_unit "duplicate int match arm is rejected" =
  let code =
    {|
      match 1 with
      | 0 -> 0
      | 0 -> 1
      | _ -> 2
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected duplicate int arm to be rejected"
  | Error _ -> ()
;;

let%test_unit "duplicate bool match arm is rejected" =
  let code =
    {|
      match true with
      | true -> 1
      | true -> 2
      | false -> 0
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected duplicate bool arm to be rejected"
  | Error _ -> ()
;;

let%test_unit "duplicate string match arm is rejected" =
  let code =
    {|
      match "a" with
      | "a" -> 1
      | "a" -> 2
      | _ -> 0
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected duplicate string arm to be rejected"
  | Error _ -> ()
;;

let%test_unit "duplicate nullary variant match arm is rejected" =
  let code =
    {|
      match `None with
      | `None -> 0
      | `None -> 1
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected duplicate nullary variant arm to be rejected"
  | Error _ -> ()
;;

let%test_unit "boolean match must be exhaustive" =
  let code =
    {|
      match true with
      | true -> 1
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected non-exhaustive boolean match to be rejected"
  | Error _ -> ()
;;

let%test_unit "boolean match with both arms is accepted" =
  let code =
    {|
      match true with
      | true -> 1
      | false -> 0
    |}
  in
  match check_program_result code with
  | Ok _ -> ()
  | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic)
;;

let%test_unit "boolean match with catch-all is accepted" =
  let code =
    {|
      match true with
      | true -> 1
      | _ -> 0
    |}
  in
  match check_program_result code with
  | Ok _ -> ()
  | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic)
;;

let%test_unit "variant exhaustiveness is still intentionally partial" =
  let code =
    {|
      match `Some(1) with
      | `Some(x) -> x
    |}
  in
  match check_program_result code with
  | Ok _ -> ()
  | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic)
;;

let%test_unit "duplicate int arm diagnostic names offending pattern" =
  let code =
    {|
      match 1 with
      | 0 -> 0
      | 0 -> 1
      | _ -> 2
    |}
  in
  let rendered = check_program_formatted code in
  if not (String.is_substring rendered ~substring:"Duplicate match arm for pattern '0'")
  then failwith rendered
;;

let%test_unit "redundant arm diagnostic names current and catch-all patterns" =
  let code =
    {|
      match 1 with
      | _ -> 0
      | 1 -> 1
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring
         rendered
         ~substring:"Redundant match arm '1': previous catch-all pattern '_' already matches all cases")
  then failwith rendered
;;

let%test_unit "boolean diagnostic reports missing false case" =
  let code =
    {|
      match true with
      | true -> 1
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring
         rendered
         ~substring:"Non-exhaustive boolean match: missing case 'false'")
  then failwith rendered
;;

let%test_unit "boolean diagnostic reports missing true case" =
  let code =
    {|
      match false with
      | false -> 0
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring rendered ~substring:"Non-exhaustive boolean match: missing case 'true'")
  then failwith rendered
;;

let%test_unit "duplicate nullary variant diagnostic names offending constructor" =
  let code =
    {|
      match `None with
      | `None -> 0
      | `None -> 1
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring rendered ~substring:"Duplicate match arm for pattern '`None'")
  then failwith rendered
;;

let%test_unit "variant match closes function parameter constructor set" =
  let code =
    {|
      let f v =
        match v with
        | `Some(x) -> x

      f(`Some(1))
      f(`None)
    |}
  in
  match check_program_result code with
  | Ok _ -> failwith "expected variant match to reject constructors outside the matched set"
  | Error _ -> ()
;;

let%test_unit "two-constructor variant match is exhaustive without wildcard" =
  let code =
    {|
      let f v =
        match v with
        | `None -> 0
        | `Some(x) -> x

      f(`Some(1))
      f(`None)
    |}
  in
  match check_program_result code with
  | Ok _ -> ()
  | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic)
;;

let%test_unit "variant exhaustiveness reports missing constructor payload wildcard" =
  let code =
    {|
      match `Some(1) with
      | `Some(1) -> 1
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring
         rendered
         ~substring:"Non-exhaustive variant match: missing case '`Some(_)'")
  then failwith rendered
;;

let%test_unit "int match without wildcard reports fallback requirement" =
  let code =
    {|
      match 1 with
      | 0 -> 0
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring rendered ~substring:"Non-exhaustive match on int: add '_' arm")
  then failwith rendered
;;

let%test_unit "variant wildcard arm is redundant after all constructors are covered" =
  let code =
    {|
      let f v =
        match v with
        | `None -> 0
        | `Some(x) -> x
        | _ -> 2

      f(`Some(1))
      f(`None)
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring
         rendered
         ~substring:"Redundant match arm '_': previous arms already cover all variant constructors")
  then failwith rendered
;;

let%test_unit "variant constructor arm is redundant after earlier payload wildcard" =
  let code =
    {|
      let f v =
        match v with
        | `Some(x) -> x
        | `Some(1) -> 1

      f(`Some(1))
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring
         rendered
         ~substring:"Redundant match arm '`Some(...)': previous arms already cover variant case '`Some(_)'")
  then failwith rendered
;;

let%test_unit "boolean wildcard arm is redundant after both literals are covered" =
  let code =
    {|
      match true with
      | true -> 1
      | false -> 0
      | _ -> 2
    |}
  in
  let rendered = check_program_formatted code in
  if
    not
      (String.is_substring
         rendered
         ~substring:"Redundant match arm '_': previous arms already cover boolean cases 'true' and 'false'")
  then failwith rendered
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

let%expect_test "record copy update of array element field is allowed" =
  let code =
    {|
      let set_task_status st new_status =
        let t = st.tasks[st.task_index] in
        let t = { t with status = new_status } in
        st.tasks[st.task_index] <- t;
        st
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "record helper remains row-polymorphic when returning a wider state" =
  let code =
    {|
      let bump_attempts st =
        let t = st.tasks[st.task_index] in
        let t = { t with attempts = t.attempts + 1 } in
        st.tasks[st.task_index] <- t;
        st

      let s =
        { autopilot = true
        ; task_index = 0
        ; tasks = [ { attempts = 0 } ]
        }

      let s2 = bump_attempts(s)
      s2.autopilot
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "state machine helper composition preserves wider state rows" =
  let code =
    {|
      let mk_task id =
        { id = id
        ; status = `Pending
        ; attempts = 0
        }

      let set_task_status st new_status =
        let t = st.tasks[st.task_index] in
        let t = { t with status = new_status } in
        st.tasks[st.task_index] <- t;
        st

      let bump_attempts st =
        let t = st.tasks[st.task_index] in
        let t = { t with attempts = t.attempts + 1 } in
        st.tasks[st.task_index] <- t;
        st

      let step st ev =
        match ev with
        | `Start ->
            let st = set_task_status(st, `Running) in
            bump_attempts(st)
        | `Done -> set_task_status(st, `Done)

      let s =
        { autopilot = true
        ; task_index = 0
        ; tasks = [ mk_task("t1") ]
        }

      let s2 = step(s, `Start)
      s2.autopilot
    |}
  in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect {| Type checking succeeded! |}]
;;

let%expect_test "module helper over state remains row-polymorphic after open" =
  let code =
    {|
      let mk_task id =
        { id = id
        ; status = `Pending
        ; attempts = 0
        }

      module Flow = struct
        let bump_attempts st =
          let t = st.tasks[st.task_index] in
          let t = { t with attempts = t.attempts + 1 } in
          st.tasks[st.task_index] <- t;
          st
      end

      open Flow

      let s =
        { autopilot = false
        ; task_index = 0
        ; tasks = [ mk_task("t1") ]
        }

      let s2 = bump_attempts(s)
      s2.autopilot
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

let%expect_test "runtime record extension remains immutable" =
  let env = Chatml_lang.create_env () in
  BuiltinModules.add_global_builtins env;
  let code =
    {|
    let p = {name = "Alice"; age = 25}
    let f p =
        let inc_age person =
            {person with age = person.age + 1}
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
  (match Chatml_resolver.eval_program env ast with
   | Ok () -> ()
   | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic));
  [%expect
    {|
    Alice 
    { age = 56; name = Alice } 
    [|1, 2|]
    |}]
;;

;;

let%expect_test "inner let binding does not leak" =
  let code = "let y = let x = 1 in x\nx\n" in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 2, characters 0-1:
    2|    x
          ^

    Type error: Unknown variable 'x'
    |}]
;;

let%expect_test "lambda parameter does not leak" =
  let code = "let f x = x\nx\n" in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 2, characters 0-1:
    2|    x
          ^

    Type error: Unknown variable 'x'
    |}]
;;

let%expect_test "match binder does not leak" =
  let code = "let v = `Some(1)\nmatch v with\n| `Some(x) -> x\n| _ -> 0\nx\n" in
  let prog = parse code in
  Chatml_typechecker.infer_program prog;
  [%expect
    {|
    line 5, characters 0-1:
    5|    x
          ^

    Type error: Unknown variable 'x'
    |}]
;;
