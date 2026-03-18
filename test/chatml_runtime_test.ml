open Core
open Chatml
module L = Chatml_lang
module E = Chatml_eval
open Chatml_builtin_modules

(** Small parsing helper identical to the one used in the type-checker tests. *)
let parse (str : string) : L.program = Chatml_parse.parse_program_exn str

(** Convenience helper that prepares an evaluation environment with the
    standard built-ins, then executes the ChatML [code] using the full
    pipeline (type-checker → resolver → interpreter).  We purposely
    thread the program through [Chatml_resolver.eval_program] so that we
    exercise *all* optimisation passes that the production runtime uses. *)
let eval_result code =
  let env = L.create_env () in
  BuiltinModules.add_global_builtins env;
  let prog = parse code in
  Chatml_resolver.eval_program env prog
;;

let run_result code =
  let env = L.create_env () in
  BuiltinModules.add_global_builtins env;
  let prog = parse code in
  Chatml_resolver.run_program env prog
;;

let eval code =
  match eval_result code with
  | Ok () -> ()
  | Error diagnostic -> failwith (Chatml_typechecker.format_diagnostic code diagnostic)
;;

let dummy_position = { Source.line = 1; column = 0; offset = 0 }
let dummy_span = { Source.left = dummy_position; right = dummy_position }
let node value : _ L.node = { value; span = dummy_span }

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
1
|}]
;;

(* ───────────────────────────────────────────────────────────────────── *)
(* 3.  Records: immutable copy-update & access                            *)
(* ───────────────────────────────────────────────────────────────────── *)

let%expect_test "record copy update leaves original unchanged" =
  let code =
    {|
      let person = {name = "Bob"; age = 20}
      let older = {person with age = person.age + 1}
      print(person.age)
      print(older.age)
      print(older.name)
    |}
  in
  eval code;
  [%expect
    {|
20
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
1
|}]
;;

let%expect_test "sequence forces first function call before second" =
  let code =
    {|
      let f () = print("a")
      let g () = print("b")
      let run () =
        f();
        g()
      run()
    |}
  in
  eval code;
  [%expect
    {|
a
b
|}]
;;

let%expect_test "unit literal prints as ()" =
  let code =
    {|
      print(())
    |}
  in
  eval code;
  [%expect
    {|
()
|}]
;;

let%expect_test "fun () -> ... defines zero-argument lambdas" =
  let code =
    {|
      let f = fun () -> 42
      print(f())
    |}
  in
  eval code;
  [%expect
    {|
42
|}]
;;

let%expect_test "local let f () = ... in ... works" =
  let code =
    {|
      let answer () = 42 in
      print(answer())
    |}
  in
  eval code;
  [%expect
    {|
42
|}]
;;

let%expect_test "recursive zero-argument function bindings parse and run" =
  let code =
    {|
      let rec ping () = 7
      print(ping())
    |}
  in
  eval code;
  [%expect
    {|
7
|}]
;;

let%expect_test "unit pattern matches unit values" =
  let code =
    {|
      match () with
      | () -> print("unit")
    |}
  in
  eval code;
  [%expect
    {|
unit
|}]
;;

let%expect_test "record literals allow trailing semicolons" =
  let code =
    {|
      let r = { x = 1; y = 2; }
      print(r.x + r.y)
    |}
  in
  eval code;
  [%expect
    {|
3
|}]
;;

let%expect_test "record patterns allow trailing semicolons" =
  let code =
    {|
      match { x = 1; y = 2; } with
      | { x = x; y = y; } -> print(x + y)
    |}
  in
  eval code;
  [%expect
    {|
3
|}]
;;

let%expect_test "runtime closure arity mismatch is reported clearly" =
  let env = L.create_env () in
  let slot = Frame_env.Slot Frame_env.SInt in
  let body = node (L.REVarLoc { depth = 0; index = 0; slot }) in
  let lam = node (L.RELambda ([ "x" ], [ slot ], body)) in
  let bad_call = node (L.REApp (lam, [ node (L.REInt 1); node (L.REInt 2) ])) in
  (try
     ignore (E.finish_eval [] (E.eval_expr env [] bad_call));
     print_endline "unexpected success"
   with
   | L.Runtime_error err -> print_endline err.message
   | Failure msg -> print_endline msg);
  [%expect {| Function arity mismatch |}]
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
4
|}]
;;

let%expect_test "module can use outer bindings without exporting them" =
  let code =
    {|
      let x = 1
      module M = struct
        let y = x
      end
      print(M.y)
    |}
  in
  eval code;
  [%expect
    {|
1
|}]
;;

let%expect_test "module does not implicitly export outer bindings" =
  let code =
    {|
      let x = 1
      module M = struct
        let y = x
      end
      print(M.x)
    |}
  in
  (match eval_result code with
   | Ok () -> print_endline "unexpected success"
   | Error _ -> print_endline "type error");
  [%expect {| type error |}]
;;

let%expect_test "module self-reference resolves through explicit exports only" =
  let code =
    {|
      module M = struct
        let id x = x
        let call n = M.id(n)
      end
      print(M.call(7))
    |}
  in
  eval code;
  [%expect
    {|
7
|}]
;;

let%expect_test "top-level closure keeps lexical binding across rebinding" =
  let code =
    {|
      let x = 1
      let f () = x
      let x = 2
      print(f())
    |}
  in
  eval code;
  [%expect
    {|
1
|}]
;;

let%expect_test "module closure keeps lexical binding across rebinding" =
  let code =
    {|
      module M = struct
        let x = 1
        let f () = x
        let x = 2
      end
      print(M.f())
    |}
  in
  eval code;
  [%expect
    {|
1
|}]
;;

let%expect_test "open inside a module does not re-export imported names" =
  let code =
    {|
      module N = struct
        let x = 1
      end
      module M = struct
        open N
        let y = x
      end
      print(M.y)
      print(M.x)
    |}
  in
  (match eval_result code with
   | Ok () -> print_endline "unexpected success"
   | Error _ -> print_endline "type error");
  [%expect {| type error |}]
;;

let%expect_test "open shadowing is rejected in the full pipeline" =
  let code =
    {|
      let x = 1
      module M = struct
        let x = 2
      end
      open M
    |}
  in
  (match eval_result code with
   | Ok () -> print_endline "unexpected success"
   | Error _ -> print_endline "type error");
  [%expect {| type error |}]
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
7
|}]
;;

let%expect_test "deep tail recursion through if does not blow the stack" =
  let code =
    {|
      let rec countdown n =
        if n == 0 then 0 else countdown(n - 1)
      print(countdown(20000))
    |}
  in
  eval code;
  [%expect
    {|
0
|}]
;;

let%expect_test "tail recursion through match branch remains in tail position" =
  let code =
    {|
      let rec count n acc =
        match n with
        | 0 -> acc
        | _ -> count(n - 1, acc + 1)
      print(count(15000, 0))
    |}
  in
  eval code;
  [%expect
    {|
15000
|}]
;;

let%expect_test "non-tail function calls inside arithmetic still work" =
  let code =
    {|
      let inc x = x + 1
      let score x = inc(x) + inc(x)
      print(score(10))
    |}
  in
  eval code;
  [%expect
    {|
22
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
5
true
false
true
false
true
false
|}]
;;

let%expect_test "float arithmetic and comparisons use explicit dotted operators" =
  let code =
    {|
      print(1.5 +. 2.25)
      print(5.0 -. 1.5)
      print(2.0 *. 3.5)
      print(7.5 /. 2.5)
      print(-.1.25)
      print(1.0 <. 2.0)
      print(2.0 >=. 2.0)
    |}
  in
  eval code;
  [%expect
    {|
3.75
3.5
7.
3.
-1.25
true
true
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
Alice
31
123
|}]
;;

let%expect_test "record extension can change field type" =
  let code =
    {|
      let person = {name = "Alice"; age = 30}
      let label = { person with age = "old" }
      print(label.age)
      print(person.age)
    |}
  in
  eval code;
  [%expect
    {|
old
30
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
4
|}]
;;

let%expect_test "generic to_string builtin" =
  let code =
    {|
      print(to_string(42))
      print(to_string(true))
      print(to_string([1, 2]))
      print(to_string({a = 1; b = 2}))
      print(to_string(`Some(1)))
    |}
  in
  eval code;
  [%expect
    {|
42
true
[|1, 2|]
{ a = 1; b = 2 }
`Some(1)
|}]
;;

let%expect_test "polymorphic equality primitive handles records and variants" =
  let code =
    {|
      print({a = 1; b = 2} == {a = 1; b = 2})
      print(`Some(1) == `Some(1))
      print(`Some(1) != `Some(2))
    |}
  in
  eval code;
  [%expect
    {|
true
true
true
|}]
;;

let%expect_test "practical string builtins" =
  let code =
    {|
      print(string_length(""))
      print(string_length("chatml"))
      print(string_is_empty(""))
      print(string_is_empty("x"))
    |}
  in
  eval code;
  [%expect
    {|
0
6
true
false
|}]
;;

let%expect_test "array_copy and swap_ref builtins" =
  let code =
    {|
      let arr = [1, 2]
      let arr2 = array_copy(arr)
      arr[0] <- 99
      print(arr[0])
      print(arr2[0])

      let r = ref("old")
      print(swap_ref(r, "new"))
      print(!r)
    |}
  in
  eval code;
  [%expect
    {|
99
1
old
new
|}]
;;

let%expect_test "record_keys and variant_tag builtins" =
  let code =
    {|
      print(record_keys({ b = 2; a = 1; c = true }))
      print(variant_tag(`Done))
      print(variant_tag(`Some(1)))
    |}
  in
  eval code;
  [%expect
    {|
[|a, b, c|]
Done
Some
|}]
;;

let%expect_test "record_keys also works on module values" =
  let code =
    {|
      module M = struct
        let z = 1
        let a = 2
      end
      print(record_keys(M))
    |}
  in
  eval code;
  [%expect
    {|
[|a, z|]
|}]
;;

let%expect_test "fail builtin surfaces a runtime diagnostic" =
  let code =
    {|
      fail("boom")
    |}
  in
  (match run_result code with
   | Ok () -> print_endline "unexpected success"
   | Error (Chatml_resolver.Type_diagnostic diagnostic) ->
     print_endline (Chatml_typechecker.format_diagnostic code diagnostic)
   | Error (Chatml_resolver.Runtime_diagnostic err) -> print_endline err.message);
  [%expect {| boom |}]
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
15
2
4
6
|}]
;;

let%expect_test "state machine helpers can update nested task state" =
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

      let st0 =
        { autopilot = true
        ; task_index = 0
        ; tasks = [ mk_task("t1") ]
        }

      let st1 = step(st0, `Start)
      print(st1.tasks[0].attempts)
      print(st1.tasks[0].status)
      print(st1.autopilot)

      let st2 = step(st1, `Done)
      print(st2.tasks[0].status)
    |}
  in
  eval code;
  [%expect
    {|
1
`Running
true
`Done
|}]
;;

let%expect_test "state machine helper imported from module keeps outer state width" =
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

      let st0 =
        { autopilot = false
        ; task_index = 0
        ; tasks = [ mk_task("t1") ]
        }

      let st1 = bump_attempts(st0)
      print(st1.autopilot)
      print(st1.tasks[0].attempts)
    |}
  in
  eval code;
  [%expect
    {|
false
1
|}]
;;

let%expect_test "ill-typed programs do not execute" =
  let code =
    {|
      let x = 1
      let y = x + "bad"
      print("should not print")
    |}
  in
  (match eval_result code with
   | Ok () -> print_endline "unexpected success"
   | Error diagnostic ->
     print_endline (Chatml_typechecker.format_diagnostic code diagnostic));
  [%expect
    {|
    line 3, characters 14-23:
    3|    x + "bad"
          ^^^^^^^^^

    Type error: Cannot unify string with int
    |}]
;;
