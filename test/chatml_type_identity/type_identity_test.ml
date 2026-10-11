open Core
module L = Chatml.Chatml_lang
module T = Chatml_typechecker

let run source =
  let env = Chatml_builtin_modules.create_default_env () in
  match
    Chatml_resolver.run_program env (Chatml.Chatml_parse.parse_program_exn source)
  with
  | Ok () -> L.find_var env "result" |> Option.value_exn
  | Error (Type_diagnostic diagnostic) -> failwith diagnostic.message
  | Error (Runtime_diagnostic diagnostic) -> failwith diagnostic.message
;;

let%expect_test "nested inference levels preserve distinct escaping type variables" =
  List.iter
    [ {|let split r = let value = r.x in fun other -> {first=value; second=other}
        let result = split({x=1})(true).second|}
    ; {|let split r = let value = r.x in fun other -> {first=value; second=other}
        let a = split({x=1; extra="ok"})(true)
        let b = split({x="text"; enabled=false})(2)
        let result = if a.second then b.second == 2 else false|}
    ; {|let keep r = let ignored = r.x in r
        let r = keep({x=1; enabled=true})
        let result = {r with x=2}.enabled|}
    ]
    ~f:(fun source ->
      match run source with
      | L.VBool value -> print_s [%sexp (value : bool)]
      | _ -> failwith "result shape changed");
  [%expect
    {|
    true
    true
    true
    |}]
;;

let%expect_test "level exit preserves identity without leaking generalization level" =
  let state = T.create_state () in
  T.enter_level state;
  let first = T.new_var state state.shared.current_lvl in
  T.exit_level state;
  T.enter_level state;
  let second = T.new_var state state.shared.current_lvl in
  T.exit_level state;
  [%test_eq: int] 0 state.shared.current_lvl;
  (match first, second with
   | T.Var { contents = Free (first, _) }, T.Var { contents = Free (second, _) } ->
     print_s [%sexp (not (String.equal first second) : bool)]
   | _ -> failwith "expected fresh unbound variables");
  [%expect {| true |}]
;;

let%expect_test "fresh naming preserves mutable and shared argument constraints" =
  List.iter
    [ {|let r = ref(fun x -> x)
        let n = (!r)(1)
        let result = (!r)(true)|}
    ; {|let same r = let value = r.x in value + r.x
        let result = same({x=true; extra=1})|}
    ; {|let first r = let value = r.x in value
        let result = first({extra=1})|}
    ]
    ~f:(fun source ->
      match T.check_program (Chatml.Chatml_parse.parse_program_exn source) with
      | Error _ -> print_endline "rejected"
      | Ok _ -> failwith "invalid program accepted");
  [%expect
    {|
    rejected
    rejected
    rejected
    |}]
;;
