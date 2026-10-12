open Core
open Support

let show source =
  match run source with
  | L.VInt n -> print_s [%sexp (n : int)]
  | L.VString s -> print_s [%sexp (s : string)]
  | _ -> failwith "Expected a scalar result"
;;

let%expect_test "generic state and regular recursive trees need no runtime type objects" =
  show
    {|type 'a completion = [ `Pending | `Succeeded('a) ]
    type ('value, 'error) outcome = [ `Ok('value) | `Error('error) ]
    type state = { name : string; job : (int, string) outcome completion }
    let current : state = { name = "compile"; job = `Pending }
    let finished = { current with job = `Succeeded(`Ok(42)) }
    let report : fun(state) -> string = fun state ->
      match state.job with
      | `Pending -> "pending"
      | `Succeeded(outcome) ->
        (match outcome with
         | `Ok(n) -> state.name ++ ": " ++ to_string(n)
         | `Error(message) -> message)
    let result = report(finished)|};
  show
    {|type 'a tree = [ `Leaf('a) | `Branch('a tree, 'a tree) ]
    let rec total : fun(int tree) -> int = fun tree ->
      match tree with
      | `Leaf(n) -> n
      | `Branch(left, right) -> total(left) + total(right)
    let result = total(`Branch(`Leaf(2), `Branch(`Leaf(3), `Leaf(5))))|};
  [%expect
    {|
    "compile: 42"
    10
    |}]
;;

let%expect_test
    "explicit function types distinguish no argument, unit, and returned functions"
  =
  show
    {|let zero : fun() -> int = fun () -> 1
    let one : fun(unit) -> int = fun ignored -> 2
    let make : fun(int) -> fun(unit) -> int = fun n -> fun ignored -> n
    let legacy : unit -> int = fun () -> 4
    let result = zero() + one(()) + make(3)(()) + legacy()|};
  [%expect {| 10 |}]
;;
