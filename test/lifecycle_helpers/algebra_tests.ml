open! Core
module L = Chatml.Chatml_lang

(* Independent oracle: classify the set of non-neutral surviving requests, rather
   than reproducing the library's sequential contributor fold. *)
type action =
  | None_
  | Continue
  | Wait_a
  | Wait_b
[@@deriving compare, equal, sexp]

let actions = [ None_; Continue; Wait_a; Wait_b ]

let name = function
  | None_ -> "none"
  | Continue -> "continue"
  | Wait_a -> "a"
  | Wait_b -> "b"
;;

let oracle values =
  match
    List.dedup_and_sort
      (List.filter values ~f:(fun x -> not (equal_action x None_)))
      ~compare:compare_action
  with
  | [] -> "none"
  | [ action ] -> name action
  | _ :: _ :: _ -> "conflict"
;;

let entry =
  {|let main input =
  let values = match input with | `Array(values) -> values | _ -> [] in
  let components = Array.mapi(values, fun index value ->
    let action = match value with
      | `String("continue") -> `Continue
      | `String("a") -> `Wait("a")
      | `String("b") -> `Wait("b")
      | _ -> `No_action
    in
    {id = "component-" ++ to_string(index); obligation = `Active; action = action}) in
  let answer = match Lifecycle.compose(components, String.equal) with
    | `Error(error) -> "conflict"
    | `Ok(plan) -> match plan.action with
      | `No_action -> "none"
      | `Continue -> "continue"
      | `Wait(wake) -> wake
  in Task.pure(`String(answer))|}
;;

let%test_unit
    "bounded independent action-set oracle includes every vector through length4"
  =
  let _, program = Support.build ~target:One_off_v1 ~entry ~example_modules:false in
  let rec vectors length =
    if Int.equal length 0
    then [ [] ]
    else
      List.concat_map actions ~f:(fun action ->
        List.map (vectors (length - 1)) ~f:(fun rest -> action :: rest))
  in
  List.iter (List.init 5 ~f:Fn.id) ~f:(fun length ->
    List.iter (vectors length) ~f:(fun values ->
      let input = `Array (List.map values ~f:(fun action -> `String (name action))) in
      let expected = `String (oracle values) in
      let actual = Support.run program input in
      if
        not
          (match actual, expected with
           | `String a, `String b -> String.equal a b
           | _ -> false)
      then
        raise_s [%sexp (values : action list), (expected : Jsonaf.t), (actual : Jsonaf.t)]))
;;

let boundary_entry =
  {|let component = fun id obligation action -> {id = id; obligation = obligation; action = action}
let describe = fun result -> match result with
  | `Ok(plan) -> `Object([
      {key = "order"; value = `Array(Array.map(plan.order, fun id -> `String(id)))},
      {key = "active"; value = `Array(Array.map(plan.active, fun id -> `String(id)))}])
  | `Error(error) -> `String(match error with
      | `Too_many_components -> "limit"
      | `Invalid_id(id) -> "invalid:" ++ id
      | `Duplicate_id(id) -> "duplicate:" ++ id
      | `Finished_with_action(id) -> "finished-action:" ++ id
      | `Conflict(pair) -> "conflict:" ++ pair.first ++ ":" ++ pair.second
      | `Active_components(ids) -> "active"
      | `Pending_action -> "pending")
let main input =
  let checked = fun components -> describe(Lifecycle.compose(components, String.equal)) in
  let peer = Lifecycle.compose([component("review", `Finished, `No_action),
                              component("counter", `Active, `No_action)], String.equal) in
  let root = match peer with
    | `Error(error) -> false
    | `Ok(plan) -> match Lifecycle.root_finish(plan, "explicit") with
      | `Error(error) -> true | `Ok(decision) -> false
  in
  let all_done = Lifecycle.compose([component("review", `Finished, `No_action)], String.equal) in
  let finished = match all_done with
    | `Error(error) -> false
    | `Ok(plan) -> match Lifecycle.root_finish(plan, "explicit") with
      | `Ok(decision) -> String.equal(decision, "explicit")
      | `Error(error) -> false
  in
  Task.pure(`Object([
    {key = "ordered"; value = checked([component("review", `Finished, `No_action), component("counter", `Active, `Continue)])},
    {key = "duplicate"; value = checked([component("review", `Finished, `No_action), component("review", `Active, `No_action)])},
    {key = "invalid"; value = checked([component("../escape", `Active, `No_action)])},
    {key = "too_many"; value = checked(Array.make(65, component("a", `Active, `No_action)))},
    {key = "finished_action"; value = checked([component("review", `Finished, `Continue)])},
    {key = "peer_blocks_finish"; value = `Bool(root)},
    {key = "explicit_finish"; value = `Bool(finished)}]))|}
;;

let%expect_test "authored order namespace rejection and peer obligations" =
  let _, program =
    Support.build ~target:One_off_v1 ~entry:boundary_entry ~example_modules:false
  in
  Support.run program `Null |> Jsonaf.to_string |> print_endline;
  [%expect
    {| {"ordered":{"order":["review","counter"],"active":["counter"]},"duplicate":"duplicate:review","invalid":"invalid:../escape","too_many":"limit","finished_action":"finished-action:review","peer_blocks_finish":true,"explicit_finish":true} |}]
;;
