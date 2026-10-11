open Core
module T = Chatml_typechecker
open T
module Eq = Chatml.Chatml_equality_requirement

(* A finite adjacency model supplies expected reachability and level minima;
   the oracle never reads the inferred graph or calls another checker walk. *)
type node =
  | Links of int list
  | Leaf of int * Eq.t option

let reachable nodes roots target =
  let rec walk pending seen =
    match pending with
    | [] -> seen
    | head :: rest when Set.mem seen head -> walk rest seen
    | head :: rest ->
      let seen = Set.add seen head in
      let children =
        if Option.value_map target ~default:false ~f:(Int.equal head)
        then []
        else (
          match nodes.(head) with
          | Links children -> children
          | Leaf _ -> [])
      in
      walk (List.rev_append children rest) seen
  in
  walk roots Int.Set.empty
;;

let test nodes roots target_index target_bound =
  let cells = Array.map nodes ~f:(fun _ -> ref (Bound Unit)) in
  Array.iteri nodes ~f:(fun index node ->
    cells.(index)
    := match node with
       | Leaf (level, equality) ->
         Free { name = "leaf" ^ Int.to_string index; level; equality }
       | Links children ->
         let args = List.map children ~f:(fun child -> Var cells.(child)) in
         let body =
           match index mod 4 with
           | 0 -> Mu ("a", Tuple (Mu ("a", Rec_var "a") :: args))
           | 1 ->
             Variant (Row (Env.singleton "Payload" (Variant_payload args), Empty_row))
           | 2 ->
             Field_receiver
               ( Shape_kind Chatml.Chatml_shape_kind.Record
               , Record (Row (Env.singleton "fields" (Fun (args, Unit)), Empty_row)) )
           | _ -> Con ("edge", [ Array (Ref (Tuple args)) ])
         in
         Bound body);
  let target =
    match target_index with
    | None ->
      ref
        (if target_bound
         then Bound Unit
         else Free { name = "target"; level = 2; equality = Some Eq.Value })
    | Some index -> cells.(index)
  in
  let before = Array.map cells ~f:(fun cell -> !cell) in
  let target_level =
    match !target with
    | Free { level; _ } -> Some level
    | Bound _ -> None
  in
  let reached = reachable nodes roots target_index in
  let expected_found =
    Option.value_map target_index ~default:false ~f:(Set.mem reached)
  in
  let actual =
    occurs target (Tuple (List.map roots ~f:(fun index -> Var cells.(index))))
  in
  assert (Bool.equal actual expected_found);
  Array.iteri cells ~f:(fun index cell ->
    match before.(index), !cell with
    | Bound _, _ -> assert (phys_equal before.(index) !cell)
    | Free old, Free now ->
      let lowered =
        match target_level with
        | Some level when Set.mem reached index -> Int.min old.level level
        | Some _ | None -> old.level
      in
      assert (now.level = lowered);
      assert (String.equal old.name now.name);
      assert (Option.equal Eq.equal old.equality now.equality)
    | _ -> assert false)
;;

let%test_unit "occurrence and level lowering agree with finite reachability" =
  let models =
    [ ( [| Links [ 1; 2 ]
         ; Links [ 3; 4 ]
         ; Links [ 4 ]
         ; Links [ 0; 4 ]
         ; Leaf (5, Some Eq.Value)
        |]
      , [ 0; 2 ]
      , None
      , false )
    ; ( [| Links [ 1; 2 ]
         ; Links [ 3; 4 ]
         ; Links [ 4 ]
         ; Links [ 0; 4 ]
         ; Leaf (2, Some Eq.Value)
        |]
      , [ 0; 2 ]
      , Some 4
      , false )
    ; [| Links [ 1 ]; Links [ 2 ]; Leaf (5, Some Eq.Value) |], [ 0 ], Some 1, true
    ; ( [| Links [ 1; 2 ]
         ; Leaf (0, None)
         ; Leaf (5, Some (Eq.Row (String.Set.of_list [ "x"; "y" ])))
         ; Leaf (8, None)
        |]
      , [ 0; 0 ]
      , None
      , false )
    ; [| Links [ 1; 2 ]; Leaf (0, None); Leaf (5, Some Eq.Value) |], [ 0 ], None, true
    ; [| Links [ 1 ]; Links [ 0 ]; Leaf (8, None) |], [ 0 ], Some 2, false
    ]
  in
  List.iter models ~f:(fun (nodes, roots, target, bound) -> test nodes roots target bound)
;;

let shared_dag depth =
  let cells = Array.init (depth + 1) ~f:(fun _ -> ref (Bound Unit)) in
  Array.iteri cells ~f:(fun index cell ->
    cell
    := if index = depth
       then Free { name = "weak"; level = 5; equality = Some Eq.Value }
       else Bound (Tuple [ Var cells.(index + 1); Var cells.(index + 1) ]));
  let target = ref (Free { name = "target"; level = 2; equality = None }) in
  cells, target
;;

let%test_unit "shared DAG occurrence visits are bounded and cancellation retains identity"
  =
  let depth = 16 in
  let bound = 8 * (depth + 1) * (depth + 1) in
  let count = ref 0 in
  let check cells target checkpoint =
    assert (not (occurs ~checkpoint target (Var cells.(0))));
    match !(cells.(depth)) with
    | Free { level = 2; equality = Some Eq.Value; _ } -> ()
    | _ -> assert false
  in
  let cells, target = shared_dag depth in
  check cells target (fun () ->
    incr count;
    if !count > bound then failwith "shared DAG traversal repeated paths");
  assert (!count > depth);
  let exception Interrupted of int ref in
  let marker = ref 0 in
  for stop = 1 to !count do
    let cells, target = shared_dag depth in
    let bound_before = Array.map cells ~f:(fun cell -> !cell) in
    let target_before = !target in
    let calls = ref 0 in
    let raised =
      try
        check cells target (fun () ->
          incr calls;
          if !calls = stop then raise (Interrupted marker));
        false
      with
      | Interrupted actual ->
        assert (phys_equal actual marker);
        true
    in
    assert raised;
    assert (phys_equal !target target_before);
    Array.iteri cells ~f:(fun index cell ->
      match bound_before.(index) with
      | Bound _ -> assert (phys_equal !cell bound_before.(index))
      | Free _ -> ());
    (* Partial level lowering is the established occurs effect. Reusing the
     graph completes the same minimum without altering links or equality. *)
    check cells target (fun () -> ())
  done
;;
