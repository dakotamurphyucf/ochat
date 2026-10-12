open Core
module T = Chatml_typechecker
module S = T.Type_snapshot
module N = S.Node
module Equality = Chatml.Chatml_equality_requirement

let limits ?(nodes = 100_000) ?(edges = 100_000) ?(labels = 100_000) () =
  S.Limits.create ~max_nodes:nodes ~max_edges:edges ~max_label_bytes:labels
  |> Or_error.ok_exn
;;

let freeze ?(checkpoint = fun () -> ()) ?(limits = limits ()) groups =
  S.freeze (List.map groups ~f:S.Input_group.create) ~limits ~checkpoint
;;

let successful = function
  | Ok value -> value
  | Error _ -> failwith "unexpected fixture error"
;;

let frozen groups = freeze groups |> successful
let node snapshot id = S.find snapshot id |> Option.value_exn
let first_root snapshot = List.hd_exn (List.hd_exn (S.roots snapshot))

let quantifier snapshot id =
  match node snapshot id with
  | N.Variable (Generalized { quantifier; _ }) -> quantifier
  | _ -> failwith "expected quantified variable"
;;

let generic name = T.Generic { name; equality = Some Equality.Value }

let%expect_test "quantifier groups are explicit; equal printed names do not join schemes" =
  let snapshot = frozen [ [ generic "a"; generic "a" ]; [ generic "a" ] ] in
  let left, right, unrelated =
    match S.roots snapshot with
    | [ [ left; right ]; [ unrelated ] ] -> left, right, unrelated
    | _ -> assert false
  in
  print_s
    [%sexp
      (( "same grouped quantifier"
       , S.Quantifier_id.equal (quantifier snapshot left) (quantifier snapshot right) )
       : string * bool)
    , (( "independent scheme quantifier"
       , S.Quantifier_id.equal (quantifier snapshot left) (quantifier snapshot unrelated)
       )
       : string * bool)];
  [%expect
    {| (("same grouped quantifier" true) ("independent scheme quantifier" false)) |}]
;;

type shape =
  | Int
  | Bool
  | Unit
  | Function of shape list * shape
  | Record of shape
  | Row of (string * shape) list * shape
  | Variant of (string * shape list) list
  | End
[@@deriving compare, equal, sexp_of]

let rec raw = function
  | Int -> T.TInt
  | Bool -> T.Boolean
  | Unit -> T.Unit
  | Function (parameters, result) -> T.Fun (List.map parameters ~f:raw, raw result)
  | Record row -> T.Record (raw row)
  | Row (fields, tail) ->
    T.Row (T.Env.of_list (List.map fields ~f:(fun (label, ty) -> label, raw ty)), raw tail)
  | Variant cases ->
    T.Variant
      (T.Row
         ( T.Env.of_list
             (List.map cases ~f:(fun (tag, arguments) ->
                tag, T.Variant_payload (List.map arguments ~f:raw)))
         , T.Empty_row ))
  | End -> T.Empty_row
;;

let rec read snapshot id =
  match node snapshot id with
  | N.Int -> Int
  | Bool -> Bool
  | Unit -> Unit
  | Function (parameters, result) ->
    Function (List.map parameters ~f:(read snapshot), read snapshot result)
  | Record row -> Record (read snapshot row)
  | Row (fields, tail) ->
    Row
      (List.map fields ~f:(fun (label, id) -> label, read snapshot id), read snapshot tail)
  | Variant row ->
    (match node snapshot row with
     | Row (cases, tail) ->
       assert (
         match node snapshot tail with
         | Empty_row -> true
         | _ -> false);
       Variant
         (List.map cases ~f:(fun (tag, payload) ->
            match node snapshot payload with
            | Variant_payload arguments -> tag, List.map arguments ~f:(read snapshot)
            | _ -> assert false))
     | _ -> assert false)
  | Empty_row -> End
  | _ -> failwith "outside bounded structural model"
;;

let%test_unit "bounded independent type model retains overlays and both argument arities" =
  (* Independent symbolic family: 2 atoms × 3 call arities × 3 constructor
     arities × 2 row-overlay choices. This is not universal type soundness. *)
  List.iter [ Int; Bool ] ~f:(fun atom ->
    List.iter [ 0; 1; 2 ] ~f:(fun call_arity ->
      List.iter [ 0; 1; 2 ] ~f:(fun payload_arity ->
        List.iter [ false; true ] ~f:(fun overlay ->
          let tail = if overlay then Row ([ "x", Bool ], End) else End in
          let expected =
            Function
              ( List.init call_arity ~f:(fun _ -> atom)
              , Record
                  (Row
                     ( [ ( "choice"
                         , Variant [ "Done", List.init payload_arity ~f:(fun _ -> Unit) ]
                         )
                       ; "x", atom
                       ]
                     , tail )) )
          in
          let snapshot = frozen [ [ raw expected ] ] in
          [%test_eq: shape] (read snapshot (first_root snapshot)) expected))))
;;

let%test_unit "free identity, finite links, immutable old results and snapshot ownership" =
  let first = ref (T.Free { name = "same"; level = 0; equality = None }) in
  let other = ref (T.Free { name = "same"; level = 1; equality = None }) in
  let old = frozen [ [ T.Var first ]; [ T.Var first; T.Var other ] ] in
  let a, b, c =
    match S.roots old with
    | [ [ a ]; [ b; c ] ] -> a, b, c
    | _ -> assert false
  in
  assert (S.Id.equal a b && not (S.Id.equal a c));
  let cycle = ref (T.Free { name = "cycle"; level = 0; equality = None }) in
  cycle := T.Bound (T.Record (T.Row (T.Env.singleton "again" (T.Var cycle), T.Empty_row)));
  let snapshot = frozen [ [ T.Var cycle; T.Var cycle ] ] in
  let root = first_root snapshot in
  let row =
    match node snapshot root with
    | Bound_link target ->
      (match node snapshot target with
       | Record row -> row
       | _ -> assert false)
    | _ -> assert false
  in
  (match node snapshot row with
   | Row ([ ("again", target) ], _) -> assert (S.Id.equal root target)
   | _ -> assert false);
  let before = N.sexp_of_t (node snapshot root) in
  cycle := T.Bound T.TInt;
  first := T.Bound T.String;
  let fresh = frozen [ [ T.Var cycle; T.Var first ] ] in
  assert (Option.is_none (S.find fresh root));
  assert (not (S.Id.equal root (first_root fresh)));
  [%test_eq: Sexp.t] before (N.sexp_of_t (node snapshot root));
  match node old a with
  | Variable (Unquantified _) -> ()
  | _ -> assert false
;;

let recursive_projection () =
  let cell = ref (T.Free { name = "recursive"; level = 0; equality = None }) in
  let inner = T.Mu ("r", T.Var cell) in
  cell
  := T.Bound (T.Row (T.Env.of_list [ "again", inner; "x", T.Rec_var "r" ], T.Empty_row));
  cell, T.Mu ("r", T.Var cell)
;;

let%test_unit
    "bound cells resolve in their lexical recursive scope, including shadowing cycles"
  =
  let _, ty = recursive_projection () in
  let snapshot = frozen [ [ ty ] ] in
  let outer, link =
    match node snapshot (first_root snapshot) with
    | Recursive (binder, body) -> binder, body
    | _ -> assert false
  in
  let row link =
    match node snapshot link with
    | Bound_link row -> row
    | _ -> assert false
  in
  let row_fields row =
    match node snapshot row with
    | Row (fields, _) -> fields
    | _ -> assert false
  in
  let fields = row_fields (row link) in
  let inner, inner_link =
    match node snapshot (List.Assoc.find_exn fields ~equal:String.equal "again") with
    | Recursive (binder, body) -> binder, body
    | _ -> assert false
  in
  assert (not (S.Recursive_binder_id.equal outer inner));
  let check_x fields expected =
    match node snapshot (List.Assoc.find_exn fields ~equal:String.equal "x") with
    | Recursive_variable binder -> assert (S.Recursive_binder_id.equal expected binder)
    | _ -> assert false
  in
  check_x fields outer;
  let inner_fields = row_fields (row inner_link) in
  check_x inner_fields inner;
  (match node snapshot (List.Assoc.find_exn inner_fields ~equal:String.equal "again") with
   | Recursive (binder, link) ->
     assert (S.Recursive_binder_id.equal inner binder && S.Id.equal inner_link link)
   | _ -> assert false);
  assert (S.node_count snapshot < 30)
;;

let%test_unit "limits include equality labels; every observed checkpoint preserves input" =
  let name = String.init 5000 ~f:(fun i -> if Int.equal (i mod 2) 0 then 'x' else 'y') in
  let cell =
    ref
      (T.Free
         { name = "weak"
         ; level = 0
         ; equality = Some (Equality.Row (String.Set.singleton name))
         })
  in
  let before = !cell in
  let groups = [ [ T.Var cell; T.Fun ([], T.Var cell) ]; [ generic "a" ] ] in
  let polls = ref 0 in
  let snapshot = freeze groups ~checkpoint:(fun () -> incr polls) |> successful in
  assert (!polls > 0);
  let exact =
    limits
      ~nodes:(S.node_count snapshot)
      ~edges:(S.edge_count snapshot)
      ~labels:(S.label_bytes snapshot)
      ()
  in
  assert (Result.is_ok (freeze groups ~limits:exact));
  List.iter
    [ limits ~nodes:(S.node_count snapshot - 1) ()
    ; limits ~edges:(S.edge_count snapshot - 1) ()
    ; limits ~labels:(S.label_bytes snapshot - 1) ()
    ]
    ~f:(fun limits -> assert (Result.is_error (freeze groups ~limits)));
  for at = 1 to !polls do
    let seen = ref 0 in
    let cancelled = T.Type_error "checkpoint collision" in
    (try
       ignore
         (freeze groups ~checkpoint:(fun () ->
            incr seen;
            if Int.equal !seen at then raise cancelled));
       assert false
     with
     | exn -> assert (phys_equal exn cancelled));
    assert (phys_equal before !cell)
  done;
  assert (Result.is_ok (freeze groups));
  List.iter [ 0; -1 ] ~f:(fun nodes ->
    assert (
      Result.is_error (S.Limits.create ~max_nodes:nodes ~max_edges:1 ~max_label_bytes:1)));
  assert (
    Result.is_error (freeze (List.init 4 ~f:(fun _ -> [])) ~limits:(limits ~nodes:3 ())))
;;

let%test_unit
    "actual checker recursive alias and builtin Json types freeze without execution"
  =
  let source =
    "type 'a tree = [`Leaf('a) | `Branch('a tree, 'a tree)]\n\
     let identity x = x\n\
     let value : int tree = `Leaf(1)\n\
     let json = Json.parse(\"null\")\n\
     let never_run = 1 / 0"
  in
  let program = Chatml.Chatml_parse.parse_program source |> successful in
  let checked = T.check_program program |> successful in
  let roots =
    Hashtbl.fold checked.T.span_types ~init:[] ~f:(fun ~key:_ ~data roots ->
      data :: roots)
  in
  assert (not (List.is_empty roots));
  let snapshot = frozen (List.map roots ~f:(fun root -> [ root ])) in
  assert (S.node_count snapshot > List.length roots)
;;

let%test_unit "bound schemes preserve group quantification and global free children" =
  let free = ref (T.Free { name = "a"; level = 0; equality = None }) in
  let bound = ref (T.Bound (T.Fun ([ generic "a" ], T.Var free))) in
  let snapshot = frozen [ [ T.Var bound; T.Var bound ]; [ T.Var bound ] ] in
  let first, repeated, independent =
    match S.roots snapshot with
    | [ [ first; repeated ]; [ independent ] ] -> first, repeated, independent
    | _ -> assert false
  in
  assert (S.Id.equal first repeated && not (S.Id.equal first independent));
  let components link =
    match node snapshot link with
    | Bound_link target ->
      (match node snapshot target with
       | Function ([ parameter ], result) -> quantifier snapshot parameter, result
       | _ -> assert false)
    | _ -> assert false
  in
  let first_quantifier, first_result = components first in
  let independent_quantifier, independent_result = components independent in
  assert (not (S.Quantifier_id.equal first_quantifier independent_quantifier));
  assert (S.Id.equal first_result independent_result)
;;

let%test_unit
    "deep input traversal is iterative and a small budget stops before unfolding"
  =
  let depth = 20_000 in
  let raw =
    List.fold (List.init depth ~f:Fn.id) ~init:T.TInt ~f:(fun child _ -> T.Ref child)
  in
  let polls = ref 0 in
  let snapshot = freeze [ [ raw ] ] ~checkpoint:(fun () -> incr polls) |> successful in
  assert (!polls > depth);
  let cursor = ref (first_root snapshot) in
  for _ = 1 to depth do
    match node snapshot !cursor with
    | Reference child -> cursor := child
    | _ -> assert false
  done;
  (match node snapshot !cursor with
   | Int -> ()
   | _ -> assert false);
  let stopped_polls = ref 0 in
  (match
     freeze [ [ raw ] ] ~limits:(limits ~nodes:5 ()) ~checkpoint:(fun () ->
       incr stopped_polls)
   with
   | Error S.Error.Node_limit -> ()
   | Ok _ | Error _ -> assert false);
  assert (!stopped_polls < 100);
  match freeze [ [ T.Rec_var "unbound" ] ] with
  | Error (S.Error.Unbound_recursive_variable "unbound") -> ()
  | Ok _ | Error _ -> assert false
;;
