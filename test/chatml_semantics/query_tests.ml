open Core
module A = Chatml_semantics
module Q = A.Snapshot
module F = A.Frozen
module H = Support

let offset body token = String.substr_index_exn body ~pattern:token

let selected_symbol snapshot input path body token =
  H.definition snapshot input path (offset body token)
;;

let scheme symbol = Q.Symbol.scheme symbol |> Option.value_exn

let binding snapshot context name =
  H.candidates snapshot context Value
  |> List.find_exn ~f:(fun binding ->
    String.equal (List.hd_exn (H.names snapshot [ binding ])) name)
;;

let function_parts snapshot root =
  match H.node snapshot (H.dereference snapshot root) with
  | Function ([ argument ], result) -> argument, result
  | _ -> failwith "expected unary function"
;;

let%expect_test "companion selection keeps implementation identity and the public view" =
  let contract = H.contract () in
  let entry = "open Catalog;; let result = identity(3)" in
  let implementation = "let identity x = x;; let private_value = 7" in
  let input =
    H.input
      contract
      ~interfaces:[ "catalog.chatml", "val identity : fun(int) -> int", [] ]
      [ "entry.chatml", entry, [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", implementation, []
      ]
  in
  let snapshot = H.complete contract input in
  let identity =
    selected_symbol snapshot input "catalog.chatml" implementation "identity"
  in
  let use = H.point snapshot input "entry.chatml" (offset entry "identity(3)") in
  let context = H.context snapshot use in
  let public = binding snapshot context "identity" in
  assert (Q.Symbol_id.equal (Q.Symbol.id identity) (Q.Binding.symbol public));
  let argument, result =
    function_parts snapshot (Q.Binding.scheme public |> Option.value_exn)
  in
  List.iter [ argument; result ] ~f:(fun id ->
    match H.node snapshot (H.dereference snapshot id) with
    | Int -> ()
    | _ -> failwith "public int specialization");
  let argument, result = function_parts snapshot (scheme identity) in
  (match H.node snapshot argument, H.node snapshot result with
   | Variable (Generalized first), Variable (Generalized second) ->
     assert (F.Quantifier_id.equal first.quantifier second.quantifier)
   | _ -> failwith "principal generalized definition");
  assert (Int.equal (List.length (Q.Symbol.specifications identity)) 1);
  let names = H.names snapshot (H.candidates snapshot context Value) in
  assert (not (List.mem names "private_value" ~equal:String.equal));
  let exports =
    Q.exports
      snapshot
      (A.Prepared.Module_id.of_implementation
         (A.Captured.id (H.source input "catalog.chatml")))
    |> Result.ok
    |> Option.value_exn
  in
  let public_names =
    match exports with
    | Available exports -> H.names snapshot (Q.Exports.values exports)
    | Unavailable _ -> failwith "checked exports"
  in
  print_s
    [%sexp
      { principal = ("forall a. fun(a) -> a" : string)
      ; public = ("fun(int) -> int" : string)
      ; public_names : string list
      ; same_definition = (true : bool)
      }];
  [%expect
    {|
    ((principal "forall a. fun(a) -> a") (public "fun(int) -> int")
     (public_names (identity)) (same_definition true))
    |}]
;;

let%test_unit "all importers and host requirements settle before immutable publication" =
  let body = "let cells = [];; let poison = 1 / 0" in
  let files =
    [ "entry.chatml", "let chosen = Catalog.cells", [ "Catalog", "catalog.chatml" ]
    ; "catalog.chatml", body, []
    ]
  in
  let before_contract = H.contract () in
  let before_input = H.input before_contract files in
  let before = H.complete before_contract before_input in
  let before_cells = selected_symbol before before_input "catalog.chatml" body "cells" in
  let element snapshot symbol =
    match H.node snapshot (scheme symbol) with
    | Array element -> H.dereference snapshot element
    | _ -> failwith "actual array"
  in
  let before_element = element before before_cells in
  (match H.node before before_element with
   | Variable (Unquantified _) -> ()
   | _ -> failwith "weak cell before constraints");
  let contract =
    H.contract ~requirements:[ "chosen", Chatml.Chatml_builtin_spec.TArray TInt ] ()
  in
  let input =
    H.input
      contract
      [ ( "entry.chatml"
        , "let dependency = Setter.ready;; let chosen = Catalog.cells"
        , [ "Catalog", "catalog.chatml"; "Setter", "setter.chatml" ] )
      ; ( "setter.chatml"
        , "let ready = Array.set(Catalog.cells, 0, 3)"
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", body, []
      ]
  in
  let after = H.complete contract input in
  let cells = selected_symbol after input "catalog.chatml" body "cells" in
  (match H.node after (element after cells) with
   | Int -> ()
   | _ -> failwith "all constraints settled");
  (match H.node before before_element with
   | Variable (Unquantified _) -> ()
   | _ -> failwith "prior snapshot mutated");
  assert (Option.is_none (F.find (Q.types after) before_element));
  assert (Int.equal (List.length (Q.host_requirements after)) 1);
  let paths =
    List.map (Q.runtime_order after) ~f:(fun id ->
      A.Captured.Id.relative_path (A.Prepared.Module_id.implementation id))
  in
  [%test_eq: string list] paths [ "catalog.chatml"; "setter.chatml"; "entry.chatml" ]
;;

let%test_unit "actual namespace contexts preserve shadows and reject unsupported gaps" =
  List.iter
    [ "type chosen = int;; let old = 1;; type later = chosen;; let old = \"new\";; let \
       result = old"
    ; "let old = 1;; type chosen = int;; let old = \"new\";; type later = chosen;; let \
       result = old"
    ]
    ~f:(fun body ->
      let contract = H.contract () in
      let input = H.input contract [ "entry.chatml", body, [] ] in
      let snapshot = H.complete contract input in
      let context =
        H.context
          snapshot
          (H.point snapshot input "entry.chatml" (offset body "result = old" + 9))
      in
      [%test_eq: string list]
        (H.names snapshot (H.candidates snapshot context Type))
        [ "chosen"; "json"; "later" ];
      assert (
        List.mem
          (H.names snapshot (H.candidates snapshot context Value))
          "old"
          ~equal:String.equal);
      assert (List.is_empty (H.candidates snapshot context Type_parameter)));
  let body =
    "let outer = 1;; let closure = fun argument -> (  argument + outer);; let result = \
     closure(2)"
  in
  let contract = H.contract () in
  let input = H.input contract [ "entry.chatml", body, [] ] in
  let snapshot = H.complete contract input in
  let gap = H.point snapshot input "entry.chatml" (offset body "(  argument" + 1) in
  (match
     Q.context_at snapshot gap ~limits:(H.query_limits ()) ~checkpoint:H.checkpoint
   with
   | Ok (Missing No_recorded_context) -> ()
   | _ -> failwith "whitespace cannot certify outer context");
  let actual =
    H.context
      snapshot
      (H.point snapshot input "entry.chatml" (offset body "argument + outer"))
  in
  assert (
    List.mem
      (H.names snapshot (H.candidates snapshot actual Value))
      "argument"
      ~equal:String.equal);
  let before = H.context snapshot (H.point snapshot input "entry.chatml" 0) in
  assert (
    not
      (List.mem
         (H.names snapshot (H.candidates snapshot before Value))
         "outer"
         ~equal:String.equal));
  let final =
    H.context snapshot (H.point snapshot input "entry.chatml" (String.length body))
  in
  assert (
    List.mem
      (H.names snapshot (H.candidates snapshot final Value))
      "result"
      ~equal:String.equal);
  let empty_input = H.input contract [ "entry.chatml", "", [] ] in
  let empty = H.complete contract empty_input in
  ignore (H.context empty (H.point empty empty_input "entry.chatml" 0) : Q.Context.t)
;;

let%test_unit
    "recursive alias syntax, parameters and references retain real lexical identities"
  =
  let body =
    "type 'a a = [ `Leaf('a) | `Branch('a a, 'a a) ];; let keep : fun(int a) -> int a = \
     fun value -> value"
  in
  let contract = H.contract () in
  let input = H.input contract [ "entry.chatml", body, [] ] in
  let snapshot = H.complete contract input in
  let alias = H.definition snapshot input "entry.chatml" (offset body "a =") in
  let alias_fact =
    Q.alias snapshot (Q.Symbol.id alias)
    |> Result.ok
    |> Option.value_exn
    |> Option.value_exn
  in
  let parameter = List.hd_exn (Q.Alias.parameters alias_fact) in
  assert (not (Q.Symbol_id.equal parameter (Q.Symbol.id alias)));
  (match H.node snapshot (Q.Alias.template alias_fact) with
   | Recursive _ -> ()
   | _ -> failwith "whole recursive template");
  let point = H.point snapshot input "entry.chatml" (offset body "Leaf('a)" + 5) in
  let references = H.references snapshot point in
  assert (
    List.exists references ~f:(fun reference ->
      match Q.Reference.target reference with
      | Symbol id -> Q.Symbol_id.equal id parameter
      | _ -> false));
  let context = H.context snapshot point in
  [%test_eq: string list]
    (H.names snapshot (H.candidates snapshot context Type_parameter))
    [ "a" ];
  [%test_eq: string list]
    (H.names snapshot (H.candidates snapshot context Type))
    [ "a"; "json" ]
;;

let%test_unit "opened companion module candidates retain restricted nested members" =
  let contract = H.contract () in
  let entry = "open Catalog;; let result = Nested.item" in
  let body = "module Nested = struct let item = 3;; let private_item = true end" in
  let input =
    H.input
      contract
      ~interfaces:[ "catalog.chatml", "module Nested : sig val item : int end", [] ]
      [ "entry.chatml", entry, [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", body, []
      ]
  in
  let snapshot = H.complete contract input in
  let context =
    H.context
      snapshot
      (H.point snapshot input "entry.chatml" (offset entry "Nested.item"))
  in
  let public = binding snapshot context "Nested" in
  let principal = H.definition snapshot input "catalog.chatml" (offset body "Nested") in
  assert (Q.Symbol_id.equal (Q.Binding.symbol public) (Q.Symbol.id principal));
  (match Q.Binding.values public with
   | Available members -> [%test_eq: string list] (H.names snapshot members) [ "item" ]
   | Unavailable _ -> failwith "actual restricted nominal members");
  let row =
    match H.node snapshot (H.dereference snapshot (scheme principal)) with
    | Module row -> row
    | _ -> failwith "principal module"
  in
  (match H.node snapshot (H.dereference snapshot row) with
   | Row (fields, _) ->
     assert (List.mem (List.map fields ~f:fst) "private_item" ~equal:String.equal)
   | _ -> failwith "principal implementation row");
  assert (
    not (Option.equal F.Id.equal (Q.Binding.scheme public) (Q.Symbol.scheme principal)))
;;

let%test_unit
    "implicit specification parameter candidates retain the finalized grouped root"
  =
  let contract = H.contract () in
  let interface = "val identity : fun('a) -> 'a" in
  let input =
    H.input
      contract
      ~interfaces:[ "catalog.chatml", interface, [] ]
      [ ( "entry.chatml"
        , "let result = Catalog.identity(1)"
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", "let identity value = value", []
      ]
  in
  let snapshot = H.complete contract input in
  let context =
    H.context snapshot (H.point snapshot input "catalog.chatmli" (offset interface "'a"))
  in
  let parameters = H.candidates snapshot context Type_parameter in
  [%test_eq: string list] (H.names snapshot parameters) [ "a" ];
  let parameter = List.hd_exn parameters in
  let principal =
    Q.symbol snapshot (Q.Binding.symbol parameter) |> Result.ok |> Option.value_exn
  in
  assert (Option.is_some (Q.Binding.scheme parameter));
  assert (Option.equal F.Id.equal (Q.Binding.scheme parameter) (Q.Symbol.scheme principal));
  match H.node snapshot (Q.Binding.scheme parameter |> Option.value_exn) with
  | Variable (Generalized _) -> ()
  | _ -> failwith "actual specification quantifier"
;;
