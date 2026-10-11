open Core
module F = Fixture
module S = Chatml_source
module P = Chatml_prepared_program
module G = Chatml_dependency_graph
module Session = Chatml_dependency_session
module C = Chatml_configured_check
module T = Chatml_typechecker
module L = Chatml.Chatml_lang
module Lookup = Chatml_configured_lookup

let checkpoint () = ()

let catalog body =
  F.input
    [ "entry.chatml", body, [ "Catalog", "catalog.chatml"; "Again", "catalog.chatml" ]
    ; "catalog.chatml", "type token = int let value = 3 let identity x = x", []
    ; "unused.chatml", "let broken = absent", [ "Unused", "unused.chatml" ]
    ]
;;

let runtime result =
  Session.graph result
  |> fun graph -> G.runtime_dependency_order graph ~checkpoint |> F.paths
;;

let value_edges result =
  G.edges (Session.graph result) |> List.filter ~f:G.Edge.is_runtime_dependency
;;

let%test_unit
    "real checker compiler references match the accepted value-only runtime convention"
  =
  List.iter
    [ "type chosen = Catalog.token let total : chosen = 1", false
    ; "open Catalog", false
    ; "open Catalog let total : token = 1", false
    ; "open Catalog let total = value", true
    ; "let total = Catalog.value", true
    ; "let unused () = Catalog.value", true
    ; "let saved = Catalog", true
    ]
    ~f:(fun (body, needs_value) ->
      let result = F.checked (catalog body) |> F.ok in
      [%test_eq: string list]
        (F.paths (G.nodes (Session.graph result)))
        [ "catalog.chatml"; "entry.chatml" ];
      [%test_eq: string list]
        (runtime result)
        (if needs_value then [ "catalog.chatml"; "entry.chatml" ] else [ "entry.chatml" ]);
      [%test_eq: bool] (not (List.is_empty (value_edges result))) needs_value)
;;

let%test_unit
    "type-only checked subtrees do not leak their own value dependencies into runtime \
     selection"
  =
  let input =
    F.input
      [ "entry.chatml", "type chosen = Types.token", [ "Types", "types.chatml" ]
      ; ( "types.chatml"
        , "type token = int let unused = Values.value"
        , [ "Values", "values.chatml" ] )
      ; "values.chatml", "let value = 7", []
      ]
  in
  let result = F.checked input |> F.ok in
  [%test_eq: string list]
    (F.paths (G.dependency_order (Session.graph result)))
    [ "values.chatml"; "types.chatml"; "entry.chatml" ];
  [%test_eq: string list] (runtime result) [ "entry.chatml" ];
  [%test_eq: int] (List.length (value_edges result)) 1
;;

let%test_unit
    "nested imported opens preserve paths, raw polymorphism and declaration-order \
     shadowing"
  =
  let input =
    F.input
      [ ( "entry.chatml"
        , {|open Catalog open Nested
      let before () = identity(1)
      let also = identity(true)
      let identity x = x + 1
      let after () = identity(2)|}
        , [ "Catalog", "catalog.chatml" ] )
      ; ( "catalog.chatml"
        , "module Nested = struct type token = int let identity x = x end"
        , [] )
      ]
  in
  let result = F.checked input |> F.ok in
  let paths = List.map (value_edges result) ~f:G.Edge.member_path in
  [%test_eq: string list list]
    paths
    [ [ "Nested"; "identity" ]; [ "Nested"; "identity" ] ];
  let unit = Session.entry result in
  let annotations = C.Checked_unit.annotations unit in
  let parsed =
    Chatml.Chatml_parse.parse_captured (F.source input "entry.chatml") ~checkpoint |> F.ok
  in
  let opens = List.take (Chatml.Chatml_parse.Unit.program parsed).stmts 2 in
  List.iter opens ~f:(fun node ->
    match T.checked_configured_annotation annotations node.span with
    | Some (Inert_open _) -> ()
    | _ -> failwith "configured open lost its inert annotation");
  let location =
    S.locate (F.source input "catalog.chatml") ~start_byte:0 ~end_byte:0 ~checkpoint
    |> Or_error.ok_exn
  in
  assert (Option.is_none (T.checked_location_configured_annotation annotations location))
;;

let%test_unit
    "aliases freeze their own configured roots without replaying stale callbacks"
  =
  let input =
    F.input
      [ ( "entry.chatml"
        , {|let answer : bool Types.boxed = {item = true; token = 4}|}
        , [ "Types", "types.chatml" ] )
      ; ( "types.chatml"
        , {|type 'a boxed = {item : 'a; token : Catalog.token}
      module Catalog = struct type token = bool end
      let proof : int boxed = {item = 1; token = 2}|}
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", "type token = int", []
      ]
  in
  let result = F.checked input |> F.ok in
  [%test_eq: string list] (runtime result) [ "entry.chatml" ];
  [%test_eq: string list]
    (F.paths (G.nodes (Session.graph result)))
    [ "catalog.chatml"; "types.chatml"; "entry.chatml" ]
;;

let%test_unit
    "imported units use their own configured scope and cannot capture importer locals"
  =
  let input =
    F.input
      [ "entry.chatml", "let total = Catalog.value", [ "Catalog", "outer.chatml" ]
      ; "outer.chatml", "let value = Catalog.value", [ "Catalog", "inner.chatml" ]
      ; "inner.chatml", "let value = 3", []
      ]
  in
  let result = F.checked input |> F.ok in
  [%test_eq: string list]
    (runtime result)
    [ "inner.chatml"; "outer.chatml"; "entry.chatml" ];
  let input =
    F.input
      [ ( "entry.chatml"
        , "let secret = 3 let total = Catalog.value"
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", "let value = secret", []
      ]
  in
  match F.checked input with
  | Ok _ -> failwith "importer lexical value escaped into dependency"
  | Error error ->
    let site = Option.value_exn error.site in
    [%test_eq: string]
      (S.Id.relative_path (S.id (Chatml_diagnostic.Site.source site)))
      "catalog.chatml"
;;

let%test_unit
    "configured opens preserve weak cells; local shadowing and ordinary inline opens \
     keep existing behavior"
  =
  let positive =
    catalog "open Catalog let int_value = identity(1) let bool_value = identity(true)"
  in
  ignore (F.checked positive |> F.ok : C.Checked_unit.t Session.t);
  let negative =
    F.input
      [ ( "entry.chatml"
        , "open Catalog let first = Array.set(cells, 0, 1) let second = Array.set(cells, \
           0, true)"
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", "let cells = []", []
      ]
  in
  assert (Result.is_error (F.checked negative));
  let ordinary =
    catalog "module Catalog = struct let value = 8 end open Catalog let total = value"
  in
  let result = F.checked ordinary |> F.ok in
  [%test_eq: string list] (F.paths (G.nodes (Session.graph result))) [ "entry.chatml" ];
  let parsed =
    Chatml.Chatml_parse.parse_captured (F.source ordinary "entry.chatml") ~checkpoint
    |> F.ok
  in
  let open_node = List.nth_exn (Chatml.Chatml_parse.Unit.program parsed).stmts 1 in
  assert (
    Option.is_none
      (T.checked_configured_annotation
         (C.Checked_unit.annotations (Session.entry result))
         open_node.span))
;;

let%test_unit
    "inline signatures resolve configured types through the same owned alias path"
  =
  let input =
    catalog "module M : sig val value : Catalog.token end = struct let value = 1 end"
  in
  let result = F.checked input |> F.ok in
  assert (
    List.exists
      (G.edges (Session.graph result))
      ~f:(fun edge -> Lookup.Reason.equal (G.Edge.reason edge) Qualified_type));
  [%test_eq: string list] (runtime result) [ "entry.chatml" ]
;;

let%expect_test
    "cycles report the actual closing owned reference; availability remains inert"
  =
  let input =
    F.input
      [ "entry.chatml", "let result = A.value", [ "A", "a.chatml" ]
      ; "a.chatml", "let value = B.value", [ "B", "b.chatml" ]
      ; "b.chatml", "let value = A.value", [ "A", "a.chatml" ]
      ]
  in
  match F.checked input with
  | Ok _ -> failwith "cycle admitted"
  | Error error ->
    let site = Option.value_exn error.site in
    print_s
      [%sexp
        (error.message : string)
      , (S.Id.relative_path (S.id (Chatml_diagnostic.Site.source site)) : string)];
    [%expect {| ("Actual configured module dependency cycle" b.chatml) |}]
;;

let%test_unit
    "independent units check at root level so mutable exports cannot be generalized by \
     an importing let or closure"
  =
  List.iter
    [ "open Catalog", "cells"
    ; "let saved = Catalog", "saved.cells"
    ; "let get () = Catalog.cells", "get()"
    ; "let (|cells, _|) = (|Catalog.cells, ()|)", "cells"
    ; "", "Catalog.cells"
    ]
    ~f:(fun (prefix, cell) ->
      List.iter
        [ "2", true; "true", false ]
        ~f:(fun (second, expected) ->
          let body =
            prefix
            ^ " let first = Array.set("
            ^ cell
            ^ ", 0, 1) let second = Array.set("
            ^ cell
            ^ ", 0, "
            ^ second
            ^ ")"
          in
          let input =
            F.input
              [ "entry.chatml", body, [ "Catalog", "catalog.chatml" ]
              ; "catalog.chatml", "let cells = []", []
              ]
          in
          match F.checked input, expected with
          | Ok _, true -> ()
          | Error error, false ->
            assert (String.is_substring error.message ~substring:"Cannot unify")
          | Error error, true -> failwith error.message
          | Ok _, false -> failwith "mutable export gained importer polymorphism"))
;;

let%test_unit
    "each actual qualified type occurrence has one located canonical member edge"
  =
  List.iter
    [ "type selected = Catalog.token", [ "Catalog" ]
    ; ( "type selected = {left : Catalog.token; right : Again.token}"
      , [ "Catalog"; "Again" ] )
    ]
    ~f:(fun (body, names) ->
      let result = F.checked (catalog body) |> F.ok in
      let edges = G.edges (Session.graph result) in
      [%test_eq: string list]
        (List.map edges ~f:G.Edge.name |> List.sort ~compare:String.compare)
        (List.sort names ~compare:String.compare);
      List.iter edges ~f:(fun edge ->
        [%test_eq: string list] (G.Edge.member_path edge) [ "token" ];
        [%test_eq: Lookup.Reason.t] (G.Edge.reason edge) Qualified_type))
;;

let%test_unit
    "missing configured names and value/type open collisions retain owned source and \
     configuration sites"
  =
  List.iter
    [ "let total = Absent.value"
    ; "let value = 0 open Catalog"
    ; "type token = bool open Catalog"
    ]
    ~f:(fun body ->
      match F.checked (catalog body) with
      | Ok _ -> failwith "invalid configured reference/open accepted"
      | Error diagnostic ->
        let path site = S.Id.relative_path (S.id (Chatml_diagnostic.Site.source site)) in
        [%test_eq: string] (path (Option.value_exn diagnostic.site)) "entry.chatml";
        assert (
          List.exists diagnostic.related ~f:(fun related ->
            String.equal (path (Chatml_diagnostic.Related.site related)) "project.json")))
;;
