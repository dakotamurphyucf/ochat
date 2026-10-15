open Core
module A = Chatml_semantics
module Q = A.Snapshot
module H = Support

let%test_unit
    "points symbols and contexts reject foreign snapshots and changed environments"
  =
  let contract = H.contract () in
  let files = [ "entry.chatml", "let value = 1;; let result = value", [] ] in
  let input = H.input contract files in
  let snapshot = H.complete contract input in
  let point = H.point snapshot input "entry.chatml" 30 in
  let context = H.context snapshot point in
  let symbol = H.definition snapshot input "entry.chatml" 4 in
  List.iter
    [ H.complete contract input
    ; H.complete contract (H.input contract ~environment:"other" files)
    ]
    ~f:(fun other ->
      (match
         Q.references_at other point ~limits:(H.query_limits ()) ~checkpoint:H.checkpoint
       with
       | Error Foreign_point -> ()
       | _ -> failwith "foreign point");
      (match Q.symbol other (Q.Symbol.id symbol) with
       | Error Foreign_symbol -> ()
       | _ -> failwith "foreign symbol");
      match
        Q.candidates
          other
          context
          ~namespace:Value
          ~limits:(H.query_limits ())
          ~checkpoint:H.checkpoint
      with
      | Error Foreign_context -> ()
      | _ -> failwith "foreign context");
  let original = H.source input "entry.chatml" in
  let changed =
    A.Captured.create
      (A.Captured.id original)
      ~contents:"let value = 2;; let result = value"
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:H.checkpoint
    |> Or_error.ok_exn
  in
  let location =
    A.Captured.locate changed ~start_byte:4 ~end_byte:4 ~checkpoint:H.checkpoint
    |> Or_error.ok_exn
  in
  (match Q.point snapshot location with
   | Error Invalid_location -> ()
   | _ -> failwith "changed revision");
  let range =
    A.Captured.locate original ~start_byte:4 ~end_byte:9 ~checkpoint:H.checkpoint
    |> Or_error.ok_exn
  in
  match Q.point snapshot range with
  | Error Nonzero_point_range -> ()
  | _ -> failwith "point must be zero width"
;;

let%test_unit
    "configured availability is separate from aliases, edges and checked exports"
  =
  let contract = H.contract () in
  let body =
    "module Unused = struct let own = 1 end;; let record = { field = 3 };; let result = \
     record.field"
  in
  let input =
    H.input
      contract
      [ ( "entry.chatml"
        , body
        , [ "Unused", "unused.chatml"; "Available", "available.chatml" ] )
      ; "unused.chatml", "let should_not_parse =", []
      ; "available.chatml", "type offered = int;; let value = 1", []
      ]
  in
  let snapshot = H.complete contract input in
  let point =
    H.point
      snapshot
      input
      "entry.chatml"
      (String.substr_index_exn body ~pattern:"record.field")
  in
  let context = H.context snapshot point in
  [%test_eq: string list]
    (H.names snapshot (H.candidates snapshot context Type))
    [ "json" ];
  let configured =
    Q.configured_modules
      snapshot
      context
      ~limits:(H.query_limits ())
      ~checkpoint:H.checkpoint
    |> Result.ok
    |> Option.value_exn
  in
  [%test_eq: string list]
    (List.map configured.items ~f:Q.Configured_module.name)
    [ "Available"; "Unused" ];
  List.iter configured.items ~f:(fun available ->
    match Q.Configured_module.exports available with
    | Unavailable Not_checked -> ()
    | _ -> failwith "unused catalog is unavailable");
  assert (
    Option.is_some (Q.Configured_module.shadowed_by (List.nth_exn configured.items 1)));
  assert (Option.is_none (Q.Configured_module.shadowed_by (List.hd_exn configured.items)));
  assert (
    Option.equal
      Q.Source_status.equal
      (Q.source_status snapshot (A.Captured.id (H.source input "available.chatml")))
      (Some Unchecked));
  assert (
    Option.equal
      Q.Source_status.equal
      (Q.source_status snapshot (A.Captured.id (H.source input "project.json")))
      (Some Metadata));
  let structural =
    H.references
      snapshot
      (H.point
         snapshot
         input
         "entry.chatml"
         (String.substr_index_exn body ~pattern:"record.field" + 7))
  in
  assert (
    List.exists structural ~f:(fun reference ->
      match Q.Reference.target reference with
      | Structural_label -> true
      | _ -> false));
  let paths =
    List.map (Q.runtime_order snapshot) ~f:(fun id ->
      A.Captured.Id.relative_path (A.Prepared.Module_id.implementation id))
  in
  [%test_eq: string list] paths [ "entry.chatml" ];
  let truncated =
    Q.configured_modules
      snapshot
      context
      ~limits:(H.query_limits ~results:1 ())
      ~checkpoint:H.checkpoint
    |> Result.ok
    |> Option.value_exn
  in
  assert truncated.truncated;
  [%test_eq: string list]
    (List.map truncated.items ~f:Q.Configured_module.name)
    [ "Available" ];
  (match
     Q.configured_modules
       snapshot
       context
       ~limits:(H.query_limits ~work:1 ())
       ~checkpoint:H.checkpoint
   with
   | Error Query_work_limit -> ()
   | _ -> failwith "configured lookup must charge work");
  let polls = ref 0 in
  assert (
    Result.is_ok
      (Q.configured_modules
         snapshot
         context
         ~limits:(H.query_limits ())
         ~checkpoint:(fun () -> incr polls)));
  assert (!polls > 0);
  let expected = Chatml_typechecker.Type_error "query callback origin" in
  for stop = 1 to !polls do
    let current = ref 0 in
    match
      Q.configured_modules
        snapshot
        context
        ~limits:(H.query_limits ())
        ~checkpoint:(fun () ->
          incr current;
          if Int.equal stop !current then raise expected)
    with
    | _ -> failwith "interrupted query must not return a page"
    | exception actual -> assert (phys_equal expected actual)
  done
;;

let%test_unit "source-owned qualified components and host definitions have honest targets"
  =
  let contract = H.contract () in
  let body =
    "let result = Catalog.Nested.item;; let length = String.length(\"x\");; let \
     async_string = Task.pure(\"s\");; let other = Catalog.async_int"
  in
  let catalog_body =
    "module Nested = struct let item = 3 end;; let async_int = Task.pure(3)"
  in
  let input =
    H.input
      contract
      [ "entry.chatml", body, [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", catalog_body, []
      ]
  in
  let snapshot = H.complete contract input in
  let start = String.substr_index_exn body ~pattern:"Catalog.Nested.item" in
  let components =
    List.map
      [ start; start + 8; start + 15 ]
      ~f:(fun offset ->
        H.references snapshot (H.point snapshot input "entry.chatml" offset)
        |> List.hd_exn)
  in
  [%test_eq: string list]
    (List.map components ~f:(fun reference ->
       A.Captured.read (H.source input "entry.chatml") (Q.Reference.selection reference)
       |> Or_error.ok_exn))
    [ "Catalog"; "Nested"; "item" ];
  let item = List.last_exn components in
  (match Q.Reference.target item with
   | Symbol id ->
     let symbol = Q.symbol snapshot id |> Result.ok |> Option.value_exn in
     (match Q.Symbol.origin symbol with
      | Authored location ->
        [%test_eq: string]
          (A.Captured.Id.relative_path (A.Captured.Location.source location))
          "catalog.chatml"
      | _ -> failwith "actual definition owner")
   | _ -> failwith "resolved member");
  let host =
    H.references
      snapshot
      (H.point
         snapshot
         input
         "entry.chatml"
         (String.substr_index_exn body ~pattern:"String.length" + 7))
    |> List.hd_exn
  in
  (match Q.Reference.target host with
   | Symbol id ->
     let symbol = Q.symbol snapshot id |> Result.ok |> Option.value_exn in
     (match Q.Symbol.origin symbol with
      | Host { contract = owner; member_path } ->
        assert (
          A.Captured.Digest.equal
            owner
            (Chatml.Chatml_compiler_contract.fingerprint
               contract
               ~checkpoint:H.checkpoint));
        [%test_eq: string list] member_path [ "String"; "length" ]
      | _ -> failwith "host symbol has no fabricated source site")
   | _ -> failwith "actual host member");
  let pure_symbol path body =
    let offset = String.substr_index_exn body ~pattern:"Task.pure" + 6 in
    H.references snapshot (H.point snapshot input path offset)
    |> List.hd_exn
    |> Q.Reference.target
  in
  match pure_symbol "entry.chatml" body, pure_symbol "catalog.chatml" catalog_body with
  | Symbol string_use, Symbol int_use -> assert (Q.Symbol_id.equal string_use int_use)
  | _ -> failwith "actual polymorphic host callback"
;;

let%test_unit
    "recorded type regions and bounded candidate pages expose exact proven facts"
  =
  let contract = H.contract () in
  let body = "let number = 3;; let result = number + 1" in
  let input = H.input contract [ "entry.chatml", body, [] ] in
  let snapshot = H.complete contract input in
  let point =
    H.point
      snapshot
      input
      "entry.chatml"
      (String.substr_index_exn body ~pattern:"number + 1")
  in
  (match
     Q.type_at snapshot point ~limits:(H.query_limits ()) ~checkpoint:H.checkpoint
   with
   | Ok (Selected root) ->
     (match H.node snapshot (H.dereference snapshot root) with
      | Int -> ()
      | _ -> failwith "settled read type")
   | _ -> failwith "positive reference type");
  let context = H.context snapshot point in
  let all = H.candidates snapshot context Value in
  let first =
    Q.candidates
      snapshot
      context
      ~namespace:Value
      ~limits:(H.query_limits ~results:1 ())
      ~checkpoint:H.checkpoint
    |> Result.ok
    |> Option.value_exn
  in
  assert first.truncated;
  [%test_eq: string list]
    (H.names snapshot first.items)
    (List.take (H.names snapshot all) 1);
  let unseen = H.point snapshot input "entry.chatml" 5 in
  match
    Q.references_at snapshot unseen ~limits:(H.query_limits ()) ~checkpoint:H.checkpoint
  with
  | Ok (Available { items = []; truncated = false }) -> ()
  | _ -> failwith "binder token is not a certified reference"
;;
