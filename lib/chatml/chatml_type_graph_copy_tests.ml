open Core
open Chatml_inferred_type
module Copy = Chatml_type_graph_copy

let checkpoint () = ()

let limits
      ?(max_roots = 64)
      ?(max_type_nodes = 4096)
      ?(max_variable_cells = 64)
      ?(max_edges = 4096)
      ?(max_work = 16384)
      ?(max_depth = 64)
      ()
  =
  Copy.Limits.create
    ~max_roots
    ~max_type_nodes
    ~max_variable_cells
    ~max_edges
    ~max_work
    ~max_depth
  |> Or_error.ok_exn
;;

let copy roots ~limits =
  match Copy.copy_detached roots ~limits ~checkpoint with
  | Ok roots -> roots
  | Error error -> raise_s [%sexp (error : Copy.Error.t)]
;;

let variable name =
  ref (Free { name; level = 3; equality = Some Equality_requirement.Value })
;;

let root_cell = function
  | Var cell -> cell
  | _ -> failwith "expected variable root"
;;

(* Canonical IDs come from first traversal, not physical addresses. The cell
   table is installed before walking a Bound body so cyclic graphs terminate. *)
let witness roots =
  let atom value = Sexp.Atom value in
  let node name children = Sexp.List (atom name :: children) in
  let cells = ref [] in
  let contents = ref [] in
  let next_id = ref 0 in
  let rec shape = function
    | Var cell ->
      let id =
        match
          List.find_map !cells ~f:(fun (id, prior) ->
            if phys_equal prior cell then Some id else None)
        with
        | Some id -> id
        | None ->
          let id = !next_id in
          Int.incr next_id;
          cells := (id, cell) :: !cells;
          let content =
            match !cell with
            | Free { name; level; equality } ->
              node
                "Free"
                [ atom name
                ; [%sexp (level : int)]
                ; [%sexp (equality : Equality_requirement.t option)]
                ]
            | Bound body -> node "Bound" [ shape body ]
          in
          contents := (id, content) :: !contents;
          id
      in
      node "Var" [ [%sexp (id : int)] ]
    | Fun (arguments, result) ->
      node "Fun" [ node "Arguments" (List.map arguments ~f:shape); shape result ]
    | Generic { name; equality } ->
      node "Generic" [ atom name; [%sexp (equality : Equality_requirement.t option)] ]
    | Con (name, arguments) -> node "Con" (atom name :: List.map arguments ~f:shape)
    | Mu (name, body) -> node "Mu" [ atom name; shape body ]
    | Rec_var name -> node "Rec_var" [ atom name ]
    | Ref body -> node "Ref" [ shape body ]
    | Record row -> node "Record" [ shape row ]
    | Module row -> node "Module" [ shape row ]
    | Field_receiver (kind, row) -> node "Field_receiver" [ shape kind; shape row ]
    | Shape_kind kind -> node "Shape_kind" [ [%sexp (kind : Shape_kind.t)] ]
    | Variant row -> node "Variant" [ shape row ]
    | Variant_payload values -> node "Variant_payload" (List.map values ~f:shape)
    | Tuple values -> node "Tuple" (List.map values ~f:shape)
    | Array body -> node "Array" [ shape body ]
    | TInt -> atom "TInt"
    | TFloat -> atom "TFloat"
    | Boolean -> atom "Boolean"
    | String -> atom "String"
    | Row (fields, tail) ->
      let fields =
        Env.bindings fields
        |> List.map ~f:(fun (name, value) -> node "Field" [ atom name; shape value ])
      in
      node "Row" [ node "Fields" fields; shape tail ]
    | Empty_row -> atom "Empty_row"
    | Unit -> atom "Unit"
  in
  let ordered pairs = List.sort pairs ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
  let root_shapes = List.map roots ~f:shape in
  let cell_shapes =
    ordered !contents
    |> List.map ~f:(fun (id, content) -> node "Cell" [ [%sexp (id : int)]; content ])
  in
  ( node "Graph" [ node "Roots" root_shapes; node "Cells" cell_shapes ]
  , List.map (ordered !cells) ~f:snd )
;;

let assert_cell_correspondence originals copies =
  List.iter2_exn originals copies ~f:(fun original clone ->
    assert (not (phys_equal original clone)));
  List.iteri originals ~f:(fun i original ->
    List.iteri copies ~f:(fun j clone ->
      let original_j = List.nth_exn originals j in
      let copy_i = List.nth_exn copies i in
      assert (Bool.equal (phys_equal original original_j) (phys_equal copy_i clone))))
;;

let%test_unit "bounded detached copy preserves all small directed graphs" =
  for size = 1 to 4 do
    let rec enumerate remaining successors =
      if remaining > 0
      then
        for successor = -1 to size - 1 do
          enumerate (remaining - 1) (successor :: successors)
        done
      else (
        let successors = List.rev successors in
        let originals = List.init size ~f:(fun index -> variable (Int.to_string index)) in
        List.iter2_exn originals successors ~f:(fun cell successor ->
          if successor >= 0
          then
            cell
            := Bound
                 (Record
                    (Row
                       ( Env.singleton "next" (Var (List.nth_exn originals successor))
                       , Empty_row ))));
        let before = List.map originals ~f:(fun cell -> !cell) in
        let roots =
          List.map originals ~f:(fun cell -> Var cell) @ [ Var (List.hd_exn originals) ]
        in
        let source_witness, source_cells = witness roots in
        let copied_roots = copy roots ~limits:(limits ()) in
        let copy_witness, copied_cells = witness copied_roots in
        assert (Sexp.equal source_witness copy_witness);
        assert_cell_correspondence source_cells copied_cells;
        let copied = List.map copied_roots ~f:root_cell in
        assert (phys_equal (List.hd_exn copied) (List.last_exn copied));
        List.iter2_exn originals before ~f:(fun original snapshot ->
          assert (phys_equal !original snapshot));
        List.iter copied ~f:(fun cell -> cell := Bound Boolean);
        List.iter2_exn originals before ~f:(fun original snapshot ->
          assert (phys_equal !original snapshot));
        List.iter originals ~f:(fun cell -> cell := Bound String);
        List.iter copied ~f:(fun cell ->
          match !cell with
          | Bound Boolean -> ()
          | _ -> failwith "original mutation reached copy"))
    in
    enumerate size []
  done
;;

let error roots ~limits =
  match Copy.copy_detached roots ~limits ~checkpoint with
  | Error (Copy.Error.Limit_exceeded dimension) -> dimension
  | Ok _ -> failwith "expected a copy limit"
;;

let%expect_test "each traversal dimension rejects before publishing a partial copy" =
  let original = variable "r" in
  let before = !original in
  let cases =
    [ error [ Var original ] ~limits:(limits ~max_roots:0 ())
    ; error [ Var original ] ~limits:(limits ~max_type_nodes:0 ())
    ; error [ Var original ] ~limits:(limits ~max_variable_cells:0 ())
    ; error [ Var original ] ~limits:(limits ~max_edges:0 ())
    ; error [ Var original ] ~limits:(limits ~max_work:1 ())
    ; error [ Array Unit ] ~limits:(limits ~max_depth:1 ())
    ]
  in
  assert (phys_equal !original before);
  List.iter cases ~f:(fun dimension -> print_s [%sexp (dimension : Copy.Error.dimension)]);
  [%expect
    {|
    Roots
    Type_nodes
    Variable_cells
    Edges
    Work
    Depth
    |}]
;;

let%test_unit "every inferred constructor survives one bounded structural witness" =
  let generic = Generic { name = "binder"; equality = Some Equality_requirement.Value } in
  let original = variable "weak" in
  let roots =
    [ Fun ([ Var original; generic ], Con ("Box", [ Array TInt; Ref TFloat ]))
    ; Mu
        ( "loop"
        , Variant
            (Row
               ( Env.singleton "Again" (Variant_payload [ Rec_var "loop"; Var original ])
               , Empty_row )) )
    ; Module
        (Row
           ( Env.of_list
               [ ( "record"
                 , Record
                     (Row
                        ( Env.singleton
                            "field"
                            (Field_receiver
                               ( Shape_kind Shape_kind.Record
                               , Row (Env.singleton "x" String, Empty_row) ))
                        , Empty_row )) )
               ; "module-kind", Field_receiver (Shape_kind Shape_kind.Module, Unit)
               ; "generic", Fun ([ generic ], generic)
               ]
           , Empty_row ))
    ; Tuple [ Boolean; String; Unit; TFloat; Empty_row ]
    ]
  in
  let source_witness, source_cells = witness roots in
  let copied = copy roots ~limits:(limits ()) in
  let copied_witness, copied_cells = witness copied in
  assert (Sexp.equal source_witness copied_witness);
  assert_cell_correspondence source_cells copied_cells;
  original := Bound Boolean;
  assert (Sexp.equal copied_witness (fst (witness copied)))
;;

let%test_unit "exact limit boundary accepts one variable and empty zero-limit graph" =
  let zero =
    limits
      ~max_roots:0
      ~max_type_nodes:0
      ~max_variable_cells:0
      ~max_edges:0
      ~max_work:0
      ~max_depth:0
      ()
  in
  assert (List.is_empty (copy [] ~limits:zero));
  let exact =
    limits
      ~max_roots:1
      ~max_type_nodes:1
      ~max_variable_cells:1
      ~max_edges:1
      ~max_work:4
      ~max_depth:1
      ()
  in
  let original = variable "exact" in
  let copied = copy [ Var original ] ~limits:exact |> List.hd_exn |> root_cell in
  assert (not (phys_equal original copied));
  assert (
    Copy.Error.equal_dimension
      (error [ Var original ] ~limits:(limits ~max_work:3 ()))
      Copy.Error.Work)
;;

let%test_unit "checkpoint cancellation in identity search leaves originals unchanged" =
  let originals = List.init 64 ~f:(fun index -> variable (Int.to_string index)) in
  let before = List.map originals ~f:(fun cell -> !cell) in
  let polls = ref 0 in
  let expected = Failure "cancel-copy" in
  let cancelled () =
    Int.incr polls;
    if !polls = 100 then raise expected
  in
  (match
     Copy.copy_detached
       (List.map originals ~f:(fun cell -> Var cell))
       ~limits:(limits ())
       ~checkpoint:cancelled
   with
   | exception error when phys_equal error expected -> ()
   | _ -> failwith "copy did not cancel");
  List.iter2_exn originals before ~f:(fun original snapshot ->
    assert (phys_equal !original snapshot))
;;

let%test_unit "invalid depth policy rejects before copy" =
  assert (
    Result.is_error
      (Copy.Limits.create
         ~max_roots:1
         ~max_type_nodes:1
         ~max_variable_cells:1
         ~max_edges:1
         ~max_work:1
         ~max_depth:257))
;;
