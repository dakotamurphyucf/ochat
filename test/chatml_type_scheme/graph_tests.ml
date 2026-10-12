open! Core
open Chatml_typechecker
module Scheme = Chatml_typechecker.Type_scheme
module Equality = Chatml.Chatml_equality_requirement

let quiet () = ()

let fresh () =
  let index = ref 0 in
  fun () ->
    incr index;
    "fresh" ^ Int.to_string !index
;;

let instantiate ?(checkpoint = quiet) ty =
  Scheme.instantiate ty ~level:0 ~fresh_name:(fresh ()) ~checkpoint
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Scheme.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

module Graph = struct
  type t =
    { edges : int array
    ; cells : tv ref array
    ; high : tv ref
    ; weak : tv ref
    ; before : (tv ref * tv) list
    }

  let create edges =
    let high =
      ref (Free { name = "eligible"; level = 1; equality = Some Equality.Value })
    in
    let weak = ref (Free { name = "captured"; level = 0; equality = None }) in
    let cells = Array.init (Array.length edges) ~f:(fun _ -> ref (Bound Unit)) in
    Array.iteri cells ~f:(fun index cell ->
      cell
      := Bound
           (Record
              (Row
                 ( Env.of_list
                     [ "node", Con (Int.to_string index, [])
                     ; "next", Var cells.(edges.(index))
                     ; "eligible", Var high
                     ; "weak", Var weak
                     ]
                 , Empty_row ))));
    let before =
      List.map (high :: weak :: Array.to_list cells) ~f:(fun cell -> cell, !cell)
    in
    { edges; cells; high; weak; before }
  ;;

  let roots t = Tuple [ Var t.cells.(0); Var t.cells.(Array.length t.cells - 1) ]

  let unchanged t =
    List.iter t.before ~f:(fun (cell, before) -> assert (phys_equal !cell before))
  ;;

  let rec row fuel = function
    | Var { contents = Bound body } when fuel > 0 -> row (fuel - 1) body
    | Record (Row (fields, Empty_row)) -> fields
    | _ -> assert false
  ;;

  (* The oracle walks labelled edges for a fixed, non-vacuous horizon. Expected
     labels come only from the handwritten finite model, never from transform. *)
  let observe t ty ~quantified =
    let roots =
      match ty with
      | Tuple [ first; last ] -> [ 0, first; Array.length t.cells - 1, last ]
      | _ -> assert false
    in
    let eligible = ref None in
    List.iter roots ~f:(fun (start, root) ->
      let rec walk remaining expected ty =
        if remaining > 0
        then (
          let fields = row (Array.length t.cells + 1) ty in
          (match Map.find_exn fields "node" with
           | Con (name, []) -> assert (String.equal name (Int.to_string expected))
           | _ -> assert false);
          (match Map.find_exn fields "weak" with
           | Var cell -> assert (phys_equal cell t.weak)
           | _ -> assert false);
          (match quantified, Map.find_exn fields "eligible" with
           | true, Generic { name; equality = Some Equality.Value } ->
             assert (String.equal name "eligible")
           | false, Var cell ->
             assert (not (phys_equal cell t.high));
             (match !eligible with
              | None -> eligible := Some cell
              | Some previous -> assert (phys_equal previous cell));
             (match !cell with
              | Free { level = 0; equality = Some Equality.Value; _ } -> ()
              | _ -> assert false)
           | _ -> assert false);
          walk (remaining - 1) t.edges.(expected) (Map.find_exn fields "next"))
      in
      walk ((2 * Array.length t.cells) + 1) start root);
    !eligible
  ;;

  let no_original_bound t root =
    let seen = ref [] in
    let pending = ref [ root ] in
    while not (List.is_empty !pending) do
      let ty = List.hd_exn !pending in
      pending := List.tl_exn !pending;
      match ty with
      | Var cell ->
        Array.iter t.cells ~f:(fun original -> assert (not (phys_equal original cell)));
        if not (List.exists !seen ~f:(phys_equal cell))
        then (
          seen := cell :: !seen;
          match !cell with
          | Free _ -> ()
          | Bound body -> pending := body :: !pending)
      | Record row -> pending := row :: !pending
      | Row (fields, tail) -> pending := (tail :: Map.data fields) @ !pending
      | Tuple values | Con (_, values) -> pending := values @ !pending
      | Generic _ | Empty_row -> ()
      | _ -> assert false
    done
  ;;
end

let models = [ [| 0 |]; [| 1; 0 |]; [| 1; 2; 0 |]; [| 1; 1; 0; 2 |] ]

let%test_unit "finite labelled graph model preserves cycles, weak sharing and freshness" =
  List.iter models ~f:(fun edges ->
    let graph = Graph.create edges in
    let generalized = Scheme.generalise (Graph.roots graph) ~level:0 ~checkpoint:quiet in
    assert (Option.is_none (Graph.observe graph generalized ~quantified:true));
    Graph.no_original_bound graph generalized;
    let first = instantiate generalized in
    let second = instantiate generalized in
    let first_high = Option.value_exn (Graph.observe graph first ~quantified:false) in
    let second_high = Option.value_exn (Graph.observe graph second ~quantified:false) in
    assert (not (phys_equal first_high second_high));
    Graph.no_original_bound graph first;
    Graph.no_original_bound graph second;
    Graph.unchanged graph)
;;

let%test_unit "each observed interruption leaves original graph cells reusable" =
  let graph = Graph.create [| 1; 2; 0 |] in
  let root = Graph.roots graph in
  let scheme = Scheme.generalise root ~level:0 ~checkpoint:quiet in
  let operations =
    [ (fun checkpoint -> Scheme.generalise root ~level:0 ~checkpoint)
    ; (fun checkpoint -> instantiate ~checkpoint scheme)
    ]
  in
  List.iter operations ~f:(fun operation ->
    let calls = ref 0 in
    ignore (operation (fun () -> incr calls) : typ);
    assert (!calls > 0);
    for stop = 1 to !calls do
      let remaining = ref stop in
      let original = Failure ("scheme checkpoint " ^ Int.to_string stop) in
      let caught =
        try
          ignore
            (operation (fun () ->
               decr remaining;
               if !remaining = 0 then raise original)
             : typ);
          false
        with
        | exn ->
          assert (phys_equal exn original);
          true
      in
      assert caught;
      Graph.unchanged graph
    done;
    ignore (operation quiet : typ);
    Graph.unchanged graph)
;;

let%test_unit "bound projections distinguish representation and value generics" =
  let cell = ref (Bound (Generic { name = "same"; equality = None })) in
  let before = !cell in
  match instantiate (Tuple [ Var cell; Field_receiver (Var cell, Empty_row) ]) with
  | Tuple [ Var value; Field_receiver (Var kind, Empty_row) ] ->
    assert (not (phys_equal value kind));
    assert (phys_equal !cell before)
  | _ -> assert false
;;

let%test_unit "equality conjunction preserves row exemptions and finite conflicts" =
  let generic equality = Generic { name = "row"; equality = Some equality } in
  let row labels = Equality.Row (String.Set.of_list labels) in
  (match
     instantiate (Tuple [ generic (row [ "x"; "y" ]); generic (row [ "y"; "z" ]) ])
   with
   | Tuple [ Var left; Var right ] ->
     assert (phys_equal left right);
     (match !left with
      | Free { equality = Some (Row labels); _ } ->
        assert (Set.equal labels (String.Set.singleton "y"))
      | _ -> assert false)
   | _ -> assert false);
  (match
     Scheme.instantiate
       (Tuple [ generic Equality.Value; generic (row []) ])
       ~level:0
       ~fresh_name:(fresh ())
       ~checkpoint:quiet
   with
   | Error (Conflicting_equality Value_and_row) -> ()
   | Ok _ | Error Representation_equality -> assert false);
  match
    Scheme.instantiate
      (Field_receiver (generic Equality.Value, Empty_row))
      ~level:0
      ~fresh_name:(fresh ())
      ~checkpoint:quiet
  with
  | Error Representation_equality -> ()
  | Ok _ | Error (Conflicting_equality _) -> assert false
;;

let%test_unit "deep structural transforms use bounded native stack" =
  let root = ref (Generic { name = "deep"; equality = None }) in
  for _ = 1 to 20_000 do
    root := Array !root
  done;
  let result = instantiate !root in
  let rec leaf = function
    | Array inner -> leaf inner
    | ty -> ty
  in
  match leaf result with
  | Var { contents = Free { level = 0; _ } } -> ()
  | _ -> assert false
;;

let%test_unit "lexical recursive binder spelling and generic identity survive copying" =
  let value = Generic { name = "result"; equality = None } in
  let scheme =
    Mu
      ( "outer"
      , Tuple
          [ value
          ; Mu
              ( "outer"
              , Variant
                  (Row
                     ( Env.of_list
                         [ "Leaf", Variant_payload [ value ]
                         ; "Next", Variant_payload [ Rec_var "outer" ]
                         ]
                     , Empty_row )) )
          ] )
  in
  match instantiate scheme with
  | Mu ("outer", Tuple [ Var outside; Mu ("outer", Variant (Row (fields, Empty_row))) ])
    ->
    (match Map.find_exn fields "Leaf", Map.find_exn fields "Next" with
     | Variant_payload [ Var inside ], Variant_payload [ Rec_var "outer" ] ->
       assert (phys_equal outside inside)
     | _ -> assert false)
  | _ -> assert false
;;

let%test_unit "fresh-name exceptions preserve identity and do not alter scheme cells" =
  let original = ref (Bound (Generic { name = "value"; equality = None })) in
  let before = !original in
  let scheme =
    Tuple
      [ Var original
      ; Generic { name = "other"; equality = None }
      ; Field_receiver (Var original, Empty_row)
      ]
  in
  let calls = ref 0 in
  let allocate = fresh () in
  ignore
    (Scheme.instantiate scheme ~level:0 ~checkpoint:quiet ~fresh_name:(fun () ->
       incr calls;
       allocate ())
     : (typ, Scheme.Error.t) Result.t);
  assert (!calls > 0);
  for stop = 1 to !calls do
    let remaining = ref stop in
    let sentinel = Failure ("fresh-name " ^ Int.to_string stop) in
    let caught =
      try
        ignore
          (Scheme.instantiate scheme ~level:0 ~checkpoint:quiet ~fresh_name:(fun () ->
             decr remaining;
             if !remaining = 0 then raise sentinel;
             allocate ())
           : (typ, Scheme.Error.t) Result.t);
        false
      with
      | exn ->
        assert (phys_equal exn sentinel);
        true
    in
    assert caught;
    assert (phys_equal !original before)
  done;
  ignore (instantiate scheme : typ);
  assert (phys_equal !original before)
;;
