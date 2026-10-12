open! Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_binding_pattern
module M = Binding_model

let rec require_value actual expected =
  match actual, expected with
  | L.VInt a, M.Number b -> [%test_eq: int] a b
  | L.VBool a, M.Flag b -> [%test_eq: bool] a b
  | L.VString a, M.Text b -> [%test_eq: string] a b
  | L.VTuple actual, M.Product expected ->
    List.iter2_exn (Chatml.Chatml_tuple.to_list actual) expected ~f:require_value
  | _ -> failwith "Independent binder value mismatch"
;;

let output tree =
  match M.leaves tree with
  | [] -> "()", None
  | [ (name, value) ] -> name, Some value
  | leaves ->
    ( "(|" ^ String.concat (List.map leaves ~f:fst) ~sep:", " ^ "|)"
    , Some (M.Product (List.map leaves ~f:snd)) )
;;

let require_output actual = function
  | None -> assert (L.equal_value actual L.VUnit)
  | Some expected -> require_value actual expected
;;

let slots slots = List.map slots ~f:Chatml.Frame_env.string_of_packed_slot

let%test_unit "finite independent binder model agrees with leaf values and local slots" =
  [%test_eq: int] (List.length M.examples) 8;
  List.iter M.examples ~f:(fun model ->
    let leaves = M.leaves model in
    let declaration = "let " ^ M.pattern model ^ " = " ^ M.source (M.value model) in
    let env = L.create_env () in
    let resolved = Support.resolved declaration in
    Chatml.Chatml_eval.eval_program env resolved;
    [%test_eq: int] (Hashtbl.length env.bindings) (List.length leaves);
    List.iter leaves ~f:(fun (name, expected) ->
      require_value (Option.value_exn (L.find_var env name)) expected);
    (match resolved.stmts with
     | [ { value = L.RSLet (pattern, _); _ } ] ->
       [%test_eq: string list]
         (B.binders pattern |> List.map ~f:fst)
         (List.map leaves ~f:fst);
       [%test_eq: int] (B.count pattern) (List.length leaves)
     | _ -> failwith "Expected one model declaration");
    let expression, expected = output model in
    let source =
      "let result = let before = 99 in "
      ^ declaration
      ^ " in let after = true in "
      ^ expression
    in
    require_output (Support.run source) expected;
    match (Support.resolved source).stmts with
    | [ { value = L.RSLet (_, { value = RELetBlock (bindings, layout, _); _ }); _ } ] ->
      [%test_eq: int] (List.length bindings) 3;
      [%test_eq: string list]
        (slots layout)
        ([ "int" ] @ List.map leaves ~f:(fun (_, value) -> M.slot value) @ [ "bool" ])
    | _ -> failwith "Expected fused mixed binder frame")
;;

let%test_unit "parameter arity is independent of zero or many flattened leaf slots" =
  List.iter M.examples ~f:(fun model ->
    let expression, expected = output model in
    let return_type = Option.value_map expected ~default:"unit" ~f:M.annotation in
    let source =
      "let f : fun("
      ^ M.annotation (M.value model)
      ^ ", int) -> "
      ^ return_type
      ^ " = fun "
      ^ M.pattern model
      ^ " extra -> "
      ^ expression
      ^ "\nlet result = f("
      ^ M.source (M.value model)
      ^ ", 5)"
    in
    require_output (Support.run source) expected;
    match (Support.resolved source).stmts with
    | { value = L.RSLet (_, { value = RELambda (params, layout, _); _ }); _ } :: _ ->
      [%test_eq: int] (List.length params) 2;
      [%test_eq: string list]
        (slots layout)
        (List.map (M.leaves model) ~f:(fun (_, value) -> M.slot value) @ [ "int" ])
    | _ -> failwith "Expected contextual model lambda")
;;
