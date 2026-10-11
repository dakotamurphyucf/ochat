open! Core
module M = Product_model
module L = Chatml.Chatml_lang
module Product = Chatml.Chatml_tuple
module T = Chatml_typechecker

let rec assert_value expected actual =
  match expected, actual with
  | M.Number expected, L.VInt actual -> [%test_eq: int] actual expected
  | Flag expected, VBool actual -> [%test_eq: bool] actual expected
  | Text expected, VString actual -> [%test_eq: string] actual expected
  | Product expected, VTuple actual ->
    let actual = Product.to_list actual in
    [%test_eq: int] (List.length actual) (List.length expected);
    List.iter2_exn expected actual ~f:assert_value
  | _ -> raise_s [%sexp "Ground model shape differs", (expected : M.t)]
;;

let rec assert_type expected actual =
  match expected, T.resolve_bound_type actual with
  | M.Number _, T.TInt | Flag _, Boolean | Text _, String -> ()
  | Product expected, Tuple actual ->
    [%test_eq: int] (List.length actual) (List.length expected);
    List.iter2_exn expected actual ~f:assert_type
  | _ -> raise_s [%sexp "Ground model type differs", (expected : M.t)]
;;

let%test_unit "bounded product basis preserves source/type/ordered runtime shape" =
  [%test_eq: int] (List.length M.products) 28;
  List.iter M.products ~f:(fun expected ->
    let source = "let result : " ^ M.annotation expected ^ " = " ^ M.source expected in
    let program = Support.parse source in
    let checked =
      match T.check_program program with
      | Ok checked -> checked
      | Error diagnostic -> failwith (T.format_diagnostic source diagnostic)
    in
    let rhs =
      match program.stmts with
      | [ { value =
              L.SLet ({ value = Chatml.Chatml_binding_pattern.Name "result"; _ }, rhs)
          ; _
          }
        ] -> rhs
      | _ -> failwith "Expected model's sole annotated result"
    in
    assert_type expected (T.checked_lookup_span_type checked rhs.span |> Option.value_exn);
    assert_value expected (Support.run source);
    [%test_eq: string]
      (Support.run source |> Chatml.Chatml_builtin_spec.value_to_string)
      (M.display expected);
    let components =
      match expected with
      | M.Product values -> values
      | _ -> assert false
    in
    let names = List.mapi components ~f:(fun index _ -> "p" ^ Int.to_string index) in
    let matched =
      Support.run
        (source
         ^ "\nlet result = match result with | (|"
         ^ String.concat ~sep:", " names
         ^ "|) -> ["
         ^ String.concat
             ~sep:", "
             (List.map names ~f:(fun name -> "to_string(" ^ name ^ ")"))
         ^ "]")
    in
    match matched with
    | L.VArray values ->
      [%test_eq: int] (Array.length values) (List.length components);
      List.iter2_exn components (Array.to_list values) ~f:(fun component value ->
        match value with
        | L.VString actual -> [%test_eq: string] actual (M.display component)
        | _ -> failwith "Expected each ordered match binding's display")
    | _ -> failwith "Expected matched model result array")
;;

let%test_unit "product helpers visit in source order and never copy mutable leaves" =
  assert (Option.is_none (Product.of_list []));
  assert (Option.is_none (Product.of_list [ 1 ]));
  List.iter [ 2; 3; 7 ] ~f:(fun arity ->
    let values = List.init arity ~f:Fn.id in
    let product = Product.of_list values |> Option.value_exn in
    let observed = ref [] in
    let visit value =
      observed := value :: !observed;
      value
    in
    ignore (Product.map product ~f:visit);
    [%test_eq: int list] (List.rev !observed) values;
    observed := [];
    Product.iter product ~f:(fun value -> ignore (visit value));
    [%test_eq: int list] (List.rev !observed) values;
    [%test_eq: int list]
      (Product.fold product ~init:[] ~f:(fun acc value -> acc @ [ value ]))
      values);
  let cell = ref 0 in
  let product = Product.create cell cell ~rest:[] in
  product.first := 7;
  [%test_eq: int] !(product.second) 7
;;
