open Core
module R = Chatml_typechecker.Semantic
module B = Chatml_typechecker.Semantic_binding
module S = Chatml_source

let offset location = (S.Location.span location).left.offset
let file location = S.Location.source location |> S.Id.relative_path

let binding facts symbol =
  List.find_exn (R.Settled.bindings facts) ~f:(fun binding ->
    B.Symbol_id.equal (B.symbol binding) symbol)
;;

let authored facts ~file:expected_file ~name ~kind =
  List.filter (R.Settled.bindings facts) ~f:(fun binding ->
    B.Kind.equal (B.kind binding) kind
    && String.equal (B.name binding) name
    &&
    match B.definition binding with
    | Authored location -> String.equal (file location) expected_file
    | Implicit_specification_parameter _ | Host _ -> false)
;;

let resolve facts context ~namespace ~name =
  let rec visit context =
    let fact =
      List.find_exn (R.Settled.contexts facts) ~f:(fun fact ->
        B.Context_id.equal context fact.R.Fact.id)
    in
    match fact.transition with
    | Root -> None
    | Enter parent -> visit parent
    | Define { parent; namespace = declared_namespace; binding = symbol } ->
      if
        B.Namespace.equal namespace declared_namespace
        && String.equal name (B.name (binding facts symbol))
      then Some symbol
      else visit parent
    | Join { values; types } ->
      Option.bind
        (match namespace with
         | Value -> values
         | Type | Type_parameter -> types)
        ~f:visit
  in
  visit context
;;

let reference_at facts ~file:expected_file ~offset:expected_offset =
  let refs =
    List.filter (R.Settled.references facts) ~f:(fun reference ->
      String.equal (file reference.R.Fact.location) expected_file
      && Int.equal (offset reference.location) expected_offset)
  in
  List.hd_exn refs
;;

let target_binding reference =
  match reference.R.Fact.target with
  | Binding symbol -> symbol
  | Configured_module _ | Structural_label | Unlocated_module_member | Language_type _ ->
    failwith "expected actual nominal binding"
;;
