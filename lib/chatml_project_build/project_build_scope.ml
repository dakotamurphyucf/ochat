open Core
module Model = Project_build_model
module Project = Chatml_project
module Catalog = Chatml_catalog
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Budget = Project_build_budget
module Input = Model.Catalog_input

let environments catalog ~checkpoint =
  let add map scope =
    checkpoint ();
    Map.set map ~key:(Project.Scope_spec.name scope) ~data:scope
  in
  let initial =
    add String.Map.empty (Project.Selection.environment (Catalog.selection catalog))
  in
  List.fold (Catalog.sources catalog) ~init:initial ~f:(fun selected source ->
    checkpoint ();
    match Catalog.scope_for ~checkpoint catalog ~source:(Chatml_source.id source) with
    | None -> selected
    | Some scope -> add selected scope)
  |> Map.data
;;

let dependencies catalog ~checkpoint =
  let selected =
    List.fold
      (environments catalog ~checkpoint)
      ~init:String.Set.empty
      ~f:(fun names scope ->
        checkpoint ();
        match Project.Scope_spec.bindings scope with
        | None -> names
        | Some bindings ->
          List.fold bindings ~init:names ~f:(fun names binding ->
            checkpoint ();
            match Project.Module_ref.catalog binding.module_ref with
            | Self -> names
            | Dependency name -> Set.add names name))
  in
  Project.dependencies (Catalog.project catalog)
  |> List.filter ~f:(fun dependency ->
    checkpoint ();
    Set.mem selected (Project.Dependency.name dependency))
  |> List.sort ~compare:(fun left right ->
    checkpoint ();
    String.compare (Project.Dependency.name left) (Project.Dependency.name right))
;;

let check_requirement scope ~contract ~fingerprint ~checkpoint =
  checkpoint ();
  let requirement = Project.Scope_spec.compiler_requirement scope in
  let compatible =
    Option.for_all
      (Project.Compiler_requirement.compiler_family requirement)
      ~f:(fun family -> String.equal family (Contract.compiler_family contract))
    && Option.for_all
         (Project.Compiler_requirement.allowed_targets requirement)
         ~f:(fun targets ->
           List.exists targets ~f:(fun name ->
             checkpoint ();
             String.equal name (Contract.target_name contract)))
    && Option.for_all
         (Project.Compiler_requirement.exact_contract requirement)
         ~f:(fun required -> Chatml_source.Digest.equal required fingerprint)
  in
  if compatible
  then Ok ()
  else
    Error
      (Model.Error.create
         ?declared_at:(Project.Compiler_requirement.declared_at requirement)
         Compiler_requirement)
;;

let bindings budget owner ~catalogs scope =
  let checkpoint () = Budget.checkpoint budget in
  let fail ?declared_at problem =
    let declared_at = Option.value declared_at ~default:(Input.declared_at owner) in
    Error (Model.Error.create ~declared_at problem)
  in
  let target name =
    let open Result.Let_syntax in
    let%bind selection =
      match
        List.find owner.Input.dependencies ~f:(fun dependency ->
          checkpoint ();
          Budget.compare_string
            budget
            (Project.Lock.Dependency_selection.name dependency)
            name
          = 0)
      with
      | Some selection -> Ok selection
      | None -> fail (Invalid_dependency name)
    in
    let declaration = Project.Lock.Dependency_selection.declaration selection in
    match
      List.find catalogs ~f:(fun input ->
        Budget.compare_id budget (Input.declaration input) declaration = 0)
    with
    | Some input -> Ok input.catalog
    | None -> fail (Missing_catalog declaration)
  in
  match Project.Scope_spec.bindings scope with
  | None ->
    Ok
      (List.map (Catalog.bindings owner.catalog) ~f:(fun binding ->
         checkpoint ();
         { Prepared.Scope.name = Catalog.Binding.name binding
         ; module_id = Catalog.Binding.module_id binding
         ; declared_at =
             Project.Declared_at.location (Catalog.Binding.declared_at binding)
         }))
  | Some bindings ->
    let open Result.Let_syntax in
    List.fold_result bindings ~init:[] ~f:(fun resolved binding ->
      checkpoint ();
      let%bind catalog =
        match Project.Module_ref.catalog binding.module_ref with
        | Self -> Ok owner.catalog
        | Dependency name -> target name
      in
      let reference = Project.Module_ref.name binding.module_ref in
      let%map selected =
        match
          List.find (Catalog.bindings catalog) ~f:(fun candidate ->
            Budget.compare_string budget (Catalog.Binding.name candidate) reference = 0)
        with
        | Some selected -> Ok selected
        | None ->
          fail ~declared_at:binding.declared_at (Missing_module binding.module_ref)
      in
      { Prepared.Scope.name = binding.name
      ; module_id = Catalog.Binding.module_id selected
      ; declared_at = Project.Declared_at.location binding.declared_at
      }
      :: resolved)
    |> Result.map ~f:List.rev
;;
