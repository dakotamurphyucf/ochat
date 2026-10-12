open Core
module Model = Project_build_model
module Budget = Project_build_budget
module Scope = Project_build_scope
module Dependency = Project_build_dependency
module Input = Model.Catalog_input
module Project = Chatml_project
module Catalog = Chatml_catalog
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Lock = Project.Lock

let failure input problem =
  Error (Model.Error.create ~declared_at:(Input.declared_at input) problem)
;;

let namespace input = Captured.Id.package_namespace (Input.declaration input)

let normalize budget input =
  let checkpoint () = Budget.checkpoint budget in
  checkpoint ();
  let open Result.Let_syntax in
  let%bind () =
    match input.Input.origin with
    | Local -> Ok ()
    | Git { repository = _; pin = _; package_root } ->
      List.fold_result (Catalog.sources input.catalog) ~init:() ~f:(fun () source ->
        checkpoint ();
        let id = Captured.id source in
        (* Source.Id already admits exactly the portable relative-path domain. *)
        let member =
          Source.Portable_path.create (Captured.Id.relative_path id) |> Or_error.ok_exn
        in
        Dependency.repository_member ~package_root ~member ~checkpoint
        |> Result.map ~f:(fun _ -> ())
        |> Result.map_error ~f:(fun _ ->
          Model.Error.create
            ~declared_at:(Input.declared_at input)
            (Invalid_repository_member id)))
  in
  if Option.exists input.Input.acquisition_key ~f:String.is_empty
  then failure input Invalid_acquisition_key
  else (
    let dependencies =
      List.sort input.dependencies ~compare:(fun left right ->
        Budget.compare_string
          budget
          (Lock.Dependency_selection.name left)
          (Lock.Dependency_selection.name right))
    in
    let rec validate = function
      | [] | [ _ ] -> Ok { input with dependencies }
      | left :: (right :: _ as rest) ->
        if
          Budget.compare_string
            budget
            (Lock.Dependency_selection.name left)
            (Lock.Dependency_selection.name right)
          = 0
        then failure input (Invalid_dependency (Lock.Dependency_selection.name right))
        else validate rest
    in
    validate dependencies)
;;

let same_input budget left right =
  let same_id left right = Budget.compare_id budget left right = 0 in
  let same_string left right = Budget.compare_string budget left right = 0 in
  let same_origin (left : Lock.Catalog.origin) (right : Lock.Catalog.origin) =
    match left, right with
    | Lock.Catalog.Local, Local -> true
    | Git left, Git right ->
      same_string left.repository right.repository
      && Project.Git_pin.equal left.pin right.pin
      && Option.equal Source.Portable_path.equal left.package_root right.package_root
    | Local, Git _ | Git _, Local -> false
  in
  let same_definition left right =
    same_id
      (Prepared.Definition.implementation left)
      (Prepared.Definition.implementation right)
    && Option.equal
         same_id
         (Prepared.Definition.interface left)
         (Prepared.Definition.interface right)
  in
  let same_binding left right =
    same_string (Catalog.Binding.name left) (Catalog.Binding.name right)
    && same_id
         (Prepared.Module_id.implementation (Catalog.Binding.module_id left))
         (Prepared.Module_id.implementation (Catalog.Binding.module_id right))
  in
  let selected catalog =
    let selection = Catalog.selection catalog in
    ( Project.Scope_spec.name (Project.Selection.environment selection)
    , Option.map (Project.Selection.entry selection) ~f:Project.Entry.name )
  in
  let left_environment, left_entry = selected left.Input.catalog in
  let right_environment, right_entry = selected right.Input.catalog in
  same_id (Input.declaration left) (Input.declaration right)
  && (match left.acquisition_key, right.acquisition_key with
      | Some left, Some right -> same_string left right
      | None, _ | _, None -> false)
  && same_origin left.origin right.origin
  && same_string left_environment right_environment
  && Option.equal same_string left_entry right_entry
  && List.equal
       (Budget.equal_source budget)
       (Catalog.sources left.catalog)
       (Catalog.sources right.catalog)
  && List.equal
       same_definition
       (Catalog.definitions left.catalog)
       (Catalog.definitions right.catalog)
  && List.equal
       same_binding
       (Catalog.bindings left.catalog)
       (Catalog.bindings right.catalog)
  && List.equal
       (fun left right ->
          same_string
            (Lock.Dependency_selection.name left)
            (Lock.Dependency_selection.name right)
          && same_id
               (Lock.Dependency_selection.declaration left)
               (Lock.Dependency_selection.declaration right))
       left.dependencies
       right.dependencies
;;

let canonical budget inputs =
  let open Result.Let_syntax in
  let%bind inputs =
    List.fold_result inputs ~init:[] ~f:(fun inputs input ->
      let%map input = normalize budget input in
      input :: inputs)
  in
  let inputs =
    List.sort inputs ~compare:(fun left right ->
      Budget.compare_string budget (namespace left) (namespace right))
  in
  let rec collect selected = function
    | [] -> Ok (List.rev selected)
    | input :: rest ->
      Budget.checkpoint budget;
      (match selected with
       | previous :: _
         when Budget.compare_string budget (namespace previous) (namespace input) = 0 ->
         if same_input budget previous input
         then collect selected rest
         else failure input (Namespace_conflict (namespace input))
       | [] | _ :: _ -> collect (input :: selected) rest)
  in
  collect [] inputs
;;

let check_target budget owner dependency target =
  let checkpoint () = Budget.checkpoint budget in
  let same left right = Budget.compare_string budget left right = 0 in
  let target_project = Catalog.project target.Input.catalog in
  let expected_namespace =
    Option.value
      (Project.Dependency.namespace_override dependency)
      ~default:(Project.namespace target_project)
  in
  let open Result.Let_syntax in
  let%bind resolved =
    Dependency.resolve
      dependency
      ~owner_origin:owner.Input.origin
      ~owner_declaration:(Input.declaration owner)
      ~checkpoint
    |> Result.map_error ~f:(fun _ ->
      Model.Error.create
        ~declared_at:(Project.Dependency.declared_at dependency)
        (Invalid_dependency (Project.Dependency.name dependency)))
  in
  let%bind selection =
    Project.select
      target_project
      ~request:(Library { environment = Project.Dependency.environment dependency })
      ~checkpoint
    |> Result.map_error ~f:(fun error -> Model.Error.create (Declaration error))
  in
  if
    Dependency.matches
      resolved
      ~origin:target.origin
      ~declaration:(Input.declaration target)
      ~checkpoint
    && same expected_namespace (namespace target)
    && same
         (Project.Scope_spec.name (Project.Selection.environment selection))
         (Project.Scope_spec.name
            (Project.Selection.environment (Catalog.selection target.catalog)))
  then Ok ()
  else failure owner (Invalid_dependency (Project.Dependency.name dependency))
;;

let select budget root ~catalogs =
  let checkpoint () = Budget.checkpoint budget in
  let same_id left right = Budget.compare_id budget left right = 0 in
  let open Result.Let_syntax in
  let%bind catalogs = canonical budget (root :: catalogs) in
  let find declaration =
    List.find catalogs ~f:(fun input -> same_id (Input.declaration input) declaration)
  in
  let rec visit visited = function
    | [] ->
      (match
         List.find catalogs ~f:(fun input ->
           not (List.exists visited ~f:(fun id -> same_id id (Input.declaration input))))
       with
       | None -> Ok catalogs
       | Some extra -> failure extra (Unexpected_catalog (Input.declaration extra)))
    | declaration :: pending ->
      checkpoint ();
      if List.exists visited ~f:(same_id declaration)
      then visit visited pending
      else (
        let%bind owner =
          match find declaration with
          | Some owner -> Ok owner
          | None -> failure root (Missing_catalog declaration)
        in
        let dependencies = Scope.dependencies owner.catalog ~checkpoint in
        let rec edges selected requested pending =
          checkpoint ();
          match selected, requested with
          | [], [] -> Ok pending
          | selected :: selections, dependency :: dependencies ->
            let name = Project.Dependency.name dependency in
            if
              Budget.compare_string budget (Lock.Dependency_selection.name selected) name
              <> 0
            then failure owner (Invalid_dependency name)
            else (
              let target_id = Lock.Dependency_selection.declaration selected in
              let%bind target =
                match find target_id with
                | Some target -> Ok target
                | None -> failure owner (Missing_catalog target_id)
              in
              let%bind () = check_target budget owner dependency target in
              edges selections dependencies (target_id :: pending))
          | selection :: _, [] ->
            failure owner (Invalid_dependency (Lock.Dependency_selection.name selection))
          | [], dependency :: _ ->
            failure owner (Invalid_dependency (Project.Dependency.name dependency))
        in
        let%bind pending = edges owner.dependencies dependencies pending in
        visit (declaration :: visited) pending)
  in
  visit [] [ Input.declaration root ]
;;
