open Core
module Model = Project_build_model
module Project = Chatml_project
module Captured = Chatml_source
module Lock = Project.Lock
module Dependency = Project_build_dependency
module Budget = Project_build_budget

type t =
  { previous : Lock.t option
  ; preserves_git_selections : bool
  }

let previous_lock t = t.previous
let preserves_git_selections t = t.preserves_git_selections

let create operation ~document ~checkpoint =
  let previous, preserves_git_selections =
    match operation with
    | Model.Operation.Initial_prepare -> None, false
    | Build { previous_lock } -> Some previous_lock, true
    | Update { previous_lock } -> previous_lock, false
  in
  let open Result.Let_syntax in
  let%map previous =
    match previous with
    | None ->
      checkpoint ();
      Ok None
    | Some source ->
      Lock.decode source ~limits:document ~checkpoint
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun error -> Model.Error.create (Declaration error))
  in
  { previous; preserves_git_selections }
;;

let check_root t ~origin ~project ~budget =
  let checkpoint () = Budget.checkpoint budget in
  checkpoint ();
  if not t.preserves_git_selections
  then Ok ()
  else (
    let previous =
      Option.bind t.previous ~f:(fun lock ->
        List.find (Lock.catalogs lock) ~f:(fun catalog ->
          Budget.compare_id budget (Lock.Catalog.declaration catalog) (Lock.root lock) = 0))
    in
    let admitted =
      match previous with
      | None -> false
      | Some previous ->
        (match origin, Lock.Catalog.origin previous with
         | Lock.Catalog.Local, Local -> true
         | Local, Git _ | Git _, Local -> false
         | Git requested, Git selected ->
           Budget.compare_string budget requested.repository selected.repository = 0
           && Project.Git_pin.equal requested.pin selected.pin
           && Option.equal
                Source.Portable_path.equal
                requested.package_root
                selected.package_root
           && Budget.compare_string
                budget
                (Source.Portable_path.to_string project)
                (Captured.Id.relative_path (Lock.Catalog.declaration previous))
              = 0)
    in
    if admitted then Ok () else Error (Model.Error.create Needs_update))
;;

let selected_dependency lock ~owner ~name ~budget =
  let checkpoint () = Budget.checkpoint budget in
  let owner_id = Captured.id (Project.source owner) in
  let find_catalog declaration =
    List.find (Lock.catalogs lock) ~f:(fun catalog ->
      checkpoint ();
      Budget.compare_id budget (Lock.Catalog.declaration catalog) declaration = 0)
  in
  let open Option.Let_syntax in
  let%bind catalog = find_catalog owner_id in
  let%bind dependency =
    List.find (Lock.Catalog.dependencies catalog) ~f:(fun dependency ->
      checkpoint ();
      Budget.compare_string budget (Lock.Dependency_selection.name dependency) name = 0)
  in
  find_catalog (Lock.Dependency_selection.declaration dependency)
;;

let check_dependency t ~owner ~owner_origin dependency ~budget =
  let checkpoint () = Budget.checkpoint budget in
  checkpoint ();
  let open Result.Let_syntax in
  let%bind resolved =
    Dependency.resolve
      dependency
      ~owner_origin
      ~owner_declaration:(Captured.id (Project.source owner))
      ~checkpoint
    |> Result.map_error ~f:(fun _ ->
      Model.Error.create
        ~declared_at:(Project.Dependency.declared_at dependency)
        (Invalid_dependency (Project.Dependency.name dependency)))
  in
  if not t.preserves_git_selections
  then Ok ()
  else (
    let selected =
      Option.bind t.previous ~f:(fun lock ->
        selected_dependency lock ~owner ~name:(Project.Dependency.name dependency) ~budget)
    in
    let admitted =
      match selected with
      | None ->
        (match Dependency.origin resolved with
         | Lock.Catalog.Local -> true
         | Git _ -> false)
      | Some previous ->
        (match Dependency.origin resolved, Lock.Catalog.origin previous with
         | Local, Local -> true
         | Local, Git _ | Git _, Local -> false
         | Git _, Git _ ->
           Dependency.matches
             resolved
             ~origin:(Lock.Catalog.origin previous)
             ~declaration:(Lock.Catalog.declaration previous)
             ~checkpoint
           && Option.for_all
                (Project.Dependency.environment dependency)
                ~f:(fun environment ->
                  Budget.compare_string
                    budget
                    environment
                    (Lock.Catalog.selected_environment previous)
                  = 0)
           && Option.for_all
                (Project.Dependency.namespace_override dependency)
                ~f:(fun namespace ->
                  Budget.compare_string budget namespace (Lock.Catalog.namespace previous)
                  = 0))
    in
    if admitted
    then Ok ()
    else
      Error
        (Model.Error.create
           ~declared_at:(Project.Dependency.declared_at dependency)
           Needs_update))
;;

let validate_captures t ~budget ~root ~catalogs =
  let module Budget = Project_build_budget in
  let module Input = Model.Catalog_input in
  let checkpoint () = Budget.checkpoint budget in
  let same_id left right = Budget.compare_id budget left right = 0 in
  let same_origin (left : Lock.Catalog.origin) (right : Lock.Catalog.origin) =
    match left, right with
    | Local, Local -> true
    | Git left, Git right ->
      Budget.compare_string budget left.repository right.repository = 0
      && Project.Git_pin.equal left.pin right.pin
      && Option.equal Source.Portable_path.equal left.package_root right.package_root
    | Local, Git _ | Git _, Local -> false
  in
  let rec same_sources expected actual =
    checkpoint ();
    match expected, actual with
    | [], [] -> true
    | expected :: expected_rest, actual :: actual_rest ->
      same_id (Lock.Source_entry.id expected) (Captured.id actual)
      && Captured.Digest.equal
           (Lock.Source_entry.digest expected)
           (Captured.digest actual)
      && Int.equal (Lock.Source_entry.byte_length expected) (Captured.byte_length actual)
      && same_sources expected_rest actual_rest
    | [], _ :: _ | _ :: _, [] -> false
  in
  checkpoint ();
  if not t.preserves_git_selections
  then Ok ()
  else
    let open Result.Let_syntax in
    let%bind () =
      let previous_root =
        Option.bind t.previous ~f:(fun lock ->
          if not (same_id (Lock.root lock) (Input.declaration root))
          then None
          else
            List.find (Lock.catalogs lock) ~f:(fun previous ->
              same_id (Lock.Catalog.declaration previous) (Lock.root lock)))
      in
      match previous_root with
      | Some previous when same_origin (Lock.Catalog.origin previous) root.Input.origin ->
        Ok ()
      | None | Some _ ->
        Error (Model.Error.create ~declared_at:(Input.declared_at root) Needs_update)
    in
    List.fold_result catalogs ~init:() ~f:(fun () input ->
      checkpoint ();
      match input.Input.origin with
      | Local -> Ok ()
      | Git _ ->
        let previous =
          Option.bind t.previous ~f:(fun lock ->
            List.find (Lock.catalogs lock) ~f:(fun previous ->
              same_id (Lock.Catalog.declaration previous) (Input.declaration input)))
        in
        let fail problem =
          Error (Model.Error.create ~declared_at:(Input.declared_at input) problem)
        in
        (match previous with
         | None -> fail Needs_update
         | Some previous ->
           if not (same_origin (Lock.Catalog.origin previous) input.origin)
           then fail Needs_update
           else if
             same_sources
               (Lock.Catalog.sources previous)
               (Chatml_catalog.sources input.catalog)
           then Ok ()
           else fail Lock_mismatch))
;;
