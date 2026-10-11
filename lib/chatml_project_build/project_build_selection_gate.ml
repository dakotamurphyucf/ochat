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

module Owner = struct
  type gate = t

  type t =
    { gate : gate
    ; catalog : Lock.Catalog.t
    }

  let declaration t = Lock.Catalog.declaration t.catalog
end

let find_catalog t declaration ~budget =
  Option.bind t.previous ~f:(fun lock ->
    List.find (Lock.catalogs lock) ~f:(fun catalog ->
      Budget.checkpoint budget;
      Budget.compare_id budget (Lock.Catalog.declaration catalog) declaration = 0))
;;

let same_origin (left : Lock.Catalog.origin) (right : Lock.Catalog.origin) ~budget =
  match left, right with
  | Lock.Catalog.Local, Local -> true
  | Local, Git _ | Git _, Local -> false
  | Git left, Git right ->
    Budget.compare_string budget left.repository right.repository = 0
    && Project.Git_pin.equal left.pin right.pin
    && Option.equal Source.Portable_path.equal left.package_root right.package_root
;;

let matches_owner previous ~origin ~project ~budget =
  same_origin origin (Lock.Catalog.origin previous) ~budget
  &&
  match origin with
  | Local -> true
  | Git _ ->
    Budget.compare_string
      budget
      project
      (Captured.Id.relative_path (Lock.Catalog.declaration previous))
    = 0
;;

let root_owner t ~origin ~project ~budget =
  Budget.checkpoint budget;
  if not t.preserves_git_selections
  then Ok None
  else (
    let previous =
      Option.bind t.previous ~f:(fun lock -> find_catalog t (Lock.root lock) ~budget)
    in
    match previous with
    | Some catalog when matches_owner catalog ~origin ~project ~budget ->
      Ok (Some { Owner.gate = t; catalog })
    | None | Some _ -> Error (Model.Error.create Needs_update))
;;

let check_root t ~origin ~project ~budget =
  root_owner t ~origin ~project:(Source.Portable_path.to_string project) ~budget
;;

let selected_dependency t previous ~name ~budget =
  let open Option.Let_syntax in
  let%bind dependency =
    List.find (Lock.Catalog.dependencies previous.Owner.catalog) ~f:(fun dependency ->
      Budget.checkpoint budget;
      Budget.compare_string budget (Lock.Dependency_selection.name dependency) name = 0)
  in
  let%map catalog =
    find_catalog t (Lock.Dependency_selection.declaration dependency) ~budget
  in
  { Owner.gate = t; catalog }
;;

let check_dependency t ~previous_owner ~owner ~owner_origin dependency ~budget =
  let checkpoint () = Budget.checkpoint budget in
  let declared_at = Project.Dependency.declared_at dependency in
  let fail problem = Error (Model.Error.create ~declared_at problem) in
  checkpoint ();
  let open Result.Let_syntax in
  (* Affinity is lifecycle identity, not semantic source equality. Never consume
     another gate's association, even for an Initial/Update operation. *)
  let%bind () =
    match previous_owner with
    | Some previous when not (phys_equal previous.Owner.gate t) ->
      fail (Invalid_dependency (Project.Dependency.name dependency))
    | None | Some _ -> Ok ()
  in
  let%bind () =
    if not t.preserves_git_selections
    then Ok ()
    else (
      match previous_owner with
      | None -> Ok ()
      | Some previous ->
        if
          matches_owner
            previous.Owner.catalog
            ~origin:owner_origin
            ~project:(Captured.Id.relative_path (Captured.id (Project.source owner)))
            ~budget
        then Ok ()
        else fail Needs_update)
  in
  let%bind resolved =
    Dependency.resolve
      dependency
      ~owner_origin
      ~owner_declaration:(Captured.id (Project.source owner))
      ~checkpoint
    |> Result.map_error ~f:(fun _ ->
      Model.Error.create
        ~declared_at
        (Invalid_dependency (Project.Dependency.name dependency)))
  in
  if not t.preserves_git_selections
  then Ok None
  else (
    let selected =
      Option.bind previous_owner ~f:(fun previous ->
        selected_dependency t previous ~name:(Project.Dependency.name dependency) ~budget)
    in
    let admitted =
      match selected with
      | None ->
        (match Dependency.origin resolved with
         | Local -> true
         | Git _ -> false)
      | Some previous ->
        (match Dependency.origin resolved, Lock.Catalog.origin previous.Owner.catalog with
         | Local, Local -> true
         | Local, Git _ | Git _, Local -> false
         | Git _, Git _ ->
           Dependency.matches
             resolved
             ~origin:(Lock.Catalog.origin previous.catalog)
             ~declaration:(Owner.declaration previous)
             ~checkpoint)
    in
    if admitted then Ok selected else fail Needs_update)
;;

let validate_overlap previous actual ~budget =
  let path expected = Captured.Id.relative_path (Lock.Source_entry.id expected) in
  let expected =
    List.sort (Lock.Catalog.sources previous) ~compare:(fun left right ->
      Budget.compare_string budget (path left) (path right))
  in
  let actual =
    List.sort actual ~compare:(fun left right ->
      Budget.compare_string
        budget
        (Captured.Id.relative_path (Captured.id left))
        (Captured.Id.relative_path (Captured.id right)))
  in
  let rec overlap expected actual =
    Budget.checkpoint budget;
    match expected, actual with
    | [], _ | _, [] -> true
    | first :: rest, source :: sources ->
      let comparison =
        Budget.compare_string
          budget
          (path first)
          (Captured.Id.relative_path (Captured.id source))
      in
      if comparison < 0
      then overlap rest actual
      else if comparison > 0
      then overlap expected sources
      else
        Captured.Digest.equal (Lock.Source_entry.digest first) (Captured.digest source)
        && Int.equal (Lock.Source_entry.byte_length first) (Captured.byte_length source)
        && overlap rest sources
  in
  overlap expected actual
;;

let validate_captures t ~budget ~root ~catalogs =
  let module Input = Model.Catalog_input in
  let checkpoint () = Budget.checkpoint budget in
  checkpoint ();
  if not t.preserves_git_selections
  then Ok ()
  else
    let open Result.Let_syntax in
    let%bind previous_root =
      root_owner
        t
        ~origin:root.Input.origin
        ~project:(Captured.Id.relative_path (Input.declaration root))
        ~budget
      |> Result.map_error ~f:(fun error ->
        Model.Error.create
          ~declared_at:(Input.declared_at root)
          (Model.Error.problem error))
    in
    let same_id left right = Budget.compare_id budget left right = 0 in
    let same_previous left right =
      Option.equal
        (fun left right -> same_id (Owner.declaration left) (Owner.declaration right))
        left
        right
    in
    let find declaration =
      List.find catalogs ~f:(fun input ->
        checkpoint ();
        same_id (Input.declaration input) declaration)
    in
    (* Pair identity is necessary: a current alias merge must not erase a second
       prior route. All comparisons/visits consume this invocation's budget. *)
    let rec visit visited = function
      | [] -> Ok ()
      | (input, previous) :: pending ->
        checkpoint ();
        if
          List.exists visited ~f:(fun (declaration, old) ->
            checkpoint ();
            same_id declaration (Input.declaration input) && same_previous old previous)
        then visit visited pending
        else (
          let fail problem =
            Error (Model.Error.create ~declared_at:(Input.declared_at input) problem)
          in
          let%bind () =
            match input.Input.origin, previous with
            | Local, _ -> Ok ()
            | Git _, None -> fail Needs_update
            | Git _, Some previous ->
              if
                not
                  (matches_owner
                     previous.catalog
                     ~origin:input.origin
                     ~project:(Captured.Id.relative_path (Input.declaration input))
                     ~budget)
              then fail Needs_update
              else if
                validate_overlap
                  previous.catalog
                  (Chatml_catalog.sources input.catalog)
                  ~budget
              then Ok ()
              else fail Lock_mismatch
          in
          let dependencies = Project_build_scope.dependencies input.catalog ~checkpoint in
          let%bind pending =
            List.fold_result dependencies ~init:pending ~f:(fun pending dependency ->
              let%bind old_target =
                check_dependency
                  t
                  ~previous_owner:previous
                  ~owner:(Chatml_catalog.project input.catalog)
                  ~owner_origin:input.origin
                  dependency
                  ~budget
              in
              let name = Project.Dependency.name dependency in
              let%bind selection =
                List.find input.dependencies ~f:(fun selection ->
                  Budget.compare_string
                    budget
                    (Lock.Dependency_selection.name selection)
                    name
                  = 0)
                |> Result.of_option
                     ~error:
                       (Model.Error.create
                          ~declared_at:(Project.Dependency.declared_at dependency)
                          (Invalid_dependency name))
              in
              let id = Lock.Dependency_selection.declaration selection in
              let%map target =
                find id
                |> Result.of_option
                     ~error:
                       (Model.Error.create
                          ~declared_at:(Project.Dependency.declared_at dependency)
                          (Missing_catalog id))
              in
              (target, old_target) :: pending)
          in
          visit ((Input.declaration input, previous) :: visited) pending)
    in
    visit [] [ root, previous_root ]
;;
