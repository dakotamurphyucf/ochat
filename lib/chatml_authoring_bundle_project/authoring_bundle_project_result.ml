open Core
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source
module Build = Chatml_project_build
module Project = Chatml_project
module Prepared = Chatml_prepared_program
module Wire_source = Agent_protocol.Authoring_common.Source

type error =
  | Declaration of Project.Error.t
  | Mismatch

let selection_request selection =
  match selection.Agent_protocol.Authoring_context.Context.Selection.target with
  | Entry name -> Project.Selection.Entry { name; environment = selection.environment }
  | Library -> Project.Selection.Library { environment = selection.environment }
;;

let equal_origin left right =
  match left, right with
  | Project.Lock.Catalog.Local, Project.Lock.Catalog.Local -> true
  | Git left, Git right ->
    String.equal left.repository right.repository
    && Project.Git_pin.equal left.pin right.pin
    && Option.equal Source.Portable_path.equal left.package_root right.package_root
  | Local, Git _ | Git _, Local -> false
;;

let equal_dependency left right =
  String.equal
    (Project.Lock.Dependency_selection.name left)
    (Project.Lock.Dependency_selection.name right)
  && Captured.Id.equal
       (Project.Lock.Dependency_selection.declaration left)
       (Project.Lock.Dependency_selection.declaration right)
;;

let equal_routes project lock ~checkpoint =
  let expected = Bundle.Manifest.Project.routes project in
  let actual = Project.Lock.catalogs lock in
  let rec equal_dependencies expected actual =
    match expected, actual with
    | [], [] -> true
    | [], _ :: _ | _ :: _, [] -> false
    | expected :: expected_rest, actual :: actual_rest ->
      checkpoint ();
      equal_dependency expected actual && equal_dependencies expected_rest actual_rest
  in
  if not (Int.equal (List.length expected) (List.length actual))
  then false
  else (
    let actual =
      List.fold
        actual
        ~init:(Map.empty (module Captured.Id))
        ~f:(fun map catalog ->
          checkpoint ();
          Map.set map ~key:(Project.Lock.Catalog.declaration catalog) ~data:catalog)
    in
    List.for_all expected ~f:(fun route ->
      checkpoint ();
      match Map.find actual (Bundle.Manifest.Route.declaration route) with
      | None -> false
      | Some catalog ->
        equal_origin
          (Bundle.Manifest.Route.origin route)
          (Project.Lock.Catalog.origin catalog)
        && equal_dependencies
             (Bundle.Manifest.Route.dependencies route)
             (Project.Lock.Catalog.dependencies catalog)))
;;

let equal_sources expected actual ~checkpoint =
  let actual =
    List.fold
      actual
      ~init:(Map.empty (module Captured.Id))
      ~f:(fun map source ->
        checkpoint ();
        Map.set map ~key:(Captured.id source) ~data:source)
  in
  Int.equal (List.length expected) (Map.length actual)
  && List.for_all expected ~f:(fun source ->
    checkpoint ();
    Option.value_map
      (Map.find actual (Captured.id source))
      ~default:false
      ~f:(fun actual ->
        Captured.Digest.equal (Captured.digest source) (Captured.digest actual)
        && Int.equal (Captured.byte_length source) (Captured.byte_length actual)))
;;

let reconcile project build ~root_source ~expected_sources ~document ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let lock = Build.lock build in
  let prepared = Build.prepared build in
  let root_id = Bundle.Manifest.Project.root_source project in
  let%bind declaration =
    Project.decode root_source ~limits:document ~checkpoint
    |> Result.map_error ~f:(fun error -> Declaration error)
  in
  let%bind selected =
    Project.select
      declaration
      ~request:(selection_request (Bundle.Manifest.Project.selection project))
      ~checkpoint
    |> Result.map_error ~f:(fun error -> Declaration error)
  in
  let environment = Project.Scope_spec.name (Project.Selection.environment selected) in
  let expected_entry =
    match Bundle.Manifest.Project.selection project with
    | { target = Entry name; _ } -> Project.Lock.Entry.Declared name
    | { target = Library; _ } -> Captured
  in
  let actual_entry = Project.Lock.Entry.selection (Project.Lock.entry_selection lock) in
  let equal_entry =
    match expected_entry, actual_entry with
    | Declared left, Declared right -> String.equal left right
    | Captured, Captured -> true
    | Declared _, Captured | Captured, Declared _ -> false
  in
  let captured_entry_matches =
    match Bundle.Manifest.Project.entry project with
    | Declared -> true
    | Captured captured ->
      Captured.Id.equal
        (Wire_source.id (Bundle.Manifest.Project.Captured_entry.source captured))
        (Project.Lock.entry lock)
  in
  if
    (not (Captured.Id.equal root_id (Captured.id root_source)))
    || (not (Captured.Id.equal root_id (Project.Lock.root lock)))
    || (not (String.equal environment (Project.Lock.environment lock)))
    || (not equal_entry)
    || (not captured_entry_matches)
    || (not (Captured.Id.equal (Project.Lock.entry lock) (Prepared.entry prepared)))
    || (not (equal_routes project lock ~checkpoint))
    || (not (equal_sources expected_sources (Prepared.sources prepared) ~checkpoint))
    || not
         (Captured.Digest.equal
            (Project.Lock.compiler_contract lock)
            (Prepared.compiler_contract prepared))
  then Error Mismatch
  else Ok ()
;;
