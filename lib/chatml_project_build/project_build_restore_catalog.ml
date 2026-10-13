open Core
module Model = Project_build_model
module Budget = Project_build_budget
module Project = Chatml_project
module Catalog = Chatml_catalog
module Captured = Chatml_source

let directory_key directory =
  match Project.Source_dir.directory directory with
  | Root -> "."
  | Relative path -> Source.Portable_path.to_string path
;;

let create budget ~project ~request ~captured_entry ~sources =
  let checkpoint () = Budget.checkpoint budget in
  let catalog result =
    Result.map_error result ~f:(fun error -> Model.Error.create (Catalog error))
  in
  let open Result.Let_syntax in
  let%bind discovery =
    Catalog.Plan.Discovery.create project ~request ~checkpoint |> catalog
  in
  let directories =
    List.fold
      (Project.source_dirs project)
      ~init:String.Map.empty
      ~f:(fun directories directory ->
        checkpoint ();
        let key = directory_key directory in
        if Map.mem directories key
        then directories
        else Map.set directories ~key ~data:directory)
  in
  let%bind observations =
    List.fold_result sources ~init:[] ~f:(fun observations source ->
      checkpoint ();
      let path = Captured.Id.relative_path (Captured.id source) in
      let parent, name =
        match String.rsplit2 path ~on:'/' with
        | None -> ".", path
        | Some pair -> pair
      in
      match Map.find directories parent with
      | None -> Ok observations
      | Some directory ->
        let%map observation =
          Catalog.Plan.Discovery.observe discovery ~directory ~name |> catalog
        in
        (match observation with
         | None -> observations
         | Some observation -> observation :: observations))
  in
  let%bind plan = Catalog.Plan.create discovery ~observations ~checkpoint |> catalog in
  let%bind sources =
    match captured_entry with
    | None -> Ok sources
    | Some entry ->
      let entry_id = Captured.id entry in
      let matching =
        List.filter sources ~f:(fun source ->
          checkpoint ();
          Budget.compare_id budget (Captured.id source) entry_id = 0)
      in
      (match matching with
       | [] -> Ok sources
       | [ candidate ] ->
         if not (Budget.equal_source budget candidate entry)
         then Error (Model.Error.create (Duplicate_capture entry_id))
         else (
           let is_declaration =
             Budget.compare_id budget entry_id (Captured.id (Project.source project)) = 0
           in
           let is_planned =
             List.exists (Catalog.Plan.members plan) ~f:(fun member ->
               checkpoint ();
               String.equal
                 (Source.Portable_path.to_string (Catalog.Plan.Member.path member))
                 (Captured.Id.relative_path entry_id))
           in
           if is_declaration || is_planned
           then Ok sources
           else
             Ok
               (List.filter sources ~f:(fun source ->
                  checkpoint ();
                  Budget.compare_id budget (Captured.id source) entry_id <> 0)))
       | _ :: _ :: _ -> Error (Model.Error.create (Duplicate_capture entry_id)))
  in
  Catalog.create plan ~sources ~checkpoint |> catalog
;;
