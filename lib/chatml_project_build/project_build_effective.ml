open Core
module Model = Project_build_model
module Budget = Project_build_budget
module Gate = Project_build_selection_gate
module Assembly = Project_build_assembly
module Limits = Model.Limits
module Error = Model.Error
module Entry = Model.Entry
module Operation = Model.Operation
module Captured = Chatml_source
module Project = Chatml_project
module Lock = Project.Lock
module Catalog = Chatml_catalog
module Input = Model.Catalog_input

module Route = struct
  type t =
    { declaration : Captured.Id.t
    ; origin : Lock.Catalog.origin
    ; dependencies : Lock.Dependency_selection.t list
    }

  let create ~declaration ~origin ~dependencies = { declaration; origin; dependencies }
  let declaration t = t.declaration
end

type decoded =
  { route : Route.t
  ; project : Project.t
  ; source : Captured.t
  }

let fail problem = Error (Error.create problem)

let compose_with_budget
      budget
      ~root_project
      ~selection
      ~entry
      ~sources
      ~routes
      ~previous
      ~lock_id
      ~contract
      ~limits
  =
  let open Result.Let_syntax in
  let checkpoint () = Budget.checkpoint budget in
  let same_id left right = Budget.compare_id budget left right = 0 in
  let same_string left right = Budget.compare_string budget left right = 0 in
  let old_lock_id =
    Option.map previous ~f:(fun build -> Captured.id (Assembly.lock_source build))
  in
  let%bind () =
    if Option.exists old_lock_id ~f:(fun id -> same_id id lock_id)
    then fail Invalid_lock_identity
    else Ok ()
  in
  let%bind () =
    match previous with
    | None -> Ok ()
    | Some build ->
      if same_id (Lock.root (Assembly.lock build)) root_project
      then Ok ()
      else fail Needs_update
  in
  (* Preflight every supplied byte before sorting, decoding or retaining a
         second inventory. Assembly charges the selected bytes and new lock. *)
  let%bind _ =
    List.fold_result sources ~init:0 ~f:(fun total source ->
      checkpoint ();
      let bytes = Captured.byte_length source in
      if bytes > Int.max_value - total
      then fail Accounting_overflow
      else (
        let total = total + bytes in
        if
          Option.exists (Limits.max_input_bytes limits) ~f:(fun maximum ->
            total > maximum)
        then fail Limit_exceeded
        else Ok total))
  in
  let%bind source_by_id =
    List.fold_result
      sources
      ~init:(Map.empty (module Captured.Id))
      ~f:(fun seen source ->
        checkpoint ();
        let id = Captured.id source in
        if same_id id lock_id || Option.exists old_lock_id ~f:(same_id id)
        then fail Invalid_lock_identity
        else if Map.mem seen id
        then fail (Duplicate_capture id)
        else Ok (Map.set seen ~key:id ~data:source))
  in
  let%bind route_by_id =
    List.fold_result
      routes
      ~init:(Map.empty (module Captured.Id))
      ~f:(fun seen route ->
        checkpoint ();
        let id = route.Route.declaration in
        if Map.mem seen id
        then fail (Duplicate_capture id)
        else if not (Map.mem source_by_id id)
        then fail (Missing_capture id)
        else Ok (Map.set seen ~key:id ~data:route))
  in
  let%bind decoded =
    Map.fold
      route_by_id
      ~init:(Ok (Map.empty (module Captured.Id)))
      ~f:(fun ~key:id ~data:route result ->
        let%bind result = result in
        checkpoint ();
        let source = Map.find_exn source_by_id id in
        let%map project =
          Project.decode source ~limits:(Limits.document limits) ~checkpoint
          |> Result.map_error ~f:(fun error -> Error.create (Declaration error))
        in
        Map.set result ~key:id ~data:{ route; project; source })
  in
  let%bind () =
    List.fold_result sources ~init:() ~f:(fun () source ->
      checkpoint ();
      let id = Captured.id source in
      let namespace = Captured.Id.package_namespace id in
      let owned =
        Map.exists decoded ~f:(fun decoded ->
          checkpoint ();
          same_string namespace (Captured.Id.package_namespace decoded.route.declaration))
      in
      if owned then Ok () else fail (Unexpected_catalog id))
  in
  let%bind root =
    Map.find decoded root_project
    |> Result.of_option ~error:(Error.create (Missing_catalog root_project))
  in
  let select project request =
    Project.select project ~request ~checkpoint
    |> Result.map_error ~f:(fun error -> Error.create (Declaration error))
  in
  let%bind root_selection = select root.project selection in
  let%bind requests =
    Map.fold
      decoded
      ~init:(Ok (Map.singleton (module Captured.Id) root_project selection))
      ~f:(fun ~key:_ ~data:owner result ->
        let%bind requests = result in
        List.fold_result
          owner.route.dependencies
          ~init:requests
          ~f:(fun requests association ->
            checkpoint ();
            let name = Lock.Dependency_selection.name association in
            let target_id = Lock.Dependency_selection.declaration association in
            let%bind dependency =
              List.find (Project.dependencies owner.project) ~f:(fun dependency ->
                checkpoint ();
                same_string (Project.Dependency.name dependency) name)
              |> Result.of_option ~error:(Error.create (Invalid_dependency name))
            in
            let%bind target =
              Map.find decoded target_id
              |> Result.of_option ~error:(Error.create (Missing_catalog target_id))
            in
            let request =
              Project.Selection.Library
                { environment = Project.Dependency.environment dependency }
            in
            let%bind target_selection = select target.project request in
            let target_environment =
              Project.Scope_spec.name (Project.Selection.environment target_selection)
            in
            match Map.find requests target_id with
            | None -> Ok (Map.set requests ~key:target_id ~data:request)
            | Some previous_request ->
              let%bind selected = select target.project previous_request in
              if
                not
                  (same_string
                     target_environment
                     (Project.Scope_spec.name (Project.Selection.environment selected)))
              then fail (Invalid_dependency name)
              else Ok requests))
  in
  let%bind () =
    if Map.length requests <> Map.length decoded
    then (
      match
        Map.fold decoded ~init:None ~f:(fun ~key:id ~data:_ missing ->
          checkpoint ();
          if Option.is_none missing && not (Map.mem requests id) then Some id else missing)
      with
      | Some id -> fail (Unexpected_catalog id)
      | None -> fail Lock_mismatch)
    else Ok ()
  in
  let%bind inputs =
    Map.fold decoded ~init:(Ok []) ~f:(fun ~key:id ~data:decoded result ->
      let%bind inputs = result in
      checkpoint ();
      let request = Map.find_exn requests id in
      let namespace = Captured.Id.package_namespace id in
      let captures =
        List.filter sources ~f:(fun source ->
          checkpoint ();
          same_string namespace (Captured.Id.package_namespace (Captured.id source)))
      in
      let%map catalog =
        Project_build_restore_catalog.create
          budget
          ~project:decoded.project
          ~request
          ~sources:captures
      in
      { Input.catalog
      ; origin = decoded.route.origin
      ; acquisition_key = None
      ; dependencies = decoded.route.dependencies
      }
      :: inputs)
  in
  let roots, catalogs =
    List.partition_tf inputs ~f:(fun input ->
      checkpoint ();
      same_id (Input.declaration input) root_project)
  in
  let%bind root_input =
    match roots with
    | [ root ] -> Ok root
    | [] -> fail (Missing_catalog root_project)
    | _ :: _ -> fail Lock_mismatch
  in
  let%bind () =
    if
      match entry, Project.Selection.entry root_selection with
      | Entry.Declared, Some _ | Captured _, None -> false
      | Declared, None | Captured _, Some _ -> true
    then fail Invalid_entry_selection
    else Ok ()
  in
  let%bind () =
    match entry with
    | Declared -> Ok ()
    | Captured source ->
      let id = Captured.id source in
      if same_id id lock_id || Option.exists old_lock_id ~f:(same_id id)
      then fail Invalid_lock_identity
      else (
        match Map.find source_by_id id with
        | None -> Ok ()
        | Some present ->
          if Budget.equal_source budget present source
          then Ok ()
          else fail (Duplicate_capture id))
  in
  let operation =
    match previous with
    | None -> Operation.Build_without_previous
    | Some build -> Build { previous_lock = Assembly.lock_source build }
  in
  let%bind gate = Gate.create operation ~document:(Limits.document limits) ~checkpoint in
  let%bind catalogs = Project_build_closure.select budget root_input ~catalogs in
  let%bind () = Gate.validate_captures gate ~budget ~root:root_input ~catalogs in
  let%map assembly =
    Assembly.create
      budget
      root_input
      ~catalogs
      ~entry
      ~contract
      ~lock:(Generate lock_id)
      ~limits
  in
  assembly
;;

let compose
      ~root_project
      ~selection
      ~entry
      ~sources
      ~routes
      ~previous
      ~lock_id
      ~contract
      ~limits
      ~checkpoint
  =
  Budget.run limits ~checkpoint ~f:(fun budget ->
    compose_with_budget
      budget
      ~root_project
      ~selection
      ~entry
      ~sources
      ~routes
      ~previous
      ~lock_id
      ~contract
      ~limits)
;;

let recompose previous ~sources ~lock_id ~contract ~limits ~checkpoint =
  Budget.run limits ~checkpoint ~f:(fun budget ->
    let checkpoint () = Budget.checkpoint budget in
    let same_id left right = Budget.compare_id budget left right = 0 in
    let receipt = Assembly.lock previous in
    let old_lock = Assembly.lock_source previous in
    let old_lock_id = Captured.id old_lock in
    let open Result.Let_syntax in
    let%bind sources, found_lock =
      List.fold_result sources ~init:([], false) ~f:(fun (sources, found) source ->
        checkpoint ();
        let id = Captured.id source in
        if same_id id old_lock_id
        then
          if (not found) && Budget.equal_source budget source old_lock
          then Ok (sources, true)
          else fail Lock_mismatch
        else Ok (source :: sources, found))
    in
    let%bind () = if found_lock then Ok () else fail Lock_mismatch in
    let sources = List.rev sources in
    let routes =
      List.map (Lock.catalogs receipt) ~f:(fun catalog ->
        checkpoint ();
        Route.create
          ~declaration:(Lock.Catalog.declaration catalog)
          ~origin:(Lock.Catalog.origin catalog)
          ~dependencies:(Lock.Catalog.dependencies catalog))
    in
    let root_project = Lock.root receipt in
    let environment = Some (Lock.environment receipt) in
    let%bind selection, entry, sources =
      match Lock.Entry.selection (Lock.entry_selection receipt) with
      | Declared name ->
        Ok (Project.Selection.Entry { name; environment }, Entry.Declared, sources)
      | Captured ->
        let id = Lock.entry receipt in
        let catalog_member =
          List.exists (Lock.catalogs receipt) ~f:(fun catalog ->
            checkpoint ();
            List.exists (Lock.Catalog.sources catalog) ~f:(fun member ->
              checkpoint ();
              same_id (Lock.Source_entry.id member) id))
        in
        let%bind found, catalog_sources =
          List.fold_result sources ~init:(None, []) ~f:(fun (found, kept) source ->
            checkpoint ();
            if same_id (Captured.id source) id
            then (
              match found with
              | Some _ -> fail (Duplicate_capture id)
              | None -> Ok (Some source, if catalog_member then source :: kept else kept))
            else Ok (found, source :: kept))
        in
        let%map source =
          found |> Result.of_option ~error:(Error.create (Missing_capture id))
        in
        ( Project.Selection.Library { environment }
        , Entry.Captured source
        , List.rev catalog_sources )
    in
    compose_with_budget
      budget
      ~root_project
      ~selection
      ~entry
      ~sources
      ~routes
      ~previous:(Some previous)
      ~lock_id
      ~contract
      ~limits)
;;
