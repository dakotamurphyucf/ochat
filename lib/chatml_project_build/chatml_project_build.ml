open Core
module Model = Project_build_model
module Budget = Project_build_budget
module Gate = Project_build_selection_gate
module Assembly = Project_build_assembly
module Limits = Model.Limits
module Error = Model.Error
module Entry = Model.Entry
module Operation = Model.Operation
module Accounting = Model.Accounting

module Catalog_input = struct
  type t = Model.Catalog_input.t

  let create catalog ~origin ~acquisition_key ~dependencies =
    { Model.Catalog_input.catalog
    ; origin
    ; acquisition_key = Some acquisition_key
    ; dependencies
    }
  ;;

  let declaration = Model.Catalog_input.declaration
  let declared_at = Model.Catalog_input.declared_at
end

module Selection_gate = struct
  type t = Gate.t

  module Owner = Gate.Owner

  let create operation ~limits ~checkpoint =
    Budget.run limits ~checkpoint ~f:(fun budget ->
      let checkpoint () = Budget.checkpoint budget in
      (match operation with
       | Operation.Initial_prepare | Build_without_previous
       | Update { previous_lock = None } -> ()
       | Build { previous_lock } | Update { previous_lock = Some previous_lock } ->
         Budget.charge_bytes budget (Chatml_source.byte_length previous_lock));
      Gate.create operation ~document:(Limits.document limits) ~checkpoint)
  ;;

  let previous_lock = Gate.previous_lock
  let preserves_git_selections = Gate.preserves_git_selections

  let check_root t ~origin ~project ~limits ~checkpoint =
    Budget.run limits ~checkpoint ~f:(fun budget ->
      Gate.check_root t ~origin ~project ~budget)
  ;;

  let check_dependency
        t
        ~previous_owner
        ~owner
        ~owner_origin
        dependency
        ~limits
        ~checkpoint
    =
    Budget.run limits ~checkpoint ~f:(fun budget ->
      Gate.check_dependency t ~previous_owner ~owner ~owner_origin dependency ~budget)
  ;;
end

let selected_dependencies catalog ~limits ~checkpoint =
  Budget.run limits ~checkpoint ~f:(fun budget ->
    Ok
      (Project_build_scope.dependencies catalog ~checkpoint:(fun () ->
         Budget.checkpoint budget)))
;;

module Dependency = struct
  module Resolved = Project_build_dependency

  type t = Resolved.t

  let origin = Resolved.origin
  let project = Resolved.project

  let resolve dependency ~owner_origin ~owner_declaration ~limits ~checkpoint =
    Budget.run limits ~checkpoint ~f:(fun budget ->
      Resolved.resolve dependency ~owner_origin ~owner_declaration ~checkpoint:(fun () ->
        Budget.checkpoint budget)
      |> Result.map_error ~f:(fun _ ->
        Model.Error.create
          ~declared_at:(Chatml_project.Dependency.declared_at dependency)
          (Invalid_dependency (Chatml_project.Dependency.name dependency))))
  ;;

  let repository_member ~package_root ~member ~limits ~checkpoint =
    Budget.run limits ~checkpoint ~f:(fun budget ->
      Resolved.repository_member ~package_root ~member ~checkpoint:(fun () ->
        Budget.checkpoint budget)
      |> Result.map_error ~f:(fun _ ->
        Model.Error.create (Invalid_dependency "repository member")))
  ;;
end

type t =
  { assembly : Assembly.t
  ; accounting : Accounting.t
  }

let prepared t = Assembly.prepared t.assembly
let lock t = Assembly.lock t.assembly
let lock_source t = Assembly.lock_source t.assembly
let accounting t = t.accounting

let compose root ~catalogs ~entry ~selection_gate ~contract ~lock_id ~limits ~checkpoint =
  Budget.run limits ~checkpoint ~f:(fun budget ->
    let open Result.Let_syntax in
    let%bind catalogs = Project_build_closure.select budget root ~catalogs in
    let%bind () = Gate.validate_captures selection_gate ~budget ~root ~catalogs in
    Assembly.create
      budget
      root
      ~catalogs
      ~entry
      ~contract
      ~lock:(Generate lock_id)
      ~limits)
  |> Result.map ~f:(fun (assembly, accounting) -> { assembly; accounting })
;;

let restore lock_source ~sources ~contract ~limits ~checkpoint =
  Budget.run limits ~checkpoint ~f:(fun budget ->
    Project_build_restore.restore budget ~limits ~contract ~lock_source ~sources)
  |> Result.map ~f:(fun (assembly, accounting) -> { assembly; accounting })
;;

module Effective = struct
  module Route = Project_build_effective.Route

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
    Project_build_effective.compose
      ~root_project
      ~selection
      ~entry
      ~sources
      ~routes
      ~previous:(Option.map previous ~f:(fun build -> build.assembly))
      ~lock_id
      ~contract
      ~limits
      ~checkpoint
    |> Result.map ~f:(fun (assembly, accounting) -> { assembly; accounting })
  ;;

  let recompose previous ~sources ~lock_id ~contract ~limits ~checkpoint =
    Project_build_effective.recompose
      previous.assembly
      ~sources
      ~lock_id
      ~contract
      ~limits
      ~checkpoint
    |> Result.map ~f:(fun (assembly, accounting) -> { assembly; accounting })
  ;;
end

let recompose t ~contract ~lock_id ~limits ~checkpoint =
  Budget.run limits ~checkpoint ~f:(fun budget ->
    let module Lock = Chatml_project.Lock in
    let module Captured = Chatml_source in
    let old_lock = lock_source t in
    let fail problem = Error (Error.create problem) in
    let open Result.Let_syntax in
    Budget.checkpoint budget;
    let%bind () =
      match Lock.Entry.selection (Lock.entry_selection (lock t)) with
      | Declared _ -> fail Invalid_entry_selection
      | Captured -> Ok ()
    in
    let%bind sources =
      List.fold_result
        (Chatml_prepared_program.sources (prepared t))
        ~init:[]
        ~f:(fun sources source ->
          let id = Captured.id source in
          if Budget.compare_id budget id lock_id = 0
          then fail Invalid_lock_identity
          else if Budget.compare_id budget id (Captured.id old_lock) = 0
          then Ok sources
          else Ok (source :: sources))
    in
    let%bind { Project_build_restore.Reconstruction.receipt = _; root; catalogs; entry } =
      Project_build_restore.reconstruct budget ~limits ~lock_source:old_lock ~sources
    in
    let%bind catalogs = Project_build_closure.select budget root ~catalogs in
    Assembly.create
      budget
      root
      ~catalogs
      ~entry
      ~contract
      ~lock:(Generate lock_id)
      ~limits)
  |> Result.map ~f:(fun (assembly, accounting) -> { assembly; accounting })
;;
