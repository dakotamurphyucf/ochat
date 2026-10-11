open Core
module Model = Project_build_model
module Budget = Project_build_budget
module Scope = Project_build_scope
module Receipt = Project_build_receipt
module Input = Model.Catalog_input
module Project = Chatml_project
module Catalog = Chatml_catalog
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

type t =
  { source : Captured.t
  ; scope : Prepared.Scope.t
  ; receipt : Project.Lock.Entry.t
  ; environment : string
  }

let source t = t.source
let scope t = t.scope
let receipt t = t.receipt
let environment t = t.environment

let create budget root ~catalogs ~entry ~contract =
  let checkpoint () = Budget.checkpoint budget in
  let declared_at = Input.declared_at root in
  let invalid () = Model.Error.create ~declared_at Invalid_entry_selection in
  let selection = Catalog.selection root.Input.catalog in
  let own_environment = Project.Selection.environment selection in
  let open Result.Let_syntax in
  let%bind source, environment, receipt_selection =
    match entry, Project.Selection.entry selection with
    | Model.Entry.Captured source, None ->
      Ok (source, own_environment, Project.Lock.Entry.Captured)
    | Declared, Some declared ->
      if
        Budget.compare_string
          budget
          (Project.Entry.target_name declared)
          (Contract.target_name contract)
        <> 0
      then Error (invalid ())
      else (
        let%bind id = Result.of_option (Catalog.entry root.catalog) ~error:(invalid ()) in
        let%bind source =
          List.find (Catalog.sources root.catalog) ~f:(fun source ->
            Budget.compare_id budget (Captured.id source) id = 0)
          |> Result.of_option ~error:(invalid ())
        in
        let%map environment =
          Catalog.scope_for ~checkpoint root.catalog ~source:id
          |> Result.of_option ~error:(invalid ())
        in
        source, environment, Project.Lock.Entry.Declared (Project.Entry.name declared))
    | Captured _, Some _ | Declared, None -> Error (invalid ())
  in
  let%bind bindings = Scope.bindings budget root ~catalogs environment in
  let%bind scope =
    Prepared.Scope.create
      (Captured.id source)
      ~bindings
      ~selected_at:
        (Option.map
           (Project.Scope_spec.declared_at environment)
           ~f:Project.Declared_at.location)
      ~checkpoint
    |> Result.map_error ~f:(fun _ -> invalid ())
  in
  let%map receipt =
    Project.Lock.Entry.create (Receipt.source_entry source) ~selection:receipt_selection
    |> Result.map_error ~f:(fun _ -> invalid ())
  in
  { source; scope; receipt; environment = Project.Scope_spec.name environment }
;;
