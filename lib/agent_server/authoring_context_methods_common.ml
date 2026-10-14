open! Core
open Authoring_context_service_core
open Authoring_context_activity
open Authoring_context_owner

let protocol_error error = Result.map_error error ~f:(fun _ -> Error.Invalid_request)
let source_error error = Result.map_error error ~f:(fun _ -> Error.Source_unavailable)

let ledger_error = function
  | Ledger.Error.Limit_exceeded | Generation_overflow -> Error.Resource_limit
  | Stale_generation | Stale_document | Revision_not_increasing | Document_not_open ->
    Error.Stale_source
  | Unknown_context | Duplicate_context | Invalid_refresh_identity -> Error.Stale_context
  | Unknown_source _ | Read_only_source _ | Duplicate_editable_source _ ->
    Error.Source_unavailable
  | Open_documents -> Error.Busy
  | Incompatible_environment
  | Static_selection_unavailable
  | Invalid_requested_environment -> Error.Selection_unavailable
  | Unsupported_basis | Capture _ -> Error.Invalid_request
;;

let check_snapshot t entry ledger =
  let open Result.Let_syntax in
  let%bind () = check_entry t entry in
  if phys_equal (Project.Project_cache.contexts t.project_cache) ledger
  then Ok ()
  else Error Error.Busy
;;

let root_for entry id =
  List.Assoc.find entry.roots id ~equal:P.Id.Authoring_root.equal
  |> Result.of_option ~error:Error.Source_unavailable
;;

let portable_member value =
  Source.Portable_path.create value
  |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
;;

let identity_for t entry id =
  let open Result.Let_syntax in
  let principal =
    Operator_authorization.principal (Connection_context.actor entry.connection)
  in
  let%bind negotiation =
    entry.negotiation |> Result.of_option ~error:Error.Stale_negotiation
  in
  Ledger.Identity.create
    ~principal:(P.Id.Principal.to_string principal.id)
    ~host_incarnation:(P.Id.Server.to_string t.host)
    ~connection_lease:(Connection_context.Authoring_negotiation.lease_label negotiation)
    ~nonce:(P.Id.Authoring_context.to_string id)
  |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
;;

let project_selection (selection : P.Authoring.Context.Selection.t) =
  match selection.target with
  | Entry name ->
    Chatml_project.Selection.Entry { name; environment = selection.environment }
  | Library -> Chatml_project.Selection.Library { environment = selection.environment }
;;

let editable_local_sources build =
  let lock = Build.lock build in
  let lock_id = Captured.id (Build.lock_source build) in
  Chatml_project.Lock.catalogs lock
  |> List.concat_map ~f:(fun catalog ->
    match Chatml_project.Lock.Catalog.origin catalog with
    | Chatml_project.Lock.Catalog.Local ->
      Chatml_project.Lock.Catalog.sources catalog
      |> List.map ~f:Chatml_project.Lock.Source_entry.id
    | Chatml_project.Lock.Catalog.Git _ -> [])
  |> List.filter ~f:(fun id -> not (Captured.Id.equal id lock_id))
  |> List.dedup_and_sort ~compare:Captured.Id.compare
;;

let selected_sources ledger identity ~checkpoint =
  match Ledger.basis ledger ~identity with
  | None -> Error Error.Stale_context
  | Some Ledger.Basis.Project ->
    Ledger.inputs ledger ~identity ~checkpoint
    |> Result.map ~f:Ledger.Inputs.sources
    |> Result.map_error ~f:ledger_error
  | Some Ledger.Basis.Document ->
    Ledger.document_inputs ledger ~identity ~checkpoint
    |> Result.map ~f:(fun input -> [ Ledger.Document.Inputs.source input ])
    |> Result.map_error ~f:ledger_error
;;

let captured_source sources id =
  List.find sources ~f:(fun source -> Captured.Id.equal (Captured.id source) id)
  |> Result.of_option ~error:Error.Source_unavailable
;;
