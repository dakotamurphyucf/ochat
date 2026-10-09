open! Core
module P = Agent_protocol
module S = Agent_store.Organization_state
module V = P.Session_organization.Values

type t = S.t

let create snapshot = snapshot
let host_id t = S.server_id t
let revision t = S.revision t

let authorize_query t ~principal query =
  let open Result.Let_syntax in
  if not (P.Session_organization.Query.requires_organization_view query)
  then Ok ()
  else if not (P.Principal.has_scope principal View_organization)
  then
    Error
      (P.Error.create
         Permission_denied
         ~message:"organization filters require organization.view"
         ~retryable:false
         ())
  else (
    let%bind () =
      match query.P.Session_organization.Query.project with
      | Any | Unassigned -> Ok ()
      | Project id -> Result.map (S.visible_project t ~principal id) ~f:(fun _ -> ())
    in
    List.fold_result query.collection_all_of ~init:() ~f:(fun () id ->
      Result.map (S.visible_collection t ~principal id) ~f:(fun _ -> ())))
;;

let effective t ~principal raw =
  let open Result.Let_syntax in
  let missing () =
    Error
      (P.Error.create
         Journal_corrupt
         ~message:"session membership references absent host organization identity"
         ~retryable:false
         ())
  in
  let%bind project =
    match raw.V.project_id with
    | None -> Ok None
    | Some id ->
      S.project_entry t id
      |> Result.of_option
           ~error:
             (P.Error.create
                Journal_corrupt
                ~message:"session membership references absent host organization identity"
                ~retryable:false
                ())
      |> Result.map ~f:Option.some
  in
  let%bind collections =
    List.map raw.collection_ids ~f:(fun id ->
      match S.collection_entry t id with
      | Some entry -> Ok entry
      | None -> missing ())
    |> Result.all
  in
  if not (P.Principal.has_scope principal View_organization)
  then Ok V.empty
  else (
    let visible creator =
      P.Id.Principal.equal principal.id creator
      || P.Principal.has_scope principal Administer_configuration
    in
    let project_id =
      Option.bind project ~f:(fun entry ->
        if
          Option.is_none entry.S.Project_entry.deleted_at
          && visible entry.group.creator_principal_id
        then Some entry.group.id
        else None)
    in
    let collection_ids =
      List.filter_map collections ~f:(fun entry ->
        if
          Option.is_none entry.S.Collection_entry.deleted_at
          && visible entry.group.creator_principal_id
        then Some entry.group.id
        else None)
    in
    V.create ~project_id ~collection_ids)
;;
