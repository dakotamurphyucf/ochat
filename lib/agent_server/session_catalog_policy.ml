open! Core
module P = Agent_protocol

let active_owner ~now attachments =
  List.find_map attachments ~f:(fun (attachment : P.Session.Attachment.t) ->
    match attachment.mode, attachment.owner_lease with
    | (Read_only | Read_write), _ | Owner_read_write, None -> None
    | Owner_read_write, Some lease ->
      let deadline =
        Option.value lease.disconnect_grace_until ~default:lease.expires_at
      in
      if P.Timestamp.compare now deadline < 0 then lease.principal_id else None)
;;

let matches (r : P.Session.List_request.t) (entry : P.Session_catalog.t) =
  let s = entry.session in
  let creator = Option.first_some r.creator_principal_id r.owner_principal_id in
  Option.value_map
    r.desired_state
    ~default:true
    ~f:(P.Session.equal_desired_state s.desired_state)
  && Option.value_map r.prompt_id ~default:true ~f:(fun id ->
    match s.spec.prompt with
    | Catalog actual -> P.Id.Prompt_definition.equal id actual
    | Local_path _ | Generated _ -> false)
  && Option.value_map r.workspace_id ~default:true ~f:(fun id ->
    match s.spec.workspace with
    | Configured actual -> P.Id.Workspace_definition.equal id actual
    | Current | Local_path _ -> false)
  && Option.value_map creator ~default:true ~f:(fun id ->
    Option.exists s.creator ~f:(P.Id.Principal.equal id))
  && Option.value_map r.active_owner_principal_id ~default:true ~f:(fun id ->
    Option.exists entry.active_owner_principal_id ~f:(P.Id.Principal.equal id))
  && (match r.archive with
      | Active -> not entry.archived
      | Archived -> entry.archived
      | All -> true)
  && List.for_all r.labels ~f:(fun (key, value) ->
    Option.exists
      (List.Assoc.find s.spec.labels key ~equal:String.equal)
      ~f:(String.equal value))
;;

let compare
      (sort : P.Session_catalog_query.Sort.t)
      (a : P.Session_catalog.t)
      (b : P.Session_catalog.t)
  =
  let primary =
    match sort.field with
    | Created_at -> P.Timestamp.compare a.session.created_at b.session.created_at
    | Updated_at -> P.Timestamp.compare a.session.updated_at b.session.updated_at
    | Display_name ->
      Option.compare
        String.compare
        a.session.spec.display_name
        b.session.spec.display_name
  in
  let primary =
    match sort.direction with
    | Ascending -> primary
    | Descending -> Int.neg primary
  in
  if primary = 0 then P.Id.Session.compare a.session.id b.session.id else primary
;;
