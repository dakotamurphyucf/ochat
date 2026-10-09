open Core
module P = Agent_protocol
module S = Agent_store.Organization_state
module Store = Agent_store.Organization_store

let handles = function
  | P.Command.Project_create _ -> true
  | P.Command.Project_get _ -> true
  | P.Command.Project_list _ -> true
  | P.Command.Project_update _ -> true
  | P.Command.Project_delete _ -> true
  | P.Command.Collection_create _ -> true
  | P.Command.Collection_get _ -> true
  | P.Command.Collection_list _ -> true
  | P.Command.Collection_update _ -> true
  | P.Command.Collection_delete _ -> true
  | _ -> false
;;

let handle store ~server_id ~principal ~now command =
  let open Result.Let_syntax in
  let check_host host =
    if P.Id.Server.equal host server_id
    then Ok ()
    else
      Error (P.Error.invalid_request "organization host differs from initialized server")
  in
  let snapshot () =
    Store.snapshot_checked store
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let mutate method_name idempotency_key candidate mutation =
    let%bind request_digest = S.request_digest mutation in
    let audit =
      Agent_store.Idempotency_store.Command_audit.
        { key =
            { principal_id = principal.P.Principal.id
            ; session_id = None
            ; method_name
            ; idempotency_key
            }
        ; request_digest
        ; protected_record = false
        }
    in
    Store.mutate store ~principal ~audit ~now ~candidate mutation
  in
  match command with
  | P.Command.Project_get request ->
    let%bind () = check_host request.host_id in
    let%bind state = snapshot () in
    let%map value = S.visible_project state ~principal request.id in
    P.Method_result.Project_get value
  | P.Command.Project_list request ->
    let%bind () = check_host request.host_id in
    let%bind () =
      if P.Principal.has_scope principal View_organization
      then Ok ()
      else
        Error
          (P.Error.create
             Permission_denied
             ~message:"missing organization view scope"
             ~retryable:false
             ())
    in
    let%map state = snapshot () in
    let items =
      S.projects state
      |> List.filter_map ~f:(fun entry ->
        if Option.is_some entry.S.Project_entry.deleted_at
        then None
        else (
          match S.visible_project state ~principal entry.group.id with
          | Error _ -> None
          | Ok group ->
            if
              Option.value_map
                request.creator_principal_id
                ~default:true
                ~f:(P.Id.Principal.equal group.creator_principal_id)
            then Some group
            else None))
      |> List.sort ~compare:(fun (left : P.Organization_group.Project.t) right ->
        let order = P.Timestamp.compare left.created_at right.created_at in
        if order = 0 then P.Id.Project.compare left.id right.id else order)
    in
    P.Method_result.Project_list P.Page.{ items; next_cursor = None }
  | P.Command.Project_create request ->
    let%bind () = check_host request.host_id in
    let%bind result =
      mutate
        "project.create"
        request.idempotency_key
        (Some (S.Candidate.Project (P.Id.Project.create ())))
        (S.Mutation.Create_project request)
    in
    (match result with
     | P.Organization_result.Project_created value ->
       Ok (P.Method_result.Project_create value)
     | _ ->
       Error
         (P.Error.create
            Internal_error
            ~message:"organization receipt result mismatch"
            ~retryable:false
            ()))
  | P.Command.Project_update request ->
    let%bind () = check_host request.host_id in
    let%bind result =
      mutate
        "project.update"
        request.idempotency_key
        None
        (S.Mutation.Update_project request)
    in
    (match result with
     | P.Organization_result.Project_updated value ->
       Ok (P.Method_result.Project_update value)
     | _ ->
       Error
         (P.Error.create
            Internal_error
            ~message:"organization receipt result mismatch"
            ~retryable:false
            ()))
  | P.Command.Project_delete request ->
    let%bind () = check_host request.host_id in
    let%bind result =
      mutate
        "project.delete"
        request.idempotency_key
        None
        (S.Mutation.Delete_project request)
    in
    (match result with
     | P.Organization_result.Project_deleted value ->
       Ok (P.Method_result.Project_delete value)
     | _ ->
       Error
         (P.Error.create
            Internal_error
            ~message:"organization receipt result mismatch"
            ~retryable:false
            ()))
  | P.Command.Collection_get request ->
    let%bind () = check_host request.host_id in
    let%bind state = snapshot () in
    let%map value = S.visible_collection state ~principal request.id in
    P.Method_result.Collection_get value
  | P.Command.Collection_list request ->
    let%bind () = check_host request.host_id in
    let%bind () =
      if P.Principal.has_scope principal View_organization
      then Ok ()
      else
        Error
          (P.Error.create
             Permission_denied
             ~message:"missing organization view scope"
             ~retryable:false
             ())
    in
    let%map state = snapshot () in
    let items =
      S.collections state
      |> List.filter_map ~f:(fun entry ->
        if Option.is_some entry.S.Collection_entry.deleted_at
        then None
        else (
          match S.visible_collection state ~principal entry.group.id with
          | Error _ -> None
          | Ok group ->
            if
              Option.value_map
                request.creator_principal_id
                ~default:true
                ~f:(P.Id.Principal.equal group.creator_principal_id)
            then Some group
            else None))
      |> List.sort ~compare:(fun (left : P.Organization_group.Collection.t) right ->
        let order = P.Timestamp.compare left.created_at right.created_at in
        if order = 0 then P.Id.Collection.compare left.id right.id else order)
    in
    P.Method_result.Collection_list P.Page.{ items; next_cursor = None }
  | P.Command.Collection_create request ->
    let%bind () = check_host request.host_id in
    let%bind result =
      mutate
        "collection.create"
        request.idempotency_key
        (Some (S.Candidate.Collection (P.Id.Collection.create ())))
        (S.Mutation.Create_collection request)
    in
    (match result with
     | P.Organization_result.Collection_created value ->
       Ok (P.Method_result.Collection_create value)
     | _ ->
       Error
         (P.Error.create
            Internal_error
            ~message:"organization receipt result mismatch"
            ~retryable:false
            ()))
  | P.Command.Collection_update request ->
    let%bind () = check_host request.host_id in
    let%bind result =
      mutate
        "collection.update"
        request.idempotency_key
        None
        (S.Mutation.Update_collection request)
    in
    (match result with
     | P.Organization_result.Collection_updated value ->
       Ok (P.Method_result.Collection_update value)
     | _ ->
       Error
         (P.Error.create
            Internal_error
            ~message:"organization receipt result mismatch"
            ~retryable:false
            ()))
  | P.Command.Collection_delete request ->
    let%bind () = check_host request.host_id in
    let%bind result =
      mutate
        "collection.delete"
        request.idempotency_key
        None
        (S.Mutation.Delete_collection request)
    in
    (match result with
     | P.Organization_result.Collection_deleted value ->
       Ok (P.Method_result.Collection_delete value)
     | _ ->
       Error
         (P.Error.create
            Internal_error
            ~message:"organization receipt result mismatch"
            ~retryable:false
            ()))
  | _ -> Error (P.Error.invalid_request "command is not an organization operation")
;;

let receipt store ~server_id ~principal ~now command =
  let open Result.Let_syntax in
  let%bind mutation, method_name, key, host =
    match command with
    | P.Command.Project_create request ->
      Ok
        ( S.Mutation.Create_project request
        , "project.create"
        , request.idempotency_key
        , request.host_id )
    | P.Command.Project_update request ->
      Ok
        ( S.Mutation.Update_project request
        , "project.update"
        , request.idempotency_key
        , request.host_id )
    | P.Command.Project_delete request ->
      Ok
        ( S.Mutation.Delete_project request
        , "project.delete"
        , request.idempotency_key
        , request.host_id )
    | P.Command.Collection_create request ->
      Ok
        ( S.Mutation.Create_collection request
        , "collection.create"
        , request.idempotency_key
        , request.host_id )
    | P.Command.Collection_update request ->
      Ok
        ( S.Mutation.Update_collection request
        , "collection.update"
        , request.idempotency_key
        , request.host_id )
    | P.Command.Collection_delete request ->
      Ok
        ( S.Mutation.Delete_collection request
        , "collection.delete"
        , request.idempotency_key
        , request.host_id )
    | _ -> Error (P.Error.invalid_request "organization reads have no command receipt")
  in
  if not (P.Id.Server.equal host server_id)
  then Error (P.Error.invalid_request "organization receipt host mismatch")
  else (
    let%bind request_digest = S.request_digest mutation in
    let key =
      Agent_store.Idempotency_store.Key.
        { principal_id = principal.P.Principal.id
        ; session_id = None
        ; method_name
        ; idempotency_key = key
        }
    in
    let%map result = Store.receipt store ~principal ~now ~key ~request_digest in
    match result with
    | None -> P.Command_receipt.Missing
    | Some result ->
      let narrow =
        match result with
        | P.Organization_result.Project_created value ->
          P.Command_receipt.Project_mutation
            { project_id = value.id; revision = value.revision }
        | P.Organization_result.Project_updated value ->
          P.Command_receipt.Project_mutation
            { project_id = value.id; revision = value.revision }
        | P.Organization_result.Project_deleted value ->
          P.Command_receipt.Deleted_project
            { project_id = value.id; revision = value.revision }
        | P.Organization_result.Collection_created value ->
          P.Command_receipt.Collection_mutation
            { collection_id = value.id; revision = value.revision }
        | P.Organization_result.Collection_updated value ->
          P.Command_receipt.Collection_mutation
            { collection_id = value.id; revision = value.revision }
        | P.Organization_result.Collection_deleted value ->
          P.Command_receipt.Deleted_collection
            { collection_id = value.id; revision = value.revision }
      in
      P.Command_receipt.Committed narrow)
;;
