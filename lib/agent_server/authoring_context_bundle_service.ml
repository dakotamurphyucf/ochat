open! Core
module P = Agent_protocol
module W = P.Authoring.Wire
module B = P.Authoring_bundle.Bundle
module Service = Authoring_context_service_core
module Upload = Authoring_upload_host
module Store = Agent_store.Authoring_upload_store

let upload_error = Result.map_error ~f:(fun error -> Service.Error.Upload_failure error)
let protocol_error = Result.map_error ~f:(fun _ -> Service.Error.Invalid_request)

let owner t actor =
  let principal = Operator_authorization.principal actor in
  B.Owner.create ~host:t.Service.host ~principal:principal.id
;;

let terminal_status upload ~actor ~upload_id ~checkpoint =
  match
    Upload.status upload ~actor ~request:(B.Status.create_request ~upload_id) ~checkpoint
  with
  | Ok (Store.Status.Published receipt) -> Some (B.Commit.Published receipt)
  | Ok (Store.Status.Rejected reason) -> Some (B.Commit.Rejected reason)
  | Ok (Open _ | Ready | Validating | Expired) | Error _ -> None
;;

let handle_bundle ?script_selection t ~actor ~connection request =
  let open Result.Let_syntax in
  let%bind () = Service.check_domain t in
  let request_bytes = String.length (Jsonaf.to_string (W.Request.params request)) in
  if request_bytes > t.protocol_limits.max_request_bytes
  then Error Service.Error.Resource_limit
  else (
    let%bind upload = t.upload |> Result.of_option ~error:Service.Error.Uninstalled in
    let checkpoint () = Eio.Fiber.yield () in
    let run ~operation f =
      Service.with_request
        ~feature:Service.Selected_feature.Bundle_upload
        ~final_check:(fun () ->
          Upload.check_upload_current_now upload ~actor ~operation |> upload_error)
        t
        ~actor
        ~connection
        ~checkpoint
        (fun _ ~checkpoint -> f ~checkpoint)
    in
    let with_parent ~operation ~upload_operation expected f =
      Authoring_context_owner.with_parent_scope
        ?script_selection
        t
        ~actor
        ~connection
        ~expected
        ~operation
        ~checkpoint
        ~f:(fun scope ~checkpoint ->
          let%bind () =
            Upload.check_upload_current_now upload ~actor ~operation:upload_operation
            |> upload_error
          in
          f ~parent_admission:(Some (Authoring_parent_scope.admission scope)) ~checkpoint)
    in
    match request with
    | W.Request.Bundle_begin begin_request ->
      let perform ~parent_admission ~checkpoint =
        let%bind response =
          Upload.begin_upload
            upload
            ~actor
            ~connection
            ~parent_admission
            ~request:begin_request
            ~checkpoint
          |> upload_error
        in
        Authoring_context_owner.checked_response t (W.Response.Bundle_begin response)
      in
      (match begin_request.parent_context with
       | None ->
         run ~operation:Upload.Upload_policy.Operation.Write (fun ~checkpoint ->
           perform ~parent_admission:None ~checkpoint)
       | Some expected ->
         with_parent
           ~operation:Upload.Parent_admission.Operation.Begin
           ~upload_operation:Upload.Upload_policy.Operation.Write
           expected
           perform)
    | W.Request.Bundle_chunk chunk_request ->
      run ~operation:Upload.Upload_policy.Operation.Write (fun ~checkpoint ->
        let%bind response =
          Upload.chunk upload ~actor ~request:chunk_request ~checkpoint |> upload_error
        in
        Authoring_context_owner.checked_response t (W.Response.Bundle_chunk response))
    | W.Request.Bundle_commit commit_request ->
      let%bind claim =
        run ~operation:Upload.Upload_policy.Operation.Commit (fun ~checkpoint ->
          Upload.claim_commit upload ~actor ~request:commit_request ~checkpoint
          |> upload_error)
      in
      let parent =
        match claim with
        | Store.Claim_result.Published { parent_context; _ }
        | Rejected { parent_context; _ } -> parent_context
        | Ready sealed -> Store.sealed_parent_context sealed
      in
      let finish ~parent_admission ~checkpoint =
        let%bind outcome =
          match claim with
          | Store.Claim_result.Published { receipt; _ } -> Ok (B.Commit.Published receipt)
          | Rejected { reason; _ } -> Ok (B.Commit.Rejected reason)
          | Ready sealed ->
            (match
               Upload.publish
                 upload
                 ~actor
                 ~connection
                 ~parent_admission
                 ~sealed
                 ~checkpoint
             with
             | Ok receipt -> Ok (B.Commit.Published receipt)
             | Error ((Upload.Error.Invalid_closure | Resource_limit) as error) ->
               (match
                  terminal_status
                    upload
                    ~actor
                    ~upload_id:commit_request.upload_id
                    ~checkpoint
                with
                | Some terminal -> Ok terminal
                | None -> Error (Service.Error.Upload_failure error))
             | Error error -> Error (Service.Error.Upload_failure error))
        in
        let%bind response =
          B.Commit.create_response
            ~owner:(owner t actor)
            ~upload_id:commit_request.upload_id
            ~idempotency_key:commit_request.idempotency_key
            ~outcome
          |> protocol_error
        in
        Authoring_context_owner.checked_response t (W.Response.Bundle_commit response)
      in
      (match parent with
       | None ->
         run ~operation:Upload.Upload_policy.Operation.Commit (fun ~checkpoint ->
           finish ~parent_admission:None ~checkpoint)
       | Some expected ->
         with_parent
           ~operation:Upload.Parent_admission.Operation.Publish
           ~upload_operation:Upload.Upload_policy.Operation.Commit
           expected
           finish)
    | W.Request.Bundle_status status_request ->
      run ~operation:Upload.Upload_policy.Operation.Write (fun ~checkpoint ->
        let%bind status =
          Upload.status upload ~actor ~request:status_request ~checkpoint |> upload_error
        in
        let state =
          match status with
          | Store.Status.Open progress -> B.Status.Staging progress
          | Ready | Validating -> B.Status.Validating
          | Published receipt -> B.Status.Published receipt
          | Rejected reason -> B.Status.Rejected reason
          | Expired -> B.Status.Expired
        in
        let%bind response =
          B.Status.create
            ~owner:(owner t actor)
            ~upload_id:status_request.upload_id
            ~state
          |> protocol_error
        in
        Authoring_context_owner.checked_response t (W.Response.Bundle_status response))
    | W.Request.Capabilities
    | Roots
    | Host_list _
    | Host_read _
    | Context_open _
    | Context_refresh _
    | Context_close _
    | Source_list _
    | Source_read _
    | Document_version _
    | Document_replace _
    | Document_close _
    | Analyze _
    | Prefix _
    | Query _
    | Lexical _
    | Assistance _ -> Error Service.Error.Uninstalled)
;;
