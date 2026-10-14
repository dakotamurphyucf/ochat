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

let handle_bundle t ~actor ~connection request =
  let open Result.Let_syntax in
  let%bind () = Service.check_domain t in
  let request_bytes = String.length (Jsonaf.to_string (W.Request.params request)) in
  if request_bytes > t.protocol_limits.max_request_bytes
  then Error Service.Error.Resource_limit
  else (
    let%bind upload = t.upload |> Result.of_option ~error:Service.Error.Uninstalled in
    let checkpoint () = Eio.Fiber.yield () in
    let run f =
      Service.with_request
        ~feature:Service.Selected_feature.Bundle_upload
        ~final_check:(fun () ->
          Upload.check_upload_current_now upload ~actor |> upload_error)
        t
        ~actor
        ~connection
        ~checkpoint
        (fun _ ~checkpoint -> f ~checkpoint)
    in
    match request with
    | W.Request.Bundle_begin begin_request ->
      run (fun ~checkpoint ->
        let%bind response =
          Upload.begin_upload upload ~actor ~request:begin_request ~checkpoint
          |> upload_error
        in
        Authoring_context_owner.checked_response t (W.Response.Bundle_begin response))
    | W.Request.Bundle_chunk chunk_request ->
      run (fun ~checkpoint ->
        let%bind response =
          Upload.chunk upload ~actor ~request:chunk_request ~checkpoint |> upload_error
        in
        Authoring_context_owner.checked_response t (W.Response.Bundle_chunk response))
    | W.Request.Bundle_commit commit_request ->
      run (fun ~checkpoint ->
        let%bind claim =
          Upload.claim_commit upload ~actor ~request:commit_request ~checkpoint
          |> upload_error
        in
        let%bind outcome =
          match claim with
          | Store.Claim_result.Published receipt -> Ok (B.Commit.Published receipt)
          | Rejected reason -> Ok (B.Commit.Rejected reason)
          | Ready sealed ->
            (match Upload.publish upload ~actor ~sealed ~checkpoint with
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
        Authoring_context_owner.checked_response t (W.Response.Bundle_commit response))
    | W.Request.Bundle_status status_request ->
      run (fun ~checkpoint ->
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
