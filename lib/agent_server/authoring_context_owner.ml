open! Core
open Authoring_context_service_core
open Authoring_context_activity

let source_host_error = function
  | Sources.Error.Unauthorized -> Error.Unauthorized
  | Expired_actor -> Error.Expired_actor
  | Foreign_root | Unsupported_source_selection -> Error.Source_unavailable
  | Closed | Foreign_domain -> Error.Closed
  | Busy -> Error.Busy
  | Context_limit | Result_limit | Document_bundle_limit
  | Document_parser Prompt.Chat_markdown.Generated.Parser_limit -> Error.Resource_limit
  | Sources _ | Root _ | Local _ | Project _ | Source_capture _ | Document_parser _ ->
    Error.Source_unavailable
;;

let upload_host_error = function
  | Upload.Error.Unauthorized -> Error.Unauthorized
  | Expired_actor -> Error.Expired_actor
  | Foreign_domain | Closed -> Error.Closed
  | Busy | Store Busy -> Error.Busy
  | Selection_unavailable -> Error.Selection_unavailable
  | Stale_capture -> Error.Stale_context
  | Invalid_closure -> Error.Source_unavailable
  | Resource_limit | Store Limit_exceeded -> Error.Resource_limit
  | Store Expired | Store Missing_or_foreign | Store Sealed -> Error.Stale_context
  | Store Storage_failure -> Error.Storage_unavailable
  | Store Invalid_manifest
  | Store Idempotency_conflict
  | Store Invalid_offset
  | Store Incomplete -> Error.Source_unavailable
;;

exception Contract_limit

let selected_contract_fingerprint contract ~checkpoint =
  let budget = 64 * 1024 * 1024 in
  let allocated = ref 0 in
  try
    let encoded =
      Chatml.Chatml_compiler_contract.input_bytes
        contract
        ~checkpoint
        ~allocate:(fun bytes ->
          if bytes < 0 || bytes > budget - !allocated then raise Contract_limit;
          allocated := !allocated + bytes)
    in
    if encoded < 0 || encoded > 8 * 1024 * 1024
    then Error Error.Resource_limit
    else Ok (Chatml.Chatml_compiler_contract.fingerprint contract ~checkpoint)
  with
  | Contract_limit -> Error Error.Resource_limit
;;

let check_document_static ?ledger t context ~actor ~checkpoint =
  let open Result.Let_syntax in
  let ledger =
    Option.value ledger ~default:(Project.Project_cache.contexts t.project_cache)
  in
  let%bind view =
    Ledger.find_document ledger ~identity:context.identity
    |> Result.of_option ~error:Error.Stale_context
  in
  match
    ( Ledger.Document.View.selected_environment view
    , Ledger.Document.View.compiler_contract view
    , Ledger.Document.View.static_graph view )
  with
  | None, None, None -> Ok ()
  | Some environment, Some contract, Some graph ->
    let requested_environment =
      match context.origin with
      | Host_document { default_environment; _ }
      | Supplied_document { default_environment; _ } -> default_environment
      | Host_project _ | Uploaded_project _ -> None
    in
    (match
       ( t.selection_policy.document_static context.original_actor ~requested_environment
       , t.selection_policy.document_static actor ~requested_environment )
     with
     | Some original, Some current ->
       let matches build =
         let prepared = Build.prepared build in
         Chatml_source.Digest.equal graph (Chatml_prepared_program.fingerprint prepared)
         && Chatml_source.Digest.equal
              contract
              (Chatml_prepared_program.compiler_contract prepared)
         && String.equal
              environment
              (Chatml_prepared_program.Environment.name
                 (Chatml_prepared_program.environment prepared))
       in
       checkpoint ();
       if matches original && matches current then Ok () else Error Error.Stale_context
     | Some _, None | None, Some _ | None, None -> Error Error.Selection_unavailable)
  | None, Some _, _ | Some _, None, _ | _, _, None | None, None, Some _ ->
    Error Error.Stale_context
;;

let check_context_owner ?ledger t context ~actor ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = check_actor_context context.original_actor actor in
  match context.owner with
  | Host_project owner ->
    let%bind () =
      Sources.Context.check_current owner ~actor |> Result.map_error ~f:source_host_error
    in
    (match context.origin with
     | Host_project { project_member; selection; _ } ->
       let%bind project =
         Source.Portable_path.create project_member
         |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
       in
       let root = Sources.Context.root owner in
       (match
          ( t.selection_policy.project_contract
              context.original_actor
              ~root
              ~project
              ~selection
          , t.selection_policy.project_contract actor ~root ~project ~selection )
        with
        | Some original, Some current ->
          let%bind original = selected_contract_fingerprint original ~checkpoint in
          let%bind current = selected_contract_fingerprint current ~checkpoint in
          let ledger =
            Option.value ledger ~default:(Project.Project_cache.contexts t.project_cache)
          in
          (match Ledger.find ledger ~identity:context.identity with
           | Some view
             when Captured.Digest.equal original (Ledger.View.compiler_contract view)
                  && Captured.Digest.equal current original -> Ok ()
           | Some _ | None -> Error Error.Stale_context)
        | Some _, None | None, Some _ | None, None -> Error Error.Selection_unavailable)
     | Uploaded_project _ | Host_document _ | Supplied_document _ ->
       Error Error.Stale_context)
  | Host_document owner ->
    let%bind () =
      Sources.Document_context.check_current owner ~actor
      |> Result.map_error ~f:source_host_error
    in
    check_document_static ?ledger t context ~actor ~checkpoint
  | Uploaded_project owner ->
    (match t.upload with
     | None -> Error Error.Uninstalled
     | Some upload ->
       Upload.check_context_current upload ~actor owner ~checkpoint
       |> Result.map_error ~f:upload_host_error)
  | Supplied_document original ->
    (match t.supplied_document with
     | None -> Error Error.Uninstalled
     | Some policy ->
       if policy.Supplied_document_policy.authorize original && policy.authorize actor
       then check_document_static ?ledger t context ~actor ~checkpoint
       else Error Error.Unauthorized)
;;

let check_context_selection_now ?ledger t context ~actor =
  match context.owner with
  | Uploaded_project capture ->
    (match t.upload with
     | None -> Error Error.Uninstalled
     | Some upload ->
       Upload.check_context_selection_now upload ~actor capture
       |> Result.map_error ~f:upload_host_error)
  | Host_project _ | Host_document _ | Supplied_document _ ->
    check_context_owner ?ledger t context ~actor ~checkpoint:Fn.id
;;

let with_current
      t
      ~actor
      ~connection
      ~(expected : P.Authoring.Context.Expected.t)
      ~checkpoint
      ~f
  =
  let expected_view = expected in
  let final_context = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match !final_context with
      | None -> Error Error.Stale_context
      | Some context -> check_context_selection_now t context ~actor)
    ~commit:(fun value ->
      match !final_context with
      | None -> Error Error.Stale_context
      | Some context ->
        (match find_connection t connection with
         | Some entry ->
           (match find_context entry context.id with
            | Some actual
              when phys_equal actual context
                   && P.Authoring.Context.Expected.equal
                        expected
                        (P.Authoring.Context.Expected.of_view actual.view) -> Ok value
            | Some _ | None -> Error Error.Stale_context)
         | None -> Error Error.Stale_negotiation))
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let%bind context, ledger =
         with_gate t (fun () ->
           match find_context entry expected.context_id with
           | None -> Error Error.Stale_context
           | Some context ->
             if
               P.Authoring.Context.Expected.equal
                 expected
                 (P.Authoring.Context.Expected.of_view context.view)
             then Ok (context, Project.Project_cache.contexts t.project_cache)
             else Error Error.Stale_context)
       in
       let%bind () = check_context_owner t context ~actor ~checkpoint in
       final_context := Some context;
       let current =
         Current.
           { owner = ledger
           ; domain = t.domain
           ; context
           ; expected = expected_view
           ; live = ref true
           }
       in
       Exn.protect
         ~finally:(fun () -> current.live := false)
         ~f:(fun () ->
           let%bind value = f current ~checkpoint in
           let%bind () = check_context_owner t context ~actor ~checkpoint in
           let%map () =
             with_gate t (fun () ->
               match find_context entry context.id with
               | Some actual
                 when phys_equal actual context
                      && P.Authoring.Context.Expected.equal
                           expected
                           (P.Authoring.Context.Expected.of_view actual.view) -> Ok ()
               | Some _ | None -> Error Error.Stale_context)
           in
           value))
;;

let view_of_context t ledger ~id ~identity ~origin ~checkpoint =
  Authoring_context_projection.view_of_context
    ~host:t.host
    ~max_sources:t.protocol_limits.max_sources
    ~max_source_bytes:t.protocol_limits.max_source_bytes
    ~max_context_source_bytes:t.protocol_limits.max_context_source_bytes
    ledger
    ~id
    ~identity
    ~origin
    ~checkpoint
  |> Result.map_error ~f:(fun () -> Error.Resource_limit)
;;

let checked_response t response =
  if
    Authoring_context_projection.response_fits
      ~max_response_bytes:t.limits.max_response_bytes
      ~max_protocol_bytes:t.protocol_limits.max_result_bytes
      response
  then Ok response
  else Error Error.Resource_limit
;;
