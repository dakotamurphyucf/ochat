open! Core
open Authoring_context_service_core
open Authoring_context_activity

let source_host_error = function
  | Sources.Error.Unauthorized -> Error.Unauthorized
  | Expired_actor -> Error.Expired_actor
  | Foreign_root | Unsupported_source_selection | Project_declaration_unavailable ->
    Error.Source_unavailable
  | Selected_script_unavailable -> Error.Selection_unavailable
  | Closed | Foreign_domain -> Error.Closed
  | Busy -> Error.Busy
  | Context_limit | Result_limit | Document_bundle_limit
  | Document_parser Prompt.Chat_markdown.Generated.Parser_limit -> Error.Resource_limit
  | Sources _
  | Root _
  | Local _
  | Project _
  | Source_capture _
  | Document_parser _
  | Invalid_project_declaration -> Error.Source_unavailable
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

module Candidate_pin = struct
  type t =
    { owner : Authoring_context_service_core.t
    ; connection : Connection_context.t
    ; entry : connection_entry
    ; negotiation : Connection_context.Authoring_negotiation.t
    ; context : context_entry
    ; expected : P.Authoring.Context.Expected.t
    ; basis : Current.basis
    ; document_head : Retained_document_capture.t option
    }

  let current_live (current : Current.t) =
    !(current.live) && Int.equal current.domain (Stdlib.Domain.self () :> int)
  ;;

  let same_document_head t head =
    match t.basis with
    | Current.Project -> true
    | Current.Document -> Retained_document_capture.same_head t.document_head head
  ;;

  let same_basis_now t =
    match
      ( t.basis
      , Ledger.basis
          (Project.Project_cache.contexts t.owner.project_cache)
          ~identity:t.context.identity )
    with
    | Current.Project, Some Ledger.Basis.Project
    | Current.Document, Some Ledger.Basis.Document -> true
    | Current.Project, (Some Ledger.Basis.Document | None)
    | Current.Document, (Some Ledger.Basis.Project | None) -> false
  ;;

  let of_current owner current ~connection =
    if not (current_live current)
    then Error Error.Stale_context
    else (
      match find_connection owner connection with
      | None -> Error Error.Stale_negotiation
      | Some entry ->
        (match entry.negotiation with
         | Some negotiation
           when valid_negotiation entry
                && Option.exists
                     (find_context entry current.context.id)
                     ~f:(phys_equal current.context)
                && P.Authoring.Context.Expected.equal
                     current.expected
                     (P.Authoring.Context.Expected.of_view current.context.view) ->
           Ok
             { owner
             ; connection
             ; entry
             ; negotiation
             ; context = current.context
             ; expected = current.expected
             ; basis = Current.basis current
             ; document_head = current.retained_document_capture
             }
         | Some _ | None -> Error Error.Stale_negotiation))
  ;;

  let matches_current t current =
    if
      current_live current
      && phys_equal t.context current.context
      && P.Authoring.Context.Expected.equal t.expected current.expected
      && (match t.basis, Current.basis current with
          | Current.Project, Current.Project | Current.Document, Current.Document -> true
          | Current.Project, Current.Document | Current.Document, Current.Project -> false)
      && same_document_head t current.retained_document_capture
    then Ok ()
    else Error Error.Stale_context
  ;;

  let check_now t ~actor =
    let open Result.Let_syntax in
    let%bind () = check_domain t.owner in
    let%bind () = check_actor t.connection actor in
    let%bind () = check_actor_context t.context.original_actor actor in
    let%bind () =
      try_with_gate t.owner (fun () ->
        match find_connection t.owner t.connection with
        | Some entry when phys_equal entry t.entry ->
          let%bind () = check_entry t.owner entry in
          if
            valid_negotiation entry
            && Option.exists entry.negotiation ~f:(phys_equal t.negotiation)
            && Option.exists (find_context entry t.context.id) ~f:(phys_equal t.context)
            && P.Authoring.Context.Expected.equal
                 t.expected
                 (P.Authoring.Context.Expected.of_view t.context.view)
            && same_basis_now t
            && same_document_head t t.owner.document_capture
          then Ok ()
          else Error Error.Stale_context
        | Some _ | None -> Error Error.Stale_negotiation)
    in
    check_context_selection_now t.owner t.context ~actor
  ;;
end

let with_current
      ?(feature = Selected_feature.Core_authoring)
      ?(final_check = fun () -> Ok ())
      ?(publish = fun value -> Ok value)
      t
      ~actor
      ~connection
      ~(expected : P.Authoring.Context.Expected.t)
      ~checkpoint
      ~f
  =
  let expected_view = expected in
  let final_context = ref None in
  let borrowed_heads = ref None in
  with_request
    ~feature
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match !final_context with
      | None -> Error Error.Stale_context
      | Some context ->
        let open Result.Let_syntax in
        let%bind () = check_context_selection_now t context ~actor in
        final_check ())
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
                        (P.Authoring.Context.Expected.of_view actual.view) ->
              (match !borrowed_heads with
               | Some
                   ( project_head
                   , document_head
                   , capture_head
                   , index_head
                   , prefix_head
                   , template_head )
                 when phys_equal project_head t.project_cache
                      && phys_equal document_head t.document_cache
                      && Retained_document_capture.same_head
                           capture_head
                           t.document_capture
                      && phys_equal index_head t.analysis_index
                      && Authoring_provisional_index.same_head
                           prefix_head
                           t.provisional_index
                      && phys_equal template_head t.checked_templates -> publish value
               | Some _ | None -> Error Error.Stale_context)
            | Some _ | None -> Error Error.Stale_context)
         | None -> Error Error.Stale_negotiation))
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let%bind
           ( context
           , ledger
           , project_head
           , document_head
           , capture_head
           , index_head
           , prefix_head
           , template_head )
         =
         with_gate t (fun () ->
           match find_context entry expected.context_id with
           | None -> Error Error.Stale_context
           | Some context ->
             if
               P.Authoring.Context.Expected.equal
                 expected
                 (P.Authoring.Context.Expected.of_view context.view)
             then
               Ok
                 ( context
                 , Project.Project_cache.contexts t.project_cache
                 , t.project_cache
                 , t.document_cache
                 , t.document_capture
                 , t.analysis_index
                 , t.provisional_index
                 , t.checked_templates )
             else Error Error.Stale_context)
       in
       let%bind () = check_context_owner t context ~actor ~checkpoint in
       final_context := Some context;
       borrowed_heads
       := Some
            ( project_head
            , document_head
            , capture_head
            , index_head
            , prefix_head
            , template_head );
       let current =
         Current.
           { owner = ledger
           ; domain = t.domain
           ; context
           ; expected = expected_view
           ; live = ref true
           ; retained_document_capture = capture_head
           ; borrowed_project_cache = project_head
           ; borrowed_document_cache = document_head
           ; borrowed_analysis_index = index_head
           ; borrowed_provisional_index = prefix_head
           ; borrowed_checked_templates = template_head
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

let parent_scope_error = function
  | Authoring_parent_scope.Error.Foreign_operation | Stale_parent -> Error.Stale_context
  | Unauthorized -> Error.Unauthorized
  | Busy -> Error.Busy
  | Prior_unavailable -> Error.Selection_unavailable
;;

let with_parent_scope
      ?script_selection
      t
      ~actor
      ~connection
      ~expected
      ~operation
      ~checkpoint
      ~f
  =
  with_current
    ~feature:Selected_feature.Bundle_upload
    t
    ~actor
    ~connection
    ~expected
    ~checkpoint
    ~f:(fun current ~checkpoint ->
      let open Result.Let_syntax in
      let context = current.Current.context in
      let%bind origin =
        match context.owner with
        | Host_project _ ->
          let%map inputs = Current.project_inputs current ~checkpoint in
          Authoring_parent_scope.Origin.Project (Ledger.Inputs.base inputs)
        | Uploaded_project capture ->
          let%map inputs = Current.project_inputs current ~checkpoint in
          Authoring_parent_scope.Origin.Uploaded_project
            { previous = Ledger.Inputs.base inputs
            ; pin = Upload.Context_capture.published_pin capture
            }
        | Host_document _ | Supplied_document _ ->
          let%bind inputs = Current.document_inputs current ~checkpoint in
          let selected =
            match current.retained_document_capture with
            | Some retained
              when Retained_document_capture.matches_current retained ~context ~inputs ->
              Some
                Authoring_parent_scope.Origin.
                  { capture = Retained_document_capture.capture retained
                  ; rows = Retained_document_capture.selection_rows retained
                  ; original_actor = context.original_actor
                  ; script_selection
                  }
            | Some _ | None -> None
          in
          Ok (Authoring_parent_scope.Origin.Document { inputs; selected })
      in
      let live = current.live in
      let scope_error = function
        | Error.Busy -> Authoring_parent_scope.Error.Busy
        | Unauthorized | Expired_actor | Foreign_connection -> Unauthorized
        | Selection_unavailable -> Prior_unavailable
        | Uninstalled
        | Unselected_feature
        | Closed
        | Stale_context
        | Stale_source
        | Stale_negotiation
        | Source_unavailable
        | Invalid_request
        | Invalid_position _
        | Resource_limit
        | Storage_unavailable
        | Analysis_unavailable
        | Checked_template_failed
        | Upload_failure _ -> Stale_parent
      in
      let check_current_now () =
        let open Result.Let_syntax in
        let%bind () =
          (match context.owner with
           | Uploaded_project capture ->
             (match t.upload with
              | None -> Error Error.Uninstalled
              | Some upload ->
                Upload.check_context_selection_policy_now upload ~actor capture
                |> Result.map_error ~f:upload_host_error)
           | Host_project _ | Host_document _ | Supplied_document _ ->
             check_context_selection_now t context ~actor)
          |> Result.map_error ~f:scope_error
        in
        try_with_gate t (fun () ->
          let%bind () = check_domain t in
          let%bind () = check_actor_context context.original_actor actor in
          match find_connection t connection with
          | Some entry ->
            let%bind () = check_entry t entry in
            if not (valid_negotiation entry)
            then Error Error.Stale_negotiation
            else (
              match find_context entry context.id with
              | Some actual
                when phys_equal actual context
                     && P.Authoring.Context.Expected.equal
                          expected
                          (P.Authoring.Context.Expected.of_view actual.view)
                     && phys_equal current.borrowed_project_cache t.project_cache
                     && phys_equal current.borrowed_document_cache t.document_cache
                     && Retained_document_capture.same_head
                          current.retained_document_capture
                          t.document_capture
                     && phys_equal current.borrowed_analysis_index t.analysis_index
                     && Authoring_provisional_index.same_head
                          current.borrowed_provisional_index
                          t.provisional_index
                     && phys_equal current.borrowed_checked_templates t.checked_templates
                -> Ok ()
              | Some _ | None -> Error Error.Stale_context)
          | None -> Error Error.Stale_negotiation)
        |> Result.map_error ~f:scope_error
      in
      let%bind scope =
        Authoring_parent_scope.issue
          ~operation
          ~host:t.host
          ~connection
          ~actor
          ~expected
          ~live
          ~check_current_now
          ~origin
        |> Result.map_error ~f:parent_scope_error
      in
      f scope ~checkpoint)
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
