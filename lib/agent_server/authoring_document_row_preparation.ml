open! Core
module P = Agent_protocol
module Analysis = P.Authoring.Analysis
module Owner = Authoring_context_service_core
module Host = Authoring_context_owner
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Parsed_capture = Sources.Document_context.Parsed_capture
module Project_capture = Sources.Document_context.Project_capture
module Markdown = Prompt.Chat_markdown
module Selection = Authoring_script_selection
module Metadata = Authoring_project_metadata
module Rows = Authoring_document_selection_rows

type t =
  { owner : Owner.t
  ; env : Eio_unix.Stdenv.base
  ; script_selection : Selection.Policy.t option
  ; project_metadata : Metadata.Policy.t option
  ; project_build_limits : Chatml_project_build.Limits.t
  ; max_document_bundle_files : int
  ; max_document_bundle_bytes : int
  ; max_parser_work_steps : int
  }

let create
      ~owner
      ~env
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~max_document_bundle_files
      ~max_document_bundle_bytes
      ~max_parser_work_steps
  =
  let defaults = Chatmd_source_bundle.default_limits in
  if
    max_document_bundle_files <= 0
    || max_document_bundle_files > defaults.max_files
    || max_document_bundle_bytes <= 0
    || max_document_bundle_bytes > defaults.max_bundle_bytes
    || max_parser_work_steps <= 0
    || max_parser_work_steps > 1_000_000
  then Or_error.error_string "row preparation limits exceed finite host/parser range"
  else
    Ok
      { owner
      ; env
      ; script_selection
      ; project_metadata
      ; project_build_limits
      ; max_document_bundle_files
      ; max_document_bundle_bytes
      ; max_parser_work_steps
      }
;;

let metadata_for_declaration
      t
      current
      ~actor
      ~inputs
      ~capture
      ~parsed
      ~declaration
      ~checkpoint
  =
  let bundle_sources = Chatmd_source_bundle.sources (Parsed_capture.bundle capture) in
  let rec remaining count bytes = function
    | [] ->
      if count >= t.max_document_bundle_files
      then 0
      else t.max_document_bundle_bytes - bytes
    | (_, contents) :: rest ->
      checkpoint ();
      let length = String.length contents in
      if
        count >= t.max_document_bundle_files
        || length > t.max_document_bundle_bytes - bytes
      then 0
      else remaining (count + 1) (bytes + length) rest
  in
  let remaining_bytes = remaining 0 0 bundle_sources in
  match
    ( t.project_metadata
    , Markdown.Declaration.project_selection declaration
    , current.Owner.Current.context.owner )
  with
  | Some _, Some _, Owner.Host_document _ when remaining_bytes <= 0 ->
    Error Owner.Error.Resource_limit
  | Some policy, Some _, Owner.Host_document document ->
    let defaults = Document_schema.Limits.default in
    let open Result.Let_syntax in
    let%bind schema_limits =
      Document_schema.Limits.create
        ~max_bytes:(Int.min remaining_bytes (Document_schema.Limits.max_bytes defaults))
        ~max_depth:(Document_schema.Limits.max_depth defaults)
        ~max_fields:(Document_schema.Limits.max_fields defaults)
        ~max_nodes:(Document_schema.Limits.max_nodes defaults)
      |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
    in
    (match
       Sources.Document_context.capture_project_declaration
         document
         ~actor
         ~parsed:capture
         ~declaration
         ~schema_limits
         ~max_locator_bytes:1024
         ~max_path_components:256
         ~max_identity_bytes:(1024 * 1024)
         ~max_work_steps:t.max_parser_work_steps
         ~checkpoint
     with
     | Ok project
       when Metadata.Policy.authorize_now
              policy
              ~original_actor:current.context.original_actor
              ~actor
              ~current:inputs
              ~parsed
              ~declaration
              ~capture:project -> Ok (Some project)
     | Ok _ -> Ok None
     | Error Sources.Error.Project_declaration_unavailable
     | Error Sources.Error.Invalid_project_declaration -> Ok None
     | Error error -> Error (Host.source_host_error error))
  | None, _, _ | Some _, None, _ | Some _, Some _, Owner.Supplied_document _ -> Ok None
  | Some _, Some _, (Owner.Host_project _ | Owner.Uploaded_project _) ->
    Error Owner.Error.Stale_context
;;

let selected_for_declaration
      t
      current
      ~actor
      ~inputs
      ~parsed
      ~declaration
      ~metadata
      ~checkpoint
  =
  let raw_selected =
    Option.bind t.script_selection ~f:(fun policy ->
      Selection.Policy.select
        policy
        ~original_actor:current.Owner.Current.context.original_actor
        ~actor
        ~current:inputs
        ~parsed
        ~declaration
        ~metadata)
  in
  match raw_selected, metadata with
  | Some selected, Some project ->
    (match
       Project_capture.matches_project_contents
         project
         ~build:(Selection.Selected.build selected)
         ~max_compare_bytes:t.max_document_bundle_bytes
         ~checkpoint
     with
     | Ok true -> Ok (raw_selected, Some selected)
     | Ok false -> Ok (raw_selected, None)
     | Error _ -> Error Owner.Error.Resource_limit)
  | Some _, None | None, _ -> Ok (raw_selected, raw_selected)
;;

let selected_matches_declaration t ~inputs ~parsed ~declaration selected ~checkpoint =
  match
    Authoring_selected_script_overlay.prepare
      selected
      ~declaration
      ~max_body_bytes:t.max_document_bundle_bytes
      ~limits:t.project_build_limits
      ~checkpoint
  with
  | Error error ->
    (match Authoring_semantic_error.selected_entry_overlay error with
     | Owner.Error.Resource_limit -> Error Owner.Error.Resource_limit
     | _ -> Ok false)
  | Ok build ->
    (match
       Chatmd_authoring_analysis.Script.assess
         inputs
         ~parsed:(Some parsed)
         ~script_id:(Some (Markdown.Declaration.script_id declaration))
         ~selection:(Chatmd_authoring_analysis.Selection.Validated_build build)
         ~max_membership_work_steps:t.max_parser_work_steps
         ~checkpoint
     with
     | Error Chatmd_authoring_analysis.Error.Membership_limit ->
       Error Owner.Error.Resource_limit
     | Error _ | Ok (Chatmd_authoring_analysis.Script.Unavailable _) -> Ok false
     | Ok (Ready script) ->
       Ok (phys_equal (Chatmd_authoring_analysis.Script.declaration script) declaration))
;;

let prepare
      t
      current
      ~actor
      ~inputs
      ~capture
      ~parsed
      declaration
      ~declaring_source
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind metadata =
    metadata_for_declaration
      t
      current
      ~actor
      ~inputs
      ~capture
      ~parsed
      ~declaration
      ~checkpoint
  in
  let original_actor = current.context.original_actor in
  let check_metadata () =
    match metadata, t.project_metadata with
    | None, _ -> Ok ()
    | Some capture, Some policy ->
      if
        Metadata.Policy.authorize_now
          policy
          ~original_actor
          ~actor
          ~current:inputs
          ~parsed
          ~declaration
          ~capture
      then Ok ()
      else Error Owner.Error.Selection_unavailable
    | Some _, None -> Error Owner.Error.Selection_unavailable
  in
  let%bind raw_selected, usable =
    selected_for_declaration
      t
      current
      ~actor
      ~inputs
      ~parsed
      ~declaration
      ~metadata
      ~checkpoint
  in
  let check_raw_selected () =
    match raw_selected, t.script_selection with
    | None, None -> Ok ()
    | None, Some policy ->
      if
        Option.is_none
          (Selection.Policy.select
             policy
             ~original_actor
             ~actor
             ~current:inputs
             ~parsed
             ~declaration
             ~metadata)
      then Ok ()
      else Error Owner.Error.Selection_unavailable
    | Some selected, Some policy ->
      if
        Selection.Policy.same_now
          policy
          ~original_actor
          ~actor
          ~current:inputs
          ~parsed
          ~declaration
          ~metadata
          ~selected
      then Ok ()
      else Error Owner.Error.Selection_unavailable
    | Some _, None -> Error Owner.Error.Selection_unavailable
  in
  let final_check () =
    let%bind () = check_metadata () in
    check_raw_selected ()
  in
  match usable with
  | Some selected ->
    let%map matches =
      selected_matches_declaration t ~inputs ~parsed ~declaration selected ~checkpoint
    in
    ( (if matches
       then
         Rows.Row.independently_selected
           ~parsed
           ~declaration
           ~declaring_source
           ~metadata
           ~selected
       else
         Rows.Row.no_validated_build
           ~parsed
           ~declaration
           ~declaring_source
           ~metadata
           ~reason:No_validated_project)
    , final_check )
  | None when Option.is_some raw_selected ->
    Ok
      ( Rows.Row.no_validated_build
          ~parsed
          ~declaration
          ~declaring_source
          ~metadata
          ~reason:No_validated_project
      , final_check )
  | None ->
    (match metadata, current.context.owner with
     | Some metadata, Owner.Host_document document ->
       let result =
         Sources.Document_context.prepare_selected_script
           document
           ~actor
           ~parsed:capture
           ~declaration
           ~project_capture:metadata
           ~select_contract:t.owner.selection_policy.project_contract
           ~clock:t.env#mono_clock
           ~max_script_bytes:t.max_document_bundle_bytes
           ~max_locator_bytes:1024
           ~max_path_components:256
           ~max_work_steps:t.max_parser_work_steps
           ~checkpoint
       in
       (match result with
        | Ok source ->
          let admission_limits =
            Selection.Limits.create
              ~max_identity_bytes:(16 * 1024 * 1024)
              ~max_work_steps:t.max_parser_work_steps
            |> Or_error.ok_exn
          in
          (match
             Selection.Selected.admit
               ~build:(Sources.Document_context.Script_capture.build source)
               ~contract:(Sources.Document_context.Script_capture.contract source)
               ~limits:admission_limits
               ~checkpoint
           with
           | Ok selected ->
             let final_check () =
               let%bind () = final_check () in
               Sources.Document_context.check_selected_now
                 document
                 ~script:source
                 ~actor
                 ~select_contract:t.owner.selection_policy.project_contract
               |> Result.map_error ~f:Host.source_host_error
             in
             let%map matches =
               selected_matches_declaration
                 t
                 ~inputs
                 ~parsed
                 ~declaration
                 selected
                 ~checkpoint
             in
             ( (if matches
                then
                  Rows.Row.host_prepared
                    ~parsed
                    ~declaration
                    ~declaring_source
                    ~metadata
                    ~source
                    ~selected
                else
                  Rows.Row.no_validated_build
                    ~parsed
                    ~declaration
                    ~declaring_source
                    ~metadata:(Some metadata)
                    ~reason:No_validated_project)
             , final_check )
           | Error Selection.Error.Contract_mismatch ->
             Ok
               ( Rows.Row.no_validated_build
                   ~parsed
                   ~declaration
                   ~declaring_source
                   ~metadata:(Some metadata)
                   ~reason:No_validated_project
               , final_check )
           | Error (Identity_limit | Work_limit) -> Error Owner.Error.Resource_limit)
        | Error
            ( Sources.Error.Project_declaration_unavailable
            | Sources.Error.Invalid_project_declaration
            | Sources.Error.Selected_script_unavailable ) ->
          Ok
            ( Rows.Row.no_validated_build
                ~parsed
                ~declaration
                ~declaring_source
                ~metadata:(Some metadata)
                ~reason:No_validated_project
            , final_check )
        | Error error -> Error (Host.source_host_error error))
     | None, _ | Some _, Owner.Supplied_document _ ->
       Ok
         ( Rows.Row.no_validated_build
             ~parsed
             ~declaration
             ~declaring_source
             ~metadata
             ~reason:No_validated_project
         , final_check )
     | Some _, (Owner.Host_project _ | Owner.Uploaded_project _) ->
       Error Owner.Error.Stale_context)
;;
