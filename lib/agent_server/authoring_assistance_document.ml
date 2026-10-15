open! Core
module Owner = Authoring_context_service_core
module Current = Owner.Current
module Error = Owner.Error
module Wire = Agent_protocol.Authoring_assistance.Assistance
module Wire_source = Agent_protocol.Authoring.Source
module Ledger = Chatml_authoring_context
module Captured = Chatml_source
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration
module Script = Chatmd_authoring_analysis.Script
module Chatmd = Chatmd_authoring_assistance
module Chatml = Chatml_authoring_fallback
module Mapped = Chatmd_authoring_fallback
module Highlight = Language_highlighting.Highlight_snapshot
module Source = Authoring_assistance_source
module Capture = Authoring_assistance_capture
module At_declaration = Authoring_assistance_declaration
module Project = Authoring_assistance_project
module Paths = Authoring_assistance_paths
module Local = Authoring_assistance_local
module Limits = Authoring_assistance_admission.Limits
module Selected_tools = Authoring_assistance_admission.Selected_tool_policy
module Script_policy = Authoring_script_selection.Policy
module Project_capture = Authoring_source_host.Document_context.Project_capture
module Project_policy = Authoring_project_metadata.Policy
module Checked_template = Authoring_checked_template_suggestion

type t =
  { result : Wire.Result.t
  ; check_now : unit -> (unit, Error.t) result
  }

let result t = t.result
let check_now t = t.check_now ()

let parser_input source ~max_bytes ~checkpoint =
  let bytes = Captured.contents source in
  Input.create
    ~role:Root
    ~path:(Captured.Id.relative_path (Captured.id source))
    ~authored:bytes
    ~effective:bytes
    ~max_input_bytes:max_bytes
    ~checkpoint
  |> Result.map_error ~f:(function
    | Input.Error.Input_byte_limit -> Error.Resource_limit
    | Empty_path | Import_changed | Invalid_range -> Error.Invalid_request)
;;

let unavailable request reason =
  Wire.Result.create ~request ~items:[] ~status:(Wire.Status.Unavailable reason)
  |> Result.map_error ~f:(fun _ -> Error.Resource_limit)
;;

let script_error = function
  | Chatmd_authoring_analysis.Error.Membership_limit -> Error.Resource_limit
  | Stale_document | Script_body_mismatch -> Error.Stale_source
  | Invalid_selection
  | Foreign_declaration
  | Ambiguous_declaration
  | Selection_mismatch
  | Compiler _
  | Projection _
  | Policy_mismatch
  | Invalid_occupancy
  | Retention_limit -> Error.Selection_unavailable
;;

let mapped_error = function
  | Mapped.Error.Stale_document | Foreign_authored_input -> Error.Stale_source
  | Stale_selected_build -> Error.Selection_unavailable
  | Work_limit -> Error.Resource_limit
;;

let fallback_error = function
  | Chatml.Error.Stale_request_source -> Error.Stale_source
  | Stale_selected_build | Contract_mismatch | Invalid_projection ->
    Error.Selection_unavailable
  | Contract_identity_limit
  | Contract_work_limit
  | Projection_work_limit
  | Output_byte_limit
  | Lexical_admission _
  | Wire _ -> Error.Resource_limit
;;

let selected_contract_error = function
  | Chatml.Error.Contract_identity_limit
  | Contract_work_limit
  | Projection_work_limit
  | Output_byte_limit -> Error.Resource_limit
  | Stale_request_source
  | Stale_selected_build
  | Contract_mismatch
  | Invalid_projection
  | Lexical_admission _
  | Wire _ -> Error.Selection_unavailable
;;

let prepare
      current
      ~actor
      ~request
      ~script_selection
      ~project_metadata
      ~selected_tools
      ~project_build_limits
      ~(limits : Limits.t)
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind inputs = Current.document_inputs current ~checkpoint in
  let root = Ledger.Document.Inputs.source inputs in
  let requested = Wire.Request.source request in
  let is_root = Captured.Id.equal (Captured.id root) (Wire_source.id requested) in
  let%bind root_source =
    if is_root
    then Source.admit current ~source:requested ~checkpoint |> Result.map ~f:fst
    else Ok root
  in
  let%bind retained = Current.document_capture current ~checkpoint in
  let%bind admitted =
    match retained with
    | None -> Ok None
    | Some capture ->
      Capture.admit
        ~current:inputs
        ~capture
        ~requested
        ~max_inputs:limits.max_imported_inputs
        ~max_source_bytes:limits.max_parser_input_bytes
        ~max_total_bytes:limits.max_capture_admission_bytes
        ~max_work_steps:limits.max_projection_work_steps
        ~checkpoint
      |> Result.map ~f:Option.some
  in
  let%bind authored =
    if is_root
    then Ok root_source
    else
      Option.value_map
        admitted
        ~default:(Error Error.Source_unavailable)
        ~f:(fun admitted -> Ok (Capture.requested_source admitted))
  in
  let%bind view_input, parsed, captured_inputs, unique_view =
    match admitted with
    | Some admitted ->
      (match Capture.unique_view_input admitted with
       | Some input ->
         Ok (input, Some (Capture.parsed admitted), Capture.captured_inputs admitted, true)
       | None ->
         let%map input =
           parser_input authored ~max_bytes:limits.max_parser_input_bytes ~checkpoint
         in
         input, Some (Capture.parsed admitted), Capture.captured_inputs admitted, false)
    | None ->
      let%map input =
        parser_input authored ~max_bytes:limits.max_parser_input_bytes ~checkpoint
      in
      input, None, [], false
  in
  let original_actor = current.context.original_actor in
  let context = current.context.view in
  let selected_registry =
    Option.bind selected_tools ~f:(fun policy ->
      Selected_tools.current policy ~original_actor ~actor ~context)
  in
  let%bind () =
    match selected_registry with
    | None -> Ok ()
    | Some selected when Selected_tools.Registry.within_limits selected limits -> Ok ()
    | Some _ -> Error Error.Resource_limit
  in
  let%bind located =
    match parsed, admitted with
    | Some parsed, Some admitted ->
      At_declaration.at_authored_cursor
        ~parsed
        ~inputs:(Capture.requested_inputs admitted)
        ~authored
        ~cursor_byte:(Wire.Request.cursor_byte request)
        ~max_source_bytes:limits.max_parser_input_bytes
        ~max_work_steps:limits.max_projection_work_steps
        ~checkpoint
    | None, _ | Some _, None -> Ok (At_declaration.Exact None)
  in
  let declaration =
    match located with
    | At_declaration.Exact declaration -> declaration
    | Transformed -> None
  in
  (* Repeated genuine imports may have different effective views. A unique
     declaration still selects its actual parser input; otherwise only the
     authored local scan is available, with no invented Parsed membership. *)
  let%bind lexical_input, facts_parsed =
    match located, declaration with
    | At_declaration.Transformed, _ ->
      let%map input =
        parser_input authored ~max_bytes:limits.max_parser_input_bytes ~checkpoint
      in
      input, None
    | Exact _, Some declaration ->
      Ok (Input.Range.input (Declaration.range declaration), parsed)
    | Exact _, None -> Ok (view_input, if unique_view then parsed else None)
  in
  let%bind metadata =
    match retained, parsed, declaration with
    | Some retained, Some _, Some declaration ->
      let%map candidate = Current.project_capture current ~checkpoint in
      Option.filter candidate ~f:(fun candidate ->
        Project_capture.matches candidate ~parsed:retained ~declaration)
    | None, _, _ | Some _, None, _ | Some _, Some _, None -> Ok None
  in
  let%bind used_metadata =
    match metadata, project_metadata, parsed, declaration with
    | Some capture, Some policy, Some parsed, Some declaration ->
      if
        Project_policy.authorize_now
          policy
          ~original_actor
          ~actor
          ~current:inputs
          ~parsed
          ~declaration
          ~capture
      then Ok (Some (policy, parsed, declaration, capture))
      else Error Error.Selection_unavailable
    | None, _, _, _ | Some _, None, _, _ -> Ok None
    | Some _, Some _, None, _ | Some _, Some _, Some _, None ->
      Error Error.Selection_unavailable
  in
  let admitted_metadata =
    Option.map used_metadata ~f:(fun (_, _, _, capture) -> capture)
  in
  let selection_witness =
    match script_selection, parsed, declaration with
    | Some policy, Some parsed, Some declaration ->
      Some
        ( policy
        , parsed
        , declaration
        , Script_policy.select
            policy
            ~original_actor
            ~actor
            ~current:inputs
            ~parsed
            ~declaration
            ~metadata:admitted_metadata )
    | _ -> None
  in
  let selected =
    Option.bind selection_witness ~f:(fun (policy, parsed, declaration, selected) ->
      Option.map selected ~f:(fun selected -> policy, parsed, declaration, selected))
  in
  let%bind selected =
    match selected, admitted_metadata with
    | Some ((_, _, _, selected) as selection), Some metadata ->
      (match
         Project_capture.matches_project_contents
           metadata
           ~build:(Authoring_script_selection.Selected.build selected)
           ~max_compare_bytes:limits.protocol_limits.max_context_source_bytes
           ~checkpoint
       with
       | Ok true -> Ok (Some selection)
       | Ok false -> Ok None
       | Error Authoring_source_host.Error.Result_limit -> Error Error.Resource_limit
       | Error _ -> Error Error.Selection_unavailable)
    | _ -> Ok selected
  in
  let%bind project =
    match used_metadata, selected with
    | Some (_, _, _, capture), _ -> Ok (Some (Project_capture.project capture))
    | None, None -> Ok None
    | None, Some (_, _, _, selected) ->
      Project.selected_project
        (Authoring_script_selection.Selected.build selected)
        ~max_project_bytes:limits.protocol_limits.max_context_source_bytes
        ~checkpoint
  in
  let%bind cursor =
    Highlight.scan_chatmd_cursor
      lexical_input
      ~cursor_byte:(Wire.Request.cursor_byte request)
      ~limits:limits.lexical
      ~checkpoint
    |> Result.map_error ~f:(fun _ -> Error.Resource_limit)
  in
  let%bind paths, path_owner =
    match current.context.owner, cursor with
    | Owner.Host_document owner, Highlight.Cursor.Available cursor when is_root ->
      let%map paths =
        Paths.host_document
          ~owner
          ~view:context
          ~source:authored
          ~cursor
          ~actor
          ~max_paths:limits.protocol_limits.max_path_candidates
          ~max_projection_work_steps:limits.max_projection_work_steps
          ~checkpoint
      in
      paths, if List.is_empty paths then None else Some owner
    | ( ( Owner.Host_project _
        | Owner.Host_document _
        | Owner.Uploaded_project _
        | Owner.Supplied_document _ )
      , (Highlight.Cursor.Available _ | Highlight.Cursor.Unavailable _) ) -> Ok ([], None)
  in
  let%bind facts =
    Chatmd.Facts.create
      ~request
      ~authored
      ~lexical_input
      ~parsed:facts_parsed
      ~captured_inputs
      ~project
        (* This service currently supplies only authenticated host-root path
         candidates. Facts uses [selected] solely for Selected_closure path
         provenance, whose project Source.Id need not share this document
         capture's namespace. Selected catalog help is admitted separately
         below against the actual Script and compiler contract. *)
      ~selected:None
      ~paths
      ~selected_tools:
        (Option.value_map
           selected_registry
           ~default:[]
           ~f:Selected_tools.Registry.selected_tools)
      ~limits:limits.chatmd
      ~checkpoint
    |> Result.map_error ~f:Local.classify
  in
  let%bind markup =
    Chatmd.produce facts ~checkpoint |> Result.map_error ~f:Local.classify
  in
  let script_body_cursor =
    match cursor with
    | Highlight.Cursor.Available cursor ->
      (match Highlight.Cursor.kind cursor with
       | Highlight.Cursor.Kind.Script_body -> true
       | Tag_name _ | Attribute_name _ | Attribute_value _ | Text | Opaque -> false)
    | Unavailable _ -> false
  in
  let%bind result, checked_script =
    match selected, script_body_cursor with
    | Some (_, parsed, declaration, selected), true ->
      let%bind build =
        Authoring_selected_script_overlay.prepare
          selected
          ~declaration
          ~max_body_bytes:limits.protocol_limits.max_context_source_bytes
          ~limits:project_build_limits
          ~checkpoint
        |> Result.map_error ~f:Authoring_semantic_error.selected_entry_overlay
      in
      (match
         Script.assess
           inputs
           ~parsed:(Some parsed)
           ~script_id:(Some (Declaration.script_id declaration))
           ~selection:(Chatmd_authoring_analysis.Selection.Validated_build build)
           ~max_membership_work_steps:limits.max_projection_work_steps
           ~checkpoint
         |> Result.map_error ~f:script_error
       with
       | Error error -> Error error
       | Ok (Script.Unavailable _) -> Ok (markup, None)
       | Ok (Script.Ready script) ->
         (match
            Chatml.Selected_contract.admit
              build
              ~selected_contract:(Authoring_script_selection.Selected.contract selected)
              ~limits:limits.chatml
              ~checkpoint
          with
          | Error error ->
            (match selected_contract_error error with
             | Error.Resource_limit -> Error Error.Resource_limit
             | _ ->
               unavailable request Wire.Status.Catalog_unavailable
               |> Result.map ~f:(fun result -> result, None))
          | Ok selected_contract ->
            let%bind origin =
              Mapped.Origin.admit
                script
                ~current:inputs
                ~authored_input:lexical_input
                ~authored_source:authored
                ~selected:selected_contract
                ~limits:limits.mapped
                ~checkpoint
              |> Result.map_error ~f:mapped_error
            in
            (match origin with
             | Mapped.Origin.Unavailable -> Ok (markup, None)
             | Mapped.Origin.Ready origin ->
               let%map mapped =
                 Chatml.produce
                   origin
                   ~request
                   ~selected:(Some selected_contract)
                   ~limits:limits.chatml
                   ~checkpoint
                 |> Result.map_error ~f:fallback_error
               in
               ( (match Wire.Result.status mapped with
                  | Wire.Status.Unavailable Wire.Status.Source_map_unavailable -> markup
                  | Wire.Status.Complete
                  | Wire.Status.Truncated _
                  | Wire.Status.Unavailable _ -> mapped)
               , Some script ))))
    | None, _ | Some _, false -> Ok (markup, None)
  in
  let%bind checked =
    match checked_script, declaration with
    | Some script, Some declaration ->
      Checked_template.document
        current
        ~inputs
        ~script
        ~declaration
        ~authored_input:lexical_input
        ~authored
        ~request
        ~checkpoint
    | None, _ | Some _, None -> Ok None
  in
  let%bind result = Checked_template.include_checked result checked in
  let check_now () =
    let open Result.Let_syntax in
    let%bind () =
      match selection_witness with
      | None -> Ok ()
      | Some (policy, parsed, declaration, None) ->
        if
          Option.is_none
            (Script_policy.select
               policy
               ~original_actor
               ~actor
               ~current:inputs
               ~parsed
               ~declaration
               ~metadata:admitted_metadata)
        then Ok ()
        else Error Error.Selection_unavailable
      | Some (policy, parsed, declaration, Some selected) ->
        if
          Script_policy.same_now
            policy
            ~original_actor
            ~actor
            ~current:inputs
            ~parsed
            ~declaration
            ~metadata:admitted_metadata
            ~selected
        then Ok ()
        else Error Error.Selection_unavailable
    in
    let%bind () =
      match used_metadata with
      | None -> Ok ()
      | Some (policy, parsed, declaration, capture) ->
        if
          Project_policy.authorize_now
            policy
            ~original_actor
            ~actor
            ~current:inputs
            ~parsed
            ~declaration
            ~capture
        then Ok ()
        else Error Error.Selection_unavailable
    in
    let%bind () =
      match selected_tools, selected_registry with
      | _, None -> Ok ()
      | None, Some _ -> Error Error.Selection_unavailable
      | Some policy, Some used ->
        (match Selected_tools.current policy ~original_actor ~actor ~context with
         | Some current when Selected_tools.Registry.equal_identity current used -> Ok ()
         | None | Some _ -> Error Error.Selection_unavailable)
    in
    match path_owner with
    | None -> Ok ()
    | Some owner ->
      Authoring_source_host.Document_context.check_current owner ~actor
      |> Result.map_error ~f:Authoring_context_owner.source_host_error
  in
  Ok { result; check_now }
;;
