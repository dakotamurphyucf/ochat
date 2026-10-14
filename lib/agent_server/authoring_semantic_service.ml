open! Core
module P = Agent_protocol
module Analysis = P.Authoring.Analysis
module Owner = Authoring_context_service_core
module Host = Authoring_context_owner
module Publication = Authoring_analysis_publication
module Project = Authoring_semantic_project
module Document = Authoring_semantic_document
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Parsed_capture = Sources.Document_context.Parsed_capture
module Project_capture = Sources.Document_context.Project_capture
module Markdown = Prompt.Chat_markdown
module Selection = Authoring_script_selection
module Metadata = Authoring_project_metadata

module Limits = struct
  type t =
    { max_document_bundle_files : int
    ; max_document_bundle_bytes : int
    ; max_parser_work_steps : int
    ; max_diagnostic_work_steps : int
    ; max_work_steps : int
    }

  let create
        ~max_document_bundle_files
        ~max_document_bundle_bytes
        ~max_parser_work_steps
        ~max_diagnostic_work_steps
    =
    let defaults = Chatmd_source_bundle.default_limits in
    if
      max_document_bundle_files <= 0
      || max_document_bundle_files > defaults.max_files
      || max_document_bundle_bytes <= 0
      || max_document_bundle_bytes > defaults.max_bundle_bytes
      || max_parser_work_steps <= 0
      || max_parser_work_steps > 1_000_000
      || max_diagnostic_work_steps <= 0
      || max_diagnostic_work_steps > 1_000_000
    then Or_error.error_string "invalid semantic Analyze limits"
    else
      Ok
        { max_document_bundle_files
        ; max_document_bundle_bytes
        ; max_parser_work_steps
        ; max_diagnostic_work_steps
        ; max_work_steps = max_parser_work_steps + max_diagnostic_work_steps
        }
  ;;

  let bundle t : Chatmd_source_bundle.limits =
    let defaults = Chatmd_source_bundle.default_limits in
    { max_source_bytes = Int.min defaults.max_source_bytes t.max_document_bundle_bytes
    ; max_bundle_bytes = t.max_document_bundle_bytes
    ; max_files = t.max_document_bundle_files
    }
  ;;
end

type t =
  { owner : Owner.t
  ; env : Eio_unix.Stdenv.base
  ; document_dir : Eio.Fs.dir_ty Eio.Path.t
  ; script_selection : Selection.Policy.t option
  ; project_metadata : Metadata.Policy.t option
  ; project_build_limits : Chatml_project_build.Limits.t
  ; limits : Limits.t
  }

let create
      ~owner
      ~env
      ~document_dir
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~limits
  =
  { owner
  ; env
  ; document_dir
  ; script_selection
  ; project_metadata
  ; project_build_limits
  ; limits
  }
;;

exception Work_limit

let unavailable current target reason =
  match
    Analysis.Analyze.unavailable ~context:(Owner.Current.expected current) ~target ~reason
  with
  | Ok response -> Ok (Publication.Prepared.unavailable ~response)
  | Error _ -> Error Owner.Error.Invalid_request
;;

let unavailable_reason = function
  | Chatmd_authoring_analysis.Unavailable.No_selected_script ->
    Analysis.Analyze.No_selected_script
  | Incomplete_or_malformed_document -> Incomplete_or_malformed_document
  | No_validated_project -> No_validated_project
  | Dynamic_only_source -> Dynamic_only_source
;;

let find_declaration parsed ~script_id ~checkpoint =
  let rec visit count found = function
    | [] ->
      (match found with
       | None -> `Missing
       | Some declaration -> `Selected declaration)
    | _ :: _ when count >= 1024 -> `Limit
    | declaration :: rest ->
      checkpoint ();
      if String.equal (Markdown.Declaration.script_id declaration) script_id
      then (
        match found with
        | Some _ -> `Ambiguous
        | None -> visit (count + 1) (Some declaration) rest)
      else visit (count + 1) found rest
  in
  visit 0 None (Markdown.Parsed.declarations parsed)
;;

let capture_document t current ~actor ~source ~checkpoint =
  let limits = Limits.bundle t.limits in
  match current.Owner.Current.context.owner with
  | Owner.Host_document document ->
    Sources.Document_context.capture_parser_bundle
      ~limits
      document
      ~actor
      ~source
      ~dir:t.document_dir
      ~checkpoint
  | Supplied_document _ ->
    Parsed_capture.parse_root_only ~limits ~source ~dir:t.document_dir ~checkpoint
  | Host_project _ | Uploaded_project _ -> Error Sources.Error.Foreign_root
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
      if count >= t.limits.max_document_bundle_files
      then 0
      else t.limits.max_document_bundle_bytes - bytes
    | (_, contents) :: rest ->
      checkpoint ();
      let length = String.length contents in
      if
        count >= t.limits.max_document_bundle_files
        || length > t.limits.max_document_bundle_bytes - bytes
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
         ~max_work_steps:t.limits.max_parser_work_steps
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
         ~max_compare_bytes:t.limits.max_document_bundle_bytes
         ~checkpoint
     with
     | Ok true -> Ok (raw_selected, Some selected)
     | Ok false -> Ok (raw_selected, None)
     | Error _ -> Error Owner.Error.Resource_limit)
  | Some _, None | None, _ -> Ok (raw_selected, raw_selected)
;;

let analyze_document t current ~actor ~script_id ~checkpoint ~set_final_check =
  let open Result.Let_syntax in
  let target =
    Analysis.Target.selected_script script_id
    |> Result.map_error ~f:(fun _ -> Owner.Error.Invalid_request)
  in
  let%bind target = target in
  let%bind inputs = Owner.Current.document_inputs current ~checkpoint in
  let source = Ledger.Document.Inputs.source inputs in
  match capture_document t current ~actor ~source ~checkpoint with
  | Error (Sources.Error.Document_parser (Invalid_document | Invalid_source)) ->
    unavailable current target Incomplete_or_malformed_document
  | Error (Sources.Error.Document_parser Source_unavailable) ->
    unavailable current target Incomplete_or_malformed_document
  | Error error -> Error (Host.source_host_error error)
  | Ok capture ->
    let parsed =
      (Markdown.Generated.parsed (Parsed_capture.parsed capture)).parsed_root
    in
    let make_retained metadata =
      Owner.Retained_document_capture.prepare
        ~context:current.context
        ~inputs
        ~capture
        ~metadata
        ~max_sources:t.limits.max_document_bundle_files
        ~max_bytes:t.limits.max_document_bundle_bytes
        ~checkpoint
    in
    (match find_declaration parsed ~script_id ~checkpoint with
     | `Limit -> Error Owner.Error.Resource_limit
     | (`Missing | `Ambiguous) as missing ->
       let%bind retained = make_retained None in
       Document.prepare_parsed_only
         t.owner
         current
         ~inputs
         ~retained
         ~target
         ~reason:
           (match missing with
            | `Missing -> No_selected_script
            | `Ambiguous -> Ambiguous_script_id
            | `Limit | `Selected _ -> assert false)
         ~checkpoint
     | `Selected declaration ->
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
       let%bind raw_selected, selected =
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
       let%bind retained =
         make_retained (Option.map metadata ~f:(fun project -> declaration, project))
       in
       set_final_check (fun () ->
         let metadata_current =
           match metadata, t.project_metadata with
           | None, _ -> true
           | Some project, Some policy ->
             Metadata.Policy.authorize_now
               policy
               ~original_actor:current.context.original_actor
               ~actor
               ~current:inputs
               ~parsed
               ~declaration
               ~capture:project
           | Some _, None -> false
         in
         let selected_current =
           match raw_selected, t.script_selection with
           | None, None -> true
           | None, Some policy ->
             Option.is_none
               (Selection.Policy.select
                  policy
                  ~original_actor:current.context.original_actor
                  ~actor
                  ~current:inputs
                  ~parsed
                  ~declaration
                  ~metadata)
           | Some selected, Some policy ->
             Selection.Policy.same_now
               policy
               ~original_actor:current.context.original_actor
               ~actor
               ~current:inputs
               ~parsed
               ~declaration
               ~metadata
               ~selected
           | Some _, None -> false
         in
         if metadata_current && selected_current
         then Ok ()
         else Error Owner.Error.Selection_unavailable);
       (match selected with
        | None ->
          Document.prepare_parsed_only
            t.owner
            current
            ~inputs
            ~retained
            ~target
            ~reason:No_validated_project
            ~checkpoint
        | Some selected ->
          let selection =
            Chatmd_authoring_analysis.Selection.Validated_build
              (Selection.Selected.build selected)
          in
          let%bind admission =
            Chatmd_authoring_analysis.Script.assess
              inputs
              ~parsed:(Some parsed)
              ~script_id:(Some script_id)
              ~selection
              ~max_membership_work_steps:t.limits.max_parser_work_steps
              ~checkpoint
            |> Result.map_error ~f:(function
              | Chatmd_authoring_analysis.Error.Membership_limit ->
                Owner.Error.Resource_limit
              | _ -> Owner.Error.Selection_unavailable)
          in
          (match admission with
           | Unavailable reason ->
             Document.prepare_parsed_only
               t.owner
               current
               ~inputs
               ~retained
               ~target
               ~reason:(unavailable_reason reason)
               ~checkpoint
           | Ready script ->
             Document.prepare_checked
               t.owner
               current
               ~inputs
               ~retained
               ~script
               ~env:t.env
               ~contract:(Selection.Selected.contract selected)
               ~checkpoint)))
;;

let analyze t ~actor ~connection request =
  let expected = Analysis.Analyze.Request.context request in
  let borrowed_index = ref None in
  let final_check = ref (fun () -> Ok ()) in
  let remaining = ref t.limits.max_work_steps in
  let result =
    try
      Host.with_current
        ~feature:Owner.Selected_feature.Semantic
        ~final_check:(fun () -> !final_check ())
        ~publish:(fun prepared ->
          match !borrowed_index with
          | None -> Error Owner.Error.Stale_context
          | Some borrowed_index ->
            Publication.publish_now t.owner ~borrowed_index prepared
            |> Result.map ~f:(fun _ -> prepared))
        t.owner
        ~actor
        ~connection
        ~expected
        ~checkpoint:Eio.Fiber.check
        ~f:(fun current ~checkpoint ->
          let charge () =
            if !remaining <= 0 then raise Work_limit;
            decr remaining;
            checkpoint ()
          in
          let open Result.Let_syntax in
          let%bind index = Owner.Current.analysis_index current in
          borrowed_index := Some index;
          match Analysis.Analyze.Request.target request, Owner.Current.basis current with
          | Analysis.Target.Project, Owner.Current.Project ->
            Project.prepare
              t.owner
              current
              ~env:t.env
              ~build_limits:t.project_build_limits
              ~checkpoint:charge
          | Selected_script script_id, Document ->
            analyze_document
              t
              current
              ~actor
              ~script_id
              ~checkpoint:charge
              ~set_final_check:(fun check -> final_check := check)
          | Project, Document | Selected_script _, Project ->
            Error Owner.Error.Invalid_request)
    with
    | Work_limit -> Error Owner.Error.Resource_limit
  in
  Result.map result ~f:Publication.Prepared.response
;;
