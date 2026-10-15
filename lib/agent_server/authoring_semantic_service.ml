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
module Markdown = Prompt.Chat_markdown
module Selection = Authoring_script_selection
module Metadata = Authoring_project_metadata
module Prefix = Authoring_semantic_prefix
module Provisional_index = Authoring_provisional_index
module Prefix_limits = Prefix.Limits
module Query = Authoring_semantic_query
module Query_limits = Query.Limits
module Checked_template = Authoring_checked_template
module Rows = Authoring_document_selection_rows

module Limits = struct
  type t =
    { max_document_bundle_files : int
    ; max_document_bundle_bytes : int
    ; max_parser_work_steps : int
    ; max_diagnostic_work_steps : int
    ; rows : Rows.Limits.t
    ; max_work_steps : int
    }

  let create
        ~max_document_bundle_files
        ~max_document_bundle_bytes
        ~max_parser_work_steps
        ~max_diagnostic_work_steps
        ~max_selection_rows
        ~max_retained_selection_source_bytes
        ~max_retained_selection_input_bytes
        ~max_selection_work_steps
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
      let open Result.Let_syntax in
      let%map rows =
        Rows.Limits.create
          ~max_rows:max_selection_rows
          ~max_retained_source_bytes:max_retained_selection_source_bytes
          ~max_retained_canonical_input_bytes:max_retained_selection_input_bytes
          ~max_collection_work_steps:max_selection_work_steps
      in
      { max_document_bundle_files
      ; max_document_bundle_bytes
      ; max_parser_work_steps
      ; max_diagnostic_work_steps
      ; rows
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
  ; checked_template_limits : Checked_template.Limits.t
  ; limits : Limits.t
  ; prefix_limits : Prefix.Limits.t
  ; query_limits : Query.Limits.t
  ; row_preparation : Authoring_document_row_preparation.t
  }

let create
      ~owner
      ~env
      ~document_dir
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~(limits : Limits.t)
      ~prefix_limits
      ~query_limits
  =
  let max_recomposition_input_bytes =
    Option.value
      (Chatml_project_build.Limits.max_input_bytes project_build_limits)
      ~default:owner.Owner.protocol_limits.max_context_source_bytes
  in
  let checked_template_limits =
    Checked_template.Limits.create
      ~max_example_bytes:4096
      ~max_recomposition_work_steps:
        (Chatml_project_build.Limits.max_work_steps project_build_limits)
      ~max_recomposition_input_bytes
      ~max_dependency_depth:
        (Chatml_authoring_analysis.Policy.max_dependency_depth owner.analysis_policy)
      ~compile:(Chatml_authoring_analysis.Policy.compilation owner.analysis_policy)
    |> Or_error.ok_exn
  in
  let row_preparation =
    (* [Limits.create] has already validated these exact finite components. *)
    Authoring_document_row_preparation.create
      ~owner
      ~env
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~max_document_bundle_files:limits.max_document_bundle_files
      ~max_document_bundle_bytes:limits.max_document_bundle_bytes
      ~max_parser_work_steps:limits.max_parser_work_steps
    |> Or_error.ok_exn
  in
  { owner
  ; env
  ; document_dir
  ; script_selection
  ; project_metadata
  ; project_build_limits
  ; checked_template_limits
  ; limits
  ; prefix_limits
  ; query_limits
  ; row_preparation
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

let analyze_document
      t
      current
      ~actor
      ~script_id
      ~checked_template
      ~checkpoint
      ~set_final_check
  =
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
    let checks = ref [] in
    let%bind rows =
      Rows.collect
        ~inputs
        ~capture
        ~limits:t.limits.rows
        ~checkpoint
        ~prepare_one:(fun declaration ~parsed ~declaring_source ~checkpoint ->
          let open Result.Let_syntax in
          let%map row, check =
            Authoring_document_row_preparation.prepare
              t.row_preparation
              current
              ~actor
              ~inputs
              ~capture
              ~parsed
              declaration
              ~declaring_source
              ~checkpoint
          in
          checks := check :: !checks;
          row)
      |> Result.map_error ~f:(function
        | Rows.Preparation_error error -> error
        | Rows.Row_error Rows.Error.Foreign_capture
        | Rows.Row_error Rows.Error.Invalid_row -> Owner.Error.Stale_source
        | Rows.Row_error (Rows.Error.Row_limit | Retained_byte_limit | Work_limit) ->
          Owner.Error.Resource_limit)
    in
    let%bind retained =
      Owner.Retained_document_capture.prepare
        ~context:current.context
        ~inputs
        ~capture
        ~rows
        ~max_sources:t.limits.max_document_bundle_files
        ~max_bytes:t.limits.max_document_bundle_bytes
        ~checkpoint
    in
    set_final_check (fun () ->
      List.fold_result !checks ~init:() ~f:(fun () check -> check ()));
    (match find_declaration parsed ~script_id ~checkpoint with
     | `Limit -> Error Owner.Error.Resource_limit
     | (`Missing | `Ambiguous) as missing ->
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
       let%bind row =
         Rows.find rows ~capture ~declaration ~checkpoint
         |> Result.map_error ~f:(fun _ -> Owner.Error.Stale_source)
       in
       (match row with
        | None -> Error Owner.Error.Stale_source
        | Some row ->
          (match Rows.Row.status row with
           | Rows.Row.No_validated_build reason ->
             Document.prepare_parsed_only
               t.owner
               current
               ~inputs
               ~retained
               ~target
               ~reason
               ~checkpoint
           | Independently_selected selected | Host_prepared { selected; _ } ->
             let%bind selected_build =
               Authoring_selected_script_overlay.prepare
                 selected
                 ~declaration
                 ~max_body_bytes:t.limits.max_document_bundle_bytes
                 ~limits:t.project_build_limits
                 ~checkpoint
               |> Result.map_error ~f:Authoring_semantic_error.selected_entry_overlay
             in
             let selection =
               Chatmd_authoring_analysis.Selection.Validated_build selected_build
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
                  ~selected_build
                  ~env:t.env
                  ~contract:(Selection.Selected.contract selected)
                  ~checked_template
                  ~template_limits:t.checked_template_limits
                  ~checkpoint))))
;;

let analyze t ~actor ~connection request =
  let expected = Analysis.Analyze.Request.context request in
  let borrowed_index = ref None in
  let borrowed_identity = ref None in
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
            |> Result.map ~f:(fun _ ->
              (match !borrowed_identity, Publication.Prepared.outcome prepared with
               | Some identity, (Parsed_only | Checked_project | Checked_document) ->
                 t.owner.provisional_index
                 <- Provisional_index.retire t.owner.provisional_index ~identity
               | None, _ | Some _, No_acquisition -> ());
              prepared))
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
          borrowed_identity := Some current.context.identity;
          match Analysis.Analyze.Request.target request, Owner.Current.basis current with
          | Analysis.Target.Project, Owner.Current.Project ->
            Project.prepare
              t.owner
              current
              ~env:t.env
              ~build_limits:t.project_build_limits
              ~checked_template:
                (Analysis.Analyze.Request.prepare_checked_template request)
              ~template_limits:t.checked_template_limits
              ~checkpoint:charge
          | Selected_script script_id, Document ->
            analyze_document
              t
              current
              ~actor
              ~script_id
              ~checked_template:
                (Analysis.Analyze.Request.prepare_checked_template request)
              ~checkpoint:charge
              ~set_final_check:(fun check -> final_check := check)
          | Project, Document | Selected_script _, Project ->
            Error Owner.Error.Invalid_request)
    with
    | Work_limit -> Error Owner.Error.Resource_limit
  in
  Result.map result ~f:Publication.Prepared.response
;;

let prefix t ~actor ~connection request =
  let expected = Analysis.Prefix.Request.context request in
  let final_check = ref (fun () -> Ok ()) in
  let remaining = ref (Prefix.Limits.max_host_work_steps t.prefix_limits) in
  let result =
    try
      Host.with_current
        ~feature:Owner.Selected_feature.Semantic
        ~final_check:(fun () -> !final_check ())
        ~publish:(fun prepared ->
          Option.iter (Prefix.provisional_after prepared) ~f:(fun after ->
            t.owner.provisional_index <- after);
          Ok prepared)
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
          let%bind prepared =
            Prefix.prepare
              t.owner
              current
              ~actor
              ~env:t.env
              ~document_dir:t.document_dir
              ~script_selection:t.script_selection
              ~project_metadata:t.project_metadata
              ~project_build_limits:t.project_build_limits
              ~limits:t.prefix_limits
              request
              ~checkpoint:charge
          in
          let%map () =
            Analysis.Prefix.within_bytes
              (Prefix.response prepared)
              ~max_bytes:
                (Int.min
                   t.owner.limits.max_response_bytes
                   Analysis.Limits.max_result_bytes)
            |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
          in
          (final_check := fun () -> Prefix.final_check_now prepared);
          prepared)
    with
    | Work_limit -> Error Owner.Error.Resource_limit
  in
  Result.map result ~f:Prefix.response
;;

let query t ~actor ~connection request =
  let evidence = Analysis.Query.Request.evidence request in
  let capture =
    match evidence with
    | Certified { capture; _ } | Syntax_only capture -> capture
    | Provisional provisional -> Analysis.Prefix.capture provisional
  in
  let expected = Analysis.Capture.context capture in
  let final_check = ref (fun () -> Ok ()) in
  let result =
    Host.with_current
      ~feature:Owner.Selected_feature.Semantic
      ~final_check:(fun () -> !final_check ())
      t.owner
      ~actor
      ~connection
      ~expected
      ~checkpoint:Eio.Fiber.check
      ~f:(fun current ~checkpoint ->
        let open Result.Let_syntax in
        (* This callback runs outside Core's nonyielding publication gate. *)
        let until_yield = ref 16 in
        let query_checkpoint () =
          checkpoint ();
          decr until_yield;
          if !until_yield <= 0
          then (
            until_yield := 16;
            Eio.Fiber.yield ();
            checkpoint ())
        in
        let%map prepared =
          Query.prepare
            t.owner
            current
            ~actor
            ~script_selection:t.script_selection
            ~project_metadata:t.project_metadata
            ~project_build_limits:t.project_build_limits
            ~max_document_body_bytes:t.limits.max_document_bundle_bytes
            ~limits:t.query_limits
            request
            ~checkpoint:query_checkpoint
        in
        (final_check := fun () -> Query.final_check_now prepared);
        prepared)
  in
  Result.map result ~f:Query.response
;;
