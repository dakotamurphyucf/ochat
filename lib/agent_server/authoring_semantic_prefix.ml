open! Core
module P = Agent_protocol
module A = P.Authoring.Analysis
module Owner = Authoring_context_service_core
module Index = Authoring_provisional_index
module Wire = Authoring_analysis_wire
module Ledger = Chatml_authoring_context
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Semantics = Chatml_semantics
module Compiler = Chatml_compilation
module Captured = Chatml_source
module Sources = Authoring_source_host
module Parsed_capture = Sources.Document_context.Parsed_capture
module Project_capture = Sources.Document_context.Project_capture
module Markdown = Prompt.Chat_markdown
module Script = Chatmd_authoring_analysis.Script
module Selection = Authoring_script_selection
module Metadata = Authoring_project_metadata
module Rows = Authoring_document_selection_rows
module Rebind = Authoring_document_selection_rebind

module Limits = struct
  type t =
    { max_document_bundle_files : int
    ; max_document_bundle_bytes : int
    ; max_parser_work_steps : int
    ; max_host_work_steps : int
    }

  let create
        ~max_document_bundle_files
        ~max_document_bundle_bytes
        ~max_parser_work_steps
        ~max_host_work_steps
    =
    let defaults = Chatmd_source_bundle.default_limits in
    if
      max_document_bundle_files <= 0
      || max_document_bundle_files > defaults.max_files
      || max_document_bundle_bytes <= 0
      || max_document_bundle_bytes > defaults.max_bundle_bytes
      || max_parser_work_steps <= 0
      || max_parser_work_steps > 1_000_000
      || max_host_work_steps <= 0
      || max_host_work_steps > 1_000_000
    then
      Or_error.error_string "semantic prefix limits exceed the finite host/parser range"
    else
      Ok
        { max_document_bundle_files
        ; max_document_bundle_bytes
        ; max_parser_work_steps
        ; max_host_work_steps
        }
  ;;

  let bundle t : Chatmd_source_bundle.limits =
    let defaults = Chatmd_source_bundle.default_limits in
    { max_source_bytes = Int.min defaults.max_source_bytes t.max_document_bundle_bytes
    ; max_bundle_bytes = t.max_document_bundle_bytes
    ; max_files = t.max_document_bundle_files
    }
  ;;

  let max_host_work_steps t = t.max_host_work_steps
end

type prepared =
  { response : A.Prefix.t
  ; provisional_after : Index.t option
  ; final_check_now : unit -> (unit, Owner.Error.t) result
  }

let response t = t.response
let provisional_after t = t.provisional_after
let final_check_now t = t.final_check_now ()

let unavailable request reason =
  A.Prefix.unavailable
    ~context:(A.Prefix.Request.context request)
    ~target:(A.Prefix.Request.target request)
    ~source:(A.Prefix.Request.source request)
    ~cursor_byte:(A.Prefix.Request.cursor_byte request)
    ~reason
  |> Result.map_error ~f:(fun _ -> Owner.Error.Invalid_request)
  |> Result.map ~f:(fun response ->
    { response; provisional_after = None; final_check_now = (fun () -> Ok ()) })
;;

let invalidity_source contexts identity prepared source_id =
  match Prepared.find_source prepared source_id with
  | None -> Error Owner.Error.Analysis_unavailable
  | Some source ->
    Wire.source ~contexts ~identity source
    |> Result.map_error ~f:Authoring_semantic_error.wire
;;

let unavailable_from_lower ~contexts ~identity ~prepared = function
  | Semantics.Prefix.No_complete_statement -> Ok A.Prefix.No_complete_statement
  | Unsupported_source_phase -> Ok Unsupported_source_phase
  | Unchecked_source -> Ok Unchecked_source
  | Invalid_rebased_metadata -> Ok Invalid_rebased_metadata
  | Prefix_rejected invalidity ->
    let open Result.Let_syntax in
    let%map phase =
      match invalidity with
      | Semantics.Partial.Invalidity.Parse { source; phase } ->
        let%map source = invalidity_source contexts identity prepared source in
        let phase : A.Analyze.parser_phase =
          match phase with
          | Chatml_configured_lookup.Phase.Implementation -> A.Analyze.Implementation
          | Interface -> Interface
        in
        A.Analyze.Parse { source; phase }
      | Implementation source ->
        let%map source = invalidity_source contexts identity prepared source in
        A.Analyze.Implementation source
      | Companion source ->
        let%map source = invalidity_source contexts identity prepared source in
        A.Analyze.Companion source
      | Host_requirements -> Ok A.Analyze.Host_requirements
      | Dependency -> Ok A.Analyze.Dependency
    in
    A.Prefix.Prefix_rejected phase
;;

let run
      owner
      current
      ~env
      ~request
      ~prepared
      ~contract
      ~capture
      ~full_source
      ~compiled_cursor_byte
      ~authored_request
      ~document
      ~final_check_now
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind index = Owner.Current.provisional_index current in
  let identity = current.Owner.Current.context.identity in
  let contexts =
    Owner.Current.analysis_heads current
    |> Result.map ~f:(fun (project, _, _) ->
      Chatml_authoring_analysis.Project_cache.contexts project)
  in
  let%bind contexts = contexts in
  let%bind full_wire_source =
    Wire.source ~contexts ~identity full_source
    |> Result.map_error ~f:Authoring_semantic_error.wire
  in
  let policy = owner.Owner.analysis_policy in
  let%bind result =
    Compiler.analyze_prepared_prefix_with_contract_and_policy
      prepared
      ~source:(Captured.id full_source)
      ~cursor_byte:compiled_cursor_byte
      ~contract
      ~policy:(Compiler.Bounded (Chatml_authoring_analysis.Policy.compilation policy))
      ~semantic_limits:(Chatml_authoring_analysis.Policy.semantic policy)
      ~max_dependency_depth:(Chatml_authoring_analysis.Policy.max_dependency_depth policy)
      ~env
      ()
    |> Result.map_error ~f:Authoring_semantic_error.compilation
  in
  match result with
  | Semantics.Prefix.Unavailable reason ->
    let%bind reason = unavailable_from_lower ~contexts ~identity ~prepared reason in
    let%map response = unavailable request reason in
    { response with final_check_now }
  | Provisional provisional ->
    let%bind after, wire =
      Index.prepare
        index
        ~identity
        ~capture
        ~request
        ~provisional
        ~full_source
        ~full_wire_source
        ~compiled_cursor_byte
        ~authored_request
        ~document
        ~checkpoint
      |> Result.map_error ~f:(function
        | Index.Error.Retention_limit | Identity_exhausted -> Owner.Error.Resource_limit
        | Foreign_source | Evidence_mismatch -> Owner.Error.Analysis_unavailable)
    in
    let response = A.Prefix.provisional wire in
    Ok { response; provisional_after = Some after; final_check_now }
;;

let project owner current ~env ~build_limits ~request ~checkpoint =
  let open Result.Let_syntax in
  let%bind inputs = Owner.Current.project_inputs current ~checkpoint in
  let%bind overlay =
    Authoring_project_overlay.prepare current ~limits:build_limits ~checkpoint
    |> Result.map_error ~f:(function
      | Authoring_project_overlay.Error.Context error -> error
      | No_selected_contract -> Owner.Error.Selection_unavailable
      | Work_limit | Lock_identity_unavailable -> Owner.Error.Resource_limit
      | Invalid_overlay _ -> Owner.Error.Invalid_request)
  in
  let build = Authoring_project_overlay.Prepared.build overlay in
  let prepared = Build.prepared build in
  let%bind contexts =
    Owner.Current.analysis_heads current
    |> Result.map ~f:(fun (cache, _, _) ->
      Chatml_authoring_analysis.Project_cache.contexts cache)
  in
  let%bind full_source =
    List.fold_result (Prepared.sources prepared) ~init:None ~f:(fun found source ->
      checkpoint ();
      let%map wire =
        Wire.source ~contexts ~identity:(Ledger.Inputs.identity inputs) source
        |> Result.map_error ~f:Authoring_semantic_error.wire
      in
      if P.Authoring.Source.equal wire (A.Prefix.Request.source request)
      then Some source
      else found)
  in
  match full_source with
  | None -> Error Owner.Error.Stale_source
  | Some full_source ->
    let environment = Prepared.environment prepared in
    let%bind capture =
      A.Capture.create
        ~context:(Owner.Current.expected current)
        ~target:A.Target.Project_input
        ~input_fingerprint:(Prepared.fingerprint prepared)
        ~environment:(Prepared.Environment.name environment)
        ~required_contract:(Prepared.Environment.required_contract environment)
        ~selected_contract:(Prepared.compiler_contract prepared)
      |> Result.map_error ~f:(fun _ -> Owner.Error.Analysis_unavailable)
    in
    run
      owner
      current
      ~env
      ~request
      ~prepared
      ~contract:(Authoring_project_overlay.Prepared.contract overlay)
      ~capture
      ~full_source
      ~compiled_cursor_byte:(A.Prefix.Request.cursor_byte request)
      ~authored_request:None
      ~document:None
      ~final_check_now:(fun () -> Ok ())
      ~checkpoint
;;

let find_declaration parsed script_id ~checkpoint =
  let rec loop count found = function
    | [] -> Ok found
    | _ :: _ when count >= 1024 -> Error Owner.Error.Resource_limit
    | declaration :: tail ->
      checkpoint ();
      if String.equal (Markdown.Declaration.script_id declaration) script_id
      then (
        match found with
        | Some _ -> Ok None
        | None -> loop (count + 1) (Some declaration) tail)
      else loop (count + 1) found tail
  in
  loop 0 None (Markdown.Parsed.declarations parsed)
;;

let document
      (owner : Owner.t)
      current
      ~actor
      ~env
      ~document_dir
      ~script_selection
      ~project_metadata
      ~build_limits
      ~limits
      ~request
      ~script_id
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind inputs = Owner.Current.document_inputs current ~checkpoint in
  let source = Ledger.Document.Inputs.source inputs in
  let original_actor = current.context.original_actor in
  let%bind frozen = Owner.Current.frozen_document_bundle current ~checkpoint in
  let%bind current_capture = Owner.Current.document_capture current ~checkpoint in
  match frozen, current.retained_document_capture with
  | None, _ | _, None -> unavailable request A.Prefix.Unchecked_source
  | Some bundle, Some retained ->
    let previous = Owner.Retained_document_capture.capture retained in
    let captured =
      match current_capture with
      | Some captured -> Ok captured
      | None ->
        Parsed_capture.reparse_frozen
          ~previous
          ~source
          ~bundle
          ~limits:(Limits.bundle limits)
          ~dir:document_dir
          ~checkpoint
    in
    (match captured with
     | Error (Sources.Error.Document_parser _) ->
       unavailable request A.Prefix.Unchecked_source
     | Error error -> Error (Authoring_context_owner.source_host_error error)
     | Ok captured ->
       let parsed =
         (Markdown.Generated.parsed (Parsed_capture.parsed captured)).parsed_root
       in
       let%bind declaration = find_declaration parsed script_id ~checkpoint in
       (match declaration with
        | None -> unavailable request A.Prefix.Unchecked_source
        | Some declaration ->
          let previous_parsed =
            (Markdown.Generated.parsed (Parsed_capture.parsed previous)).parsed_root
          in
          let%bind old_declaration =
            find_declaration previous_parsed script_id ~checkpoint
            |> Result.bind ~f:(Result.of_option ~error:Owner.Error.Selection_unavailable)
          in
          let%bind old_row =
            Rows.find
              (Owner.Retained_document_capture.selection_rows retained)
              ~capture:previous
              ~declaration:old_declaration
              ~checkpoint
            |> Result.map_error ~f:(fun _ -> Owner.Error.Stale_source)
            |> Result.bind ~f:(Result.of_option ~error:Owner.Error.Selection_unavailable)
          in
          let%bind proof =
            Rebind.prove
              ~old_capture:previous
              ~rows:(Owner.Retained_document_capture.selection_rows retained)
              ~old:old_row
              ~updated:captured
              ~current_source:source
              ~script_id
              ~max_script_bytes:limits.max_document_bundle_bytes
              ~max_locator_bytes:1024
              ~max_path_components:256
              ~checkpoint
            |> Result.map_error ~f:(function
                | Foreign_owner_or_imports
                | Changed_route_or_environment
                | Ambiguous_declaration
                | No_validated_build
                -> Owner.Error.Selection_unavailable)
          in
          let metadata = Rebind.original_metadata proof in
          let%bind () =
            match metadata, project_metadata with
            | None, _ -> Ok ()
            | Some capture, Some policy ->
              if
                Project_capture.matches
                  capture
                  ~parsed:previous
                  ~declaration:(Rebind.original_declaration proof)
                && Metadata.Policy.authorize_now
                     policy
                     ~original_actor
                     ~actor
                     ~current:inputs
                     ~parsed:(Rebind.original_parsed proof)
                     ~declaration:(Rebind.original_declaration proof)
                     ~capture
              then Ok ()
              else Error Owner.Error.Selection_unavailable
            | Some _, None -> Error Owner.Error.Selection_unavailable
          in
          let selected = Some (Rebind.selected proof) in
          let host_source = Rebind.host_source proof in
          let final_check_now () =
            let metadata_current =
              match metadata, project_metadata with
              | None, _ -> true
              | Some capture, Some policy ->
                Metadata.Policy.authorize_now
                  policy
                  ~original_actor
                  ~actor
                  ~current:inputs
                  ~parsed:(Rebind.original_parsed proof)
                  ~declaration:(Rebind.original_declaration proof)
                  ~capture
              | Some _, None -> false
            in
            let open Result.Let_syntax in
            let%bind () =
              if metadata_current then Ok () else Error Owner.Error.Selection_unavailable
            in
            match host_source, selected, script_selection, current.context.owner with
            | Some source, Some _, _, Owner.Host_document document ->
              Sources.Document_context.check_selected_now
                document
                ~script:source
                ~actor
                ~select_contract:owner.selection_policy.project_contract
              |> Result.map_error ~f:Authoring_context_owner.source_host_error
            | None, Some selected, Some policy, _ ->
              if
                Selection.Policy.same_now
                  policy
                  ~original_actor
                  ~actor
                  ~current:inputs
                  ~parsed:(Rebind.original_parsed proof)
                  ~declaration:(Rebind.original_declaration proof)
                  ~metadata
                  ~selected
              then Ok ()
              else Error Owner.Error.Selection_unavailable
            | None, None, _, _ -> Ok ()
            | Some _, _, _, _ | None, Some _, None, _ ->
              Error Owner.Error.Selection_unavailable
          in
          (match selected with
           | None ->
             let%map response = unavailable request A.Prefix.Unchecked_source in
             { response with final_check_now }
           | Some selected ->
             let%bind build =
               Authoring_selected_script_overlay.prepare
                 selected
                 ~declaration
                 ~max_body_bytes:limits.max_document_bundle_bytes
                 ~limits:build_limits
                 ~checkpoint
               |> Result.map_error ~f:Authoring_semantic_error.selected_entry_overlay
             in
             let%bind admission =
               Script.assess
                 inputs
                 ~parsed:(Some parsed)
                 ~script_id:(Some script_id)
                 ~selection:(Validated_build build)
                 ~max_membership_work_steps:limits.max_parser_work_steps
                 ~checkpoint
               |> Result.map_error ~f:(function
                 | Chatmd_authoring_analysis.Error.Membership_limit ->
                   Owner.Error.Resource_limit
                 | _ -> Owner.Error.Selection_unavailable)
             in
             (match admission with
              | Unavailable _ ->
                let%map response = unavailable request A.Prefix.Unchecked_source in
                { response with final_check_now }
              | Ready script ->
                let full_source = Script.selected_source script in
                let input_sources = Parsed_capture.input_sources captured in
                let authored_source = A.Prefix.Request.source request in
                let authored_byte = A.Prefix.Request.cursor_byte request in
                let%bind contexts =
                  Owner.Current.analysis_heads current
                  |> Result.map ~f:(fun (cache, _, _) ->
                    Chatml_authoring_analysis.Project_cache.contexts cache)
                in
                let rec find_point found count = function
                  | [] -> Ok found
                  | _ :: _ when count >= 1024 -> Error Owner.Error.Resource_limit
                  | (input, captured_source) :: tail ->
                    checkpoint ();
                    let%bind wire_source =
                      Wire.source
                        ~contexts
                        ~identity:(Ledger.Document.Inputs.identity inputs)
                        captured_source
                      |> Result.map_error ~f:Authoring_semantic_error.wire
                    in
                    if P.Authoring.Source.equal wire_source authored_source
                    then (
                      match
                        Script.authored_point script ~input ~authored_byte ~checkpoint
                      with
                      | Error _ -> Error Owner.Error.Invalid_request
                      | Ok (Unavailable _) -> find_point found (count + 1) tail
                      | Ok (Exact_compiled_byte point) ->
                        (match found with
                         | None -> find_point (Some point) (count + 1) tail
                         | Some old when Int.equal old point ->
                           find_point found (count + 1) tail
                         | Some _ -> Ok None))
                    else find_point found (count + 1) tail
                in
                let%bind point = find_point None 0 input_sources in
                (match point with
                 | None ->
                   let%map response =
                     unavailable request A.Prefix.Unsupported_source_phase
                   in
                   { response with final_check_now }
                 | Some compiled_cursor_byte ->
                   let%bind authored_request =
                     A.Prefix.Authored_request.create
                       ~source:authored_source
                       ~cursor_byte:authored_byte
                     |> Result.map_error ~f:(fun _ -> Owner.Error.Invalid_request)
                   in
                   let%bind capture =
                     Owner.Current.analysis_heads current
                     |> Result.bind ~f:(fun (cache, _, _) ->
                       Wire.document_script_capture
                         ~contexts:
                           (Chatml_authoring_analysis.Project_cache.contexts cache)
                         ~expected:(Owner.Current.expected current)
                         ~source
                         ~script
                       |> Result.map_error ~f:Authoring_semantic_error.wire)
                   in
                   let%bind document =
                     Index.Document.create
                       ~script
                       ~input_sources
                       ~selection:proof
                       ~checkpoint
                     |> Result.map_error ~f:(fun _ -> Owner.Error.Analysis_unavailable)
                   in
                   run
                     owner
                     current
                     ~env
                     ~request
                     ~prepared:(Script.selected_prepared script)
                     ~contract:(Selection.Selected.contract selected)
                     ~capture
                     ~full_source
                     ~compiled_cursor_byte
                     ~authored_request:(Some authored_request)
                     ~document:(Some document)
                     ~final_check_now
                     ~checkpoint)))))
;;

let prepare
      owner
      current
      ~actor
      ~env
      ~document_dir
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~limits
      request
      ~checkpoint
  =
  let open Result.Let_syntax in
  if
    not
      (P.Authoring.Context.Expected.equal
         (A.Prefix.Request.context request)
         (Owner.Current.expected current))
  then Error Owner.Error.Stale_context
  else (
    match A.Prefix.Request.target request, Owner.Current.basis current with
    | A.Target.Project, Owner.Current.Project ->
      project owner current ~env ~build_limits:project_build_limits ~request ~checkpoint
    | Selected_script script_id, Document ->
      document
        owner
        current
        ~actor
        ~env
        ~document_dir
        ~script_selection
        ~project_metadata
        ~build_limits:project_build_limits
        ~limits
        ~request
        ~script_id
        ~checkpoint
    | Project, Document | Selected_script _, Project -> Error Owner.Error.Invalid_request)
;;
