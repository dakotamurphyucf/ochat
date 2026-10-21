open! Core
module P = Agent_protocol
module A = P.Authoring.Analysis
module Q = A.Query
module Owner = Authoring_context_service_core
module Ledger = Chatml_authoring_context
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Semantics = Chatml_semantics
module Query = Chatml_authoring_query
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Wire = Authoring_analysis_wire
module Selection = Authoring_script_selection
module Metadata = Authoring_project_metadata
module Project_capture = Authoring_source_host.Document_context.Project_capture
module Index = Authoring_analysis_index
module Prefix_index = Authoring_provisional_index
module Rows = Authoring_document_selection_rows
module Rebind = Authoring_document_selection_rebind
module Source_host = Authoring_source_host

module Limits = struct
  type t =
    { max_host_work_steps : int
    ; projection : Query.Limits.t
    }

  let create ~max_host_work_steps ~projection =
    if max_host_work_steps <= 0 || max_host_work_steps > 1_000_000
    then Or_error.error_string "semantic query host work limit exceeds finite range"
    else Ok { max_host_work_steps; projection }
  ;;
end

type prepared =
  { response : Q.t
  ; final_check_now : unit -> (unit, Owner.Error.t) result
  }

let response t = t.response
let final_check_now t = t.final_check_now ()

exception Work_limit

let lower_error = function
  | Query.Error.Stale_source | Source_unavailable -> Owner.Error.Stale_source
  | Unsupported_kind | Foreign_symbol -> Owner.Error.Invalid_request
  | Work_limit | Label_byte_limit | Member_edge_limit | Projection_limit ->
    Owner.Error.Resource_limit
  | Query Semantics.Snapshot.Error.Query_work_limit -> Owner.Error.Resource_limit
  | Query
      ( Foreign_point
      | Foreign_symbol
      | Foreign_context
      | Unknown_source
      | Unknown_module
      | Invalid_location
      | Nonzero_point_range )
  | Display Chatml_type_display.Error.Invalid_max_bytes -> Owner.Error.Invalid_request
  | Evidence_mismatch | Invalid_site_projection
  | Display Chatml_type_display.Error.Foreign_root -> Owner.Error.Analysis_unavailable
  | Wire error ->
    (match error.P.Error.code with
     | Resource_limit -> Owner.Error.Resource_limit
     | Invalid_request -> Owner.Error.Invalid_request
     | _ -> Owner.Error.Analysis_unavailable)
;;

let source_of_location inventory location ~checkpoint =
  let rec loop = function
    | [] -> Error Query.Error.Source_unavailable
    | source :: tail ->
      checkpoint ();
      if Captured.Id.equal (Captured.id source) (Captured.Location.source location)
      then
        if
          Captured.Digest.equal
            (Captured.digest source)
            (Captured.Location.source_digest location)
        then Ok source
        else Error Query.Error.Stale_source
      else loop tail
  in
  loop inventory
;;

let sites ~contexts ~identity ~inventory ~script ~input_sources ~prefix_origin ~checkpoint
  =
  fun location ~checkpoint:lower_checkpoint ->
  let open Result.Let_syntax in
  let poll () =
    checkpoint ();
    lower_checkpoint ()
  in
  let%bind native_source = source_of_location inventory location ~checkpoint:poll in
  let%bind authored =
    match script with
    | None -> Ok None
    | Some script ->
      let%bind full_location =
        match prefix_origin with
        | None -> Ok (Some location)
        | Some origin ->
          Query.Prefix_origin.full_location origin location ~checkpoint:poll
      in
      (match full_location with
       | None -> Ok None
       | Some location ->
         Document.Projection.of_location script location ~checkpoint:poll
         |> Result.map_error ~f:(fun _ -> Query.Error.Invalid_site_projection))
  in
  Wire.site_of_location
    ~contexts
    ~identity
    ~native_source
    location
    ~authored
    ~input_sources
    ~checkpoint:poll
  |> Result.map_error ~f:(function
    | Wire.Error.Wire_limit | Projection_limit -> Query.Error.Projection_limit
    | Invalid_stamp | Invalid_site -> Query.Error.Invalid_site_projection)
;;

let current_source ~contexts ~identity source inventory ~checkpoint =
  let open Result.Let_syntax in
  let rec find = function
    | [] -> Error Owner.Error.Stale_source
    | captured :: tail ->
      checkpoint ();
      let%bind wire =
        Wire.source ~contexts ~identity captured
        |> Result.map_error ~f:Authoring_semantic_error.wire
      in
      if P.Authoring.Source.equal wire source then Ok captured else find tail
  in
  find inventory
;;

let document_selection
      (owner : Owner.t)
      current
      ~actor
      ~script
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~max_document_body_bytes
      ~max_membership_work_steps
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind inputs = Owner.Current.document_inputs current ~checkpoint in
  let parsed = Document.Script.parsed script in
  let declaration = Document.Script.declaration script in
  let original_actor = current.context.original_actor in
  let%bind row =
    Owner.Current.selection_row current ~declaration ~checkpoint
    |> Result.bind ~f:(Result.of_option ~error:Owner.Error.Selection_unavailable)
  in
  let metadata_head = Rows.Row.metadata row in
  let%bind current_capture = Owner.Current.document_capture current ~checkpoint in
  let%bind metadata =
    match project_metadata, metadata_head with
    | Some policy, Some capture ->
      if
        not
          (Option.exists current_capture ~f:(fun current_capture ->
             Project_capture.matches capture ~parsed:current_capture ~declaration))
      then Error Owner.Error.Selection_unavailable
      else if
        Metadata.Policy.authorize_now
          policy
          ~original_actor
          ~actor
          ~current:inputs
          ~parsed
          ~declaration
          ~capture
      then Ok (Some capture)
      else Error Owner.Error.Selection_unavailable
    | None, Some _ -> Error Owner.Error.Selection_unavailable
    | None, None | Some _, None -> Ok None
  in
  let%bind selected, source =
    match Rows.Row.status row with
    | No_validated_build _ -> Error Owner.Error.Selection_unavailable
    | Independently_selected selected -> Ok (selected, None)
    | Host_prepared { selected; source } -> Ok (selected, Some source)
  in
  let%bind coherent =
    match metadata with
    | None -> Ok true
    | Some metadata ->
      Project_capture.matches_project_contents
        metadata
        ~build:(Selection.Selected.build selected)
        ~max_compare_bytes:max_document_body_bytes
        ~checkpoint
      |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
  in
  if not coherent
  then Error Owner.Error.Selection_unavailable
  else (
    let check_selection_now () =
      match source, script_selection, current.context.owner with
      | Some source, _, Owner.Host_document document ->
        Source_host.Document_context.check_selected_now
          document
          ~script:source
          ~actor
          ~select_contract:owner.selection_policy.project_contract
        |> Result.map_error ~f:Authoring_context_owner.source_host_error
      | None, Some policy, _ ->
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
      | Some _, _, _ | None, None, _ -> Error Owner.Error.Selection_unavailable
    in
    let%bind () = check_selection_now () in
    let%bind build =
      Authoring_selected_script_overlay.prepare
        selected
        ~declaration
        ~max_body_bytes:max_document_body_bytes
        ~limits:project_build_limits
        ~checkpoint
      |> Result.map_error ~f:Authoring_semantic_error.selected_entry_overlay
    in
    let%bind rebound =
      Document.Script.assess
        inputs
        ~parsed:(Some parsed)
        ~script_id:
          (Some
             (Document.Document_stamp.script_id (Document.Script.document_stamp script)))
        ~selection:(Document.Selection.Validated_build build)
        ~max_membership_work_steps
        ~checkpoint
      |> Result.map_error ~f:(function
        | Document.Error.Membership_limit -> Owner.Error.Resource_limit
        | _ -> Owner.Error.Selection_unavailable)
    in
    match rebound with
    | Document.Script.Unavailable _ -> Error Owner.Error.Selection_unavailable
    | Document.Script.Ready rebound ->
      if
        not
          (Document.Document_stamp.equal
             (Document.Script.document_stamp rebound)
             (Document.Script.document_stamp script))
      then Error Owner.Error.Stale_context
      else (
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
                ~parsed
                ~declaration
                ~capture
            | Some _, None -> false
          in
          let open Result.Let_syntax in
          let%bind () =
            if metadata_current then Ok () else Error Owner.Error.Selection_unavailable
          in
          check_selection_now ()
        in
        Ok (rebound, final_check_now)))
;;

let provisional_selection
      (owner : Owner.t)
      current
      ~actor
      ~script_selection
      ~project_metadata
      ~proof
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind inputs = Owner.Current.document_inputs current ~checkpoint in
  let source = Ledger.Document.Inputs.source inputs in
  let capture = Rebind.updated_capture proof in
  let original_actor = current.context.original_actor in
  let original_parsed = Rebind.original_parsed proof in
  let original_declaration = Rebind.original_declaration proof in
  let metadata = Rebind.original_metadata proof in
  if
    not
      (Captured.Id.equal
         (Captured.id source)
         (Captured.id (Source_host.Document_context.Parsed_capture.source capture)))
  then Error Owner.Error.Stale_source
  else (
    let check_now () =
      let%bind () =
        match metadata, project_metadata with
        | None, _ -> Ok ()
        | Some capture, Some policy ->
          if
            Project_capture.matches
              capture
              ~parsed:(Rebind.original_capture proof)
              ~declaration:original_declaration
            && Metadata.Policy.authorize_now
                 policy
                 ~original_actor
                 ~actor
                 ~current:inputs
                 ~parsed:original_parsed
                 ~declaration:original_declaration
                 ~capture
          then Ok ()
          else Error Owner.Error.Selection_unavailable
        | Some _, None -> Error Owner.Error.Selection_unavailable
      in
      match Rebind.host_source proof, script_selection, current.context.owner with
      | Some source, _, Owner.Host_document document ->
        Source_host.Document_context.check_selected_now
          document
          ~script:source
          ~actor
          ~select_contract:owner.selection_policy.project_contract
        |> Result.map_error ~f:Authoring_context_owner.source_host_error
      | None, Some policy, _ ->
        if
          Selection.Policy.same_now
            policy
            ~original_actor
            ~actor
            ~current:inputs
            ~parsed:original_parsed
            ~declaration:original_declaration
            ~metadata
            ~selected:(Rebind.selected proof)
        then Ok ()
        else Error Owner.Error.Selection_unavailable
      | Some _, _, _ | None, None, _ -> Error Owner.Error.Selection_unavailable
    in
    let%map () = check_now () in
    check_now)
;;

type facts =
  | Semantic of Query.Evidence.t
  | Syntax of Semantics.Partial.t

type resolved =
  { facts : facts
  ; inventory : Captured.t list
  ; script : Document.Script.t option
  ; input_sources : (Chatmd_parser_input.t * Captured.t) list
  ; prefix_origin : Query.Prefix_origin.t option
  ; final_check_now : unit -> (unit, Owner.Error.t) result
  }

let same_capture = Index.same_capture

let resolve_project
      (current : Owner.Current.t)
      ~capture
      ~request
      ~project_cache
      ~analysis_index
      ~provisional_index
      ~checkpoint
  =
  let open Result.Let_syntax in
  let identity = current.context.identity in
  let expected = Owner.Current.expected current in
  let contexts = Project.Project_cache.contexts project_cache in
  let%bind () =
    if A.Context.Expected.equal (A.Capture.context capture) expected
    then Ok ()
    else Error Owner.Error.Stale_context
  in
  let base facts inventory =
    { facts
    ; inventory
    ; script = None
    ; input_sources = []
    ; prefix_origin = None
    ; final_check_now = (fun () -> Ok ())
    }
  in
  match Q.Request.evidence request with
  | Q.Certified { capture = requested; snapshot; validated_graph } ->
    let%bind payload =
      Project.Project_cache.current project_cache ~identity
      |> Result.of_option ~error:Owner.Error.Stale_context
    in
    let stamp = Project.Project_payload.stamp payload in
    let checked = Project.Project_payload.checked payload in
    let%bind actual_capture =
      Wire.project_capture ~expected ~stamp
      |> Result.map_error ~f:Authoring_semantic_error.wire
    in
    if not (same_capture actual_capture requested)
    then Error Owner.Error.Stale_context
    else (
      let%bind row =
        Index.current analysis_index ~identity ~capture:requested ~snapshot
        |> Result.of_option ~error:Owner.Error.Stale_context
      in
      if not (Captured.Digest.equal (Index.validated_graph row) validated_graph)
      then Error Owner.Error.Stale_context
      else (
        match Project.Checked.status checked with
        | Project.Checked.Complete snapshot ->
          Ok
            (base
               (Semantic (Query.Evidence.Complete snapshot))
               (Semantics.Snapshot.sources snapshot))
        | Project.Checked.Failed _ -> Error Owner.Error.Stale_context))
  | Q.Syntax_only requested ->
    let%bind payload =
      Project.Project_cache.current project_cache ~identity
      |> Result.of_option ~error:Owner.Error.Stale_context
    in
    let stamp = Project.Project_payload.stamp payload in
    let checked = Project.Project_payload.checked payload in
    let%bind actual_capture =
      Wire.project_capture ~expected ~stamp
      |> Result.map_error ~f:Authoring_semantic_error.wire
    in
    if not (same_capture actual_capture requested)
    then Error Owner.Error.Stale_context
    else (
      match Project.Checked.status checked with
      | Project.Checked.Failed partial ->
        Ok (base (Syntax partial) (Semantics.Partial.sources partial))
      | Project.Checked.Complete _ -> Error Owner.Error.Stale_context)
  | Q.Provisional requested ->
    let%bind row =
      Prefix_index.current provisional_index ~identity requested
      |> Result.of_option ~error:Owner.Error.Stale_context
    in
    if not (same_capture (Prefix_index.capture row) capture)
    then Error Owner.Error.Stale_context
    else (
      let provisional = Prefix_index.semantics row in
      let full_source = Prefix_index.full_source row in
      let snapshot = Semantics.Provisional.snapshot provisional in
      let%bind _ =
        current_source
          ~contexts
          ~identity
          (A.Prefix.full_source requested)
          [ full_source ]
          ~checkpoint
      in
      Ok
        (base
           (Semantic (Query.Evidence.Provisional { provisional; full_source }))
           (Semantics.Snapshot.sources snapshot)))
;;

let resolve_document
      owner
      (current : Owner.Current.t)
      ~actor
      ~capture
      ~request
      ~project_cache
      ~document_cache
      ~analysis_index
      ~provisional_index
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~max_document_body_bytes
      ~max_membership_work_steps
      ~checkpoint
  =
  let open Result.Let_syntax in
  let identity = current.context.identity in
  let expected = Owner.Current.expected current in
  let contexts = Project.Project_cache.contexts project_cache in
  let%bind inputs = Owner.Current.document_inputs current ~checkpoint in
  let%bind () =
    if A.Context.Expected.equal (A.Capture.context capture) expected
    then Ok ()
    else Error Owner.Error.Stale_context
  in
  let selection script =
    document_selection
      owner
      current
      ~actor
      ~script
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~max_document_body_bytes
      ~max_membership_work_steps
      ~checkpoint
  in
  let checked_candidate () =
    Document.Document_cache.current document_cache ~contexts ~identity
    |> Result.of_option ~error:Owner.Error.Stale_context
  in
  let checked_input_sources script =
    let%bind retained =
      Owner.Current.document_capture current ~checkpoint
      |> Result.bind ~f:(Result.of_option ~error:Owner.Error.Stale_context)
    in
    let parsed =
      (Prompt.Chat_markdown.Generated.parsed
         (Authoring_source_host.Document_context.Parsed_capture.parsed retained))
        .parsed_root
    in
    if not (phys_equal parsed (Document.Script.parsed script))
    then Error Owner.Error.Stale_context
    else Ok (Authoring_source_host.Document_context.Parsed_capture.input_sources retained)
  in
  let base ~script ~input_sources ~final_check_now facts inventory prefix_origin =
    { facts
    ; inventory
    ; script = Some script
    ; input_sources
    ; prefix_origin
    ; final_check_now
    }
  in
  match Q.Request.evidence request with
  | Q.Certified { capture = requested; snapshot; validated_graph } ->
    let%bind candidate = checked_candidate () in
    let script = Document.Candidate.script candidate in
    let%bind rebound, final_check_now = selection script in
    let%bind input_sources = checked_input_sources script in
    let%bind actual_capture =
      Wire.document_capture
        ~contexts
        ~expected
        ~source:(Ledger.Document.Inputs.source inputs)
        ~script:rebound
        ~candidate
      |> Result.map_error ~f:Authoring_semantic_error.wire
    in
    if not (same_capture actual_capture requested)
    then Error Owner.Error.Stale_context
    else (
      let%bind row =
        Index.current analysis_index ~identity ~capture:requested ~snapshot
        |> Result.of_option ~error:Owner.Error.Stale_context
      in
      if not (Captured.Digest.equal (Index.validated_graph row) validated_graph)
      then Error Owner.Error.Stale_context
      else (
        match Project.Checked.status (Document.Candidate.checked candidate) with
        | Project.Checked.Complete snapshot ->
          Ok
            (base
               ~script:rebound
               ~input_sources
               ~final_check_now
               (Semantic (Query.Evidence.Complete snapshot))
               (Semantics.Snapshot.sources snapshot)
               None)
        | Project.Checked.Failed _ -> Error Owner.Error.Stale_context))
  | Q.Syntax_only requested ->
    let%bind candidate = checked_candidate () in
    let script = Document.Candidate.script candidate in
    let%bind rebound, final_check_now = selection script in
    let%bind input_sources = checked_input_sources script in
    let%bind actual_capture =
      Wire.document_capture
        ~contexts
        ~expected
        ~source:(Ledger.Document.Inputs.source inputs)
        ~script:rebound
        ~candidate
      |> Result.map_error ~f:Authoring_semantic_error.wire
    in
    if not (same_capture actual_capture requested)
    then Error Owner.Error.Stale_context
    else (
      match Project.Checked.status (Document.Candidate.checked candidate) with
      | Project.Checked.Failed partial ->
        Ok
          (base
             ~script:rebound
             ~input_sources
             ~final_check_now
             (Syntax partial)
             (Semantics.Partial.sources partial)
             None)
      | Project.Checked.Complete _ -> Error Owner.Error.Stale_context)
  | Q.Provisional requested ->
    let%bind row =
      Prefix_index.current provisional_index ~identity requested
      |> Result.of_option ~error:Owner.Error.Stale_context
    in
    let%bind document =
      Prefix_index.document row |> Result.of_option ~error:Owner.Error.Stale_context
    in
    let script = Prefix_index.Document.script document in
    let input_sources = Prefix_index.Document.input_sources document in
    let%bind final_check_now =
      provisional_selection
        owner
        current
        ~actor
        ~script_selection
        ~project_metadata
        ~proof:(Prefix_index.Document.selection document)
        ~checkpoint
    in
    let rebound = script in
    if
      (not
         (Document.Document_stamp.equal
            (Document.Script.document_stamp rebound)
            (Document.Script.document_stamp script)))
      || not (same_capture (Prefix_index.capture row) capture)
    then Error Owner.Error.Stale_context
    else (
      let provisional = Prefix_index.semantics row in
      let full_source = Prefix_index.full_source row in
      let%bind origin =
        Query.Prefix_origin.create provisional ~full_source ~checkpoint
        |> Result.map_error ~f:lower_error
      in
      let snapshot = Semantics.Provisional.snapshot provisional in
      Ok
        (base
           ~script:rebound
           ~input_sources
           ~final_check_now
           (Semantic (Query.Evidence.Provisional { provisional; full_source }))
           (Semantics.Snapshot.sources snapshot)
           (Some origin)))
;;

let prepare
      owner
      (current : Owner.Current.t)
      ~actor
      ~script_selection
      ~project_metadata
      ~project_build_limits
      ~max_document_body_bytes
      ~limits
      request
      ~checkpoint
  =
  let remaining = ref limits.Limits.max_host_work_steps in
  let charge () =
    if !remaining <= 0 then raise Work_limit;
    decr remaining;
    checkpoint ()
  in
  try
    let open Result.Let_syntax in
    let evidence = Q.Request.evidence request in
    let capture =
      match evidence with
      | Q.Certified { capture; _ } | Q.Syntax_only capture -> capture
      | Q.Provisional provisional -> A.Prefix.capture provisional
    in
    let%bind project_cache, document_cache, analysis_index =
      Owner.Current.analysis_heads current
    in
    let%bind provisional_index = Owner.Current.provisional_index current in
    let contexts = Project.Project_cache.contexts project_cache in
    let identity = current.context.identity in
    let%bind resolved =
      match Owner.Current.basis current, A.Capture.target capture with
      | Owner.Current.Project, A.Target.Project_input ->
        resolve_project
          current
          ~capture
          ~request
          ~project_cache
          ~analysis_index
          ~provisional_index
          ~checkpoint:charge
      | Owner.Current.Document, A.Target.Script_input _ ->
        resolve_document
          owner
          current
          ~actor
          ~capture
          ~request
          ~project_cache
          ~document_cache
          ~analysis_index
          ~provisional_index
          ~script_selection
          ~project_metadata
          ~project_build_limits
          ~max_document_body_bytes
          ~max_membership_work_steps:limits.max_host_work_steps
          ~checkpoint:charge
      | Owner.Current.Project, A.Target.Script_input _
      | Owner.Current.Document, A.Target.Project_input ->
        Error Owner.Error.Invalid_request
    in
    let%bind _ =
      current_source
        ~contexts
        ~identity
        (Q.Request.source request)
        resolved.inventory
        ~checkpoint:charge
    in
    let site_callback =
      sites
        ~contexts
        ~identity
        ~inventory:resolved.inventory
        ~script:resolved.script
        ~input_sources:resolved.input_sources
        ~prefix_origin:resolved.prefix_origin
        ~checkpoint:charge
    in
    let%bind payload =
      (match resolved.facts with
       | Semantic evidence ->
         Query.project
           evidence
           ~request
           ~sites:site_callback
           ~limits:limits.projection
           ~checkpoint:charge
       | Syntax partial ->
         Query.syntax_at
           partial
           ~request
           ~sites:site_callback
           ~limits:limits.projection
           ~checkpoint:charge)
      |> Result.map_error ~f:lower_error
    in
    let%bind response =
      Q.create ~request ~payload
      |> Result.map_error ~f:(fun error ->
        match error.P.Error.code with
        | Resource_limit -> Owner.Error.Resource_limit
        | Invalid_request -> Owner.Error.Invalid_request
        | _ -> Owner.Error.Analysis_unavailable)
    in
    let%map _ =
      Authoring_context_owner.checked_response
        owner
        (P.Authoring.Wire.Response.Query response)
    in
    { response; final_check_now = resolved.final_check_now }
  with
  | Work_limit -> Error Owner.Error.Resource_limit
;;
