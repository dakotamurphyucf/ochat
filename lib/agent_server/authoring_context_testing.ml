open! Core
module P = Agent_protocol
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Service = Authoring_context_service_core
module Core_owner = Authoring_context_service_core
module Owner = Authoring_context_owner
module Activity = Authoring_context_activity
module Index = Authoring_analysis_index
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis

type error =
  | Stale_context
  | Unselected_feature
  | Other

let error = function
  | Core_owner.Error.Stale_context -> Stale_context
  | Unselected_feature -> Unselected_feature
  | Uninstalled
  | Unauthorized
  | Expired_actor
  | Foreign_connection
  | Stale_negotiation
  | Stale_source
  | Source_unavailable
  | Selection_unavailable
  | Busy
  | Closed
  | Resource_limit
  | Invalid_position _
  | Invalid_request
  | Analysis_unavailable
  | Checked_template_failed
  | Storage_unavailable
  | Upload_failure _ -> Other
;;

let core_error = function
  | Stale_context -> Core_owner.Error.Stale_context
  | Unselected_feature -> Unselected_feature
  | Other -> Invalid_request
;;

type feature =
  | Semantic
  | Lexical
  | Assistance
  | Bundle_upload

type head =
  | Project_cache
  | Document_cache
  | Analysis_index
  | Provisional_index
  | Parsed_capture
  | Checked_template

type probe =
  { owner : Service.t
  ; current : Core_owner.Current.t
  }

type expired_borrow = Core_owner.Current.t

let selected_feature = function
  | Semantic -> Core_owner.Selected_feature.Semantic
  | Lexical -> Lexical
  | Assistance -> Assistance
  | Bundle_upload -> Bundle_upload
;;

let with_publication_probe
      owner
      ~actor
      ~connection
      ~expected
      ~feature
      ~checkpoint
      ~after_borrow
      ~final_check
      ~publish
  =
  Owner.with_current
    ~feature:(selected_feature feature)
    ~final_check:(fun () -> Result.map_error (final_check ()) ~f:core_error)
    ~publish:(fun current ->
      publish ();
      Ok current)
    owner
    ~actor
    ~connection
    ~expected
    ~checkpoint
    ~f:(fun current ~checkpoint:_ ->
      after_borrow { owner; current };
      Ok current)
  |> Result.map_error ~f:error
;;

let replace_head { owner; current } head =
  let open Result.Let_syntax in
  let%bind _ = Core_owner.Current.analysis_heads current |> Result.map_error ~f:error in
  Core_owner.with_gate owner (fun () ->
    let open Result.Let_syntax in
    let%map _ = Core_owner.Current.analysis_heads current in
    let contexts () = Project.Project_cache.contexts owner.project_cache in
    match head with
    | Project_cache ->
      owner.project_cache
      <- Project.Project_cache.transition owner.project_cache ~contexts:(contexts ())
    | Document_cache ->
      owner.document_cache
      <- Document.Document_cache.transition owner.document_cache ~contexts:(contexts ())
    | Analysis_index ->
      owner.analysis_index
      <- Index.create
           ~owner_nonce:(P.Id.Transaction.create ())
           ~max_rows:(Project.Policy.max_results owner.analysis_policy)
         |> Or_error.ok_exn
    | Provisional_index ->
      owner.provisional_index
      <- Authoring_provisional_index.create
           ~owner_nonce:(P.Id.Transaction.create ())
           ~max_rows:owner.limits.max_provisional_results
         |> Or_error.ok_exn
    | Parsed_capture -> owner.document_capture <- None
    | Checked_template ->
      owner.checked_templates
      <- Authoring_checked_template_cache.create
           ~max_rows:(Int.min 512 (Project.Policy.max_results owner.analysis_policy))
           ~max_retained_bytes:(2 * 1024 * 1024)
         |> Or_error.ok_exn)
  |> Result.map_error ~f:error
;;

let expired_borrow_rejects_inputs_index_and_capture current =
  match
    ( Core_owner.Current.document_inputs current ~checkpoint:ignore
    , Core_owner.Current.analysis_index current
    , Core_owner.Current.provisional_index current
    , Core_owner.Current.document_capture current ~checkpoint:ignore
    , Core_owner.Current.frozen_document_bundle current ~checkpoint:ignore )
  with
  | ( Error Core_owner.Error.Stale_context
    , Error Core_owner.Error.Stale_context
    , Error Core_owner.Error.Stale_context
    , Error Core_owner.Error.Stale_context
    , Error Core_owner.Error.Stale_context ) -> true
  | _ -> false
;;

type capture_state =
  { parsed : bool
  ; source_matches : bool
  ; project_metadata : bool
  ; selection_rows : int
  ; complete_rows : int
  }

let capture_state owner ~actor ~connection ~expected ~checkpoint =
  Owner.with_current
    ~feature:Core_owner.Selected_feature.Semantic
    owner
    ~actor
    ~connection
    ~expected
    ~checkpoint
    ~f:(fun current ~checkpoint ->
      let open Result.Let_syntax in
      let%bind capture = Core_owner.Current.document_capture current ~checkpoint in
      let%bind metadata =
        match capture with
        | None -> Ok false
        | Some capture ->
          let parsed =
            (Prompt.Chat_markdown.Generated.parsed
               (Sources.Document_context.Parsed_capture.parsed capture))
              .parsed_root
          in
          List.fold_result
            (Prompt.Chat_markdown.Parsed.declarations parsed)
            ~init:false
            ~f:(fun found declaration ->
              let%map row =
                Core_owner.Current.selection_row current ~declaration ~checkpoint
              in
              found
              || Option.exists row ~f:(fun row ->
                Option.is_some (Core_owner.Selection_rows.Row.metadata row)))
      in
      let%map index = Core_owner.Current.analysis_index current in
      let source_matches =
        match capture with
        | None -> false
        | Some capture ->
          let source = Sources.Document_context.Parsed_capture.source capture in
          Sources.Document_context.Parsed_capture.input_sources capture
          |> List.for_all ~f:(fun (input, captured) ->
            Chatml_source.Id.equal (Chatml_source.id source) (Chatml_source.id captured)
            && Chatmd_parser_input.authored_matches
                 input
                 (Chatml_source.contents captured)
                 ~checkpoint)
      in
      { parsed = Option.is_some capture
      ; source_matches
      ; project_metadata = metadata
      ; selection_rows =
          (match capture, current.retained_document_capture with
           | Some capture, Some retained
             when phys_equal
                    capture
                    (Core_owner.Retained_document_capture.capture retained) ->
             Core_owner.Retained_document_capture.selection_rows retained
             |> Core_owner.Selection_rows.retained_count
           | Some _, _ | None, _ -> 0)
      ; complete_rows = Index.retained_count index
      })
  |> Result.map_error ~f:error
;;

let provisional_count owner ~actor ~connection ~expected ~checkpoint =
  Owner.with_current
    ~feature:Core_owner.Selected_feature.Semantic
    owner
    ~actor
    ~connection
    ~expected
    ~checkpoint
    ~f:(fun current ~checkpoint:_ ->
      Core_owner.Current.provisional_index current
      |> Result.map ~f:Authoring_provisional_index.retained_count)
  |> Result.map_error ~f:error
;;

type index_view =
  { index : Index.t
  ; identity : Ledger.Identity.t
  }

let index_view owner ~actor ~connection ~expected ~checkpoint =
  Owner.with_current
    ~feature:Core_owner.Selected_feature.Semantic
    owner
    ~actor
    ~connection
    ~expected
    ~checkpoint
    ~f:(fun current ~checkpoint ->
      let open Result.Let_syntax in
      let%bind inputs = Core_owner.Current.document_inputs current ~checkpoint in
      let%map index = Core_owner.Current.analysis_index current in
      { index; identity = Ledger.Document.Inputs.identity inputs })
  |> Result.map_error ~f:error
;;

let row_matches view ~capture ~snapshot =
  Option.is_some (Index.current view.index ~identity:view.identity ~capture ~snapshot)
;;

let transition_index_to_current owner view ~actor ~connection ~expected ~checkpoint =
  Owner.with_current
    ~feature:Core_owner.Selected_feature.Semantic
    owner
    ~actor
    ~connection
    ~expected
    ~checkpoint
    ~f:(fun current ~checkpoint:_ ->
      let open Result.Let_syntax in
      let%map project, document, _ = Core_owner.Current.analysis_heads current in
      { view with index = Index.transition view.index ~project ~document })
  |> Result.map_error ~f:error
;;

let context_entry ~identity ~original_actor ~view ~owner =
  Activity.
    { id = P.Authoring.Context.id view
    ; identity
    ; original_actor
    ; origin = P.Authoring.Context.origin view
    ; owner
    ; view
    ; verified_project_contract = None
    }
;;

let preview_project_overlay
      ~context
      ~original_actor
      ~ledger
      ~identity
      ~view
      ~analysis_policy
      ~limits
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind () =
    Sources.Context.check_current context ~actor:original_actor
    |> Result.map_error ~f:(fun source_error ->
      error (Owner.source_host_error source_error))
  in
  let context =
    context_entry ~identity ~original_actor ~view ~owner:(Activity.Host_project context)
  in
  let current =
    Core_owner.Current.
      { owner = ledger
      ; domain = (Stdlib.Domain.self () :> int)
      ; context
      ; expected = P.Authoring.Context.Expected.of_view view
      ; live = ref true
      ; retained_document_capture = None
      ; borrowed_project_cache =
          Project.Project_cache.create ~contexts:ledger ~policy:analysis_policy
      ; borrowed_document_cache = Document.Document_cache.create ~policy:analysis_policy
      ; borrowed_analysis_index =
          Index.create
            ~owner_nonce:(P.Id.Transaction.create ())
            ~max_rows:(Project.Policy.max_results analysis_policy)
          |> Or_error.ok_exn
      ; borrowed_provisional_index =
          Authoring_provisional_index.create
            ~owner_nonce:(P.Id.Transaction.create ())
            ~max_rows:1
          |> Or_error.ok_exn
      ; borrowed_checked_templates =
          Authoring_checked_template_cache.create ~max_rows:1 ~max_retained_bytes:4096
          |> Or_error.ok_exn
      }
  in
  Authoring_project_overlay.prepare current ~limits ~checkpoint
  |> Result.map_error ~f:(function
    | Context owner_error -> error owner_error
    | No_selected_contract | Work_limit | Lock_identity_unavailable | Invalid_overlay _ ->
      Other)
  |> Result.map ~f:(fun prepared ->
    ( Authoring_project_overlay.Prepared.build prepared
    , Authoring_project_overlay.Prepared.contract prepared ))
;;

module Retained_document = struct
  type t =
    { context : Activity.context_entry
    ; retained : Core_owner.Retained_document_capture.t
    }

  let prepare
        ~context
        ~original_actor
        ~ledger
        ~identity
        ~view
        ~capture
        ~max_sources
        ~max_bytes
        ~checkpoint
    =
    let open Result.Let_syntax in
    let%bind () =
      Sources.Document_context.check_current context ~actor:original_actor
      |> Result.map_error ~f:(fun source_error ->
        error (Owner.source_host_error source_error))
    in
    let context =
      context_entry
        ~identity
        ~original_actor
        ~view
        ~owner:(Activity.Host_document context)
    in
    let%bind inputs =
      Ledger.document_inputs ledger ~identity ~checkpoint
      |> Result.map_error ~f:(fun _ -> Stale_context)
    in
    let row_limits =
      Core_owner.Selection_rows.Limits.create
        ~max_rows:1024
        ~max_retained_source_bytes:max_bytes
        ~max_retained_canonical_input_bytes:max_bytes
        ~max_collection_work_steps:1_000_000
      |> Or_error.ok_exn
    in
    let%bind rows =
      Core_owner.Selection_rows.collect
        ~inputs
        ~capture
        ~limits:row_limits
        ~checkpoint
        ~prepare_one:(fun declaration ~parsed ~declaring_source ~checkpoint:_ ->
          Ok
            (Core_owner.Selection_rows.Row.no_validated_build
               ~parsed
               ~declaration
               ~declaring_source
               ~metadata:None
               ~reason:Agent_protocol.Authoring.Analysis.Analyze.No_validated_project))
      |> Result.map_error ~f:(fun _ -> error Core_owner.Error.Resource_limit)
    in
    let%map retained =
      Core_owner.Retained_document_capture.prepare
        ~context
        ~inputs
        ~capture
        ~rows
        ~max_sources
        ~max_bytes
        ~checkpoint
      |> Result.map_error ~f:error
    in
    { context; retained }
  ;;

  let same_head left right =
    Core_owner.Retained_document_capture.same_head
      (Some left.retained)
      (Some right.retained)
  ;;

  let candidate_context t ~identity ~view =
    if
      P.Id.Authoring_context.equal t.context.id (P.Authoring.Context.id view)
      && Ledger.Identity.equal t.context.identity identity
    then t.context
    else { t.context with id = P.Authoring.Context.id view; identity; view }
  ;;

  let inputs ~ledger ~identity ~checkpoint =
    Ledger.document_inputs ledger ~identity ~checkpoint
    |> Result.map_error ~f:(fun _ -> Stale_context)
  ;;

  let matches_inputs t ~ledger ~identity ~view ~checkpoint =
    let open Result.Let_syntax in
    let%map inputs = inputs ~ledger ~identity ~checkpoint in
    Core_owner.Retained_document_capture.matches_current
      t.retained
      ~context:(candidate_context t ~identity ~view)
      ~inputs
  ;;

  let recompose_current t ~ledger ~identity ~view ~checkpoint =
    let open Result.Let_syntax in
    let%bind inputs = inputs ~ledger ~identity ~checkpoint in
    if
      Core_owner.Retained_document_capture.matches_import_owner
        t.retained
        ~context:(candidate_context t ~identity ~view)
        ~inputs
    then
      Core_owner.Retained_document_capture.recompose_bundle t.retained ~inputs ~checkpoint
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:error
    else Ok None
  ;;
end

let classify_compilation_error error =
  match Authoring_semantic_error.compilation error with
  | Core_owner.Error.Resource_limit -> `Resource
  | Selection_unavailable -> `Selection
  | Invalid_request -> `Invalid
  | Analysis_unavailable -> `Internal
  | Uninstalled
  | Unselected_feature
  | Unauthorized
  | Expired_actor
  | Foreign_connection
  | Stale_negotiation
  | Stale_context
  | Stale_source
  | Source_unavailable
  | Busy
  | Closed
  | Invalid_position _
  | Storage_unavailable
  | Checked_template_failed
  | Upload_failure _ -> `Unexpected
;;

let syntax_prefix_for_testing ~max_sites units ~checkpoint =
  Authoring_analysis_wire.Syntax_prefix.collect
    ~max_sites
    ~units
    ~sites:Fn.id
    ~checkpoint
    ~convert:(fun _ site -> Ok site)
  |> function
  | Ok result -> result
  | Error _ -> assert false
;;
