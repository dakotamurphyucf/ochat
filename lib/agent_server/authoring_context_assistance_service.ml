open! Core
module P = Agent_protocol
module W = P.Authoring.Wire
module Highlight = Language_highlighting.Highlight_snapshot
module Chatmd = Chatmd_authoring_assistance
module Mapped = Chatmd_authoring_fallback
module Tool_registry = Chat_response.Tool_capability
module Lexical = P.Authoring_assistance.Lexical
module Owner = Authoring_context_service_core
module Current = Owner.Current
module Source = Authoring_assistance_source
module Capture = Authoring_assistance_capture
module Scan = Authoring_assistance_lexical
module Local = Authoring_assistance_local
module Document = Authoring_assistance_document
module Overlay = Authoring_project_overlay
module Chatml = Chatml_authoring_fallback
module Assistance = P.Authoring_assistance.Assistance
module Limits = Authoring_assistance_admission.Limits
module Selected_tool_policy = Authoring_assistance_admission.Selected_tool_policy
module Checked_template = Authoring_checked_template_suggestion

type t =
  { owner : Owner.t
  ; script_selection : Authoring_script_selection.Policy.t option
  ; project_metadata : Authoring_project_metadata.Policy.t option
  ; selected_tools : Selected_tool_policy.t option
  ; project_build_limits : Chatml_project_build.Limits.t
  ; limits : Limits.t
  }

let create
      ~(owner : Owner.t)
      ~script_selection
      ~project_metadata
      ~selected_tools
      ~project_build_limits
      ~limits
  =
  let selected = limits.Limits.protocol_limits in
  let current = owner.protocol_limits in
  if
    Int.equal selected.max_sources current.max_sources
    && Int.equal selected.max_source_bytes current.max_source_bytes
    && Int.equal selected.max_context_source_bytes current.max_context_source_bytes
    && Int.equal selected.max_request_bytes current.max_request_bytes
    && Int.equal selected.max_result_bytes current.max_result_bytes
    && Int.equal selected.max_path_candidates current.max_path_candidates
  then
    Ok
      { owner
      ; script_selection
      ; project_metadata
      ; selected_tools
      ; project_build_limits
      ; limits
      }
  else Error Owner.Error.Invalid_request
;;

let request_bytes t request =
  let bytes = String.length (Jsonaf.to_string (W.Request.params request)) in
  if bytes > t.owner.protocol_limits.max_request_bytes
  then Error Owner.Error.Resource_limit
  else Ok ()
;;

let lexical_document t current request ~checkpoint =
  let open Result.Let_syntax in
  let%bind inputs = Current.document_inputs current ~checkpoint in
  let current_source = Chatml_authoring_context.Document.Inputs.source inputs in
  let requested = Lexical.Request.source request in
  let root_requested =
    Chatml_source.Id.equal
      (Chatml_source.id current_source)
      (P.Authoring.Source.id requested)
  in
  if root_requested
  then (
    let%bind source, _ = Source.admit current ~source:requested ~checkpoint in
    match Lexical.Request.view request with
    | Chatml -> Error Owner.Error.Invalid_request
    | Chatmd_authored ->
      Scan.scan_chatmd
        ~request
        ~source
        ~input:None
        ~max_parser_input_bytes:t.limits.max_parser_input_bytes
        ~limits:t.limits.lexical
        ~checkpoint
    | Chatmd_effective ->
      let%bind capture = Current.document_capture current ~checkpoint in
      let%bind capture =
        capture |> Result.of_option ~error:Owner.Error.Source_unavailable
      in
      let%bind admitted =
        Capture.admit
          ~current:inputs
          ~capture
          ~requested
          ~max_inputs:t.limits.max_imported_inputs
          ~max_source_bytes:t.limits.max_parser_input_bytes
          ~max_total_bytes:t.limits.max_capture_admission_bytes
          ~max_work_steps:t.limits.max_projection_work_steps
          ~checkpoint
      in
      let%bind input =
        Capture.unique_view_input admitted
        |> Result.of_option ~error:Owner.Error.Source_unavailable
      in
      Scan.scan_chatmd
        ~request
        ~source
        ~input:(Some input)
        ~max_parser_input_bytes:t.limits.max_parser_input_bytes
        ~limits:t.limits.lexical
        ~checkpoint)
  else (
    let%bind capture = Current.document_capture current ~checkpoint in
    let%bind capture =
      capture |> Result.of_option ~error:Owner.Error.Source_unavailable
    in
    let%bind admitted =
      Capture.admit
        ~current:inputs
        ~capture
        ~requested
        ~max_inputs:t.limits.max_imported_inputs
        ~max_source_bytes:t.limits.max_parser_input_bytes
        ~max_total_bytes:t.limits.max_capture_admission_bytes
        ~max_work_steps:t.limits.max_projection_work_steps
        ~checkpoint
    in
    let%bind input =
      match Lexical.Request.view request with
      | Chatml -> Error Owner.Error.Invalid_request
      | Chatmd_authored -> Ok None
      | Chatmd_effective ->
        Capture.unique_view_input admitted
        |> Result.of_option ~error:Owner.Error.Source_unavailable
        |> Result.map ~f:Option.some
    in
    Scan.scan_chatmd
      ~request
      ~source:(Capture.requested_source admitted)
      ~input
      ~max_parser_input_bytes:t.limits.max_parser_input_bytes
      ~limits:t.limits.lexical
      ~checkpoint)
;;

let handle_lexical t ~actor ~connection request =
  let open Result.Let_syntax in
  let%bind () = Owner.check_domain t.owner in
  let%bind () = request_bytes t (W.Request.Lexical request) in
  let checkpoint () = Eio.Fiber.yield () in
  Authoring_context_owner.with_current
    ~feature:Owner.Selected_feature.Lexical
    t.owner
    ~actor
    ~connection
    ~expected:(Lexical.Request.context request)
    ~checkpoint
    ~f:(fun current ~checkpoint ->
      let%bind result =
        match Current.basis current with
        | Project ->
          let%bind source, _ =
            Source.admit current ~source:(Lexical.Request.source request) ~checkpoint
          in
          Scan.scan_chatml ~request ~source ~limits:t.limits.lexical ~checkpoint
        | Document -> lexical_document t current request ~checkpoint
      in
      Authoring_context_owner.checked_response t.owner (W.Response.Lexical result))
;;

let catalog_unavailable request =
  Assistance.Result.create
    ~request
    ~items:[]
    ~status:(Assistance.Status.Unavailable Catalog_unavailable)
  |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
;;

let selected_contract_error = function
  | Chatml.Error.Contract_identity_limit
  | Contract_work_limit
  | Projection_work_limit
  | Output_byte_limit -> Error Owner.Error.Resource_limit
  | Stale_request_source
  | Stale_selected_build
  | Contract_mismatch
  | Invalid_projection
  | Lexical_admission _
  | Wire _ -> Ok `Unavailable
;;

let project_assistance t current request ~checkpoint =
  let open Result.Let_syntax in
  let%bind source, basis =
    Source.admit current ~source:(Assistance.Request.source request) ~checkpoint
  in
  let%bind inputs =
    match basis with
    | Source.Project inputs -> Ok inputs
    | Document _ -> Error Owner.Error.Stale_context
  in
  let%bind selected, receipt =
    match Overlay.prepare current ~limits:t.project_build_limits ~checkpoint with
    | Ok prepared ->
      (match
         Chatml.Selected_contract.admit
           (Overlay.Prepared.build prepared)
           ~selected_contract:(Overlay.Prepared.contract prepared)
           ~limits:t.limits.chatml
           ~checkpoint
       with
       | Ok selected -> Ok (`Selected selected, Some (Overlay.Prepared.overlay prepared))
       | Error error ->
         selected_contract_error error |> Result.map ~f:(fun result -> result, None))
    | Error (Overlay.Error.Context error) -> Error error
    | Error No_selected_contract -> Ok (`Local, None)
    | Error (Work_limit | Lock_identity_unavailable) -> Error Owner.Error.Resource_limit
    | Error (Invalid_overlay _) -> Ok (`Local, None)
  in
  match selected with
  | `Unavailable -> catalog_unavailable request
  | `Local ->
    Local.produce_chatml ~request ~authored:source ~limits:t.limits.chatml ~checkpoint
  | `Selected selected ->
    (match
       Chatml.produce
         (Chatml.Origin.chatml source)
         ~request
         ~selected:(Some selected)
         ~limits:t.limits.chatml
         ~checkpoint
     with
     | Ok result ->
       let%bind checked =
         match receipt with
         | None -> Ok None
         | Some receipt ->
           Checked_template.project
             current
             ~inputs
             ~receipt
             ~authored:source
             ~request
             ~checkpoint
       in
       Checked_template.include_checked result checked
     | Error (Chatml.Error.Stale_request_source | Stale_selected_build | Contract_mismatch)
       -> Error Owner.Error.Stale_source
     | Error
         ( Invalid_projection
         | Contract_identity_limit
         | Contract_work_limit
         | Projection_work_limit
         | Output_byte_limit
         | Lexical_admission _
         | Wire _ ) -> Error Owner.Error.Resource_limit)
;;

let handle_assistance t ~actor ~connection request =
  let open Result.Let_syntax in
  let%bind () = Owner.check_domain t.owner in
  let%bind () = request_bytes t (W.Request.Assistance request) in
  let prepared_document = ref None in
  let checkpoint () = Eio.Fiber.yield () in
  Authoring_context_owner.with_current
    ~feature:Owner.Selected_feature.Assistance
    ~final_check:(fun () ->
      Option.value_map !prepared_document ~default:(Ok ()) ~f:Document.check_now)
    t.owner
    ~actor
    ~connection
    ~expected:(Assistance.Request.context request)
    ~checkpoint
    ~f:(fun current ~checkpoint ->
      let%bind result, document =
        match Current.basis current with
        | Project ->
          let%map result = project_assistance t current request ~checkpoint in
          result, None
        | Document ->
          let%map prepared =
            Document.prepare
              current
              ~actor
              ~request
              ~select_contract:t.owner.selection_policy.project_contract
              ~script_selection:t.script_selection
              ~project_metadata:t.project_metadata
              ~selected_tools:t.selected_tools
              ~project_build_limits:t.project_build_limits
              ~limits:t.limits
              ~checkpoint
          in
          Document.result prepared, Some prepared
      in
      let%map response =
        Authoring_context_owner.checked_response t.owner (W.Response.Assistance result)
      in
      prepared_document := document;
      response)
;;
