open! Core
module P = Agent_protocol
module Owner = Authoring_context_service_core
module Tool_registry = Chat_response.Tool_capability
module Error = Owner.Error
module Highlight = Language_highlighting.Highlight_snapshot
module Chatmd = Chatmd_authoring_assistance
module Chatml = Chatml_authoring_fallback
module Mapped = Chatmd_authoring_fallback

module Limits = struct
  type t =
    { protocol_limits : P.Authoring.Limits.t
    ; max_scan_work_steps : int
    ; max_assistance_work_steps : int
    ; max_contract_identity_bytes : int
    ; max_contract_work_steps : int
    ; max_projection_work_steps : int
    ; max_parser_input_bytes : int
    ; max_imported_inputs : int
    ; max_selected_tools : int
    ; max_tool_metadata_bytes : int
    ; max_capture_admission_bytes : int
    ; lexical : Highlight.Limits.t
    ; chatmd : Chatmd.Limits.t
    ; chatml : Chatml.Limits.t
    ; mapped : Mapped.Limits.t
    }

  let create
        ~(protocol_limits : P.Authoring.Limits.t)
        ~max_scan_work_steps
        ~max_assistance_work_steps
        ~max_contract_identity_bytes
        ~max_contract_work_steps
        ~max_projection_work_steps
        ~max_parser_input_bytes
        ~max_imported_inputs
        ~max_selected_tools
        ~max_tool_metadata_bytes
    =
    let positive value = value > 0 && value <= P.Authoring.Limits.maximum_wire_integer in
    if
      (not
         (List.for_all
            [ max_scan_work_steps
            ; max_assistance_work_steps
            ; max_contract_identity_bytes
            ; max_contract_work_steps
            ; max_projection_work_steps
            ; max_parser_input_bytes
            ; max_imported_inputs
            ; max_selected_tools
            ; max_tool_metadata_bytes
            ]
            ~f:positive))
      || max_parser_input_bytes > protocol_limits.max_context_source_bytes
      || max_imported_inputs > protocol_limits.max_sources
      || max_tool_metadata_bytes > protocol_limits.max_context_source_bytes
      || max_selected_tools > P.Authoring_assistance.Limits.max_suggestions
      || max_contract_identity_bytes > 8 * 1024 * 1024
    then Or_error.error_string "invalid authoring assistance limits"
    else
      let open Or_error.Let_syntax in
      let total_input_cap =
        Int.min
          P.Authoring.Limits.maximum_wire_integer
          (if
             protocol_limits.max_context_source_bytes
             > P.Authoring.Limits.maximum_wire_integer / 4
           then P.Authoring.Limits.maximum_wire_integer
           else protocol_limits.max_context_source_bytes * 4)
      in
      let maximum = P.Authoring.Limits.maximum_wire_integer in
      let bounded_product a b = if a > maximum / b then maximum else a * b in
      let root_and_pair_bytes =
        bounded_product protocol_limits.max_context_source_bytes 5
      in
      let association_metadata = bounded_product max_imported_inputs 4096 in
      let max_capture_admission_bytes =
        if association_metadata > maximum - root_and_pair_bytes
        then maximum
        else root_and_pair_bytes + association_metadata
      in
      let identity_cap = Int.min 4096 protocol_limits.max_request_bytes in
      let output_cap =
        Int.min
          protocol_limits.max_result_bytes
          P.Authoring_assistance.Limits.max_result_bytes
      in
      let%bind lexical =
        Highlight.Limits.create
          ~max_source_bytes:protocol_limits.max_source_bytes
          ~max_identity_bytes:identity_cap
          ~max_spans:P.Authoring_assistance.Limits.max_spans
          ~max_issues:P.Authoring_assistance.Limits.max_issues
          ~max_work_units:max_scan_work_steps
          ~max_nesting:256
      in
      let%bind chatmd =
        Chatmd.Limits.create
          ~max_work_steps:max_assistance_work_steps
          ~max_authored_bytes:protocol_limits.max_source_bytes
          ~max_total_captured_bytes:total_input_cap
          ~max_parsed_inputs:max_imported_inputs
          ~max_paths:protocol_limits.max_path_candidates
          ~max_selected_tools
          ~max_candidates:P.Authoring_assistance.Limits.max_suggestions
          ~max_label_bytes:P.Authoring_assistance.Limits.max_label_bytes
      in
      let%bind chatml =
        Chatml.Limits.create
          ~lexical
          ~max_contract_identity_bytes
          ~max_contract_work_steps
          ~max_projection_work_steps
          ~max_output_bytes:output_cap
      in
      let%map mapped =
        Mapped.Limits.create
          ~max_source_bytes:max_parser_input_bytes
          ~max_identity_bytes:identity_cap
          ~max_work_steps:max_projection_work_steps
      in
      { protocol_limits
      ; max_scan_work_steps
      ; max_assistance_work_steps
      ; max_contract_identity_bytes
      ; max_contract_work_steps
      ; max_projection_work_steps
      ; max_parser_input_bytes
      ; max_imported_inputs
      ; max_selected_tools
      ; max_tool_metadata_bytes
      ; max_capture_admission_bytes
      ; lexical
      ; chatmd
      ; chatml
      ; mapped
      }
  ;;
end

module Selected_tool_policy = struct
  module Registry = struct
    type t =
      { selected : Tool_registry.t
      ; references : Tool_registry.reference list
      ; selected_tools : Chatmd.Selected_tool.t list
      ; count : int
      ; metadata_bytes : int
      ; fingerprint : string
      }

    let selected t = t.selected
    let references t = t.references
    let selected_tools t = t.selected_tools
    let fingerprint t = t.fingerprint

    let equal_identity a b =
      Int.equal a.count b.count && String.equal a.fingerprint b.fingerprint
    ;;

    let within_limits t limits =
      t.count <= limits.Limits.max_selected_tools
      && t.metadata_bytes <= limits.max_tool_metadata_bytes
    ;;

    let admit selected ~limits ~checkpoint =
      let count = Tool_registry.length selected in
      if count > limits.Limits.max_selected_tools
      then Error Owner.Error.Resource_limit
      else (
        let references = Tool_registry.references selected in
        let remaining = ref limits.max_tool_metadata_bytes in
        let charge n =
          if n < 0 || n > !remaining
          then Error Owner.Error.Resource_limit
          else (
            remaining := !remaining - n;
            Ok ())
        in
        let rec admit_references = function
          | [] -> Ok ()
          | (reference : Tool_registry.reference) :: tail ->
            checkpoint ();
            let open Result.Let_syntax in
            let%bind () = charge 48 in
            let%bind () = charge (String.length reference.name) in
            let%bind () = charge (String.length reference.owner) in
            let%bind () = charge (String.length reference.implementation_revision) in
            let%bind () = charge (String.length reference.fingerprint) in
            let%bind () =
              let id = P.Id.Capability.to_string reference.id in
              charge (String.length id)
            in
            admit_references tail
        in
        match admit_references references with
        | Error error -> Error error
        | Ok () ->
          (* The existing fingerprint encodes only the admitted reference
             digests. Count and byte admission above bound that allocation. *)
          checkpoint ();
          let fingerprint = Tool_registry.fingerprint selected in
          let open Result.Let_syntax in
          let%bind selection_fingerprint =
            Chatml_source.Digest.of_hex fingerprint
            |> Result.map_error ~f:(fun _ -> Owner.Error.Invalid_request)
          in
          let%map selected_tools =
            Result.all
              (List.map references ~f:(fun (reference : Tool_registry.reference) ->
                 checkpoint ();
                 let%bind binding_fingerprint =
                   Chatml_source.Digest.of_hex reference.fingerprint
                   |> Result.map_error ~f:(fun _ -> Owner.Error.Invalid_request)
                 in
                 Chatmd.Selected_tool.create
                   ~id:reference.id
                   ~name:reference.name
                   ~binding_fingerprint
                   ~selection_fingerprint
                 |> Result.map_error ~f:(fun _ -> Owner.Error.Invalid_request)))
          in
          { selected
          ; references
          ; selected_tools
          ; count
          ; metadata_bytes = limits.max_tool_metadata_bytes - !remaining
          ; fingerprint
          })
    ;;
  end

  type t =
    { current :
        original_actor:Operator_authorization.t
        -> actor:Operator_authorization.t
        -> context:P.Authoring.Context.t
        -> Registry.t option
    }

  let create ~current = { current }

  let current t ~original_actor ~actor ~context =
    t.current ~original_actor ~actor ~context
  ;;
end
