open! Core
module Wire = Agent_protocol.Authoring_assistance.Assistance
module Chatmd = Chatmd_authoring_assistance
module Chatml = Chatml_authoring_fallback
module Error = Authoring_context_service_core.Error

let classify = function
  | Chatmd.Error.Source_mismatch
  | Parsed_input_mismatch
  | Missing_import_capture
  | Foreign_import_capture -> Error.Stale_source
  | Invalid_limits
  | Selected_without_project
  | Selected_project_mismatch
  | Invalid_path_provenance -> Error.Invalid_request
  | Candidate_limit | Work_limit | Invalid_display_text | Wire _ -> Error.Resource_limit
;;

let produce_chatmd ~request ~authored ~limits ~max_parser_input_bytes ~checkpoint =
  let open Result.Let_syntax in
  let bytes = Chatml_source.contents authored in
  let%bind lexical_input =
    Chatmd_parser_input.create
      ~role:Root
      ~path:(Chatml_source.Id.relative_path (Chatml_source.id authored))
      ~authored:bytes
      ~effective:bytes
      ~max_input_bytes:max_parser_input_bytes
      ~checkpoint
    |> Result.map_error ~f:(function
      | Chatmd_parser_input.Error.Input_byte_limit -> Error.Resource_limit
      | Empty_path | Import_changed | Invalid_range -> Error.Invalid_request)
  in
  let%bind facts =
    Chatmd.Facts.create
      ~request
      ~authored
      ~lexical_input
      ~parsed:None
      ~captured_inputs:[]
      ~project:None
      ~selected:None
      ~paths:[]
      ~selected_tools:[]
      ~limits
      ~checkpoint
    |> Result.map_error ~f:classify
  in
  Chatmd.produce facts ~checkpoint |> Result.map_error ~f:classify
;;

let produce_chatml ~request ~authored ~limits ~checkpoint =
  Chatml.produce
    (Chatml.Origin.chatml authored)
    ~request
    ~selected:None
    ~limits
    ~checkpoint
  |> Result.map_error ~f:(function
    | Chatml.Error.Stale_request_source -> Error.Stale_source
    | Stale_selected_build | Contract_mismatch | Invalid_projection ->
      Error.Invalid_request
    | Contract_identity_limit
    | Contract_work_limit
    | Projection_work_limit
    | Output_byte_limit
    | Lexical_admission _
    | Wire _ -> Error.Resource_limit)
;;
