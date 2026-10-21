open! Core
module Wire = Agent_protocol.Authoring_assistance.Lexical
module Highlight = Language_highlighting.Highlight_snapshot
module Error = Authoring_context_service_core.Error
module Source = Agent_protocol.Authoring.Source

let same_owner snapshot owner =
  let source_owner = Highlight.source_owner snapshot in
  match source_owner, owner with
  | Highlight.Source_owner.Chatml actual, Wire.Owner.Chatml source ->
    Chatml_source.Id.equal actual.source_id (Source.id source)
    && Chatml_source.Digest.equal actual.source_digest (Source.digest source)
    && Int.equal (Highlight.source_byte_length snapshot) (Source.byte_length source)
  | Highlight.Source_owner.Chatmd actual, Wire.Owner.Chatmd expected ->
    let same_view =
      match actual.view, expected.view with
      | Highlight.Source_owner.Chatmd_view.Authored, Wire.Owner.Authored
      | Highlight.Source_owner.Chatmd_view.Effective, Wire.Owner.Effective -> true
      | Authored, Effective | Effective, Authored -> false
    in
    same_view
    && String.equal
         actual.input_identity
         (Chatml_source.Digest.to_hex expected.input_identity)
    && String.equal actual.view_sha256 (Chatml_source.Digest.to_hex expected.view_sha256)
    && Int.equal (Highlight.source_byte_length snapshot) expected.view_byte_length
  | Highlight.Source_owner.Chatml _, Wire.Owner.Chatmd _
  | Highlight.Source_owner.Chatmd _, Wire.Owner.Chatml _ -> false
;;

let role = function
  | Highlight.Role.Tag_name -> Wire.Role.Tag_name
  | Attribute_name -> Attribute_name
  | Attribute_value -> Attribute_value
  | Keyword -> Keyword
  | Identifier -> Identifier
  | Type_variable -> Type_variable
  | Variant_tag -> Variant_tag
  | String_literal -> String_literal
  | Number_literal -> Number_literal
  | Boolean_literal -> Boolean_literal
  | Comment -> Comment
  | Raw_delimiter -> Raw_delimiter
  | Raw_text -> Raw_text
  | Script_body -> Script_body
  | Operator -> Operator
  | Punctuation -> Punctuation
;;

let issue_kind = function
  | Highlight.Issue.Open_comment -> Wire.Issue.Open_comment
  | Open_string -> Open_string
  | Open_raw_block -> Open_raw_block
  | Open_tag -> Open_tag
  | Open_script -> Open_script
;;

let status = function
  | Highlight.Status.Reached_eof -> Wire.Status.Reached_eof
  | Truncated { next_byte; reason } ->
    let reason =
      match reason with
      | Highlight.Status.Span_limit -> Wire.Status.Span_limit
      | Issue_limit -> Issue_limit
      | Work_limit -> Work_limit
      | Nesting_limit -> Nesting_limit
    in
    Wire.Status.Truncated { next_byte; reason }
;;

let of_snapshot ~request ~owner snapshot ~checkpoint =
  if not (same_owner snapshot owner)
  then Error Error.Stale_source
  else
    let open Result.Let_syntax in
    let range source_range =
      Wire.Range.create
        ~owner
        ~start_byte:(Highlight.Range.start_byte source_range)
        ~end_byte:(Highlight.Range.end_byte source_range)
      |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
    in
    let%bind spans =
      Result.all
        (List.map (Highlight.spans snapshot) ~f:(fun span ->
           checkpoint ();
           let%map range = range (Highlight.Span.range span) in
           Wire.Span.create ~range ~role:(role (Highlight.Span.role span))))
    in
    let%bind issues =
      Result.all
        (List.map (Highlight.issues snapshot) ~f:(fun issue ->
           checkpoint ();
           let%map range = range (Highlight.Issue.range issue) in
           Wire.Issue.create ~range ~kind:(issue_kind (Highlight.Issue.kind issue))))
    in
    Wire.Result.create
      ~request
      ~owner
      ~spans
      ~issues
      ~status:(status (Highlight.status snapshot))
    |> Result.map_error ~f:(fun _ -> Error.Resource_limit)
;;

let scan_chatml ~request ~source ~limits ~checkpoint =
  match Wire.Request.view request with
  | Wire.View.Chatmd_authored | Chatmd_effective -> Error Error.Invalid_request
  | Chatml ->
    let open Result.Let_syntax in
    let%bind snapshot =
      Highlight.scan_chatml source ~limits ~checkpoint
      |> Result.map_error ~f:(function Source_too_large | Identity_too_large ->
          Error.Resource_limit)
    in
    of_snapshot
      ~request
      ~owner:(Wire.Owner.chatml (Wire.Request.source request))
      snapshot
      ~checkpoint
;;

let scan_chatmd ~request ~source ~input ~max_parser_input_bytes ~limits ~checkpoint =
  let open Result.Let_syntax in
  let requested_source = Wire.Request.source request in
  let%bind () =
    if
      Chatml_source.Id.equal (Chatml_source.id source) (Source.id requested_source)
      && Chatml_source.Digest.equal
           (Chatml_source.digest source)
           (Source.digest requested_source)
      && Int.equal
           (Chatml_source.byte_length source)
           (Source.byte_length requested_source)
    then Ok ()
    else Error Error.Stale_source
  in
  let%bind view =
    match Wire.Request.view request with
    | Wire.View.Chatml -> Error Error.Invalid_request
    | Chatmd_authored -> Ok Highlight.Source_owner.Chatmd_view.Authored
    | Chatmd_effective -> Ok Highlight.Source_owner.Chatmd_view.Effective
  in
  let%bind input =
    match input, view with
    | Some input, _ -> Ok input
    | None, Highlight.Source_owner.Chatmd_view.Effective -> Error Error.Source_unavailable
    | None, Authored ->
      let bytes = Chatml_source.contents source in
      Chatmd_parser_input.create
        ~role:Root
        ~path:(Chatml_source.Id.relative_path (Chatml_source.id source))
        ~authored:bytes
        ~effective:bytes
        ~max_input_bytes:max_parser_input_bytes
        ~checkpoint
      |> Result.map_error ~f:(function
        | Chatmd_parser_input.Error.Input_byte_limit -> Error.Resource_limit
        | Empty_path | Import_changed | Invalid_range -> Error.Invalid_request)
  in
  if
    Chatml_source.byte_length source > max_parser_input_bytes
    || Chatmd_parser_input.authored_byte_length input > max_parser_input_bytes
    || Chatmd_parser_input.effective_byte_length input > max_parser_input_bytes
  then Error Error.Resource_limit
  else if
    not
      (Chatmd_parser_input.authored_matches
         input
         (Chatml_source.contents source)
         ~checkpoint)
  then Error Error.Stale_source
  else (
    let%bind input_identity =
      Chatml_source.Digest.of_hex (Chatmd_parser_input.identity_digest input ~checkpoint)
      |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
    in
    let view_sha256, view_byte_length, wire_view =
      match view with
      | Highlight.Source_owner.Chatmd_view.Authored ->
        ( Chatmd_parser_input.authored_sha256 input
        , Chatmd_parser_input.authored_byte_length input
        , Wire.Owner.Authored )
      | Effective ->
        ( Chatmd_parser_input.effective_sha256 input
        , Chatmd_parser_input.effective_byte_length input
        , Wire.Owner.Effective )
    in
    let%bind view_sha256 =
      Chatml_source.Digest.of_hex view_sha256
      |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
    in
    let%bind owner =
      Wire.Owner.chatmd
        ~captured_source:(Wire.Request.source request)
        ~input_identity
        ~view:wire_view
        ~view_sha256
        ~view_byte_length
      |> Result.map_error ~f:(fun _ -> Error.Stale_source)
    in
    let%bind snapshot =
      Highlight.scan_chatmd input ~view ~limits ~checkpoint
      |> Result.map_error ~f:(function Source_too_large | Identity_too_large ->
          Error.Resource_limit)
    in
    of_snapshot ~request ~owner snapshot ~checkpoint)
;;
