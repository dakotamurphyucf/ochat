open! Core
module Owner = Authoring_context_service_core
module Wire = Agent_protocol.Authoring_assistance.Assistance
module Analysis_target = Agent_protocol.Authoring.Analysis.Target
module Wire_source = Agent_protocol.Authoring.Source
module Ledger = Chatml_authoring_context
module Cache = Authoring_checked_template_cache
module Witness = Authoring_checked_template.Witness
module Captured = Chatml_source
module Effective = Chatml_authoring_effective
module Prepared = Chatml_prepared_program
module Script = Chatmd_authoring_analysis.Script
module Declaration = Prompt.Chat_markdown.Declaration
module Input = Chatmd_parser_input

let same_request_source authored request =
  let source = Wire.Request.source request in
  Captured.Id.equal (Captured.id authored) (Wire_source.id source)
  && Captured.Digest.equal (Captured.digest authored) (Wire_source.digest source)
  && Int.equal (Captured.byte_length authored) (Wire_source.byte_length source)
;;

let wire_target = function
  | Chatml.Chatml_compiler_contract.Target.One_off_v1 -> Wire.Checked_template.One_off_v1
  | Tool_v1 -> Tool_v1
  | Developer_tool_v1 -> Developer_tool_v1
  | Moderator_v1 -> Moderator_v1
  | Delegated_moderator_v1 -> Delegated_moderator_v1
;;

let label = function
  | Chatml.Chatml_compiler_contract.Target.One_off_v1 -> "Checked one-off script example"
  | Tool_v1 -> "Checked tool script example"
  | Developer_tool_v1 -> "Checked developer-tool script example"
  | Moderator_v1 -> "Checked moderator script example"
  | Delegated_moderator_v1 -> "Checked delegated-moderator script example"
;;

let suggestion witness ~request ~start_byte ~end_byte =
  let open Result.Let_syntax in
  let source = Wire.Request.source request in
  let%bind replace =
    Wire.Range.create ~source ~start_byte ~end_byte
    |> Result.map_error ~f:(fun _ -> Owner.Error.Stale_source)
  in
  let%bind selected =
    Wire.Selected_catalog.create
      ~environment:(Witness.environment witness)
      ~input_fingerprint:(Witness.base_fingerprint witness)
      ~compiler_contract:(Witness.selected_contract witness)
    |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
  in
  let%bind checked =
    Wire.Checked_template.create
      ~selected
      ~target:(wire_target (Witness.target witness))
      ~example_digest:(Witness.example_digest witness)
      ~checked_input_fingerprint:(Witness.checked_input_fingerprint witness)
    |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
  in
  Wire.Suggestion.create
    ~kind:Template
    ~label:(label (Witness.target witness))
    ~insert_text:(Witness.example witness)
    ~replace
    ~evidence:(Checked_template checked)
  |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
;;

let row current ~identity ~generation ~target =
  let open Result.Let_syntax in
  let%map cache = Owner.Current.checked_templates current in
  Cache.current cache ~identity ~generation ~target
;;

let project current ~inputs ~receipt ~authored ~request ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind row =
    row
      current
      ~identity:(Ledger.Inputs.identity inputs)
      ~generation:(Ledger.Inputs.generation inputs)
      ~target:Analysis_target.project
  in
  match row with
  | None -> Ok None
  | Some row ->
    let selected = Effective.Overlay.build receipt |> Chatml_project_build.prepared in
    if
      (not (Cache.matches_project row ~inputs ~receipt))
      || (not (same_request_source authored request))
      || (not (Captured.Id.equal (Captured.id authored) (Prepared.entry selected)))
      || Wire.Request.cursor_byte request > Captured.byte_length authored
    then Ok None
    else (
      let%map suggestion =
        suggestion
          (Cache.witness row)
          ~request
          ~start_byte:0
          ~end_byte:(Captured.byte_length authored)
      in
      Some suggestion)
;;

let document
      current
      ~inputs
      ~script
      ~declaration
      ~authored_input
      ~authored
      ~request
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind target =
    Analysis_target.selected_script (Declaration.script_id declaration)
    |> Result.map_error ~f:(fun _ -> Owner.Error.Invalid_request)
  in
  let%bind row =
    row
      current
      ~identity:(Ledger.Document.Inputs.identity inputs)
      ~generation:(Ledger.Document.Inputs.generation inputs)
      ~target
  in
  match row with
  | None -> Ok None
  | Some row ->
    let selected_source = Script.selected_source script in
    if
      (not (Cache.matches_document row ~inputs ~script))
      || (not (phys_equal authored_input (Script.declaration_input script)))
      || (not (same_request_source authored request))
      || not
           (Input.authored_matches
              authored_input
              (Captured.contents authored)
              ~checkpoint)
    then Ok None
    else (
      let%bind point =
        Script.authored_point
          script
          ~input:authored_input
          ~authored_byte:(Wire.Request.cursor_byte request)
          ~checkpoint
        |> Result.map_error ~f:(fun _ -> Owner.Error.Stale_source)
      in
      match point with
      | Declaration.Extraction.Authored_point.Unavailable _ -> Ok None
      | Exact_compiled_byte compiled_byte ->
        if compiled_byte > Captured.byte_length selected_source
        then Error Owner.Error.Stale_source
        else (
          let%bind bound =
            Declaration.Extraction.bind
              declaration
              ~compiled_source:selected_source
              ~checkpoint
            |> Result.map_error ~f:(fun _ -> Owner.Error.Stale_source)
          in
          let%bind native =
            Captured.locate
              selected_source
              ~start_byte:0
              ~end_byte:(Captured.byte_length selected_source)
              ~checkpoint
            |> Result.map_error ~f:(fun _ -> Owner.Error.Stale_source)
          in
          let%bind projection =
            Declaration.Extraction.project_raw bound native ~checkpoint
            |> Result.map_error ~f:(fun _ -> Owner.Error.Stale_source)
          in
          match projection with
          | Declaration.Extraction.Exact_inline span
            when phys_equal (Declaration.Extraction.input span) authored_input ->
            let start_byte = Declaration.Extraction.start_byte span in
            let end_byte = Declaration.Extraction.end_byte span in
            if
              start_byte <= Wire.Request.cursor_byte request
              && Wire.Request.cursor_byte request <= end_byte
            then (
              let%map suggestion =
                suggestion (Cache.witness row) ~request ~start_byte ~end_byte
              in
              Some suggestion)
            else Ok None
          | Exact_inline _ | External_origin _ | Approximate_declaration _ -> Ok None))
;;

let include_checked result item =
  match item with
  | None -> Ok result
  | Some item ->
    let open Result.Let_syntax in
    let request = Wire.Result.request result in
    let%bind builder =
      Wire.Result.Builder.create ~request
      |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
    in
    let%bind checked_addition =
      Wire.Result.Builder.add builder item
      |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
    in
    let rec add_existing = function
      | [] -> Ok None
      | existing :: rest ->
        (match Wire.Result.Builder.add builder existing with
         | Ok Wire.Result.Builder.Added -> add_existing rest
         | Ok (Full reason) -> Ok (Some reason)
         | Error _ -> Error Owner.Error.Resource_limit)
    in
    let%bind omitted = add_existing (Wire.Result.items result) in
    let status =
      match checked_addition, omitted, Wire.Result.status result with
      | Full reason, _, _ -> Wire.Status.Truncated reason
      | Added, Some reason, _ -> Wire.Status.Truncated reason
      | Added, None, Wire.Status.Unavailable _ -> Wire.Status.Complete
      | Added, None, status -> status
    in
    Wire.Result.Builder.finish builder ~status
    |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
;;
