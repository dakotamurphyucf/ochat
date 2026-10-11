open Core
module C = Chatml_prompt_programs
module Input = Chatmd_parser_input
module Plan = Script_contract_plan

let binding_source plan binding ~checkpoint =
  let identity = Input.identity_digest (C.Binding.parser_input binding) ~checkpoint in
  List.find_map (Plan.programs plan) ~f:(fun program ->
    checkpoint ();
    let actual = Plan.Program.binding program in
    if
      Int.equal (C.Binding.start_byte actual) (C.Binding.start_byte binding)
      && Int.equal (C.Binding.end_byte actual) (C.Binding.end_byte binding)
      && String.equal
           identity
           (Input.identity_digest (C.Binding.parser_input actual) ~checkpoint)
    then
      Some
        (Prompt.Chat_markdown.Declaration.source_ref (Plan.Program.declaration program))
    else None)
;;

let diagnostic plan error ~checkpoint =
  let code, message, binding =
    match error with
    | C.Error.Missing_contract binding ->
      ( "chatml.program_missing_contract"
      , "Captured program lacks this host's authentic role"
      , Some binding )
    | Unexpected_contract binding ->
      ( "chatml.program_unexpected_contract"
      , "This host role is absent from captured selection"
      , Some binding )
    | Contract_mismatch binding ->
      ( "chatml.program_contract_mismatch"
      , "Captured program contract differs from this host role"
      , Some binding )
    | Script_mismatch binding ->
      ( "chatml.program_script_mismatch"
      , "Captured program differs from the actual script declaration"
      , Some binding )
    | Duplicate_program (binding, _) ->
      "chatml.program_duplicate", "Captured program identity is duplicated", Some binding
    | Limit_exceeded ->
      ( "chatml.program_restore_limit"
      , "Captured program restoration exceeds host allowance"
      , None )
    | Document _
    | Storage _
    | Provided _
    | Selected _
    | Invalid_binding _
    | Conflicting_source _
    | Invalid_inventory _ ->
      ( "chatml.program_restore_failed"
      , "Captured program inventory does not match this host"
      , None )
  in
  let source =
    Option.bind binding ~f:(fun binding -> binding_source plan binding ~checkpoint)
  in
  Chatmd_shell_spec.Diagnostic.error ?source ~code message
;;

let restore plan ~custody ~limits ~checkpoint =
  let open Result.Let_syntax in
  let parsed = Plan.parsed plan in
  let map result =
    Result.map_error result ~f:(fun error -> [ diagnostic plan error ~checkpoint ])
  in
  if C.has_active_programs custody
  then
    map (C.restore custody ~parsed ~contracts:(Plan.contracts plan) ~limits ~checkpoint)
    |> Result.map ~f:Option.some
  else (
    let%bind () = map (C.validate_selection custody ~parsed ~limits ~checkpoint) in
    let executable declaration =
      List.exists (Plan.programs plan) ~f:(fun program ->
        checkpoint ();
        phys_equal (Plan.Program.declaration program) declaration)
    in
    let%bind matched =
      map
        (C.fold_declaration_matches
           custody
           ~parsed
           ~limits
           ~checkpoint
           ~init:None
           ~f:(fun first matched ->
             let declaration = C.Declaration_match.declaration matched in
             if executable declaration then Some declaration else first))
    in
    let configured =
      List.find_map (Plan.programs plan) ~f:(fun program ->
        checkpoint ();
        let declaration = Plan.Program.declaration program in
        if Option.is_some (Prompt.Chat_markdown.Declaration.project_selection declaration)
        then Some declaration
        else None)
    in
    let omitted = Option.first_some matched configured in
    match omitted with
    | None ->
      checkpoint ();
      Ok None
    | Some declaration ->
      Error
        [ Chatmd_shell_spec.Diagnostic.error
            ~source:(Prompt.Chat_markdown.Declaration.source_ref declaration)
            ~code:"chatml.program_missing_selection"
            "Captured or configured script requires an explicit authentic active \
             selection"
        ])
;;
