open! Core
open Authoring_assistance_input
module Wire = Agent_protocol.Authoring_assistance.Assistance
module Work = Authoring_assistance_work
module Highlight_snapshot = Language_highlighting.Highlight_snapshot

module Candidate = struct
  type t =
    { kind : Wire.Suggestion.kind
    ; label : string
    ; value : string
    ; evidence : Wire.Suggestion.evidence
    }

  let create ~kind ~label ~value ~evidence = { kind; label; value; evidence }
end

let source_stamp source =
  Agent_protocol.Authoring_common.Source.create
    ~id:(Chatml_source.id source)
    ~digest:(Chatml_source.digest source)
    ~byte_length:(Chatml_source.byte_length source)
    ~editor_revision:None
;;

let tag_name tag =
  match tag with
  | Chatmd_ast.Shell_element element ->
    Some (Chatmd_shell_spec.Shell_element.to_string element)
  | _ ->
    List.find_map Chatmd_ast.official_names ~f:(fun (name, known) ->
      if Chatmd_ast.tag_equal tag known then Some name else None)
;;

type encoded =
  | Valid of string
  | Unrepresentable
  | Too_long

type rendering =
  | Plain
  | Attribute of Chatmd_attribute_lex.Quote.t option

exception Encoded_too_long
exception Invalid_edit_spelling

let character_spelling ~quote = function
  | '&' when Option.is_some quote -> "&amp;"
  | '<' when Option.is_some quote -> "&lt;"
  | '>' when Option.is_some quote -> "&gt;"
  | '>' -> raise Invalid_edit_spelling
  | '\'' when Option.equal Chatmd_attribute_lex.Quote.equal quote (Some Single) ->
    "&apos;"
  | '"' when Option.equal Chatmd_attribute_lex.Quote.equal quote (Some Double) -> "&quot;"
  | ('\'' | '"' | ' ' | '\t' | '\n' | '\r') when Option.is_none quote ->
    raise Invalid_edit_spelling
  | char -> String.of_char char
;;

let prefix_matches_encoded ~quote ~raw_prefix value =
  let length = String.length raw_prefix in
  let offset = ref 0 in
  let matches = ref true in
  (try
     String.iter value ~f:(fun char ->
       if !matches && !offset < length
       then
         String.iter (character_spelling ~quote char) ~f:(fun encoded ->
           if !offset < length
           then (
             if not (Char.equal raw_prefix.[!offset] encoded) then matches := false;
             Int.incr offset)))
   with
   | Invalid_edit_spelling -> matches := false);
  !matches && !offset >= length
;;

let encode_value ~quote ~max_bytes text =
  if not (Stdlib.String.is_valid_utf_8 text)
  then Unrepresentable
  else (
    let buffer = Buffer.create (Int.min max_bytes (String.length text)) in
    let append spelling =
      if String.length spelling > max_bytes - Buffer.length buffer
      then raise Encoded_too_long;
      Buffer.add_string buffer spelling
    in
    try
      String.iter text ~f:(fun char -> append (character_spelling ~quote char));
      let encoded = Buffer.contents buffer in
      if Option.is_some quote && String.is_suffix encoded ~suffix:"\\"
      then Unrepresentable
      else Valid encoded
    with
    | Encoded_too_long -> Too_long
    | Invalid_edit_spelling -> Unrepresentable)
;;

let input_source facts input ~work =
  List.find_map facts.Facts.captured_inputs ~f:(fun captured ->
    Work.tick work ();
    if phys_equal input (Captured_input.input captured)
    then Some (Captured_input.source captured)
    else None)
;;

let family_values prefix ~attribute =
  match prefix with
  | Chatmd_declaration_schema.Prefix.Viable families | Uncertain families ->
    List.concat_map families ~f:(fun family ->
      match Chatmd_declaration_schema.choices family ~attribute with
      | Some (Closed names | Comma_separated_tokens names) -> names
      | Some Freeform | None -> [])
    |> List.dedup_and_sort ~compare:String.compare
  | Malformed _ | Limit_exceeded -> []
;;

exception Invalid_wire_suggestion of Agent_protocol.Error.t
exception Item_limit_reached
exception Result_bytes_reached

let cursor_at_token_end authored cursor =
  let next =
    let offset = Highlight_snapshot.Cursor.cursor_byte cursor in
    if offset >= String.length authored then None else Some authored.[offset]
  in
  match Highlight_snapshot.Cursor.kind cursor with
  | Tag_name _ ->
    Option.for_all next ~f:(fun char ->
      Char.is_whitespace char || Char.equal char '>' || Char.equal char '/')
  | Attribute_name _ ->
    Option.for_all next ~f:(fun char ->
      Char.is_whitespace char
      || Char.equal char '='
      || Char.equal char '>'
      || Char.equal char '/')
  | Attribute_value { value_state; _ } ->
    (match Highlight_snapshot.Cursor.Value_state.quote value_state with
     | Some Single -> Option.for_all next ~f:(Char.equal '\'')
     | Some Double -> Option.for_all next ~f:(Char.equal '"')
     | None ->
       Option.for_all next ~f:(fun char -> Char.is_whitespace char || Char.equal char '>'))
  | Text | Script_body | Opaque -> true
;;

let produce facts ~checkpoint =
  let work = Work.create ~remaining:facts.Facts.remaining_work ~checkpoint in
  let request = facts.request in
  let request_source = Wire.Request.source request in
  let cursor_byte = Wire.Request.cursor_byte request in
  let max_items = Int.min facts.limits.max_candidates (Wire.Request.max_items request) in
  let builder = Wire.Result.Builder.create ~request in
  let seen = String.Hash_set.create () in
  let item_count = ref 0 in
  let item_limit = ref false in
  let unrepresentable = ref false in
  let result_bytes = ref false in
  let emit ~range ~raw_prefix ~rendering candidate =
    Work.tick work ();
    let Candidate.{ kind; label; value; evidence } = candidate in
    Work.charge work (String.length value);
    Work.charge work (String.length label);
    let prefix_matches =
      match rendering with
      | Plain -> String.is_prefix value ~prefix:raw_prefix
      | Attribute quote -> prefix_matches_encoded ~quote ~raw_prefix value
    in
    if not prefix_matches
    then ()
    else if String.length label > facts.limits.max_label_bytes
    then result_bytes := true
    else (
      let encoded =
        match rendering with
        | Plain ->
          if not (Stdlib.String.is_valid_utf_8 value)
          then Unrepresentable
          else if String.length value > facts.limits.max_label_bytes
          then Too_long
          else Valid value
        | Attribute quote ->
          encode_value ~quote ~max_bytes:facts.limits.max_label_bytes value
      in
      match encoded with
      | Unrepresentable -> unrepresentable := true
      | Too_long -> result_bytes := true
      | Valid insert_text ->
        if String.is_prefix insert_text ~prefix:raw_prefix
        then (
          Work.charge work (2 + String.length insert_text);
          let key =
            (match kind with
             | Tag -> "t:"
             | Attribute -> "a:"
             | Value -> "v:"
             | Reference -> "r:"
             | Path -> "p:"
             | Template -> "m:"
             | Environment -> "e:")
            ^ insert_text
          in
          if not (Hash_set.mem seen key)
          then
            if !item_count >= max_items
            then raise Item_limit_reached
            else (
              match
                Wire.Suggestion.create ~kind ~label ~insert_text ~replace:range ~evidence
              with
              | Ok item ->
                (match builder with
                 | Error error -> raise (Invalid_wire_suggestion error)
                 | Ok builder ->
                   (match Wire.Result.Builder.add builder item with
                    | Error error -> raise (Invalid_wire_suggestion error)
                    | Ok (Full Item_limit) -> raise Item_limit_reached
                    | Ok (Full _) -> raise Result_bytes_reached
                    | Ok Added ->
                      Hash_set.add seen key;
                      Int.incr item_count))
              | Error error -> raise (Invalid_wire_suggestion error))))
  in
  let result status =
    match builder with
    | Error error -> Error (Error.Wire error)
    | Ok builder ->
      Result.map_error (Wire.Result.Builder.finish builder ~status) ~f:(fun error ->
        Error.Wire error)
  in
  try
    if Work.remaining work = 0 then raise Work.Exhausted;
    let highlighter_limits =
      Highlight_snapshot.Limits.create
        ~max_source_bytes:facts.limits.max_authored_bytes
        ~max_identity_bytes:facts.limits.max_total_captured_bytes
        ~max_spans:4096
        ~max_issues:64
        ~max_work_units:(Work.remaining work)
        ~max_nesting:256
      |> Or_error.ok_exn
    in
    match
      Highlight_snapshot.scan_chatmd_cursor
        facts.lexical_input
        ~cursor_byte
        ~limits:highlighter_limits
        ~checkpoint:(Work.tick work)
    with
    | Error _ -> result (Unavailable No_local_context)
    | Ok (Unavailable _) -> result (Truncated Work_limit)
    | Ok (Available cursor) ->
      if not (cursor_at_token_end (Chatml_source.contents facts.authored) cursor)
      then result Complete
      else (
        let prefix = Highlight_snapshot.Cursor.raw_prefix cursor in
        let raw_prefix = Highlight_snapshot.Cursor.Raw_slice.contents prefix in
        let range =
          match
            Wire.Range.create
              ~source:request_source
              ~start_byte:(Highlight_snapshot.Cursor.Raw_slice.start_byte prefix)
              ~end_byte:cursor_byte
          with
          | Ok range -> range
          | Error error -> raise (Invalid_wire_suggestion error)
        in
        let candidate ~kind ~label ~value ~evidence =
          Candidate.create ~kind ~label ~value ~evidence
        in
        let lexical ~kind value =
          emit
            ~range
            ~raw_prefix
            ~rendering:Plain
            (candidate ~kind ~label:value ~value ~evidence:Lexical)
        in
        (match Highlight_snapshot.Cursor.kind cursor with
         | Opaque | Script_body -> ()
         | Text ->
           if Chatmd_tag_scope.depth (Highlight_snapshot.Cursor.scope cursor) = 0
           then lexical ~kind:Template "<user></user>"
         | Tag_name { closing } ->
           let names =
             if closing
             then
               Option.to_list
                 (Option.bind
                    (Chatmd_tag_scope.top (Highlight_snapshot.Cursor.scope cursor))
                    ~f:tag_name)
             else
               Chatmd_declaration_schema.tag_names
                 (Highlight_snapshot.Cursor.scope cursor)
                 ~checkpoint:(Work.tick work)
           in
           List.iter names ~f:(lexical ~kind:Tag)
         | Attribute_name { tag } ->
           let completed =
             List.map
               (Highlight_snapshot.Cursor.completed_attributes cursor)
               ~f:Highlight_snapshot.Cursor.Completed_attribute.decoded
           in
           let family =
             Chatmd_declaration_schema.families_of_prefix
               ~scope:(Highlight_snapshot.Cursor.scope cursor)
               ~tag
               ~completed_attributes:completed
               ~uncertain_discriminator:
                 (Highlight_snapshot.Cursor.uncertain_discriminator cursor)
               ~checkpoint:(Work.tick work)
           in
           List.iter
             (Chatmd_declaration_schema.attributes_for_prefix
                family
                ~completed_attributes:completed
                ~checkpoint:(Work.tick work))
             ~f:(fun attribute ->
               let value = Chatmd_declaration_schema.Attribute_candidate.name attribute in
               let label =
                 match
                   Chatmd_declaration_schema.Attribute_candidate.applicability attribute
                 with
                 | All_viable_families -> value
                 | Only _ -> value ^ " (conditional)"
               in
               emit
                 ~range
                 ~raw_prefix
                 ~rendering:Plain
                 (candidate ~kind:Attribute ~label ~value ~evidence:Lexical))
         | Attribute_value { tag; attribute; value_state } ->
           let completed =
             List.map
               (Highlight_snapshot.Cursor.completed_attributes cursor)
               ~f:Highlight_snapshot.Cursor.Completed_attribute.decoded
           in
           let family =
             Chatmd_declaration_schema.families_of_prefix
               ~scope:(Highlight_snapshot.Cursor.scope cursor)
               ~tag
               ~completed_attributes:completed
               ~uncertain_discriminator:
                 (Highlight_snapshot.Cursor.uncertain_discriminator cursor)
               ~checkpoint:(Work.tick work)
           in
           let quote = Highlight_snapshot.Cursor.Value_state.quote value_state in
           let offer ~kind ~evidence value =
             emit
               ~range
               ~raw_prefix
               ~rendering:(Attribute quote)
               (candidate ~kind ~label:value ~value ~evidence)
           in
           List.iter
             (family_values family ~attribute)
             ~f:(offer ~kind:Value ~evidence:Lexical);
           if String.equal attribute "environment"
           then
             Option.iter facts.project ~f:(fun project ->
               match
                 Chatml_project.Environment_names.list
                   project
                   ~max_names:facts.limits.max_candidates
                   ~checkpoint:(Work.tick work)
               with
               | Error _ -> item_limit := true
               | Ok names ->
                 (match source_stamp (Chatml_project.source project) with
                  | Error _ -> ()
                  | Ok source ->
                    List.iter
                      names
                      ~f:(offer ~kind:Environment ~evidence:(Parsed_project source))));
           if String.equal attribute "script" || String.equal attribute "moderator"
           then
             Option.iter facts.parsed ~f:(fun parsed ->
               List.iter
                 (Prompt.Chat_markdown.Parsed.declarations parsed)
                 ~f:(fun declaration ->
                   Work.tick work ();
                   let input =
                     Chatmd_parser_input.Range.input
                       (Prompt.Chat_markdown.Declaration.range declaration)
                   in
                   Option.iter (input_source facts input ~work) ~f:(fun source ->
                     let stamp =
                       if
                         Chatml_source.Id.equal
                           (Chatml_source.id source)
                           (Chatml_source.id facts.authored)
                       then Ok request_source
                       else source_stamp source
                     in
                     match stamp with
                     | Error _ -> ()
                     | Ok source ->
                       offer
                         ~kind:Reference
                         ~evidence:(Parsed_document source)
                         (Prompt.Chat_markdown.Declaration.script_id declaration))));
           if String.equal attribute "tool"
           then
             List.iter facts.selected_tools ~f:(fun tool ->
               Work.tick work ();
               offer
                 ~kind:Reference
                 ~evidence:(Selected_tool (Selected_tool.evidence tool))
                 (Selected_tool.name tool));
           if List.mem [ "src"; "project"; "path" ] attribute ~equal:String.equal
           then
             List.iter facts.paths ~f:(fun path ->
               Work.tick work ();
               let evidence =
                 match Path_candidate.origin path with
                 | Admitted_root root when not (String.equal attribute "project") ->
                   Some (Wire.Suggestion.Host_root root)
                 | Project_source { project; _ }
                   when String.equal attribute "project"
                        && Chatmd_ast.tag_equal tag Chatmd_ast.Script ->
                   Result.ok
                     (Result.map
                        (source_stamp (Chatml_project.source project))
                        ~f:(fun source -> Wire.Suggestion.Parsed_project source))
                 | Decoded_member { project; role; _ }
                   when String.equal attribute "src"
                        && Chatmd_ast.tag_equal tag Chatmd_ast.Script
                        &&
                        match role with
                        | Chatml_project.Declared_paths.Module_implementation
                        | Module_interface
                        | Entry_source -> true
                        | Source_directory | Local_dependency_project -> false ->
                   Result.ok
                     (Result.map
                        (source_stamp (Chatml_project.source project))
                        ~f:(fun source -> Wire.Suggestion.Parsed_project source))
                 | Selected_closure { prepared; _ } ->
                   if
                     not
                       (String.equal attribute "src"
                        && Chatmd_ast.tag_equal tag Chatmd_ast.Script)
                   then None
                   else
                     Result.ok
                       (Result.map
                          (Wire.Selected_catalog.create
                             ~environment:
                               (Chatml_prepared_program.Environment.name
                                  (Chatml_prepared_program.environment prepared))
                             ~input_fingerprint:
                               (Chatml_prepared_program.fingerprint prepared)
                             ~compiler_contract:
                               (Chatml_prepared_program.compiler_contract prepared))
                          ~f:(fun catalog -> Wire.Suggestion.Selected_catalog catalog))
                 | Admitted_root _ | Project_source _ | Decoded_member _ -> None
               in
               Option.iter evidence ~f:(fun evidence ->
                 offer
                   ~kind:Path
                   ~evidence
                   (Path_candidate.Edit_spelling.to_string
                      (Path_candidate.edit_spelling path))));
           if Chatmd_ast.tag_equal tag Chatmd_ast.Doc && String.equal attribute "src"
           then (
             match
               Builtin_instructions.installed_ids
                 ~max_items:facts.limits.max_candidates
                 ~checkpoint:(Work.tick work)
             with
             | Error _ -> item_limit := true
             | Ok ids ->
               List.iter ids ~f:(fun id ->
                 Work.tick work ();
                 match Builtin_instructions.find id with
                 | Error _ -> ()
                 | Ok asset ->
                   (match
                      Chatml_source.Digest.of_hex
                        (Builtin_instructions.Asset.sha256 asset)
                    with
                    | Error _ -> ()
                    | Ok sha256 ->
                      offer
                        ~kind:Template
                        ~evidence:
                          (Installed_builtin
                             { id = Builtin_instructions.Id.to_string id; sha256 })
                        ("builtin:" ^ Builtin_instructions.Id.to_string id)))));
        result
          (if !item_limit
           then Truncated Item_limit
           else if !unrepresentable
           then Truncated Unrepresentable_text
           else if !result_bytes
           then Truncated Result_bytes
           else Complete))
  with
  | Work.Exhausted -> result (Truncated Work_limit)
  | Item_limit_reached -> result (Truncated Item_limit)
  | Result_bytes_reached -> result (Truncated Result_bytes)
  | Invalid_wire_suggestion error -> Error (Error.Wire error)
;;
