open! Core
module Wire = Agent_protocol.Authoring_assistance.Assistance
module Work = Authoring_assistance_work

module Error = struct
  type t =
    | Invalid_limits
    | Source_mismatch
    | Parsed_input_mismatch
    | Missing_import_capture
    | Foreign_import_capture
    | Selected_without_project
    | Selected_project_mismatch
    | Invalid_path_provenance
    | Candidate_limit
    | Work_limit
    | Invalid_display_text
    | Wire of Agent_protocol.Error.t
  [@@deriving sexp_of]
end

module Limits = struct
  type t =
    { max_work_steps : int
    ; max_authored_bytes : int
    ; max_total_captured_bytes : int
    ; max_parsed_inputs : int
    ; max_paths : int
    ; max_selected_tools : int
    ; max_candidates : int
    ; max_label_bytes : int
    }

  let create
        ~max_work_steps
        ~max_authored_bytes
        ~max_total_captured_bytes
        ~max_parsed_inputs
        ~max_paths
        ~max_selected_tools
        ~max_candidates
        ~max_label_bytes
    =
    if
      List.exists
        [ max_work_steps
        ; max_authored_bytes
        ; max_total_captured_bytes
        ; max_parsed_inputs
        ; max_paths
        ; max_selected_tools
        ; max_candidates
        ; max_label_bytes
        ]
        ~f:(fun n -> n <= 0)
      || max_candidates > Agent_protocol.Authoring_assistance.Limits.max_suggestions
      || max_label_bytes > Agent_protocol.Authoring_assistance.Limits.max_label_bytes
    then Or_error.error_string "invalid assistance limits"
    else
      Ok
        { max_work_steps
        ; max_authored_bytes
        ; max_total_captured_bytes
        ; max_parsed_inputs
        ; max_paths
        ; max_selected_tools
        ; max_candidates
        ; max_label_bytes
        }
  ;;
end

module Captured_input = struct
  type t =
    { input : Chatmd_parser_input.t
    ; source : Chatml_source.t
    }

  let create ~input ~source ~max_bytes ~checkpoint =
    if max_bytes <= 0
    then Error Error.Invalid_limits
    else if Chatml_source.byte_length source > max_bytes
    then Error Error.Source_mismatch
    else if
      Chatml_source.byte_length source <> Chatmd_parser_input.authored_byte_length input
      || not
           (String.equal
              (Chatml_source.Digest.to_hex (Chatml_source.digest source))
              (Chatmd_parser_input.authored_sha256 input))
    then Error Error.Source_mismatch
    else if
      not
        (Chatmd_parser_input.authored_matches
           input
           (Chatml_source.contents source)
           ~checkpoint)
    then Error Error.Source_mismatch
    else Ok { input; source }
  ;;

  let input t = t.input
  let source t = t.source
end

module Path_candidate = struct
  module Edit_spelling = struct
    type t = string

    let create text =
      if
        String.is_empty text
        || String.length text > 1024
        || (not (Stdlib.String.is_valid_utf_8 text))
        || Char.equal text.[0] '/'
        || String.exists text ~f:(function
          | '\\' | ':' | '\000' | '\n' | '\r' -> true
          | _ -> false)
        || List.exists (String.split text ~on:'/') ~f:String.is_empty
      then Error Error.Invalid_path_provenance
      else Ok text
    ;;

    let to_string t = t
  end

  type captured_route =
    { authored_base_id : Chatml_source.Id.t
    ; authored_base_digest : Chatml_source.Digest.t
    ; base_directory : Source.Portable_path.t option
    ; edit_spelling : Edit_spelling.t
    }

  type origin =
    | Admitted_root of Agent_protocol.Id.Authoring_root.t
    | Project_source of
        { project : Chatml_project.t
        ; route : captured_route
        }
    | Decoded_member of
        { project : Chatml_project.t
        ; role : Chatml_project.Declared_paths.role
        ; member : Source.Portable_path.t
        ; route : captured_route
        }
    | Selected_closure of
        { prepared : Chatml_prepared_program.t
        ; source_id : Chatml_source.Id.t
        ; route : captured_route
        }

  type t =
    { origin : origin
    ; edit_spelling : Edit_spelling.t
    }

  let create ~origin ~edit_spelling = { origin; edit_spelling }
  let origin t = t.origin
  let edit_spelling t = t.edit_spelling
end

module Selected_tool = struct
  type t =
    { name : string
    ; evidence : Wire.Selected_tool.t
    }

  let create ~id ~name ~binding_fingerprint ~selection_fingerprint =
    if
      String.is_empty name
      || String.length name > Agent_protocol.Authoring_assistance.Limits.max_label_bytes
    then Error Error.Invalid_display_text
    else (
      match
        Wire.Selected_tool.create
          ~capability_id:id
          ~binding_fingerprint
          ~selected_registry_fingerprint:selection_fingerprint
      with
      | Error error -> Error (Error.Wire error)
      | Ok evidence -> Ok { name; evidence })
  ;;

  let name t = t.name
  let evidence t = t.evidence
end

module Facts = struct
  exception Capture_limit

  type t =
    { request : Wire.Request.t
    ; authored : Chatml_source.t
    ; lexical_input : Chatmd_parser_input.t
    ; parsed : Prompt.Chat_markdown.Parsed.t option
    ; captured_inputs : Captured_input.t list
    ; project : Chatml_project.t option
    ; selected : Chatml_project_build.t option
    ; paths : Path_candidate.t list
    ; selected_tools : Selected_tool.t list
    ; limits : Limits.t
    ; remaining_work : int
    }

  let checked_add ~limit current n =
    if n < 0 || n > limit - current then raise Capture_limit else current + n
  ;;

  let create
        ~request
        ~authored
        ~lexical_input
        ~parsed
        ~captured_inputs
        ~project
        ~selected
        ~paths
        ~selected_tools
        ~(limits : Limits.t)
        ~checkpoint
    =
    let work = Work.create ~remaining:limits.max_work_steps ~checkpoint in
    try
      let request_source = Wire.Request.source request in
      if
        (not
           (Chatml_source.Id.equal
              (Agent_protocol.Authoring_common.Source.id request_source)
              (Chatml_source.id authored)))
        || (not
              (Chatml_source.Digest.equal
                 (Agent_protocol.Authoring_common.Source.digest request_source)
                 (Chatml_source.digest authored)))
        || Agent_protocol.Authoring_common.Source.byte_length request_source
           <> Chatml_source.byte_length authored
        || Chatml_source.byte_length authored > limits.max_authored_bytes
        || (not
              (Chatmd_parser_input.Role.equal
                 (Chatmd_parser_input.role lexical_input)
                 Chatmd_parser_input.Role.Root))
        || Chatmd_parser_input.authored_byte_length lexical_input
           <> Chatml_source.byte_length authored
        || not
             (String.equal
                (Chatmd_parser_input.authored_sha256 lexical_input)
                (Chatml_source.Digest.to_hex (Chatml_source.digest authored)))
      then Error Error.Source_mismatch
      else (
        let parsed_inputs =
          match parsed with
          | None -> []
          | Some parsed -> Prompt.Chat_markdown.Parsed.inputs parsed
        in
        ignore
          (Work.list_length_bounded work ~maximum:limits.max_parsed_inputs parsed_inputs
           : int);
        ignore
          (Work.list_length_bounded work ~maximum:limits.max_parsed_inputs captured_inputs
           : int);
        ignore (Work.list_length_bounded work ~maximum:limits.max_paths paths : int);
        ignore
          (Work.list_length_bounded work ~maximum:limits.max_selected_tools selected_tools
           : int);
        let captured_bytes = ref (Chatml_source.byte_length authored) in
        captured_bytes
        := checked_add
             ~limit:limits.max_total_captured_bytes
             !captured_bytes
             (Chatmd_parser_input.effective_byte_length lexical_input);
        List.iter parsed_inputs ~f:(fun input ->
          Work.tick work ();
          if not (phys_equal input lexical_input)
          then (
            captured_bytes
            := checked_add
                 ~limit:limits.max_total_captured_bytes
                 !captured_bytes
                 (Chatmd_parser_input.authored_byte_length input);
            captured_bytes
            := checked_add
                 ~limit:limits.max_total_captured_bytes
                 !captured_bytes
                 (Chatmd_parser_input.effective_byte_length input)));
        List.iter captured_inputs ~f:(fun capture ->
          Work.tick work ();
          captured_bytes
          := checked_add
               ~limit:limits.max_total_captured_bytes
               !captured_bytes
               (Chatml_source.byte_length (Captured_input.source capture)));
        Option.iter project ~f:(fun project ->
          captured_bytes
          := checked_add
               ~limit:limits.max_total_captured_bytes
               !captured_bytes
               (Chatml_source.byte_length (Chatml_project.source project)));
        Option.iter selected ~f:(fun selected ->
          captured_bytes
          := checked_add
               ~limit:limits.max_total_captured_bytes
               !captured_bytes
               (Chatml_prepared_program.Accounting.input_bytes
                  (Chatml_prepared_program.accounting
                     (Chatml_project_build.prepared selected))));
        if !captured_bytes > limits.max_total_captured_bytes
        then Error Error.Candidate_limit
        else if
          not
            (Chatmd_parser_input.authored_matches
               lexical_input
               (Chatml_source.contents authored)
               ~checkpoint:(Work.tick work))
        then Error Error.Source_mismatch
        else (
          match parsed with
          | Some parsed
            when not
                   (phys_equal
                      (Prompt.Chat_markdown.Parsed.root_input parsed)
                      lexical_input) -> Error Error.Parsed_input_mismatch
          | _ ->
            let unmatched_capture =
              List.find captured_inputs ~f:(fun capture ->
                Work.tick work ();
                not
                  (List.exists parsed_inputs ~f:(fun input ->
                     Work.tick work ();
                     phys_equal input (Captured_input.input capture))))
            in
            (match unmatched_capture with
             | Some _ -> Error Error.Foreign_import_capture
             | None ->
               if
                 List.exists captured_inputs ~f:(fun capture ->
                   Work.tick work ();
                   let input = Captured_input.input capture in
                   (phys_equal input lexical_input
                    && ((not
                           (Chatml_source.Id.equal
                              (Chatml_source.id (Captured_input.source capture))
                              (Chatml_source.id authored)))
                        || not
                             (Chatml_source.Digest.equal
                                (Chatml_source.digest (Captured_input.source capture))
                                (Chatml_source.digest authored))))
                   || List.count captured_inputs ~f:(fun other ->
                        Work.tick work ();
                        phys_equal input (Captured_input.input other))
                      <> 1)
               then Error Error.Foreign_import_capture
               else if
                 List.exists parsed_inputs ~f:(fun input ->
                   Work.tick work ();
                   not
                     (List.exists captured_inputs ~f:(fun capture ->
                        Work.tick work ();
                        phys_equal input (Captured_input.input capture))))
               then Error Error.Missing_import_capture
               else if Option.is_some selected && Option.is_none project
               then Error Error.Selected_without_project
               else if
                 Option.exists selected ~f:(fun selected ->
                   match
                     Chatml_prepared_program.provenance
                       (Chatml_project_build.prepared selected)
                   with
                   | Chatml_prepared_program.Provenance.Project { declaration; _ } ->
                     not
                       (Option.exists project ~f:(fun project ->
                          let source = Chatml_project.source project in
                          Chatml_source.Id.equal declaration (Chatml_source.id source)
                          && Option.exists
                               (Chatml_prepared_program.find_source
                                  (Chatml_project_build.prepared selected)
                                  declaration)
                               ~f:(fun selected_source ->
                                 Chatml_source.Digest.equal
                                   (Chatml_source.digest selected_source)
                                   (Chatml_source.digest source))))
                   | Implicit -> true)
               then Error Error.Selected_project_mismatch
               else (
                 let selected_prepared =
                   Option.map selected ~f:Chatml_project_build.prepared
                 in
                 let valid_route path route =
                   Chatml_source.Id.equal
                     route.Path_candidate.authored_base_id
                     (Chatml_source.id authored)
                   && Chatml_source.Digest.equal
                        route.authored_base_digest
                        (Chatml_source.digest authored)
                   && String.equal
                        (Path_candidate.Edit_spelling.to_string route.edit_spelling)
                        (Path_candidate.Edit_spelling.to_string
                           (Path_candidate.edit_spelling path))
                 in
                 let invalid_path =
                   List.exists paths ~f:(fun path ->
                     Work.tick work ();
                     match Path_candidate.origin path with
                     | Admitted_root _ -> false
                     | Project_source { project = declared; route } ->
                       (not
                          (Option.exists project ~f:(fun actual ->
                             phys_equal actual declared)))
                       || not (valid_route path route)
                     | Decoded_member { project = declared; role; member; route } ->
                       (not
                          (Option.exists project ~f:(fun actual ->
                             phys_equal actual declared)))
                       || (not (valid_route path route))
                       || not
                            (match
                               Chatml_project.Declared_paths.contains
                                 declared
                                 ~role
                                 ~member
                                 ~max_entries:limits.max_paths
                                 ~checkpoint:(Work.tick work)
                             with
                             | Ok true -> true
                             | Ok false | Error _ -> false)
                     | Selected_closure { prepared; source_id; route } ->
                       (not
                          (Option.exists selected_prepared ~f:(fun actual ->
                             phys_equal actual prepared)))
                       || (not (valid_route path route))
                       || Option.is_none
                            (Chatml_prepared_program.find_source prepared source_id))
                 in
                 if invalid_path
                 then Error Error.Invalid_path_provenance
                 else
                   Ok
                     { request
                     ; authored
                     ; lexical_input
                     ; parsed
                     ; captured_inputs
                     ; project
                     ; selected
                     ; paths
                     ; selected_tools
                     ; limits
                     ; remaining_work = Work.remaining work
                     }))))
    with
    | Capture_limit -> Error Error.Candidate_limit
    | Work.Exhausted -> Error Error.Work_limit
  ;;
end
