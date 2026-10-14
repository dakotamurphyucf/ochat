open Core
module Context = Chatml_authoring_context
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Markdown = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module Analysis = Chatml_authoring_analysis

module Unavailable = struct
  type t =
    | No_selected_script
    | Incomplete_or_malformed_document
    | No_validated_project
    | Dynamic_only_source
  [@@deriving sexp_of]
end

module Error = struct
  type t =
    | Invalid_selection
    | Membership_limit
    | Foreign_declaration
    | Ambiguous_declaration
    | Stale_document
    | Script_body_mismatch
    | Selection_mismatch
    | Compiler of Analysis.Error.t
    | Projection of Markdown.Declaration.Extraction.error
    | Policy_mismatch
    | Invalid_occupancy
    | Retention_limit

  let sexp_of_t = function
    | Invalid_selection -> Sexp.Atom "invalid_selection"
    | Membership_limit -> Sexp.Atom "membership_limit"
    | Foreign_declaration -> Sexp.Atom "foreign_declaration"
    | Ambiguous_declaration -> Sexp.Atom "ambiguous_declaration"
    | Stale_document -> Sexp.Atom "stale_document"
    | Script_body_mismatch -> Sexp.Atom "script_body_mismatch"
    | Selection_mismatch -> Sexp.Atom "selection_mismatch"
    | Compiler error -> Sexp.List [ Sexp.Atom "compiler"; Analysis.Error.sexp_of_t error ]
    | Projection _ -> Sexp.Atom "projection"
    | Policy_mismatch -> Sexp.Atom "policy_mismatch"
    | Invalid_occupancy -> Sexp.Atom "invalid_occupancy"
    | Retention_limit -> Sexp.Atom "retention_limit"
  ;;
end

module Selection = struct
  type t =
    | Validated_build of Build.t
    | No_validated_build
    | Dynamic_only
end

module Document_stamp = struct
  type t =
    { identity : Context.Identity.t
    ; generation : Context.Generation.t
    ; document_source : Captured.Id.t
    ; document_digest : Captured.Digest.t
    ; document_byte_length : int
    ; parser_input_fingerprint : string
    ; script_id : string
    ; declaration_input_fingerprint : string
    ; declaration_input_byte_length : int
    ; declaration_start_byte : int
    ; declaration_end_byte : int
    ; selected_script_source : Captured.Id.t
    ; selected_script_digest : Captured.Digest.t
    ; input_fingerprint : Captured.Digest.t
    ; required_contract : Captured.Digest.t
    ; selected_contract : Captured.Digest.t
    ; environment : Prepared.Environment.t
    }

  let identity t = t.identity
  let generation t = t.generation
  let document_source t = t.document_source
  let document_digest t = t.document_digest
  let parser_input_fingerprint t = t.parser_input_fingerprint
  let script_id t = t.script_id
  let declaration_input_fingerprint t = t.declaration_input_fingerprint
  let declaration_input_byte_length t = t.declaration_input_byte_length
  let declaration_start_byte t = t.declaration_start_byte
  let declaration_end_byte t = t.declaration_end_byte
  let selected_script_source t = t.selected_script_source
  let selected_script_digest t = t.selected_script_digest
  let input_fingerprint t = t.input_fingerprint
  let required_contract t = t.required_contract
  let selected_contract t = t.selected_contract
  let environment t = t.environment

  let equal left right =
    Context.Identity.equal left.identity right.identity
    && Context.Generation.equal left.generation right.generation
    && Captured.Id.equal left.document_source right.document_source
    && Captured.Digest.equal left.document_digest right.document_digest
    && Int.equal left.document_byte_length right.document_byte_length
    && String.equal left.parser_input_fingerprint right.parser_input_fingerprint
    && String.equal left.script_id right.script_id
    && String.equal left.declaration_input_fingerprint right.declaration_input_fingerprint
    && Int.equal left.declaration_input_byte_length right.declaration_input_byte_length
    && Int.equal left.declaration_start_byte right.declaration_start_byte
    && Int.equal left.declaration_end_byte right.declaration_end_byte
    && Captured.Id.equal left.selected_script_source right.selected_script_source
    && Captured.Digest.equal left.selected_script_digest right.selected_script_digest
    && Captured.Digest.equal left.input_fingerprint right.input_fingerprint
    && Captured.Digest.equal left.required_contract right.required_contract
    && Captured.Digest.equal left.selected_contract right.selected_contract
    && String.equal
         (Prepared.Environment.name left.environment)
         (Prepared.Environment.name right.environment)
  ;;

  let sexp_of_t t =
    [%sexp
      { identity = (t.identity : Context.Identity.t)
      ; generation = (t.generation : Context.Generation.t)
      ; document_source = (t.document_source : Captured.Id.t)
      ; document_digest = (t.document_digest : Captured.Digest.t)
      ; document_byte_length = (t.document_byte_length : int)
      ; parser_input_fingerprint = (t.parser_input_fingerprint : string)
      ; script_id = (t.script_id : string)
      ; declaration_input_fingerprint = (t.declaration_input_fingerprint : string)
      ; declaration_input_byte_length = (t.declaration_input_byte_length : int)
      ; declaration_start_byte = (t.declaration_start_byte : int)
      ; declaration_end_byte = (t.declaration_end_byte : int)
      ; selected_script_source = (t.selected_script_source : Captured.Id.t)
      ; selected_script_digest = (t.selected_script_digest : Captured.Digest.t)
      ; input_fingerprint = (t.input_fingerprint : Captured.Digest.t)
      ; required_contract = (t.required_contract : Captured.Digest.t)
      ; selected_contract = (t.selected_contract : Captured.Digest.t)
      ; environment = (Prepared.Environment.name t.environment : string)
      }]
  ;;
end

module Script = struct
  type t =
    { inputs : Context.Document.Inputs.t
    ; parsed : Markdown.Parsed.t
    ; declaration : Markdown.Declaration.t
    ; selected : Build.t
    ; entry_source : Captured.t
    ; extraction : Markdown.Declaration.Extraction.bound
    ; stamp : Document_stamp.t
    }

  type admission =
    | Ready of t
    | Unavailable of Unavailable.t

  let document_stamp t = t.stamp
  let selected_source t = t.entry_source
  let selected_prepared t = Build.prepared t.selected
  let root_input t = Markdown.Parsed.root_input t.parsed
  let declaration_input t = Input.Range.input (Markdown.Declaration.range t.declaration)

  let authored_point t ~input ~authored_byte ~checkpoint =
    Markdown.Declaration.Extraction.authored_point
      t.extraction
      ~input
      ~authored_byte
      ~checkpoint
  ;;

  let assess inputs ~parsed ~script_id ~selection ~max_membership_work_steps ~checkpoint =
    let open Result.Let_syntax in
    if max_membership_work_steps <= 0
    then Error Error.Invalid_selection
    else (
      let remaining = ref max_membership_work_steps in
      let charge () =
        if !remaining <= 0
        then Error Error.Membership_limit
        else (
          decr remaining;
          checkpoint ();
          Ok ())
      in
      match script_id, parsed with
      | None, _ -> Ok (Unavailable Unavailable.No_selected_script)
      | Some _, None -> Ok (Unavailable Unavailable.Incomplete_or_malformed_document)
      | Some script_id, Some parsed ->
        if String.is_empty script_id || String.length script_id > 256
        then Error Error.Invalid_selection
        else (
          let root = Markdown.Parsed.root_input parsed in
          let current = Context.Document.Inputs.source inputs in
          let%bind () = charge () in
          if not (Input.authored_matches root (Captured.contents current) ~checkpoint)
          then Error Error.Stale_document
          else (
            let rec find selected = function
              | [] ->
                (match selected with
                 | None -> Error Error.Foreign_declaration
                 | Some declaration -> Ok declaration)
              | declaration :: tail ->
                let%bind () = charge () in
                if String.equal (Markdown.Declaration.script_id declaration) script_id
                then (
                  match selected with
                  | Some _ -> Error Error.Ambiguous_declaration
                  | None -> find (Some declaration) tail)
                else find selected tail
            in
            let%bind declaration = find None (Markdown.Parsed.declarations parsed) in
            let declaration_range = Markdown.Declaration.range declaration in
            let declaring_input = Input.Range.input declaration_range in
            let rec belongs_to_parse = function
              | [] -> Ok false
              | input :: tail ->
                let%bind () = charge () in
                if phys_equal input declaring_input
                then Ok true
                else belongs_to_parse tail
            in
            let%bind owned = belongs_to_parse (Markdown.Parsed.inputs parsed) in
            if not owned
            then Error Error.Foreign_declaration
            else (
              match selection with
              | Selection.No_validated_build ->
                Ok (Unavailable Unavailable.No_validated_project)
              | Selection.Dynamic_only -> Ok (Unavailable Unavailable.Dynamic_only_source)
              | Selection.Validated_build selected ->
                let prepared = Build.prepared selected in
                let selected_environment = Prepared.environment prepared in
                let requested_environment =
                  Markdown.Declaration.project_selection declaration
                  |> Option.bind ~f:(fun selected -> selected.environment)
                in
                if
                  Option.exists requested_environment ~f:(fun requested ->
                    not
                      (String.equal
                         requested
                         (Prepared.Environment.name selected_environment)))
                then Error Error.Selection_mismatch
                else (
                  let entry_id = Prepared.entry prepared in
                  let rec find_entry = function
                    | [] -> Error Error.Script_body_mismatch
                    | source :: tail ->
                      let%bind () = charge () in
                      if Captured.Id.equal (Captured.id source) entry_id
                      then Ok source
                      else find_entry tail
                  in
                  let%bind entry_source = find_entry (Prepared.sources prepared) in
                  let%bind extraction =
                    Markdown.Declaration.Extraction.bind
                      declaration
                      ~compiled_source:entry_source
                      ~checkpoint
                    |> Result.map_error ~f:(function
                      | Markdown.Declaration.Extraction.Stale_script_source ->
                        Error.Script_body_mismatch
                      | (Invalid_compiler_location | Invalid_authored_point) as error ->
                        Error.Projection error)
                  in
                  let stamp =
                    { Document_stamp.identity = Context.Document.Inputs.identity inputs
                    ; generation = Context.Document.Inputs.generation inputs
                    ; document_source = Captured.id current
                    ; document_digest = Captured.digest current
                    ; document_byte_length = Captured.byte_length current
                    ; parser_input_fingerprint = Input.identity_digest root ~checkpoint
                    ; script_id
                    ; declaration_input_fingerprint =
                        Input.identity_digest declaring_input ~checkpoint
                    ; declaration_input_byte_length =
                        Input.effective_byte_length declaring_input
                    ; declaration_start_byte = Input.Range.start_byte declaration_range
                    ; declaration_end_byte = Input.Range.end_byte declaration_range
                    ; selected_script_source = Captured.id entry_source
                    ; selected_script_digest = Captured.digest entry_source
                    ; input_fingerprint = Prepared.fingerprint prepared
                    ; required_contract = Prepared.compiler_contract prepared
                    ; selected_contract = Prepared.compiler_contract prepared
                    ; environment = selected_environment
                    }
                  in
                  Ok
                    (Ready
                       { inputs
                       ; parsed
                       ; declaration
                       ; selected
                       ; entry_source
                       ; extraction
                       ; stamp
                       }))))))
  ;;
end
