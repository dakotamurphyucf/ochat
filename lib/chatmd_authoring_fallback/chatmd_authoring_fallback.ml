open Core
module Captured = Chatml_source
module Fallback = Chatml_authoring_fallback
module Script = Chatmd_authoring_analysis.Script
module Context = Chatml_authoring_context
module Input = Chatmd_parser_input
module Extraction = Prompt.Chat_markdown.Declaration.Extraction

module Limits = struct
  type t =
    { max_source_bytes : int
    ; max_identity_bytes : int
    ; max_work_steps : int
    }

  let create ~max_source_bytes ~max_identity_bytes ~max_work_steps =
    if max_source_bytes <= 0 || max_identity_bytes <= 0 || max_work_steps <= 0
    then Or_error.error_string "Invalid ChatMD fallback limits"
    else Ok { max_source_bytes; max_identity_bytes; max_work_steps }
  ;;
end

module Error = struct
  type t =
    | Stale_document
    | Stale_selected_build
    | Foreign_authored_input
    | Work_limit

  let sexp_of_t = function
    | Stale_document -> Sexp.Atom "stale_document"
    | Stale_selected_build -> Sexp.Atom "stale_selected_build"
    | Foreign_authored_input -> Sexp.Atom "foreign_authored_input"
    | Work_limit -> Sexp.Atom "work_limit"
  ;;
end

module Origin = struct
  type admission =
    | Ready of Fallback.Origin.t
    | Unavailable

  exception Work_limit

  let admit script ~current ~authored_input ~authored_source ~selected ~limits ~checkpoint
    =
    let remaining = ref limits.Limits.max_work_steps in
    let charge units =
      if units < 0 || units > !remaining then raise Work_limit;
      remaining := !remaining - units;
      checkpoint ()
    in
    let equal_id left right =
      let charge_id id =
        let namespace_bytes = String.length (Captured.Id.package_namespace id) in
        let member_bytes = String.length (Captured.Id.relative_path id) in
        if
          namespace_bytes > limits.max_identity_bytes
          || member_bytes > limits.max_identity_bytes - namespace_bytes
        then raise Work_limit;
        charge namespace_bytes;
        charge member_bytes
      in
      charge_id left;
      charge_id right;
      Captured.Id.equal left right
    in
    let same_capture left right =
      equal_id (Captured.id left) (Captured.id right)
      && Captured.Digest.equal (Captured.digest left) (Captured.digest right)
      && Int.equal (Captured.byte_length left) (Captured.byte_length right)
    in
    try
      charge 1;
      let stamp = Script.document_stamp script in
      let current_source = Context.Document.Inputs.source current in
      let parser_bytes = Input.authored_byte_length authored_input in
      if
        Captured.byte_length current_source > limits.max_source_bytes
        || Captured.byte_length authored_source > limits.max_source_bytes
        || parser_bytes > limits.max_source_bytes
      then raise Work_limit;
      if
        (not
           (Context.Identity.equal
              (Chatmd_authoring_analysis.Document_stamp.identity stamp)
              (Context.Document.Inputs.identity current)))
        || (not
              (Context.Generation.equal
                 (Chatmd_authoring_analysis.Document_stamp.generation stamp)
                 (Context.Document.Inputs.generation current)))
        || (not
              (equal_id
                 (Chatmd_authoring_analysis.Document_stamp.document_source stamp)
                 (Captured.id current_source)))
        || not
             (Captured.Digest.equal
                (Chatmd_authoring_analysis.Document_stamp.document_digest stamp)
                (Captured.digest current_source))
      then Error Error.Stale_document
      else if
        not
          (Captured.Digest.equal
             (Chatml_prepared_program.fingerprint (Script.selected_prepared script))
             (Chatml_prepared_program.fingerprint
                (Fallback.Selected_contract.prepared selected)))
      then Error Error.Stale_selected_build
      else if not (phys_equal authored_input (Script.declaration_input script))
      then Ok Unavailable
      else if
        phys_equal authored_input (Script.root_input script)
        && not (same_capture current_source authored_source)
      then Error Error.Stale_document
      else if
        (not (phys_equal authored_input (Script.root_input script)))
        && not (Input.Role.equal (Input.role authored_input) Input.Role.Import)
      then Ok Unavailable
      else if
        charge parser_bytes;
        not
          (Input.authored_matches
             authored_input
             (Captured.contents authored_source)
             ~checkpoint)
      then Error Error.Foreign_authored_input
      else (
        let compiled = Script.selected_source script in
        let map =
          Fallback.Site_map.
            { compiled_point =
                (fun authored_byte ~checkpoint ->
                  match
                    Script.authored_point
                      script
                      ~input:authored_input
                      ~authored_byte
                      ~checkpoint
                  with
                  | Error _ -> Error Fallback.Error.Invalid_projection
                  | Ok (Extraction.Authored_point.Unavailable _) -> Ok None
                  | Ok (Exact_compiled_byte offset) ->
                    Captured.locate
                      compiled
                      ~start_byte:offset
                      ~end_byte:offset
                      ~checkpoint
                    |> Result.map ~f:Option.some
                    |> Result.map_error ~f:(fun _ -> Fallback.Error.Invalid_projection))
            ; authored_replacement =
                (fun location ~checkpoint ->
                  match
                    Chatmd_authoring_analysis.Projection.of_location
                      script
                      location
                      ~checkpoint
                  with
                  | Error _ -> Error Fallback.Error.Invalid_projection
                  | Ok (Some (Extraction.Exact_inline span))
                    when phys_equal (Extraction.input span) authored_input ->
                    Captured.locate
                      authored_source
                      ~start_byte:(Extraction.start_byte span)
                      ~end_byte:(Extraction.end_byte span)
                      ~checkpoint
                    |> Result.map ~f:Option.some
                    |> Result.map_error ~f:(fun _ -> Fallback.Error.Invalid_projection)
                  | Ok
                      (Some
                         (Exact_inline _ | External_origin _ | Approximate_declaration _))
                  | Ok None -> Ok None)
            }
        in
        Ok (Ready (Fallback.Origin.mapped ~compiled ~authored:authored_source ~map)))
    with
    | Work_limit -> Error Error.Work_limit
  ;;
end
