open! Core
module P = Agent_protocol
module A = P.Authoring.Analysis
module Ledger = Chatml_authoring_context
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Semantics = Chatml_semantics
module Sites = Chatml.Chatml_syntax_sites
module Input = Chatmd_parser_input

module Error = struct
  type t =
    | Invalid_stamp
    | Invalid_site
    | Wire_limit
    | Projection_limit
end

let wire ~error result = Result.map_error result ~f:(fun _ -> error)

let within_host_limit response ~max_bytes =
  match A.Analyze.within_bytes response ~max_bytes with
  | Ok () -> Ok ()
  | Error _ -> Error Error.Wire_limit
;;

let source ~contexts ~identity source =
  let editor_revision =
    Ledger.version contexts ~identity ~source:(Captured.id source)
    |> Option.bind ~f:(fun version ->
      if Captured.Digest.equal (Ledger.Version.digest version) (Captured.digest source)
      then
        Option.map
          (Ledger.Version.editor_revision version)
          ~f:Ledger.Editor_revision.to_int
      else None)
  in
  P.Authoring.Source.create
    ~id:(Captured.id source)
    ~digest:(Captured.digest source)
    ~byte_length:(Captured.byte_length source)
    ~editor_revision
  |> wire ~error:Error.Invalid_stamp
;;

let range ~contexts ~identity ~source:owner location =
  if
    (not (Captured.Id.equal (Captured.id owner) (Captured.Location.source location)))
    || not
         (Captured.Digest.equal
            (Captured.digest owner)
            (Captured.Location.source_digest location))
  then Error Error.Invalid_site
  else
    let open Result.Let_syntax in
    let%bind source = source ~contexts ~identity owner in
    let span = Captured.Location.span location in
    A.Byte_range.create ~source ~start_byte:span.left.offset ~end_byte:span.right.offset
    |> wire ~error:Error.Invalid_site
;;

let native_site ~contexts ~identity site =
  let open Result.Let_syntax in
  let%bind native =
    range
      ~contexts
      ~identity
      ~source:(Diagnostic.Site.source site)
      (Diagnostic.Site.location site)
  in
  A.Site.create ~native ~projection:None |> wire ~error:Error.Invalid_site
;;

let valid_utf8 text ~checkpoint =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String text) in
  let rec loop () =
    checkpoint ();
    match Uutf.decode decoder with
    | `Uchar _ -> loop ()
    | `End -> true
    | `Malformed _ -> false
    | `Await -> assert false
  in
  loop ()
;;

(** A bounded display of exact raw bytes. Invalid UTF-8 and text above the
    wire's 4096-byte field ceiling are visibly escaped; the native diagnostic
    stays untouched in the checked result. No replacement character conceals
    which byte was present. *)
let display_text text ~checkpoint =
  let maximum = A.Limits.max_display_bytes in
  if String.length text <= maximum && valid_utf8 text ~checkpoint
  then text, false
  else (
    let prefix = "[escaped bytes] " in
    let suffix = " [display truncated]" in
    let buffer = Buffer.create maximum in
    Buffer.add_string buffer prefix;
    let rec append offset =
      checkpoint ();
      if offset >= String.length text
      then false
      else (
        let byte = Char.to_int text.[offset] in
        let extra = if byte >= 32 && byte <= 126 && byte <> 92 then 1 else 4 in
        if Buffer.length buffer + extra + String.length suffix > maximum
        then true
        else (
          if extra = 1
          then Buffer.add_char buffer text.[offset]
          else (
            let digits = "0123456789ABCDEF" in
            Buffer.add_string buffer "\\x";
            Buffer.add_char buffer digits.[byte lsr 4];
            Buffer.add_char buffer digits.[byte land 15]);
          append (offset + 1)))
    in
    let truncated = append 0 in
    if truncated then Buffer.add_string buffer suffix;
    Buffer.contents buffer, truncated)
;;

let diagnostic ~site ~related ~message ~checkpoint =
  let open Result.Let_syntax in
  let message, message_truncated = display_text message ~checkpoint in
  let%bind related, related_truncated =
    List.fold_result
      related
      ~init:([], false)
      ~f:(fun (reversed, truncated) (site, text) ->
        let rendered, clipped = display_text text ~checkpoint in
        let%map item =
          A.Diagnostic.Related.create ~site ~message:rendered
          |> wire ~error:Error.Invalid_site
        in
        item :: reversed, truncated || clipped)
  in
  let%map result =
    A.Diagnostic.create ~message ~site ~related:(List.rev related)
    |> wire ~error:Error.Invalid_site
  in
  result, message_truncated || related_truncated
;;

let convert_diagnostics checked ~policy ~checkpoint ~convert =
  let page = Project.Diagnostic_projection.project checked ~policy ~checkpoint in
  let rec visit count reversed clipped = function
    | [] -> Ok (List.rev reversed, page.truncated || clipped)
    | _ :: _ when count >= A.Limits.max_diagnostics -> Ok (List.rev reversed, true)
    | projected :: tail ->
      let open Result.Let_syntax in
      checkpoint ();
      let%bind primary, related, sites_truncated = convert projected in
      let native = Project.Diagnostic_projection.native projected in
      let%bind item, text_truncated =
        diagnostic ~site:primary ~related ~message:native.message ~checkpoint
      in
      visit
        (count + 1)
        (item :: reversed)
        (clipped || sites_truncated || text_truncated)
        tail
  in
  visit 0 [] false page.items
;;

let project_diagnostic ~contexts ~identity ~checkpoint projected =
  let open Result.Let_syntax in
  let%bind primary =
    match Project.Diagnostic_projection.primary projected with
    | None -> Ok None
    | Some site ->
      native_site ~contexts ~identity (Project.Site_projection.native site)
      |> Result.map ~f:Option.some
  in
  let related = Project.Diagnostic_projection.related projected in
  let%map related =
    List.fold_result related ~init:[] ~f:(fun reversed (item, site) ->
      let%map site =
        native_site ~contexts ~identity (Project.Site_projection.native site)
      in
      (site, Diagnostic.Related.message item) :: reversed)
    |> Result.map ~f:List.rev
  in
  primary, related, Project.Diagnostic_projection.sites_truncated projected
;;

let syntax_role = function
  | Sites.Role.Value_reference -> A.Syntax_site.Value_reference
  | Value_binder -> Value_binder
  | Module_binder -> Module_binder
  | Module_reference -> Module_reference
  | Type_binder -> Type_binder
  | Type_parameter_binder -> Type_parameter_binder
  | Type_reference -> Type_reference
  | Signature_value_binder -> Signature_value_binder
  | Field_label -> Field_label
  | Variant_tag -> Variant_tag
  | Constraint_name -> Constraint_name
  | Constraint_parameter_reference -> Constraint_parameter_reference
;;

module Syntax_prefix = struct
  let collect ~max_sites ~units ~sites ~checkpoint ~convert =
    let rec visit_units count reversed = function
      | [] -> Ok (List.rev reversed, false)
      | unit :: tail ->
        checkpoint ();
        visit_sites count reversed unit tail (sites unit)
    and visit_sites count reversed unit units = function
      | [] -> visit_units count reversed units
      | _ :: _ when count >= max_sites -> Ok (List.rev reversed, true)
      | site :: tail ->
        let open Result.Let_syntax in
        checkpoint ();
        let%bind row = convert unit site in
        visit_sites (count + 1) (row :: reversed) unit units tail
    in
    visit_units 0 [] units
  ;;
end

let syntax ~contexts ~identity partial ~checkpoint =
  let clipped_text = ref false in
  let convert unit site =
    let open Result.Let_syntax in
    let owner = Semantics.Partial.Syntax.source unit in
    let%bind site_range =
      range ~contexts ~identity ~source:owner (Sites.Site.location site)
    in
    let%bind wire_site =
      A.Site.create ~native:site_range ~projection:None |> wire ~error:Error.Invalid_site
    in
    let name, truncated = display_text (Sites.Site.name site) ~checkpoint in
    if truncated then clipped_text := true;
    A.Syntax_site.create ~role:(syntax_role (Sites.Site.role site)) ~name ~site:wire_site
    |> wire ~error:Error.Invalid_site
  in
  let open Result.Let_syntax in
  let%map sites, clipped_sites =
    Syntax_prefix.collect
      ~max_sites:A.Limits.max_syntax_sites
      ~units:(Semantics.Partial.parsed partial)
      ~sites:(fun unit -> Sites.sites (Semantics.Partial.Syntax.sites unit))
      ~checkpoint
      ~convert
  in
  sites, clipped_sites || !clipped_text
;;

let failure_source partial ~contexts ~identity source_id =
  match
    List.find (Semantics.Partial.sources partial) ~f:(fun item ->
      Captured.Id.equal (Captured.id item) source_id)
  with
  | None -> Error Error.Invalid_stamp
  | Some captured -> source ~contexts ~identity captured
;;

let failure_phase partial ~contexts ~identity =
  let open Result.Let_syntax in
  match Semantics.Partial.invalidity partial with
  | Parse { source = source_id; phase } ->
    let%map source = failure_source partial ~contexts ~identity source_id in
    let phase : A.Analyze.parser_phase =
      match phase with
      | Chatml_configured_lookup.Phase.Implementation -> A.Analyze.Implementation
      | Interface -> A.Analyze.Interface
    in
    A.Analyze.Parse { source; phase }
  | Implementation source_id ->
    let%map source = failure_source partial ~contexts ~identity source_id in
    A.Analyze.Implementation source
  | Companion source_id ->
    let%map source = failure_source partial ~contexts ~identity source_id in
    A.Analyze.Companion source
  | Host_requirements -> Ok A.Analyze.Host_requirements
  | Dependency -> Ok A.Analyze.Dependency
;;

let result ~contexts ~identity ~capture ~checked ~snapshot ~policy ~convert ~checkpoint =
  let open Result.Let_syntax in
  let%bind diagnostics, truncated_diagnostics =
    convert_diagnostics checked ~policy ~checkpoint ~convert
  in
  match Project.Checked.status checked, snapshot with
  | Complete _, Some snapshot ->
    A.Analyze.complete
      ~capture
      ~snapshot
      ~validated_graph:(Project.Checked.input_fingerprint checked)
      ~diagnostics
      ~truncated_diagnostics
    |> wire ~error:Error.Wire_limit
  | Failed partial, None ->
    let%bind phase = failure_phase partial ~contexts ~identity in
    let%bind syntax, truncated_syntax = syntax ~contexts ~identity partial ~checkpoint in
    A.Analyze.failed
      ~capture
      ~phase
      ~syntax
      ~diagnostics
      ~truncated_syntax
      ~truncated_diagnostics
    |> wire ~error:Error.Wire_limit
  | Complete _, None | Failed _, Some _ -> Error Error.Invalid_stamp
;;

let project_capture ~expected ~stamp =
  A.Capture.create
    ~context:expected
    ~target:A.Target.Project_input
    ~input_fingerprint:(Project.Project_stamp.input_fingerprint stamp)
    ~environment:
      (Chatml_prepared_program.Environment.name (Project.Project_stamp.environment stamp))
    ~required_contract:(Project.Project_stamp.required_contract stamp)
    ~selected_contract:(Project.Project_stamp.selected_contract stamp)
  |> wire ~error:Error.Invalid_stamp
;;

let project ~contexts ~expected ~stamp ~checked ~snapshot ~policy ~checkpoint =
  let open Result.Let_syntax in
  if
    (not
       (Captured.Digest.equal
          (Project.Project_stamp.input_fingerprint stamp)
          (Project.Checked.input_fingerprint checked)))
    || (not
          (Captured.Digest.equal
             (Project.Project_stamp.required_contract stamp)
             (Project.Checked.required_contract checked)))
    || not
         (Captured.Digest.equal
            (Project.Project_stamp.selected_contract stamp)
            (Project.Checked.selected_contract checked))
  then Error Error.Invalid_stamp
  else (
    let%bind capture = project_capture ~expected ~stamp in
    let identity = Project.Project_stamp.identity stamp in
    result
      ~contexts
      ~identity
      ~capture
      ~checked
      ~snapshot
      ~policy
      ~convert:(project_diagnostic ~contexts ~identity ~checkpoint)
      ~checkpoint)
;;

let document_script_capture ~contexts ~expected ~source:document_source ~script =
  let open Result.Let_syntax in
  let stamp = Document.Script.document_stamp script in
  let selected_source = Document.Script.selected_source script in
  if
    (not
       (Captured.Id.equal
          (Captured.id document_source)
          (Document.Document_stamp.document_source stamp)))
    || (not
          (Captured.Digest.equal
             (Captured.digest document_source)
             (Document.Document_stamp.document_digest stamp)))
    || (not
          (Captured.Id.equal
             (Captured.id selected_source)
             (Document.Document_stamp.selected_script_source stamp)))
    || not
         (Captured.Digest.equal
            (Captured.digest selected_source)
            (Document.Document_stamp.selected_script_digest stamp))
  then Error Error.Invalid_stamp
  else (
    let identity = Document.Document_stamp.identity stamp in
    let%bind document_source = source ~contexts ~identity document_source in
    let%bind selected_source = source ~contexts ~identity selected_source in
    let%bind parser_root_fingerprint =
      Captured.Digest.of_hex (Document.Document_stamp.parser_input_fingerprint stamp)
      |> Result.map_error ~f:(fun _ -> Error.Invalid_stamp)
    in
    let%bind declaration_input_fingerprint =
      Captured.Digest.of_hex (Document.Document_stamp.declaration_input_fingerprint stamp)
      |> Result.map_error ~f:(fun _ -> Error.Invalid_stamp)
    in
    let%bind target =
      A.Target.script_stamp
        ~document_source
        ~parser_root_fingerprint
        ~script_id:(Document.Document_stamp.script_id stamp)
        ~declaration_input_fingerprint
        ~declaration_input_byte_length:
          (Document.Document_stamp.declaration_input_byte_length stamp)
        ~declaration_start_byte:(Document.Document_stamp.declaration_start_byte stamp)
        ~declaration_end_byte:(Document.Document_stamp.declaration_end_byte stamp)
        ~selected_script_source:selected_source
      |> wire ~error:Error.Invalid_stamp
    in
    A.Capture.create
      ~context:expected
      ~target:(A.Target.Script_input target)
      ~input_fingerprint:(Document.Document_stamp.input_fingerprint stamp)
      ~environment:
        (Chatml_prepared_program.Environment.name
           (Document.Document_stamp.environment stamp))
      ~required_contract:(Document.Document_stamp.required_contract stamp)
      ~selected_contract:(Document.Document_stamp.selected_contract stamp)
    |> wire ~error:Error.Invalid_stamp)
;;

let document_capture ~contexts ~expected ~source ~script ~candidate =
  if
    not
      (Document.Document_stamp.equal
         (Document.Candidate.stamp candidate)
         (Document.Script.document_stamp script))
  then Error Error.Invalid_stamp
  else document_script_capture ~contexts ~expected ~source ~script
;;

let document
      ~contexts
      ~expected
      ~source:document_source
      ~script
      ~candidate
      ~input_sources
      ~snapshot
      ~policy
      ~checkpoint
  =
  let open Result.Let_syntax in
  let identity = Document.Document_stamp.identity (Document.Candidate.stamp candidate) in
  let checked = Document.Candidate.checked candidate in
  let%bind capture =
    document_capture ~contexts ~expected ~source:document_source ~script ~candidate
  in
  let source_of_input input =
    List.find_map input_sources ~f:(fun (candidate, source) ->
      checkpoint ();
      if phys_equal input candidate then Some source else None)
  in
  let authored_range input ~start_byte ~end_byte =
    match source_of_input input with
    | None -> Error Error.Invalid_site
    | Some captured ->
      if
        not
          (String.equal
             (Input.authored_sha256 input)
             (Captured.Digest.to_hex (Captured.digest captured)))
      then Error Error.Invalid_site
      else (
        let%bind source = source ~contexts ~identity captured in
        A.Byte_range.create ~source ~start_byte ~end_byte
        |> wire ~error:Error.Invalid_site)
  in
  let site_of_projection (projected : Document.Projection.site) =
    let%bind native =
      range
        ~contexts
        ~identity
        ~source:(Diagnostic.Site.source (Project.Site_projection.native projected.native))
        (Diagnostic.Site.location (Project.Site_projection.native projected.native))
    in
    let%bind projection =
      match projected.authored with
      | None -> Ok None
      | Some (Prompt.Chat_markdown.Declaration.Extraction.Exact_inline span) ->
        authored_range
          (Prompt.Chat_markdown.Declaration.Extraction.input span)
          ~start_byte:(Prompt.Chat_markdown.Declaration.Extraction.start_byte span)
          ~end_byte:(Prompt.Chat_markdown.Declaration.Extraction.end_byte span)
        |> Result.map ~f:(fun range -> Some (A.Site.Authored_exact range))
      | Some (External_origin { member; body_digest; start_byte; end_byte }) ->
        Ok (Some (A.Site.External_origin { member; body_digest; start_byte; end_byte }))
      | Some (Approximate_declaration range) ->
        let input = Input.Range.input range in
        (match source_of_input input with
         | Some captured
           when String.equal
                  (Input.effective_sha256 input)
                  (Captured.Digest.to_hex (Captured.digest captured)) ->
           (match
              authored_range
                input
                ~start_byte:(Input.Range.start_byte range)
                ~end_byte:(Input.Range.end_byte range)
            with
            | Ok range -> Ok (Some (A.Site.Approximate_declaration range))
            | Error _ -> Ok None)
         | Some _ | None -> Ok None)
    in
    A.Site.create ~native ~projection |> wire ~error:Error.Invalid_site
  in
  let convert projected =
    let%bind decorated =
      Document.Projection.of_diagnostic candidate projected ~checkpoint
      |> Result.map_error ~f:(fun _ -> Error.Projection_limit)
    in
    let%bind primary =
      match Document.Projection.primary decorated with
      | None -> Ok None
      | Some site -> site_of_projection site |> Result.map ~f:Option.some
    in
    let related = Document.Projection.related decorated in
    let%map related =
      List.fold_result related ~init:[] ~f:(fun reversed (item, site) ->
        let%map site = site_of_projection site in
        (site, Diagnostic.Related.message item) :: reversed)
      |> Result.map ~f:List.rev
    in
    primary, related, Document.Projection.sites_truncated decorated
  in
  result ~contexts ~identity ~capture ~checked ~snapshot ~policy ~convert ~checkpoint
;;
