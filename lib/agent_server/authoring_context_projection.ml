open! Core
module P = Agent_protocol
module Ledger = Chatml_authoring_context
module Captured = Chatml_source

let source_metadata ledger identity source =
  let editor_revision =
    match Ledger.version ledger ~identity ~source:(Captured.id source) with
    | Some version when Ledger.Version.overlay_open version ->
      Option.map (Ledger.Version.editor_revision version) ~f:Ledger.Editor_revision.to_int
    | Some _ | None -> None
  in
  P.Authoring.Source.create
    ~id:(Captured.id source)
    ~digest:(Captured.digest source)
    ~byte_length:(Captured.byte_length source)
    ~editor_revision
  |> Result.map_error ~f:(fun _ -> ())
;;

let basis_fingerprint ~origin ~generation ~basis ~sources ~checkpoint =
  let feed context value =
    let prefix = Int.to_string (String.length value) ^ ":" in
    let context = Digestif.SHA256.feed_string context prefix in
    Digestif.SHA256.feed_string context value
  in
  let context =
    Digestif.SHA256.empty
    |> fun context ->
    feed context "ochat-authoring-context-basis-v1"
    |> fun context ->
    feed context (Sexp.to_string (P.Authoring.Context.sexp_of_origin origin))
    |> fun context ->
    feed context (Int.to_string generation)
    |> fun context ->
    feed context (Sexp.to_string (P.Authoring.Context.sexp_of_basis basis))
  in
  let context =
    List.fold sources ~init:context ~f:(fun context source ->
      checkpoint ();
      let id = P.Authoring.Source.id source in
      context
      |> fun context ->
      feed context (Captured.Id.package_namespace id)
      |> fun context ->
      feed context (Captured.Id.relative_path id)
      |> fun context ->
      feed context (Captured.Digest.to_hex (P.Authoring.Source.digest source))
      |> fun context ->
      feed context (Int.to_string (P.Authoring.Source.byte_length source))
      |> fun context ->
      feed
        context
        (match P.Authoring.Source.editor_revision source with
         | None -> "base"
         | Some revision -> Int.to_string revision))
  in
  Digestif.SHA256.(get context |> to_hex)
  |> Captured.Digest.of_hex
  |> Result.map_error ~f:(fun _ -> ())
;;

let within_source_limits
      ~max_sources
      ~max_source_bytes
      ~max_context_source_bytes
      sources
      ~checkpoint
  =
  let count = ref 0 in
  let bytes = ref 0 in
  List.for_all sources ~f:(fun source ->
    checkpoint ();
    let length = Captured.byte_length source in
    if
      !count >= max_sources
      || length > max_source_bytes
      || length > max_context_source_bytes - !bytes
    then false
    else (
      count := !count + 1;
      bytes := !bytes + length;
      true))
;;

let view_of_context
      ~host
      ~max_sources
      ~max_source_bytes
      ~max_context_source_bytes
      ledger
      ~id
      ~identity
      ~origin
      ~checkpoint
  =
  let open Result.Let_syntax in
  match Ledger.basis ledger ~identity with
  | None -> Error ()
  | Some Ledger.Basis.Project ->
    let%bind inputs =
      Ledger.inputs ledger ~identity ~checkpoint |> Result.map_error ~f:(fun _ -> ())
    in
    let captured_sources = Ledger.Inputs.sources inputs in
    let%bind () =
      if
        within_source_limits
          ~max_sources
          ~max_source_bytes
          ~max_context_source_bytes
          captured_sources
          ~checkpoint
      then Ok ()
      else Error ()
    in
    let%bind view = Ledger.find ledger ~identity |> Result.of_option ~error:() in
    let%bind sources =
      List.map captured_sources ~f:(source_metadata ledger identity) |> Result.all
    in
    let basis =
      P.Authoring.Context.Project
        { base_graph = Ledger.View.base_graph view
        ; selection =
            { environment = Ledger.View.environment view
            ; compiler_contract = Ledger.View.compiler_contract view
            }
        }
    in
    let generation = Ledger.Generation.to_int (Ledger.View.generation view) in
    let%bind basis_fingerprint =
      basis_fingerprint ~origin ~generation ~basis ~sources ~checkpoint
    in
    let editable_sources =
      List.filter sources ~f:(fun source ->
        Option.is_some (P.Authoring.Source.editor_revision source))
    in
    P.Authoring.Context.create
      ~id
      ~host
      ~origin
      ~generation
      ~basis
      ~basis_fingerprint
      ~editable_sources
    |> Result.map_error ~f:(fun _ -> ())
  | Some Ledger.Basis.Document ->
    let%bind inputs =
      Ledger.document_inputs ledger ~identity ~checkpoint
      |> Result.map_error ~f:(fun _ -> ())
    in
    let captured_source = Ledger.Document.Inputs.source inputs in
    let charged_sources =
      captured_source
      :: Option.value_map
           (Ledger.Document.Inputs.static_build inputs)
           ~default:[]
           ~f:(fun build ->
             Chatml_prepared_program.sources (Chatml_project_build.prepared build))
    in
    let%bind () =
      if
        within_source_limits
          ~max_sources
          ~max_source_bytes
          ~max_context_source_bytes
          charged_sources
          ~checkpoint
      then Ok ()
      else Error ()
    in
    let%bind view = Ledger.find_document ledger ~identity |> Result.of_option ~error:() in
    let%bind source = source_metadata ledger identity captured_source in
    let selection =
      match
        ( Ledger.Document.View.selected_environment view
        , Ledger.Document.View.compiler_contract view )
      with
      | Some environment, Some compiler_contract ->
        Some P.Authoring.Context.{ environment; compiler_contract }
      | None, None | Some _, None | None, Some _ -> None
    in
    let basis = P.Authoring.Context.Document { source; selection } in
    let generation = Ledger.Generation.to_int (Ledger.Document.View.generation view) in
    let%bind basis_fingerprint =
      basis_fingerprint ~origin ~generation ~basis ~sources:[ source ] ~checkpoint
    in
    let editable_sources =
      if Option.is_some (P.Authoring.Source.editor_revision source)
      then [ source ]
      else []
    in
    P.Authoring.Context.create
      ~id
      ~host
      ~origin
      ~generation
      ~basis
      ~basis_fingerprint
      ~editable_sources
    |> Result.map_error ~f:(fun _ -> ())
;;

let response_fits ~max_response_bytes ~max_protocol_bytes response =
  let length =
    String.length (Jsonaf.to_string (P.Authoring.Wire.Response.to_json response))
  in
  length <= max_response_bytes && length <= max_protocol_bytes
;;
