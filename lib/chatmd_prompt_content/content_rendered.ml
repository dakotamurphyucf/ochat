open Core
module B = Content_budget
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Occurrence = Prompt.Chat_markdown.Document_occurrence
module Member = Source.Portable_path
module C = Webpage_markdown.Bounded_conversion

module Source = struct
  type t =
    | Local of
        { member : Member.t
        ; contents : string
        }
    | Builtin of Builtin_instructions.Asset.t
end

module Provenance = struct
  type t =
    | Local of Member.t
    | Builtin of string
end

type t =
  { root : Input.t
  ; occurrence : Occurrence.t
  ; provenance : Provenance.t
  ; input_digest : string
  ; output_digest : string
  ; input_bytes : int
  ; contents : string
  ; accounting : B.Accounting.t
  }

let root t = t.root
let occurrence t = t.occurrence
let provenance t = t.provenance
let input_digest t = t.input_digest
let output_digest t = t.output_digest
let input_bytes t = t.input_bytes
let contents t = t.contents
let accounting t = t.accounting

let create parsed ~occurrence ~source ~limits ~checkpoint =
  let open Result.Let_syntax in
  let range = Occurrence.range occurrence in
  let invalid message = Error (B.Error.invalid ~range message) in
  let budget = B.create limits ~checkpoint in
  let%bind _, found =
    List.fold_result
      (Parsed.document_occurrences parsed)
      ~init:(0, false)
      ~f:(fun (count, found) actual ->
        let%bind () = B.work budget 1 in
        if count >= B.Limits.occurrences limits
        then invalid "document occurrence limit"
        else Ok (count + 1, found || phys_equal occurrence actual))
  in
  let%bind () = if found then Ok () else invalid "foreign document occurrence" in
  let%bind _ =
    Member.create (Input.path (Parsed.root_input parsed))
    |> Result.map_error ~f:(fun _ ->
      B.Error.invalid ~range "nonportable registering Root")
  in
  let%bind base =
    Member.create (Input.path (Input.Range.input range))
    |> Result.map_error ~f:(fun _ -> B.Error.invalid ~range "nonportable document input")
  in
  let%bind provenance, contents =
    match Occurrence.selection occurrence, source with
    | Local_capture, Source.Local { member; contents } ->
      let%bind () =
        B.work
          budget
          (String.length (Member.to_string base)
           + String.length (Occurrence.src occurrence))
      in
      let%bind expected =
        Source_loader.resolve_member
          base
          ~reference:(Occurrence.src occurrence)
          ~max_reference_bytes:1024
          ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          B.Error.invalid ~range "invalid local document member")
      in
      if Member.equal member expected
      then Ok (Provenance.Local member, contents)
      else invalid "observed document member differs from actual declaration"
    | Builtin id, Source.Builtin asset ->
      if
        String.equal
          id
          (Builtin_instructions.Asset.id asset |> Builtin_instructions.Id.to_string)
      then Ok (Provenance.Builtin id, Builtin_instructions.Asset.contents asset)
      else invalid "builtin asset differs from actual declaration"
    | Dynamic, (Source.Local _ | Builtin _) ->
      invalid "dynamic document cannot be captured"
    | Local_capture, Builtin _ | Builtin _, Local _ ->
      invalid "document source kind differs from declaration"
  in
  let%bind () =
    B.add
      budget
      (B.Accounting.create
         ~occurrences:1
         ~input_bytes:(String.length contents)
         ~output_bytes:0
         ~work_steps:0
         ~nodes:0)
  in
  let%bind input_digest = B.hash budget contents in
  let used = B.accounting budget in
  let%bind conversion_limits =
    C.Limits.create
      ~max_input_bytes:(String.length contents)
      ~max_output_bytes:(B.Limits.output_bytes limits)
      ~max_work_steps:(B.Limits.work_steps limits - B.Accounting.work_steps used)
      ~max_nodes:(B.Limits.nodes limits)
      ~max_depth:(B.Limits.depth limits)
    |> Result.map_error ~f:(fun _ ->
      B.Error.invalid ~range "invalid conversion allowances")
  in
  let mode =
    if Occurrence.cleanup_html occurrence
    then C.Mode.Strip_html
    else if Occurrence.markdown occurrence
    then Html_to_markdown
    else Verbatim
  in
  let%bind output =
    C.convert mode ~contents ~limits:conversion_limits ~checkpoint
    |> Result.map_error ~f:(B.Error.conversion range)
  in
  let a = C.Output.accounting output in
  let%bind () =
    B.add
      budget
      (B.Accounting.create
         ~occurrences:0
         ~input_bytes:0
         ~output_bytes:(C.Accounting.output_bytes a)
         ~work_steps:(C.Accounting.work_steps a)
         ~nodes:(C.Accounting.nodes a))
  in
  let contents = C.Output.contents output in
  let%bind output_digest = B.hash budget contents in
  checkpoint ();
  Ok
    { root = Parsed.root_input parsed
    ; occurrence
    ; provenance
    ; input_digest
    ; output_digest
    ; input_bytes = C.Accounting.input_bytes a
    ; contents
    ; accounting = B.accounting budget
    }
;;
