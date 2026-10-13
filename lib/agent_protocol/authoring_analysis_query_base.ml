open! Core
module C = Authoring_codec
module A = Authoring_analysis_core
module Source = Authoring_common.Source
module Digest = Chatml_source.Digest

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value
let field fields name decode = C.required_as fields name decode
let opt fields name decode = C.optional_as fields name decode
let list ~max decode json = C.list ~max_items:max decode json
let enum ~name choices = Json_codec.enum ~name choices

module Query = struct
  type evidence =
    | Certified of
        { capture : A.Capture.t
        ; snapshot : A.Snapshot_id.t
        ; validated_graph : Digest.t
        }
    | Provisional of A.Prefix.provisional
    | Syntax_only of A.Capture.t

  let evidence_to_json = function
    | Certified { capture; snapshot; validated_graph } ->
      obj
        [ "kind", str "certified"
        ; "capture", A.Capture.to_json capture
        ; "snapshot", A.Snapshot_id.to_json snapshot
        ; "validated_graph", C.digest_json validated_graph
        ]
    | Provisional provisional ->
      obj
        [ "kind", str "provisional"
        ; "prefix", A.Prefix.to_json (A.Prefix.Provisional provisional)
        ]
    | Syntax_only capture ->
      obj [ "kind", str "syntax_only"; "capture", A.Capture.to_json capture ]
  ;;

  let evidence_of_json json =
    let* fields =
      C.fields
        ~allowed:[ "kind"; "capture"; "snapshot"; "validated_graph"; "prefix" ]
        json
    in
    let* kind = field fields "kind" (C.string ~max_bytes:24) in
    match kind with
    | "certified" ->
      let* _ =
        C.fields ~allowed:[ "kind"; "capture"; "snapshot"; "validated_graph" ] json
      in
      let* capture = field fields "capture" A.Capture.of_json in
      let* snapshot = field fields "snapshot" A.Snapshot_id.of_json in
      let* validated_graph = field fields "validated_graph" C.digest in
      Ok (Certified { capture; snapshot; validated_graph })
    | "provisional" ->
      let* _ = C.fields ~allowed:[ "kind"; "prefix" ] json in
      let* prefix = field fields "prefix" A.Prefix.of_json in
      (match prefix with
       | A.Prefix.Provisional value -> Ok (Provisional value)
       | A.Prefix.Unavailable _ -> C.invalid "query requires a provisional prefix")
    | "syntax_only" ->
      let* _ = C.fields ~allowed:[ "kind"; "capture" ] json in
      let* capture = field fields "capture" A.Capture.of_json in
      Ok (Syntax_only capture)
    | _ -> C.invalid "unknown query evidence"
  ;;

  let equal_evidence left right =
    match left, right with
    | Certified left, Certified right ->
      A.Capture.equal left.capture right.capture
      && A.Snapshot_id.equal left.snapshot right.snapshot
      && Digest.equal left.validated_graph right.validated_graph
    | Provisional left, Provisional right -> A.Prefix.equal_provisional left right
    | Syntax_only left, Syntax_only right -> A.Capture.equal left right
    | _ -> false
  ;;

  type namespace =
    | Value
    | Type
    | Type_parameter
  [@@deriving compare, equal, sexp_of]

  let namespace_pairs = [ "value", Value; "type", Type; "type_parameter", Type_parameter ]

  let namespace_to_json namespace =
    str
      (List.find_map namespace_pairs ~f:(fun (name, item) ->
         if equal_namespace item namespace then Some name else None)
       |> Option.value_exn)
  ;;

  let namespace_of_json = enum ~name:"analysis namespace" namespace_pairs

  type kind =
    | References
    | Definitions
    | Type_at
    | Candidates of namespace
    | Configured_modules
    | Syntax_at
  [@@deriving compare, equal, sexp_of]

  let kind_to_json = function
    | References -> obj [ "kind", str "references" ]
    | Definitions -> obj [ "kind", str "definitions" ]
    | Type_at -> obj [ "kind", str "type_at" ]
    | Candidates namespace ->
      obj [ "kind", str "candidates"; "namespace", namespace_to_json namespace ]
    | Configured_modules -> obj [ "kind", str "configured_modules" ]
    | Syntax_at -> obj [ "kind", str "syntax_at" ]
  ;;

  let kind_of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "namespace" ] json in
    let* kind = field fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "candidates" ->
      let* namespace = field fields "namespace" namespace_of_json in
      Ok (Candidates namespace)
    | "references" | "definitions" | "type_at" | "configured_modules" | "syntax_at" ->
      let* _ = C.fields ~allowed:[ "kind" ] json in
      Ok
        (match kind with
         | "references" -> References
         | "definitions" -> Definitions
         | "type_at" -> Type_at
         | "configured_modules" -> Configured_modules
         | _ -> Syntax_at)
    | _ -> C.invalid "unknown analysis query kind"
  ;;

  module Request = struct
    type t =
      { evidence : evidence
      ; source : Source.t
      ; byte_offset : int
      ; kind : kind
      ; max_items : int
      }

    let create ~evidence ~source ~byte_offset ~kind ~max_items =
      let* _ = A.bounded_nonnegative byte_offset in
      if byte_offset > Source.byte_length source
      then C.invalid "query point exceeds source"
      else if max_items <= 0 || max_items > A.Limits.max_query_items
      then C.invalid "invalid query result limit"
      else (
        match evidence, kind with
        | Syntax_only _, Syntax_at ->
          Ok { evidence; source; byte_offset; kind; max_items }
        | Syntax_only _, _ | (Certified _ | Provisional _), Syntax_at ->
          C.invalid "query evidence and kind disagree"
        | Provisional prefix, _ ->
          if
            Source.equal source (A.Prefix.prefix_source prefix)
            && byte_offset <= A.Prefix.end_byte prefix
          then Ok { evidence; source; byte_offset; kind; max_items }
          else C.invalid "query point differs from provisional prefix"
        | Certified _, _ -> Ok { evidence; source; byte_offset; kind; max_items })
    ;;

    let evidence t = t.evidence
    let source t = t.source
    let byte_offset t = t.byte_offset
    let kind t = t.kind
    let max_items t = t.max_items

    let equal a b =
      equal_evidence a.evidence b.evidence
      && Source.equal a.source b.source
      && Int.equal a.byte_offset b.byte_offset
      && equal_kind a.kind b.kind
      && Int.equal a.max_items b.max_items
    ;;

    let sexp_of_t _ = Sexp.Atom "authoring_query_request"

    let to_json t =
      obj
        [ "evidence", evidence_to_json t.evidence
        ; "source", Source.to_json t.source
        ; "byte_offset", C.integer t.byte_offset
        ; "kind", kind_to_json t.kind
        ; "max_items", C.integer t.max_items
        ]
    ;;

    let of_json json =
      let* () = C.admit_json ~max_bytes:A.Limits.max_result_bytes json in
      let* fields =
        C.fields
          ~allowed:[ "evidence"; "source"; "byte_offset"; "kind"; "max_items" ]
          json
      in
      let* evidence = field fields "evidence" evidence_of_json in
      let* source = field fields "source" Source.of_json in
      let* byte_offset = field fields "byte_offset" C.nonnegative in
      let* kind = field fields "kind" kind_of_json in
      let* max_items = field fields "max_items" C.positive in
      create ~evidence ~source ~byte_offset ~kind ~max_items
    ;;
  end

  type unavailable =
    | Not_checked
    | No_recorded_context
    | No_recorded_type
    | No_nominal_members
    | No_syntax
  [@@deriving compare, equal, sexp_of]

  let unavailable_pairs =
    [ "not_checked", Not_checked
    ; "no_recorded_context", No_recorded_context
    ; "no_recorded_type", No_recorded_type
    ; "no_nominal_members", No_nominal_members
    ; "no_syntax", No_syntax
    ]
  ;;

  let unavailable_to_json value =
    str
      (List.find_map unavailable_pairs ~f:(fun (name, item) ->
         if equal_unavailable item value then Some name else None)
       |> Option.value_exn)
  ;;

  let unavailable_of_json = enum ~name:"query unavailable reason" unavailable_pairs

  type 'a page =
    { items : 'a list
    ; truncated : bool
    }

  type 'a found =
    | Available of 'a
    | Unavailable of unavailable

  type 'a selected =
    | Selected of 'a
    | Missing of unavailable
    | Ambiguous of 'a page

  let page_to_json encode page =
    obj [ "items", C.array encode page.items; "truncated", C.boolean page.truncated ]
  ;;

  let page_of_json ~max decode json =
    let* fields = C.fields ~allowed:[ "items"; "truncated" ] json in
    let* items = field fields "items" (list ~max decode) in
    let* truncated = field fields "truncated" Json_codec.bool in
    Ok { items; truncated }
  ;;

  let found_to_json encode = function
    | Available value -> obj [ "status", str "available"; "value", encode value ]
    | Unavailable reason ->
      obj [ "status", str "unavailable"; "reason", unavailable_to_json reason ]
  ;;

  let found_of_json decode json =
    let* fields = C.fields ~allowed:[ "status"; "value"; "reason" ] json in
    let* status = field fields "status" (C.string ~max_bytes:16) in
    match status with
    | "available" ->
      let* _ = C.fields ~allowed:[ "status"; "value" ] json in
      let* value = field fields "value" decode in
      Ok (Available value)
    | "unavailable" ->
      let* _ = C.fields ~allowed:[ "status"; "reason" ] json in
      let* reason = field fields "reason" unavailable_of_json in
      Ok (Unavailable reason)
    | _ -> C.invalid "unknown query fact status"
  ;;

  let selected_to_json encode = function
    | Selected value -> obj [ "status", str "selected"; "value", encode value ]
    | Missing reason ->
      obj [ "status", str "missing"; "reason", unavailable_to_json reason ]
    | Ambiguous page ->
      obj [ "status", str "ambiguous"; "page", page_to_json encode page ]
  ;;

  let selected_of_json ~max decode json =
    let* fields = C.fields ~allowed:[ "status"; "value"; "reason"; "page" ] json in
    let* status = field fields "status" (C.string ~max_bytes:16) in
    match status with
    | "selected" ->
      let* _ = C.fields ~allowed:[ "status"; "value" ] json in
      let* value = field fields "value" decode in
      Ok (Selected value)
    | "missing" ->
      let* _ = C.fields ~allowed:[ "status"; "reason" ] json in
      let* reason = field fields "reason" unavailable_of_json in
      Ok (Missing reason)
    | "ambiguous" ->
      let* _ = C.fields ~allowed:[ "status"; "page" ] json in
      let* page = field fields "page" (page_of_json ~max decode) in
      Ok (Ambiguous page)
    | _ -> C.invalid "unknown query selection status"
  ;;
end
