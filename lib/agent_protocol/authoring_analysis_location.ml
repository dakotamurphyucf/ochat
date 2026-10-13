open! Core
module C = Authoring_codec
module Context = Authoring_context_core.Context
module Source = Authoring_common.Source
module Digest = Chatml_source.Digest

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value
let field fields name decode = C.required_as fields name decode
let opt fields name decode = C.optional_as fields name decode
let list ~max decode json = C.list ~max_items:max decode json
let enum ~name choices = Json_codec.enum ~name choices

module Limits = struct
  let max_result_bytes = 2 * 1024 * 1024
  let max_diagnostics = 256
  let max_syntax_sites = 4096
  let max_query_items = 512
  let max_label_bytes = 1024 * 1024
  let max_display_bytes = 4096
end

let bounded_nonnegative value =
  if value >= 0 && value <= C.maximum_wire_integer
  then Ok value
  else C.invalid "invalid analysis byte coordinate"
;;

let bounded_string ~max value = C.utf8_text ~max_bytes:max (`String value)

module Byte_range = struct
  type t =
    { source : Source.t
    ; start_byte : int
    ; end_byte : int
    }

  let create ~source ~start_byte ~end_byte =
    let* _ = bounded_nonnegative start_byte in
    let* _ = bounded_nonnegative end_byte in
    if start_byte <= end_byte && end_byte <= Source.byte_length source
    then Ok { source; start_byte; end_byte }
    else C.invalid "analysis byte range exceeds its source"
  ;;

  let source t = t.source
  let start_byte t = t.start_byte
  let end_byte t = t.end_byte

  let to_json t =
    obj
      [ "source", Source.to_json t.source
      ; "start_byte", C.integer t.start_byte
      ; "end_byte", C.integer t.end_byte
      ]
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "source"; "start_byte"; "end_byte" ] json in
    let* source = field fields "source" Source.of_json in
    let* start_byte = field fields "start_byte" C.nonnegative in
    let* end_byte = field fields "end_byte" C.nonnegative in
    create ~source ~start_byte ~end_byte
  ;;
end

module Site = struct
  type projection =
    | Authored_exact of Byte_range.t
    | External_origin of
        { member : string
        ; body_digest : Digest.t
        ; start_byte : int
        ; end_byte : int
        }
    | Approximate_declaration of Byte_range.t

  type t =
    { native : Byte_range.t
    ; projection : projection option
    }

  let create ~native ~projection =
    let* () =
      match projection with
      | None | Some (Authored_exact _) | Some (Approximate_declaration _) -> Ok ()
      | Some (External_origin { member; start_byte; end_byte; _ }) ->
        let* _ = C.portable_member_string member in
        let* _ = bounded_nonnegative start_byte in
        let* _ = bounded_nonnegative end_byte in
        if
          start_byte <= end_byte
          && end_byte <= Source.byte_length (Byte_range.source native)
        then Ok ()
        else C.invalid "external projection range exceeds compiled source"
    in
    Ok { native; projection }
  ;;

  let native t = t.native
  let projection t = t.projection

  let projection_to_json = function
    | Authored_exact range ->
      obj [ "kind", str "authored_exact"; "range", Byte_range.to_json range ]
    | Approximate_declaration range ->
      obj [ "kind", str "approximate_declaration"; "range", Byte_range.to_json range ]
    | External_origin { member; body_digest; start_byte; end_byte } ->
      obj
        [ "kind", str "external_origin"
        ; "member", str member
        ; "body_digest", C.digest_json body_digest
        ; "start_byte", C.integer start_byte
        ; "end_byte", C.integer end_byte
        ]
  ;;

  let projection_of_json json =
    let* fields =
      C.fields
        ~allowed:[ "kind"; "range"; "member"; "body_digest"; "start_byte"; "end_byte" ]
        json
    in
    let* kind = field fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "authored_exact" | "approximate_declaration" ->
      let* strict = C.fields ~allowed:[ "kind"; "range" ] json in
      let* range = field strict "range" Byte_range.of_json in
      Ok
        (if String.equal kind "authored_exact"
         then Authored_exact range
         else Approximate_declaration range)
    | "external_origin" ->
      let* strict =
        C.fields
          ~allowed:[ "kind"; "member"; "body_digest"; "start_byte"; "end_byte" ]
          json
      in
      let* member = field strict "member" C.portable_member in
      let* body_digest = field strict "body_digest" C.digest in
      let* start_byte = field strict "start_byte" C.nonnegative in
      let* end_byte = field strict "end_byte" C.nonnegative in
      Ok (External_origin { member; body_digest; start_byte; end_byte })
    | _ -> C.invalid "unknown analysis site projection"
  ;;

  let to_json t =
    obj
      ([ "native", Byte_range.to_json t.native ]
       @ C.optional_field "projection" t.projection projection_to_json)
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "native"; "projection" ] json in
    let* native = field fields "native" Byte_range.of_json in
    let* projection = opt fields "projection" projection_of_json in
    create ~native ~projection
  ;;
end
