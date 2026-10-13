open! Core
module C = Authoring_codec

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value

module Features = struct
  let core = "authoring.core.v1"
  let bundle = "authoring.bundle.v1"
end

module Encoding = struct
  type t = Chatml_editor_position.Encoding.t =
    | Utf8_bytes
    | Utf16_units
  [@@deriving compare, equal, sexp_of]

  let to_json = function
    | Utf8_bytes -> str "utf8_bytes"
    | Utf16_units -> str "utf16_units"
  ;;

  let of_json =
    Json_codec.enum
      ~name:"authoring position encoding"
      [ "utf8_bytes", Utf8_bytes; "utf16_units", Utf16_units ]
  ;;
end

module Method = struct
  type t =
    | Capabilities
    | Roots
    | Host_list
    | Host_read
    | Context_open
    | Context_refresh
    | Context_close
    | Source_list
    | Source_read
    | Document_version
    | Document_replace
    | Document_close
    | Bundle_begin
    | Bundle_chunk
    | Bundle_commit
    | Bundle_status
  [@@deriving compare, equal, sexp_of]

  let to_string = function
    | Capabilities -> "authoring.capabilities"
    | Roots -> "authoring.roots"
    | Host_list -> "authoring.host.list"
    | Host_read -> "authoring.host.read"
    | Context_open -> "authoring.context.open"
    | Context_refresh -> "authoring.context.refresh"
    | Context_close -> "authoring.context.close"
    | Source_list -> "authoring.source.list"
    | Source_read -> "authoring.source.read"
    | Document_version -> "authoring.document.version"
    | Document_replace -> "authoring.document.replace"
    | Document_close -> "authoring.document.close"
    | Bundle_begin -> "authoring.bundle.begin"
    | Bundle_chunk -> "authoring.bundle.chunk"
    | Bundle_commit -> "authoring.bundle.commit"
    | Bundle_status -> "authoring.bundle.status"
  ;;

  let all =
    [ Capabilities
    ; Roots
    ; Host_list
    ; Host_read
    ; Context_open
    ; Context_refresh
    ; Context_close
    ; Source_list
    ; Source_read
    ; Document_version
    ; Document_replace
    ; Document_close
    ; Bundle_begin
    ; Bundle_chunk
    ; Bundle_commit
    ; Bundle_status
    ]
  ;;

  let of_string value =
    match List.find all ~f:(fun method_ -> String.equal (to_string method_) value) with
    | Some method_ -> Ok method_
    | None -> C.invalid "unknown authoring method"
  ;;
end

module Failure = struct
  type invalid_position =
    | Invalid_utf8
    | Out_of_bounds
    | Inside_scalar
    | Inside_surrogate
    | Inside_crlf
    | Foreign_source
  [@@deriving compare, equal, sexp_of]

  type reason =
    | Unauthorized
    | Context_lease_stale
    | Stale_context
    | Stale_source
    | Invalid_position of invalid_position
    | Source_unavailable
    | Environment_unavailable
    | Work_limit
    | Cancelled
    | Other
  [@@deriving compare, equal, sexp_of]

  let position_name = function
    | Invalid_utf8 -> "invalid_utf8"
    | Out_of_bounds -> "out_of_bounds"
    | Inside_scalar -> "inside_scalar"
    | Inside_surrogate -> "inside_surrogate"
    | Inside_crlf -> "inside_crlf"
    | Foreign_source -> "foreign_source"
  ;;

  let reason_name = function
    | Unauthorized -> "unauthorized"
    | Context_lease_stale -> "context_lease_stale"
    | Stale_context -> "stale_context"
    | Stale_source -> "stale_source"
    | Invalid_position _ -> "invalid_position"
    | Source_unavailable -> "source_unavailable"
    | Environment_unavailable -> "environment_unavailable"
    | Work_limit -> "work_limit"
    | Cancelled -> "cancelled"
    | Other -> "other"
  ;;

  let code_matches (code : Protocol_error.code) = function
    | Unauthorized -> Protocol_error.equal_code code Protocol_error.Permission_denied
    | Context_lease_stale -> Protocol_error.equal_code code Protocol_error.Lease_stale
    | Stale_context | Stale_source ->
      Protocol_error.equal_code code Protocol_error.Conflict
    | Invalid_position _ -> Protocol_error.equal_code code Protocol_error.Invalid_request
    | Source_unavailable | Environment_unavailable ->
      Protocol_error.equal_code code Protocol_error.Workspace_unavailable
    | Work_limit -> Protocol_error.equal_code code Protocol_error.Resource_limit
    | Cancelled -> Protocol_error.equal_code code Protocol_error.Interrupted
    | Other -> false
  ;;

  let attach (error : Protocol_error.t) reason =
    if not (code_matches error.code reason)
    then C.invalid "authoring reason does not match protocol error code"
    else (
      let fields = [ "authoring_reason", str (reason_name reason) ] in
      let fields =
        match reason with
        | Invalid_position position ->
          fields @ [ "position_reason", str (position_name position) ]
        | _ -> fields
      in
      Ok { error with data = obj fields })
  ;;

  let classify (error : Protocol_error.t) =
    let reason =
      match C.fields ~allowed:[ "authoring_reason"; "position_reason" ] error.data with
      | Error _ -> Other
      | Ok fields ->
        (match C.required_as fields "authoring_reason" (C.string ~max_bytes:32) with
         | Error _ -> Other
         | Ok "unauthorized" -> Unauthorized
         | Ok "context_lease_stale" -> Context_lease_stale
         | Ok "stale_context" -> Stale_context
         | Ok "stale_source" -> Stale_source
         | Ok "source_unavailable" -> Source_unavailable
         | Ok "environment_unavailable" -> Environment_unavailable
         | Ok "work_limit" -> Work_limit
         | Ok "cancelled" -> Cancelled
         | Ok "invalid_position" ->
           (match C.required_as fields "position_reason" (C.string ~max_bytes:32) with
            | Ok "invalid_utf8" -> Invalid_position Invalid_utf8
            | Ok "out_of_bounds" -> Invalid_position Out_of_bounds
            | Ok "inside_scalar" -> Invalid_position Inside_scalar
            | Ok "inside_surrogate" -> Invalid_position Inside_surrogate
            | Ok "inside_crlf" -> Invalid_position Inside_crlf
            | Ok "foreign_source" -> Invalid_position Foreign_source
            | Ok _ | Error _ -> Other)
         | Ok _ -> Other)
    in
    if code_matches error.code reason then reason else Other
  ;;
end

module Limits = struct
  let maximum_wire_integer = C.maximum_wire_integer

  type t =
    { max_sources : int
    ; max_source_bytes : int
    ; max_context_source_bytes : int
    ; max_request_bytes : int
    ; max_result_bytes : int
    ; max_path_candidates : int
    }
  [@@deriving sexp_of]

  let maximum_wire_bytes = 32 * 1024 * 1024

  let create
        ~max_sources
        ~max_source_bytes
        ~max_context_source_bytes
        ~max_request_bytes
        ~max_result_bytes
        ~max_path_candidates
    =
    if
      max_sources <= 0
      || max_sources > 4096
      || max_source_bytes <= 0
      || max_source_bytes > 8 * 1024 * 1024
      || max_context_source_bytes < max_source_bytes
      || max_context_source_bytes > maximum_wire_bytes
      || max_request_bytes <= 0
      || max_request_bytes > maximum_wire_bytes
      || max_result_bytes <= 0
      || max_result_bytes > maximum_wire_bytes
      || max_path_candidates <= 0
      || max_path_candidates > 4096
    then C.invalid "invalid authoring limits"
    else
      Ok
        { max_sources
        ; max_source_bytes
        ; max_context_source_bytes
        ; max_request_bytes
        ; max_result_bytes
        ; max_path_candidates
        }
  ;;

  let to_json t =
    obj
      [ "max_sources", C.integer t.max_sources
      ; "max_source_bytes", C.integer t.max_source_bytes
      ; "max_context_source_bytes", C.integer t.max_context_source_bytes
      ; "max_request_bytes", C.integer t.max_request_bytes
      ; "max_result_bytes", C.integer t.max_result_bytes
      ; "max_path_candidates", C.integer t.max_path_candidates
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "max_sources"
          ; "max_source_bytes"
          ; "max_context_source_bytes"
          ; "max_request_bytes"
          ; "max_result_bytes"
          ; "max_path_candidates"
          ]
        json
    in
    let* max_sources = C.required_as fields "max_sources" C.positive in
    let* max_source_bytes = C.required_as fields "max_source_bytes" C.positive in
    let* max_context_source_bytes =
      C.required_as fields "max_context_source_bytes" C.positive
    in
    let* max_request_bytes = C.required_as fields "max_request_bytes" C.positive in
    let* max_result_bytes = C.required_as fields "max_result_bytes" C.positive in
    let* max_path_candidates = C.required_as fields "max_path_candidates" C.positive in
    create
      ~max_sources
      ~max_source_bytes
      ~max_context_source_bytes
      ~max_request_bytes
      ~max_result_bytes
      ~max_path_candidates
  ;;
end

module Capabilities = struct
  type t =
    { methods : string list
    ; encodings : Encoding.t list
    ; limits : Limits.t
    }
  [@@deriving sexp_of]

  let create ~methods ~encodings ~limits =
    let unique_methods = List.dedup_and_sort methods ~compare:String.compare in
    let unique_encodings = List.dedup_and_sort encodings ~compare:Encoding.compare in
    let has_core_method =
      List.exists methods ~f:(fun name ->
        match Method.of_string name with
        | Ok
            ( Method.Capabilities
            | Method.Bundle_begin
            | Method.Bundle_chunk
            | Method.Bundle_commit
            | Method.Bundle_status ) -> false
        | Ok _ -> true
        | Error _ -> false)
    in
    if
      List.length methods <> List.length unique_methods
      || List.length encodings <> List.length unique_encodings
      || List.length methods = 0
      || List.length methods > 64
      || List.exists methods ~f:(fun value ->
        String.length value > 96
        || (not (String.is_prefix value ~prefix:"authoring."))
        || not
             (String.for_all value ~f:(function
                | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> true
                | _ -> false)))
      || (not
            (List.mem methods (Method.to_string Method.Capabilities) ~equal:String.equal))
      || (has_core_method && List.is_empty encodings)
    then C.invalid "invalid authoring capabilities"
    else Ok { methods = unique_methods; encodings = unique_encodings; limits }
  ;;

  let supports t method_ =
    List.mem t.methods (Method.to_string method_) ~equal:String.equal
  ;;

  let supports_encoding t encoding = List.mem t.encodings encoding ~equal:Encoding.equal

  let to_json t =
    obj
      [ "methods", C.array str t.methods
      ; "encodings", C.array Encoding.to_json t.encodings
      ; "limits", Limits.to_json t.limits
      ]
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "methods"; "encodings"; "limits" ] json in
    let* methods =
      C.required_as fields "methods" (C.list ~max_items:64 (C.string ~max_bytes:96))
    in
    let* encodings =
      C.required_as fields "encodings" (C.list ~max_items:2 Encoding.of_json)
    in
    let* limits = C.required_as fields "limits" Limits.of_json in
    create ~methods ~encodings ~limits
  ;;
end

module Source = struct
  type t =
    { id : Chatml_source.Id.t
    ; digest : Chatml_source.Digest.t
    ; byte_length : int
    ; editor_revision : int option
    }
  [@@deriving equal, sexp_of]

  let create ~id ~digest ~byte_length ~editor_revision =
    let* () = C.validate_source_id id in
    if byte_length < 0 || byte_length > Limits.maximum_wire_bytes
    then C.invalid "invalid authoring source length"
    else if
      Option.exists editor_revision ~f:(fun revision ->
        revision <= 0 || revision > C.maximum_wire_integer)
    then C.invalid "invalid authoring editor revision"
    else Ok { id; digest; byte_length; editor_revision }
  ;;

  let id t = t.id
  let digest t = t.digest
  let byte_length t = t.byte_length
  let editor_revision t = t.editor_revision

  let to_json t =
    obj
      ([ "id", C.source_id_json t.id
       ; "digest", C.digest_json t.digest
       ; "byte_length", C.integer t.byte_length
       ]
       @ C.optional_field "editor_revision" t.editor_revision C.integer)
  ;;

  let of_json json =
    let* fields =
      C.fields ~allowed:[ "id"; "digest"; "byte_length"; "editor_revision" ] json
    in
    let* id = C.required_as fields "id" C.source_id in
    let* digest = C.required_as fields "digest" C.digest in
    let* byte_length = C.required_as fields "byte_length" C.nonnegative in
    let* editor_revision = C.optional_as fields "editor_revision" C.positive in
    create ~id ~digest ~byte_length ~editor_revision
  ;;
end

module Position = struct
  type t = Chatml_editor_position.Position.t

  let sexp_of_t t =
    [%sexp
      { line = (Chatml_editor_position.Position.line t : int)
      ; column = (Chatml_editor_position.Position.column t : int)
      }]
  ;;

  let to_json t =
    obj
      [ "line", C.integer (Chatml_editor_position.Position.line t)
      ; "column", C.integer (Chatml_editor_position.Position.column t)
      ]
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "line"; "column" ] json in
    let* line = C.required_as fields "line" C.nonnegative in
    let* column = C.required_as fields "column" C.nonnegative in
    match Chatml_editor_position.Position.create ~line ~column with
    | Ok value -> Ok value
    | Error _ -> C.invalid "invalid authoring position"
  ;;
end

module Site = struct
  type precision =
    | Exact
    | Approximate
  [@@deriving compare, equal, sexp_of]

  type t =
    { source : Source.t
    ; encoding : Encoding.t
    ; start : Position.t
    ; finish : Position.t
    ; precision : precision
    }
  [@@deriving sexp_of]

  let create ~source ~encoding ~start ~finish ~precision =
    if Chatml_editor_position.Position.compare start finish > 0
    then C.invalid "reversed authoring site"
    else Ok { source; encoding; start; finish; precision }
  ;;

  let source t = t.source
  let encoding t = t.encoding
  let start t = t.start
  let finish t = t.finish
  let precision t = t.precision

  let precision_json = function
    | Exact -> str "exact"
    | Approximate -> str "approximate"
  ;;

  let precision_of_json =
    Json_codec.enum
      ~name:"authoring site precision"
      [ "exact", Exact; "approximate", Approximate ]
  ;;

  let to_json t =
    obj
      [ "source", Source.to_json t.source
      ; "encoding", Encoding.to_json t.encoding
      ; "start", Position.to_json t.start
      ; "finish", Position.to_json t.finish
      ; "precision", precision_json t.precision
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields ~allowed:[ "source"; "encoding"; "start"; "finish"; "precision" ] json
    in
    let* source = C.required_as fields "source" Source.of_json in
    let* encoding = C.required_as fields "encoding" Encoding.of_json in
    let* start = C.required_as fields "start" Position.of_json in
    let* finish = C.required_as fields "finish" Position.of_json in
    let* precision = C.required_as fields "precision" precision_of_json in
    create ~source ~encoding ~start ~finish ~precision
  ;;
end
