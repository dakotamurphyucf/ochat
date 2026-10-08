open! Core
module P = Agent_protocol
module J = P.Json_codec
module D = Document_schema

let integer = J.bounded_int ~min:0 ~max:Int.max_value
let signed_integer = J.bounded_int ~min:Int.min_value ~max:Int.max_value
let integer_json n = `Number (Int.to_string n)
let int64_json n = `String (Int64.to_string n)

let int64 = function
  | `String text ->
    (match Int64.of_string_opt text with
     | Some n when String.equal (Int64.to_string n) text -> Ok n
     | _ -> Error (P.Error.invalid_request "invalid decimal int64"))
  | _ -> Error (P.Error.invalid_request "expected decimal int64 string")
;;

let nonnegative_int64 json =
  let%bind.Result n = int64 json in
  if Int64.(n >= 0L) then Ok n else Error (P.Error.invalid_request "negative counter")
;;

let nullable f = function
  | `Null -> Ok None
  | json -> Result.map (f json) ~f:Option.some
;;

let option_json f value = Option.value_map value ~default:`Null ~f
let list_json f values = `Array (List.map values ~f)
let raw json = Ok json
let text_json text = `String text
let bool_json value = if value then `True else `False
let required = J.required_as
let object_ = J.fields
let list = J.list

let protocol_error error =
  D.Error.Invalid_field { path = []; reason = error.P.Error.message }
;;

let document_result result = Result.map_error result ~f:protocol_error

let shape_exn fields =
  match D.Shape.object_ fields with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp "invalid storage shape", (error : D.Error.t)]
;;

let array_shape_exn ?(allow_empty_identity = false) ?identity_field shape =
  match D.Shape.array shape ~identity_field ~allow_empty_identity with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp "invalid storage array shape", (error : D.Error.t)]
;;

let fields_shape names = shape_exn (List.map names ~f:(fun name -> name, D.Shape.value))
let nullable_shape = D.Shape.nullable

let document_shape payload =
  shape_exn
    [ "format", D.Shape.value
    ; "schema_version", D.Shape.value
    ; "kind", D.Shape.value
    ; "payload", payload
    ]
;;

let codec_exn ~limits ~kind ~shape ~decode ~encode =
  match
    D.Domain_codec.create
      ~limits
      ~kind
      ~version:1
      ~shape
      ~supported_semantics:[]
      ~decode:(fun json -> document_result (decode json))
      ~encode:(fun value -> document_result (encode value))
  with
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "invalid storage codec", (error : D.Error.t)]
;;

let inspect json = D.Document.inspect ~limits:D.Limits.default json
let document_json = D.Document.json
let limits = Agent_store.Document_fields.limits

let validate_document document ~limits ~kind =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate ~limits (D.Document.json document) in
  if not (String.equal (D.Document.kind document) kind)
  then Error (D.Error.Wrong_kind { expected = kind; actual = D.Document.kind document })
  else if D.Document.version document <> 1
  then
    Error (D.Error.Wrong_version { expected = 1; actual = D.Document.version document })
  else (
    match D.Document.required_semantics document with
    | [] -> Ok ()
    | first :: _ -> Error (D.Error.Required_semantics_unknown first))
;;

let tagged_shape_exn ~discriminator cases =
  match D.Shape.tagged_object ~discriminator cases with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp "invalid storage tagged shape", (error : D.Error.t)]
;;

let host_counter_to_json value = int64_json (Int64.of_int value)

let host_counter_of_json json =
  let%bind.Result n = nonnegative_int64 json in
  match Int64.to_int n with
  | Some value -> Ok value
  | None -> Error (P.Error.invalid_request "counter exceeds platform range")
;;

let upgrade document ~limits ~kind =
  let open Result.Let_syntax in
  let%bind conversion =
    D.Conversion.create
      ~limits
      ~targets:[ kind, 1 ]
      ~max_steps:256
      ~max_operations:100_000
      ~steps:[]
  in
  D.Conversion.upgrade conversion document
;;

let moderator_of_jsonaf json =
  let%map.Result _ = Moderator_checkpoint.decode (Some json) in
  json
;;
