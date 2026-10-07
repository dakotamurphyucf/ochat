open! Core
module D = Document_schema

let limits ~max_bytes =
  D.Limits.create ~max_bytes ~max_depth:256 ~max_fields:1_000_000 ~max_nodes:2_000_000
;;

let invalid name reason = Error (D.Error.Invalid_field { path = [ name ]; reason })

let required json name decode =
  match D.Json.field json ~name with
  | Absent -> invalid name "required field is absent"
  | Null -> decode `Null
  | Value value -> decode value
;;

let optional json name decode =
  match D.Json.field json ~name with
  | Absent | Null -> Ok None
  | Value value -> Result.map (decode value) ~f:Option.some
;;

let string = function
  | `String value -> Ok value
  | _ -> invalid "value" "expected a string"
;;

let boolean = function
  | `True -> Ok true
  | `False -> Ok false
  | _ -> invalid "value" "expected a boolean"
;;

let decimal json =
  let open Result.Let_syntax in
  let%bind value = string json in
  if
    String.equal value "0"
    || ((not (String.is_empty value))
        && Char.(value.[0] >= '1' && value.[0] <= '9')
        && String.for_all value ~f:Char.is_digit)
  then (
    match Int64.of_string_opt value with
    | Some value when Int64.(value >= zero) -> Ok value
    | _ -> invalid "value" "decimal counter exceeds int64 range")
  else invalid "value" "expected a nonnegative canonical decimal string"
;;

let array = function
  | `Array values -> Ok values
  | _ -> invalid "value" "expected an array"
;;

let document ?(limits = D.Limits.default) json = D.Document.inspect ~limits json

let digest json =
  let open Result.Let_syntax in
  let%bind value = string json in
  if
    String.length value = 64
    && String.for_all value ~f:(fun ch ->
      Char.is_digit ch || Char.(ch >= 'a' && ch <= 'f'))
  then Ok value
  else invalid "value" "expected a lowercase SHA-256 digest"
;;

let decimal_json value = `String (Int64.to_string value)
let option_json value ~f = Option.value_map value ~default:`Null ~f

let shape fields =
  match D.Shape.object_ fields with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp "invalid static document shape", (error : D.Error.t)]
;;

let upgrade document ~limits ~kind =
  let open Result.Let_syntax in
  let%bind conversion =
    D.Conversion.create
      ~limits
      ~targets:[ kind, 1 ]
      ~max_steps:1
      ~max_operations:1
      ~steps:[]
  in
  D.Conversion.upgrade conversion document
;;

let protocol result =
  Result.map_error result ~f:(fun error ->
    D.Error.Invalid_field { path = []; reason = error.Agent_protocol.Error.message })
;;

let store result = Result.map_error result ~f:(fun error -> Store_error.Document error)

let expect document ~kind ~version =
  if not (String.equal (D.Document.kind document) kind)
  then Error (D.Error.Wrong_kind { expected = kind; actual = D.Document.kind document })
  else if D.Document.version document <> version
  then
    Error
      (D.Error.Unsupported_version
         { kind; version = D.Document.version document; target = version })
  else (
    match D.Document.required_semantics document with
    | [] -> Ok ()
    | first :: _ -> Error (D.Error.Required_semantics_unknown first))
;;

let record_error = function
  | Document_record.Error.Frame error -> Store_error.Framing error
  | Incomplete_frame -> Store_error.Missing "incomplete document frame"
  | Trailing_bytes -> Store_error.Corrupt "document file contains trailing bytes"
  | Document error -> Store_error.Document error
  | Digest_mismatch { expected; actual } ->
    Store_error.Corrupt ("stored document digest differs: " ^ expected ^ " / " ^ actual)
  | Invalid_digest digest -> Store_error.Corrupt ("invalid document digest: " ^ digest)
;;

let rec iter_strings json ~f =
  match json with
  | `String value -> f value
  | `Object fields ->
    List.iter fields ~f:(fun (key, value) ->
      f key;
      iter_strings value ~f)
  | `Array values -> List.iter values ~f:(fun value -> iter_strings value ~f)
  | `Null | `True | `False | `Number _ -> ()
;;
