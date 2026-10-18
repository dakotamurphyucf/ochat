open! Core
module Error = Protocol_error

let invalid message = Error (Error.invalid_request message)
let limit message = Error (Error.create Error.Resource_limit ~message ~retryable:false ())

let measure ~limits json =
  match Document_schema.Json.validate_and_measure ~limits json with
  | Ok bytes -> Ok bytes
  | Error (Document_schema.Error.Limit_exceeded _) ->
    limit "developer JSON exceeds its admitted bound"
  | Error _ -> invalid "invalid developer JSON"
;;

let validate ~limits json = Result.map (measure ~limits json) ~f:ignore

let fields ~allowed json =
  match Json_codec.fields json with
  | Error _ -> invalid "developer object is malformed or has duplicate fields"
  | Ok fields ->
    (match json with
     | `Object entries ->
       if
         List.for_all entries ~f:(fun (name, _) ->
           List.mem allowed name ~equal:String.equal)
       then Ok fields
       else invalid "unknown developer field"
     | `Null | `True | `False | `Number _ | `String _ | `Array _ ->
       invalid "expected developer object")
;;

let required fields name =
  match Json_codec.required fields name with
  | Ok value -> Ok value
  | Error _ -> invalid ("missing developer field: " ^ name)
;;

let optional fields name =
  match Json_codec.optional fields name with
  | None -> Ok None
  | Some `Null -> invalid ("null developer field: " ^ name)
  | Some value -> Ok (Some value)
;;

let string ~max_bytes = function
  | `String value when String.length value <= max_bytes -> Ok value
  | `String _ -> limit "developer string exceeds its admitted bound"
  | `Null | `True | `False | `Number _ | `Array _ | `Object _ ->
    invalid "expected developer string"
;;

let max_integer = Authoring_codec.maximum_wire_integer

let canonical_integer ~min = function
  | `Number encoded ->
    (match Int.of_string_opt encoded with
     | Some number
       when number >= min
            && number <= max_integer
            && String.equal encoded (Int.to_string number) -> Ok number
     | Some _ | None -> invalid "expected canonical bounded developer integer")
  | `Null | `True | `False | `String _ | `Array _ | `Object _ ->
    invalid "expected developer integer"
;;

let nonnegative = canonical_integer ~min:0
let positive = canonical_integer ~min:1

let finite_positive_float = function
  | `Number encoded ->
    (match Float.of_string_opt encoded with
     | Some value when Float.is_finite value && Float.(value > 0.) -> Ok value
     | Some _ | None -> invalid "expected finite positive developer number")
  | `Null | `True | `False | `String _ | `Array _ | `Object _ ->
    invalid "expected developer number"
;;

let boolean = function
  | `True -> Ok true
  | `False -> Ok false
  | `Null | `Number _ | `String _ | `Array _ | `Object _ ->
    invalid "expected developer Boolean"
;;

let array ~max_items decode = function
  | `Array values when List.length values <= max_items ->
    Result.all (List.map values ~f:decode)
  | `Array _ -> limit "developer list exceeds its admitted bound"
  | `Null | `True | `False | `Number _ | `String _ | `Object _ ->
    invalid "expected developer array"
;;

let integer_json value = `Number (Int.to_string value)

let optional_field name value encode =
  match value with
  | None -> []
  | Some value -> [ name, encode value ]
;;

let digest = Authoring_codec.digest
let digest_json = Authoring_codec.digest_json
let source_id = Authoring_codec.source_id
let source_id_json = Authoring_codec.source_id_json
