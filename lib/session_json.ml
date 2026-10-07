open! Core

let shape_exn = function
  | Ok shape -> shape
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let object_ fields = Document_schema.Shape.object_ fields |> shape_exn

let array ?identity shape =
  Document_schema.Shape.array shape ~identity_field:identity |> shape_exn
;;

let nullable = Document_schema.Shape.nullable
let value = Document_schema.Shape.value

let fields = function
  | `Object fields -> Ok fields
  | _ -> Error "expected a named-field object"
;;

let field fields name decode =
  match List.Assoc.find fields name ~equal:String.equal with
  | None -> Error ("missing required field: " ^ name)
  | Some value -> Result.map_error (decode value) ~f:(fun error -> name ^ ": " ^ error)
;;

let string = function
  | `String value -> Ok value
  | _ -> Error "expected a string"
;;

let bool = function
  | `True -> Ok true
  | `False -> Ok false
  | _ -> Error "expected a boolean"
;;

let int64 json =
  Result.bind (string json) ~f:(fun text ->
    match Int64.of_string_opt text with
    | Some value when String.equal text (Int64.to_string value) -> Ok value
    | None | Some _ -> Error "expected a canonical signed decimal int64 string")
;;

let int json =
  Result.bind (int64 json) ~f:(fun value ->
    match Int64.to_int value with
    | Some value -> Ok value
    | None -> Error "integer exceeds the runtime range")
;;

let option decode = function
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

let list decode = function
  | `Array values -> List.map values ~f:decode |> Result.all
  | _ -> Error "expected an array"
;;

let encode_string value = `String value
let encode_bool value = if value then `True else `False
let encode_int value = `String (Int.to_string value)
let encode_int64 value = `String (Int64.to_string value)

let encode_option encode = function
  | None -> `Null
  | Some value -> encode value
;;

let encode_list encode values = `Array (List.map values ~f:encode)

let unique ?(allow_empty = false) values =
  match List.find_a_dup values ~compare:String.compare with
  | Some name -> Error ("duplicate identity: " ^ name)
  | None ->
    if (not allow_empty) && List.exists values ~f:String.is_empty
    then Error "empty identity"
    else Ok ()
;;

let named_values decode json =
  let open Result.Let_syntax in
  let%bind entries =
    list
      (fun json ->
         let%bind fields = fields json in
         let%bind name = field fields "name" string in
         let%map value = field fields "value" decode in
         name, value)
      json
  in
  let%map () = unique ~allow_empty:true (List.map entries ~f:fst) in
  entries
;;

let encode_named_values encode entries =
  encode_list
    (fun (name, value) -> `Object [ "name", `String name; "value", encode value ])
    entries
;;

let named_values_shape shape =
  Document_schema.Shape.array
    ~allow_empty_identity:true
    (object_ [ "name", value; "value", shape ])
    ~identity_field:(Some "name")
  |> shape_exn
;;

let bounded decode json =
  match Document_schema.Json.validate ~limits:Document_schema.Limits.default json with
  | Error error -> Error (Sexp.to_string_hum ([%sexp_of: Document_schema.Error.t] error))
  | Ok () -> decode json
;;
