open Core

type fields = (string * Jsonaf.t) list

let invalid message = Error (Protocol_error.invalid_request message)

let fields = function
  | `Object fields ->
    let names = List.map fields ~f:fst in
    (match List.find_a_dup names ~compare:String.compare with
     | None -> Ok fields
     | Some name -> invalid ("duplicate object field: " ^ name))
  | _ -> invalid "expected a JSON object"
;;

let required fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | Some value -> Ok value
  | None -> invalid ("missing required field: " ^ name)
;;

let required_as fields name decode = Result.bind (required fields name) ~f:decode
let optional fields name = List.Assoc.find fields name ~equal:String.equal

let optional_as fields name decode =
  match optional fields name with
  | None -> Ok None
  | Some json -> Result.map (decode json) ~f:Option.some
;;

let to_alist fields = fields

let string = function
  | `String value -> Ok value
  | _ -> invalid "expected a JSON string"
;;

let bool = function
  | `True -> Ok true
  | `False -> Ok false
  | _ -> invalid "expected a JSON boolean"
;;

let list decode = function
  | `Array values -> Result.all (List.map values ~f:decode)
  | _ -> invalid "expected a JSON array"
;;

let bounded_int ~min ~max = function
  | `Number encoded ->
    (match Int.of_string_opt encoded with
     | Some value when value >= min && value <= max -> Ok value
     | Some _ -> invalid "JSON integer is outside the allowed range"
     | None -> invalid "expected a JSON integer")
  | _ -> invalid "expected a JSON integer"
;;

let bounded_int64 ~min ~max = function
  | `Number encoded ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.between value ~low:min ~high:max -> Ok value
     | Some _ -> invalid "JSON integer is outside the allowed range"
     | None -> invalid "expected a JSON integer")
  | _ -> invalid "expected a JSON integer"
;;

let enum ~name values json =
  let open Result.Let_syntax in
  let%bind encoded = string json in
  match List.Assoc.find values encoded ~equal:String.equal with
  | Some value -> Ok value
  | None -> invalid (sprintf "unknown %s value: %s" name encoded)
;;

let validate_required_features ~supported ~required =
  match List.find required ~f:(Fn.non (Set.mem supported)) with
  | None -> Ok ()
  | Some feature ->
    Error
      (Protocol_error.create
         Protocol_error.Incompatible_protocol
         ~message:("unsupported required feature: " ^ feature)
         ~retryable:false
         ())
;;

let rec depth = function
  | `Object fields ->
    List.fold fields ~init:1 ~f:(fun maximum (_, value) ->
      Int.max maximum (1 + depth value))
  | `Array values ->
    List.fold values ~init:1 ~f:(fun maximum value -> Int.max maximum (1 + depth value))
  | `Null | `True | `False | `Number _ | `String _ -> 1
;;

let validate_limits ~max_depth ~max_bytes json =
  if max_depth < 1 || max_bytes < 0
  then invalid "JSON limits must be non-negative and depth must be positive"
  else if depth json > max_depth
  then invalid "JSON payload exceeds the maximum depth"
  else if String.length (Jsonaf.to_string json) > max_bytes
  then invalid "JSON payload exceeds the maximum encoded size"
  else Ok ()
;;

let rec canonical json =
  let open Result.Let_syntax in
  match json with
  | `Object object_fields ->
    let%bind object_fields = fields (`Object object_fields) in
    let%map object_fields =
      Result.all
        (List.map object_fields ~f:(fun (name, value) ->
           Result.map (canonical value) ~f:(fun value -> name, value)))
    in
    `Object (List.sort object_fields ~compare:(fun (a, _) (b, _) -> String.compare a b))
  | `Array values ->
    Result.map (Result.all (List.map values ~f:canonical)) ~f:(fun v -> `Array v)
  | (`Null | `True | `False | `Number _ | `String _) as value -> Ok value
;;

let canonical_string json = Result.map (canonical json) ~f:Jsonaf.to_string
